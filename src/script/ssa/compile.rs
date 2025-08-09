use crate::script::{*};
use std::collections::HashMap;

use slotmap::{SlotMap, SparseSecondaryMap};

slotmap::new_key_type! {
	pub struct VariableKey;
}


#[derive(Debug)]
pub struct CompiledResult {
	pub functions: HashMap<String, ssa::Function>,
}



pub fn compile(_error_ctx: &ErrorContext, ast: &ast::SyntaxTree) -> anyhow::Result<CompiledResult> {
	let mut functions = HashMap::<String, ssa::Function>::new();

	for item in ast.items.iter() {
		match item {
			ast::AstItem::Fn { name, body, .. } | ast::AstItem::Event { name, body, .. } => {
				let mut compile_ctx = FnCompileCtx::default();

				// TODO(pat.m): fill in function parameters?
				compile_ctx.scope_stack.push(Scope::default());

				let function = compile_function(compile_ctx, body)?;
				functions.insert(name.text.clone(), function);
			}

			_ => anyhow::bail!("Unimplemented item {item:?}"),
		}
	}

	Ok(CompiledResult {
		functions
	})
}

struct Variable {
	current_inst: SparseSecondaryMap<ssa::BasicBlockKey, ssa::InstKey>,
}

#[derive(Default)]
struct Scope {
	variables: HashMap<String, VariableKey>,
}

struct LoopContext {
	condition_block: ssa::BasicBlockKey,
	body_block: ssa::BasicBlockKey,
	exit_block: ssa::BasicBlockKey,
}

#[derive(Default)]
struct FnCompileCtx {
	scope_stack: Vec<Scope>,
	variables: SlotMap<VariableKey, Variable>,
	insts_by_hash: HashMap<u64, ssa::InstKey>,

	loop_stack: Vec<LoopContext>,
}

impl FnCompileCtx {
	fn new_variable(&mut self, name: impl Into<String>) -> VariableKey {
		let variable = self.variables.insert(Variable {
			current_inst: SparseSecondaryMap::default()
		});

		let scope = self.scope_stack.last_mut().unwrap();
		scope.variables.insert(name.into(), variable);

		variable
	}

	fn lookup_variable(&mut self, name: impl AsRef<str>) -> VariableKey {
		let name = name.as_ref();

		for scope in self.scope_stack.iter().rev() {
			if let Some(variable_key) = scope.variables.get(name) {
				return *variable_key;
			}
		}

		// TODO(pat.m): need to start thinking about spans
		panic!("{name} not found in scope");
	}

	fn write_variable(&mut self, variable: VariableKey, current_inst: ssa::InstKey, block: ssa::BasicBlockKey) {
		self.variables[variable].current_inst.insert(block, current_inst);
	}

	fn read_variable(&mut self, variable: VariableKey, builder: &mut ssa::BlockBuilder) -> ssa::InstKey {
		let variable_insts = &mut self.variables[variable].current_inst;
		if let Some(local_inst) = variable_insts.get(builder.id) {
			return *local_inst
		}

		// TODO(pat.m): search predecessors and insert phi if more than one predecessor
		unimplemented!()
	}

	fn memoize_inst(&mut self, builder: &mut ssa::BlockBuilder, inst_data: ssa::InstData) -> ssa::InstKey {
		if !inst_data.can_be_memoized() {
			return builder.add_inst(inst_data);
		}

		let hash = inst_data.hash();
		self.insts_by_hash.entry(hash)
			.or_insert_with(move || builder.add_inst(inst_data))
			.clone()
	}

	fn memoize_named_inst(&mut self, builder: &mut ssa::BlockBuilder, name: impl AsRef<str>, inst_data: ssa::InstData) -> ssa::InstKey {
		let name = name.as_ref();

		if !inst_data.can_be_memoized() {
			return builder.add_named_inst(name, inst_data);
		}

		let hash = inst_data.hash();
		self.insts_by_hash.entry(hash)
			.or_insert_with(move || builder.add_named_inst(name, inst_data))
			.clone()
	}

	fn push_scope(&mut self) {
		self.scope_stack.push(Scope::default());
	}

	fn pop_scope(&mut self) {
		self.scope_stack.pop();
	}
}


fn compile_function(mut compile_ctx: FnCompileCtx, body: &ast::AstBlock) -> anyhow::Result<ssa::Function> {
	let mut function = ssa::Function::new();
	let function_exit = function.exit;

	let mut builder = function.build_block(function.entry);

	let final_inst = compile_ast_block(&mut compile_ctx, &mut builder, body)?;

	// Jump to exit block
	if builder.id != function_exit {
		builder.jump_and_switch(function_exit);
	}

	builder.add_inst(ssa::InstData::Return { value: final_inst });

	Ok(function)
}


fn ast_expr_is_safe_to_omit(expr: &ast::AstExpression) -> bool {
	use ast::AstExpression::*;

	match expr {
		Error => true,

		Name(..) => true,
		Lookup{parent, ..} => ast_expr_is_safe_to_omit(parent),
		LiteralString(..) | LiteralInt(..) | LiteralFloat(..) | LiteralBool(..) => true,

		UnaryOp {argument, ..} => ast_expr_is_safe_to_omit(&argument),
		BinaryOp {left, right, ..} => ast_expr_is_safe_to_omit(left) || ast_expr_is_safe_to_omit(right),
		AssignOp{..} => false,

		// TODO(pat.m): pure function calls would be nice
		Call{..} => false,
		Block(block) => ast_block_is_safe_to_omit(block),

		// Declaring a binding counts as a sideeffect
		Let{..} => false,
		If{condition, then_block, else_block} => ast_expr_is_safe_to_omit(&condition)
			&& ast_block_is_safe_to_omit(then_block)
			&& else_block.as_ref().map_or(true, ast_block_is_safe_to_omit),

		// Loops are a sideeffect (for now)
		While{..} => false,
		Loop{..} => false,
	}
}

fn ast_block_is_safe_to_omit(block: &ast::AstBlock) -> bool {
	block.body.iter().all(|arg| ast_expr_is_safe_to_omit(arg))
}


fn compile_ast_expr(compile_ctx: &mut FnCompileCtx, builder: &mut ssa::BlockBuilder, expr: &ast::AstExpression) -> anyhow::Result<ssa::InstKey> {
	use ast::AstExpression as E;
	use ast::{UnaryOpKind, BinaryOpKind};

	Ok(match expr {
		E::Block(block) => {
			compile_ctx.push_scope();
			let block_value = compile_ast_block(compile_ctx, builder, block)?;
			compile_ctx.pop_scope();
			block_value
		},
		E::Call(call) => compile_ast_call(compile_ctx, builder, call)?,

		E::LiteralInt(value, ..) => compile_ctx.memoize_inst(builder, ssa::InstData::ConstInt(*value)),
		E::LiteralFloat(value, ..) => compile_ctx.memoize_inst(builder, ssa::InstData::ConstFloat(ssa::LiteralFloat(*value))),
		E::LiteralBool(value, ..) => compile_ctx.memoize_inst(builder, ssa::InstData::ConstBool(*value)),
		E::LiteralString(value, ..) => compile_ctx.memoize_inst(builder, ssa::InstData::ConstString(value.text.clone())),

		E::UnaryOp{kind, argument} => {
			let argument = compile_ast_expr(compile_ctx, builder, &argument)?;
			match kind {
				UnaryOpKind::Not => compile_ctx.memoize_inst(builder, ssa::InstData::Not(argument)),
				UnaryOpKind::Negate => compile_ctx.memoize_inst(builder, ssa::InstData::Negate(argument)),
			}
		}

		E::BinaryOp{kind, left, right} => {
			let left = compile_ast_expr(compile_ctx, builder, &left)?;
			let right = compile_ast_expr(compile_ctx, builder, &right)?;
			match kind {
				BinaryOpKind::Add => compile_ctx.memoize_inst(builder, ssa::InstData::Add(left, right)),
				BinaryOpKind::Subtract => compile_ctx.memoize_inst(builder, ssa::InstData::Sub(left, right)),
				BinaryOpKind::Multiply => compile_ctx.memoize_inst(builder, ssa::InstData::Mul(left, right)),
				BinaryOpKind::Divide => compile_ctx.memoize_inst(builder, ssa::InstData::Div(left, right)),
				BinaryOpKind::Remainder => compile_ctx.memoize_inst(builder, ssa::InstData::Rem(left, right)),

				BinaryOpKind::Lesser => compile_ctx.memoize_inst(builder, ssa::InstData::CmpLesser(left, right)),
				BinaryOpKind::Greater => compile_ctx.memoize_inst(builder, ssa::InstData::CmpGreater(left, right)),
				BinaryOpKind::LesserEqual => compile_ctx.memoize_inst(builder, ssa::InstData::CmpLesserEqual(left, right)),
				BinaryOpKind::GreaterEqual => compile_ctx.memoize_inst(builder, ssa::InstData::CmpGreaterEqual(left, right)),
				BinaryOpKind::Equal => compile_ctx.memoize_inst(builder, ssa::InstData::CmpEqual(left, right)),
				BinaryOpKind::NotEqual => compile_ctx.memoize_inst(builder, ssa::InstData::CmpNotEqual(left, right)),
			}
		}

		E::AssignOp{kind, left, right} => {
			let name = match &**left {
				E::Name(s) => &s.text,
				_ => unimplemented!(),
			};

			let variable_key = compile_ctx.lookup_variable(name);
			let right_value = compile_ast_expr(compile_ctx, builder, &right)?;

			if let ast::AssignOpKind::Assign = kind {
				compile_ctx.write_variable(variable_key, right_value, builder.id);
				return Ok(right_value);
			}

			let prev_value = compile_ctx.read_variable(variable_key, builder);

			let inst_data = match kind {
				ast::AssignOpKind::AddAssign => ssa::InstData::Add(prev_value, right_value),
				ast::AssignOpKind::SubtractAssign => ssa::InstData::Sub(prev_value, right_value),
				ast::AssignOpKind::MultiplyAssign => ssa::InstData::Mul(prev_value, right_value),
				ast::AssignOpKind::DivideAssign => ssa::InstData::Div(prev_value, right_value),
				ast::AssignOpKind::RemainderAssign => ssa::InstData::Rem(prev_value, right_value),
				ast::AssignOpKind::Assign => unreachable!(),
			};

			let new_value = compile_ctx.memoize_named_inst(builder, name, inst_data);
			compile_ctx.write_variable(variable_key, new_value, builder.id);

			new_value
		}

		E::Name(name) => {
			let variable_key = compile_ctx.lookup_variable(&name.text);
			compile_ctx.read_variable(variable_key, builder)
		}

		E::Let{name, value} => {
			let value = compile_ast_expr(compile_ctx, builder, &value)?;
			builder.function.update_inst_name(value, &name.text);

			let variable_key = compile_ctx.new_variable(&name.text);
			compile_ctx.write_variable(variable_key, value, builder.id);
			value
		}

		E::While{condition, body} => {
			let condition_block = builder.function.new_block();
			let body_block = builder.function.new_block();
			let exit_block = builder.function.new_block();

			compile_ctx.push_scope();
			compile_ctx.loop_stack.push(LoopContext { condition_block, body_block, exit_block });

			// Condition
			builder.jump_and_switch(condition_block);

			let condition = compile_ast_expr(compile_ctx, builder, condition)?;
			builder.add_named_inst("while", ssa::InstData::JumpIf {
				condition,
				then_block: body_block,
				else_block: exit_block,
			});

			// Body
			builder.switch_to_block(body_block);
			compile_ast_block(compile_ctx, builder, body)?;
			builder.jump(condition_block);

			// Exit
			builder.switch_to_block(exit_block);

			compile_ctx.loop_stack.pop();
			compile_ctx.pop_scope();

			// TODO(pat.m): would be nice to not need this
			builder.const_unit()
		}

		E::Error => anyhow::bail!("Error in AST"),
		_ => unimplemented!()
	})
}


fn compile_ast_block(compile_ctx: &mut FnCompileCtx, builder: &mut ssa::BlockBuilder, block: &ast::AstBlock) -> anyhow::Result<ssa::InstKey> {
	for (index, expression) in block.body.iter().enumerate() {
		let is_tail_expr = block.has_tail_expression && index == block.body.len() - 1;
		if is_tail_expr {
			return compile_ast_expr(compile_ctx, builder, expression)
		}

		// Expressions without side effects aren't observable unless they are tail expressions, so omit them.
		if ast_expr_is_safe_to_omit(expression) {
			continue
		}

		compile_ast_expr(compile_ctx, builder, expression)?;
	}

	// No tail expr
	Ok(builder.const_unit())
}


fn compile_ast_call(compile_ctx: &mut FnCompileCtx, builder: &mut ssa::BlockBuilder, call: &ast::AstCall) -> anyhow::Result<ssa::InstKey> {
	let call_name = compile_ast_expr(compile_ctx, builder, &call.name)?;

	let arguments = call.arguments.iter()
		.map(|arg| compile_ast_expr(compile_ctx, builder, arg))
		.collect::<Result<_, _>>()?;

	Ok(builder.add_inst(ssa::InstData::Call {
		target: call_name,
		arguments,
	}))
}




use crate::script::{*};
use std::collections::HashMap;


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

struct ScopeVariable {
	current_inst: ssa::InstKey,
}

#[derive(Default)]
struct Scope {
	variables: HashMap<String, ScopeVariable>,
}

struct LoopContext {
	condition_block: ssa::BasicBlockKey,
	body_block: ssa::BasicBlockKey,
	exit_block: ssa::BasicBlockKey,
}

#[derive(Default)]
struct FnCompileCtx {
	scope_stack: Vec<Scope>,

	loop_stack: Vec<LoopContext>,
}

impl FnCompileCtx {
	fn insert_variable(&mut self, name: impl Into<String>, current_inst: ssa::InstKey) {
		let scope = self.scope_stack.last_mut().unwrap();
		scope.variables.insert(name.into(), ScopeVariable {
			current_inst,
		});
	}

	fn find_variable_inst(&self, name: impl AsRef<str>) -> ssa::InstKey {
		let name = name.as_ref();

		for scope in self.scope_stack.iter().rev() {
			if let Some(variable) = scope.variables.get(name) {
				return variable.current_inst;
			}
		}

		// TODO(pat.m): need to start thinking about spans
		panic!("{name} not found in scope");
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
		builder.jump(function_exit);
		builder = function.build_block(function_exit);
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

		E::LiteralInt(value, ..) => builder.const_int(*value),
		E::LiteralFloat(value, ..) => builder.const_float(*value),
		E::LiteralBool(value, ..) => builder.add_inst(ssa::InstData::ConstBool(*value)),
		E::LiteralString(value, ..) => builder.add_inst(ssa::InstData::ConstString(value.text.clone())),

		E::UnaryOp{kind, argument} => {
			let argument = compile_ast_expr(compile_ctx, builder, &argument)?;
			match kind {
				UnaryOpKind::Not => builder.add_inst(ssa::InstData::Not(argument)),
				UnaryOpKind::Negate => builder.add_inst(ssa::InstData::Negate(argument)),
			}
		}

		E::BinaryOp{kind, left, right} => {
			let left = compile_ast_expr(compile_ctx, builder, &left)?;
			let right = compile_ast_expr(compile_ctx, builder, &right)?;
			match kind {
				BinaryOpKind::Add => builder.add_inst(ssa::InstData::Add(left, right)),
				BinaryOpKind::Subtract => builder.add_inst(ssa::InstData::Sub(left, right)),
				BinaryOpKind::Multiply => builder.add_inst(ssa::InstData::Mul(left, right)),
				BinaryOpKind::Divide => builder.add_inst(ssa::InstData::Div(left, right)),
				BinaryOpKind::Remainder => builder.add_inst(ssa::InstData::Rem(left, right)),

				BinaryOpKind::Lesser => builder.add_inst(ssa::InstData::CmpLesser(left, right)),
				BinaryOpKind::Greater => builder.add_inst(ssa::InstData::CmpGreater(left, right)),
				BinaryOpKind::LesserEqual => builder.add_inst(ssa::InstData::CmpLesserEqual(left, right)),
				BinaryOpKind::GreaterEqual => builder.add_inst(ssa::InstData::CmpGreaterEqual(left, right)),
				BinaryOpKind::Equal => builder.add_inst(ssa::InstData::CmpEqual(left, right)),
				BinaryOpKind::NotEqual => builder.add_inst(ssa::InstData::CmpNotEqual(left, right)),
			}
		}

		E::Name(name) => compile_ctx.find_variable_inst(&name.text),

		E::Let{name, value} => {
			let value = compile_ast_expr(compile_ctx, builder, &value)?;
			builder.function.update_inst_name(value, &name.text);
			compile_ctx.insert_variable(&name.text, value);

			// TODO(pat.m): find a way not to need this
			builder.const_unit()
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
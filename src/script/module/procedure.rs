use super::{SpannedString, Span, ErrorContext, parser};
use parser::*;

#[derive(Debug)]
pub struct Procedure {
	pub parameters: Vec<String>,
	pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
	PushNull,
	PushBool(bool),
	PushInt(i64),
	PushFloat(f64),
	PushString(String),

	Lookup(String),
	TableLookup(String),

	Discard(u32),
	CallFunction(u32),

	Add, Subtract,
	Divide, Multiply,
	Remainder,
}

impl Instruction {
	pub fn pushes(&self) -> u32 {
		use Instruction::*;
		match self {
			PushNull | PushBool(_) | PushInt(_) | PushFloat(_) | PushString(_) | Lookup(_)  => 1,
			Add | Subtract | Divide | Multiply | Remainder  => 1,
			CallFunction(_) => 1,
			_ => 0
		}
	}

	pub fn pops(&self) -> u32 {
		use Instruction::*;
		match self {
			TableLookup(_) => 1,
			Add | Subtract | Divide | Multiply | Remainder  => 2,
			Discard(count) => *count,
			CallFunction(arguments) => *arguments + 1,
			_ => 0
		}
	}

	pub fn adjusts_stack(&self) -> i64 {
		self.pushes() as i64 - self.pops() as i64
	}

	pub fn has_side_effects(&self) -> bool {
		match self {
			Instruction::CallFunction(_) => true,
			_ => false
		}
	}
}


struct BuilderCtx<'e> {
	error_ctx: &'e ErrorContext<'e>,
	instructions: Vec<Instruction>,
}

impl BuilderCtx<'_> {
	fn push(&mut self, inst: Instruction) { self.instructions.push(inst); }

	fn null(&mut self) { self.push(Instruction::PushNull); }
	fn bool(&mut self, value: bool) { self.push(Instruction::PushBool(value)); }
	fn int(&mut self, value: i64) { self.push(Instruction::PushInt(value)); }
	fn float(&mut self, value: f64) { self.push(Instruction::PushFloat(value)); }
	fn string(&mut self, value: String) { self.push(Instruction::PushString(value)); }
	fn discard(&mut self, count: u32) { build_discard(self, count); }

	fn call(&mut self, num_args: u32) { self.push(Instruction::CallFunction(num_args)); }
}


pub fn build_procedure(error_ctx: &'_ ErrorContext<'_>, parameters: &[SpannedString], block: &AstBlock) -> anyhow::Result<Procedure> {
	let mut ctx = BuilderCtx {
		error_ctx,
		instructions: Vec::new(),
	};

	build_block(&mut ctx, block);

	Ok(Procedure {
		parameters: parameters.iter().map(|s| s.text.clone()).collect(),
		instructions: ctx.instructions,
	})
}

fn build_discard(ctx: &mut BuilderCtx<'_>, count: u32) {
	let mut count = count as usize;
	let num_instructions = ctx.instructions.len();

	if count == 0 { return }

	assert!(count <= num_instructions);

	println!("=== discard ===");

	for index in (0 .. num_instructions).rev() {
		let inst = &ctx.instructions[index];

		println!("checking: {inst:?}");
		println!(" --- pushes: {}", inst.pushes());
		println!(" --- pops: {}", inst.pops());
		println!(" --- adjusts_stack: {}", inst.adjusts_stack());

		if inst.has_side_effects() {
			println!(" --- side effects");
			break
		}

		count += inst.pops() as usize;
		count -= inst.pushes() as usize;

		ctx.instructions.pop();

		if count == 0 {
			break
		}
	}

	if count > 0 {
		ctx.push(Instruction::Discard(count as u32));
	}
}

fn build_block(ctx: &mut BuilderCtx<'_>, block: &AstBlock) {
	for expression in block.body.iter() {
		match expression {
			AstExpression::Call { name, arguments } => {
				build_call_expression(ctx, name, arguments);
				ctx.discard(1);
			},

			_ => {
				build_push_expression(ctx, expression);
				ctx.discard(1);
			}
		}
	}

	// TODO(pat.m): take value from last expression
	ctx.null();
}

fn build_call_expression(ctx: &mut BuilderCtx<'_>, name: &AstExpression, arguments: &[AstExpression]) {
	for argument in arguments.iter() {
		build_push_expression(ctx, argument);
	}

	build_push_expression(ctx, name);

	ctx.call(arguments.len() as u32);
}

fn build_push_expression(ctx: &mut BuilderCtx<'_>, expr: &AstExpression) {
	match expr {
		AstExpression::Name(name) => ctx.push(Instruction::Lookup(name.text.clone())),
		AstExpression::Lookup{parent, key} => {
			build_push_expression(ctx, parent);
			ctx.push(Instruction::TableLookup(key.text.clone()))
		},
		AstExpression::LiteralBool(value, _span) => ctx.bool(*value),
		AstExpression::LiteralInt(value, _span) => ctx.int(*value),
		AstExpression::LiteralFloat(value, _span) => ctx.float(*value),
		AstExpression::LiteralString(value) => ctx.string(value.text.clone()),

		AstExpression::Block(block) => build_block(ctx, block),

		AstExpression::BinaryOp{kind, left, right} => {
			build_push_expression(ctx, left);
			build_push_expression(ctx, right);

			match kind {
				BinaryOpKind::Add => ctx.push(Instruction::Add),
				BinaryOpKind::Subtract => ctx.push(Instruction::Subtract),
				BinaryOpKind::Divide => ctx.push(Instruction::Divide),
				BinaryOpKind::Multiply => ctx.push(Instruction::Multiply),
				BinaryOpKind::Remainder => ctx.push(Instruction::Remainder),
			}
		}

		AstExpression::Call { name, arguments } => {
			build_call_expression(ctx, name, arguments);
		},

		_ => {
			ctx.error_ctx.error(expr.span(), format!("Unimplemented {expr:?}"));
			ctx.null();
		}
	}
}

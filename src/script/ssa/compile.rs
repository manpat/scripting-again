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
				let compile_ctx = FnCompileCtx::default();

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


#[derive(Default)]
struct FnCompileCtx {
	// scopes, variables, etc
}


fn compile_function(mut compile_ctx: FnCompileCtx, body: &ast::AstBlock) -> anyhow::Result<ssa::Function> {
	let mut function = ssa::Function::new();

	let then_block = function.new_block();
	let else_block = function.new_block();

	let mut builder = function.build_block(function.entry);
	let a = builder.const_int("a", 3);
	let b = builder.const_int("b", 4);
	let add = builder.add_inst("add", ssa::InstData::Add(a, b));

	builder.add_inst("if", ssa::InstData::JumpIf{
		condition: add, then_block, else_block,
	});

	let mut builder = function.build_block(then_block);
	builder.jump("jmp", builder.function.exit);

	let mut builder = function.build_block(else_block);
	builder.jump("jmp", builder.function.exit);

	Ok(function)
}
pub mod script;



fn main() -> anyhow::Result<()> {
	let content = std::fs::read_to_string("scripts/test.script")?;

	let module = script::Module::compile(&content)?;

	dbg!(module);

	Ok(())
}

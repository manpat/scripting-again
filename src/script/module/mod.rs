mod parser;
mod ast;
mod procedure;

use crate::script::{*};

use std::collections::HashMap;

#[derive(Debug)]
pub struct Module {
	functions: HashMap<String, procedure::Procedure>,
	events: HashMap<String, procedure::Procedure>,
}

impl Module {
	pub fn compile(text: &str) -> anyhow::Result<Module> {
		let error_ctx = ErrorContext::new(text);

		let token_tree = lexer::parse(&error_ctx, text)?;
		if error_ctx.has_errors() {
			anyhow::bail!("Failed to lex");
		}

		let syntax_tree = parser::parse(&error_ctx, &token_tree)?;
		if error_ctx.has_errors() {
			anyhow::bail!("Failed to parse");
		}

		let mut functions = HashMap::new();
		let mut events = HashMap::new();

		for item in syntax_tree.items {
			match item {
				ast::AstItem::Fn{name, parameters, body} => {
					if functions.contains_key(&name.text) {
						error_ctx.error(name.span, format!("Function {name} already defined"));
						continue
					}

					println!("Building {name}: {body:#?}");

					let procedure = procedure::build_procedure(&error_ctx, &parameters, &body)?;
					functions.insert(name.text.clone(), procedure);
				}

				ast::AstItem::Event{name, parameters, body} => {
					if events.contains_key(&name.text) {
						error_ctx.error(name.span, format!("Event {name} already defined"));
						continue
					}

					println!("Building {name}: {body:#?}");

					let procedure = procedure::build_procedure(&error_ctx, &parameters, &body)?;
					events.insert(name.text.clone(), procedure);
				}

				_ => anyhow::bail!("Shouldn't be able to get here"),
			}
		}

		Ok(Module {
			functions,
			events
		})
	}
}





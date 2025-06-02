mod lexer;
mod parser;
mod procedure;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Module {
	functions: HashMap<String, procedure::Procedure>,
	events: HashMap<String, procedure::Procedure>,
}

impl Module {
	pub fn compile(text: &str) -> anyhow::Result<Module> {
		let mut error_ctx = ErrorContext {
			errors: Vec::new(),
			full_text: text,
		};

		let token_tree = lexer::parse(&mut error_ctx, text)?;
		if error_ctx.has_errors() {
			anyhow::bail!("Failed to lex");
		}

		let syntax_tree = parser::parse(&mut error_ctx, &token_tree)?;
		if error_ctx.has_errors() {
			anyhow::bail!("Failed to parse");
		}

		let mut functions = HashMap::new();
		let mut events = HashMap::new();

		for item in syntax_tree.items {
			match item {
				parser::AstItem::Fn{name, parameters, body} => {
					if functions.contains_key(&name) {
						// TODO(pat.m): spans!
						error_ctx.error_at_end(format!("Function {name} already defined"));
						continue
					}

					println!("Building {name}: {body:#?}");

					let procedure = procedure::build_procedure(&mut error_ctx, &parameters, &body)?;
					functions.insert(name.clone(), procedure);
				}

				parser::AstItem::Event{name, parameters, body} => {
					if events.contains_key(&name) {
						// TODO(pat.m): spans!
						error_ctx.error_at_end(format!("Event {name} already defined"));
						continue
					}

					println!("Building {name}: {body:#?}");

					let procedure = procedure::build_procedure(&mut error_ctx, &parameters, &body)?;
					events.insert(name.clone(), procedure);
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




#[derive(Debug, Copy, Clone)]
pub struct Span {
	pub begin: usize,
	pub end: usize,
}

impl From<usize> for Span {
	fn from(o: usize) -> Span {
		Span{ begin: o, end: o + 1 }
	}
}

struct Error {
	text: String,
	span: Span,
}

pub struct ErrorContext<'s> {
	errors: Vec<Error>,
	full_text: &'s str,
}

impl ErrorContext<'_> {
	pub fn has_errors(&self) -> bool {
		!self.errors.is_empty()
	}

	pub fn error(&mut self, span: impl Into<Span>, text: impl Into<String>) {
		let text = text.into();
		let span = span.into();

		let (line, _col) = index_to_line_column(self.full_text, span.begin);
		eprintln!("error:{line}: {text}");

		self.errors.push(Error {
			text: text.into(),
			span: span.into()
		});
	}

	pub fn error_at_end(&mut self, text: impl Into<String>) {
		self.error(self.full_text.len(), text);
	}

	pub fn unexpected_character(&mut self, span: impl Into<Span>, ch: char) {
		self.error(span, format!("Unexpected '{ch}'"));
	}

	pub fn unexpected_escape(&mut self, span: impl Into<Span>, ch: u8) {
		self.error(span, format!("Unexpected '\\{ch}'"));
	}

	pub fn expected_token(&mut self, token: &lexer::Token, expected: &str) {
		self.error(token.span, format!("Expected {expected} but got token '{:?}'", token.kind));
	}

	pub fn unexpected_token(&mut self, token: &lexer::Token) {
		self.error(token.span, format!("Unexpected token '{:?}'", token.kind));
	}

	pub fn unexpected_eof(&mut self, expected: impl AsRef<str>) {
		let expected = expected.as_ref();
		self.error_at_end(format!("Expected {expected} but got end of file"));
	}
}

fn index_to_line_column(text: &str, index: usize) -> (u32, u32) {
	let prefix = &text[..index];
	let num_lines = prefix.lines().count() as u32;
	let last_line = prefix.lines().next_back().unwrap().len() as u32;
	(num_lines, last_line + 1)
}
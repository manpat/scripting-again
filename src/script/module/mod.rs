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
		let error_ctx = ErrorContext {
			errors: Default::default(),
			full_text: text,
		};

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
				parser::AstItem::Fn{name, parameters, body} => {
					if functions.contains_key(&name.text) {
						error_ctx.error(name.span, format!("Function {name} already defined"));
						continue
					}

					println!("Building {name}: {body:#?}");

					let procedure = procedure::build_procedure(&error_ctx, &parameters, &body)?;
					functions.insert(name.text.clone(), procedure);
				}

				parser::AstItem::Event{name, parameters, body} => {
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






pub struct ErrorContext<'s> {
	errors: std::cell::RefCell<Vec<SpannedString>>,
	full_text: &'s str,
}

impl ErrorContext<'_> {
	pub fn has_errors(&self) -> bool {
		!self.errors.borrow().is_empty()
	}

	pub fn error(&self, span: impl Into<Span>, text: impl Into<String>) {
		let text = text.into();
		let span = span.into();

		let (line, col) = index_to_line_column(self.full_text, span.begin);
		eprintln!("error:{line}:{col}: {text}");

		self.errors.borrow_mut().push(SpannedString {
			text: text.into(),
			span: span.into()
		});
	}

	pub fn error_at_end(&self, text: impl Into<String>) {
		self.error(self.full_text.len(), text);
	}

	pub fn unexpected_character(&self, span: impl Into<Span>, ch: char) {
		self.error(span, format!("Unexpected '{ch}'"));
	}

	pub fn unexpected_escape(&self, span: impl Into<Span>, ch: u8) {
		self.error(span, format!("Unexpected '\\{ch}'"));
	}

	pub fn expected_token(&self, token: &lexer::Token, expected: &str) {
		self.error(token.span, format!("Expected {expected} but got token '{:?}'", token.kind));
	}

	pub fn unexpected_token(&self, token: &lexer::Token) {
		self.error(token.span, format!("Unexpected token '{:?}'", token.kind));
	}

	pub fn unexpected_eof(&self, expected: impl AsRef<str>) {
		let expected = expected.as_ref();
		self.error_at_end(format!("Expected {expected} but got end of file"));
	}
}

fn index_to_line_column(text: &str, index: usize) -> (u32, u32) {
	let prefix = &text[..(index+1).min(text.len())];
	let num_lines = prefix.lines().count() as u32;
	let last_line = prefix.lines().next_back().unwrap().len() as u32;
	(num_lines, last_line)
}




#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
	pub begin: usize,
	pub end: usize,
}

impl Span {
	pub fn invalid() -> Span {
		Span{ begin: 0, end: 0 }
	}

	pub fn join(left: Span, right: Span) -> Span {
		Span {
			begin: left.begin.min(right.begin),
			end: left.end.max(right.end),
		}
	}

	pub fn extend(&mut self, other: Span) {
		self.begin = self.begin.min(other.begin);
		self.end = self.end.max(other.end);
	}
}

impl From<usize> for Span {
	fn from(o: usize) -> Span {
		Span{ begin: o, end: o + 1 }
	}
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SpannedString {
	pub text: String,
	pub span: Span,
}

impl<S> From<(S, Span)> for SpannedString
	where S: Into<String>
{
	fn from((text, span): (S, Span)) -> SpannedString {
		SpannedString {
			text: text.into(),
			span,
		}
	}
}

impl std::borrow::Borrow<str> for SpannedString {
	fn borrow(&self) -> &str {
		&self.text
	}
}

impl std::ops::Deref for SpannedString {
	type Target = str;

	fn deref(&self) -> &Self::Target {
		&self.text
	}
}

impl std::fmt::Display for SpannedString {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.text.fmt(f)
	}
}
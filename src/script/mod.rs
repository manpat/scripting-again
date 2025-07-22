mod lexer;
mod ast;

pub mod module;
pub mod execution;

pub use module::*;
pub use execution::*;


mod ssa {
	mod types;
	mod compile;
	mod formatting;

	pub use types::*;
	pub use compile::*;
}


// #[derive(Debug)]
// pub struct Instance {
// 	id: (),
// 	storage: (),
// }


// #[derive(Debug)]
// pub struct Library {
// 	// Required context
// }





pub struct ErrorContext<'s> {
	errors: std::cell::RefCell<Vec<SpannedString>>,
	full_text: &'s str,
}

impl ErrorContext<'_> {
	pub fn new(full_text: &str) -> ErrorContext<'_> {
		ErrorContext {
			errors: Default::default(),
			full_text,
		}
	}

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
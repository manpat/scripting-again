use super::{Span, ErrorContext};

#[cfg(test)]
mod tests;


#[derive(Debug, PartialEq)]
pub enum TokenKind {
	Error,

	LeftParen,
	RightParen,

	LeftBrace,
	RightBrace,

	LeftSquare,
	RightSquare,

	Semicolon,
	Colon,
	Comma,
	Dot,

	Bang,
	Percent,
	Slash,
	Asterisk,
	Minus,
	Plus,
	Equal,

	Word(String),
	LiteralString(String),
	LiteralInt(i64),
	LiteralFloat(f64),
	LiteralBool(bool),

	Event,
	Fn,
}

impl TokenKind {
	pub fn is_word(&self) -> bool {
		matches!(self, TokenKind::Word(_))
	}
	pub fn is_string_literal(&self) -> bool {
		matches!(self, TokenKind::LiteralString(_))
	}
}

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

impl Token {
	fn error(index: usize) -> Token {
		Token {
			kind: TokenKind::Error,
			span: Span{ begin: index, end: index + 1 },
		}
	}
}

#[derive(Debug)]
pub struct TokenTree {
	pub tokens: Vec<Token>,
}

pub fn parse(error_ctx: &ErrorContext<'_>, text: &str) -> anyhow::Result<TokenTree> {
	let mut tokens = Vec::new();

	let mut parser = Lexer {
		error_ctx,
		text: text.as_bytes(),
		span_offset: 0,
	};

	parser.consume_whitespace();

	while !parser.is_eof() && !parser.error_ctx.has_errors() {
		let token = match parser.peek() {
			b'(' => Token { kind: TokenKind::LeftParen, span: parser.consume(1) },
			b')' => Token { kind: TokenKind::RightParen, span: parser.consume(1) },
			b'{' => Token { kind: TokenKind::LeftBrace, span: parser.consume(1) },
			b'}' => Token { kind: TokenKind::RightBrace, span: parser.consume(1) },
			b'[' => Token { kind: TokenKind::LeftSquare, span: parser.consume(1) },
			b']' => Token { kind: TokenKind::RightSquare, span: parser.consume(1) },
			b';' => Token { kind: TokenKind::Semicolon, span: parser.consume(1) },
			b':' => Token { kind: TokenKind::Colon, span: parser.consume(1) },
			b',' => Token { kind: TokenKind::Comma, span: parser.consume(1) },
			b'.' => Token { kind: TokenKind::Dot, span: parser.consume(1) },

			b'!' => Token { kind: TokenKind::Bang, span: parser.consume(1) },
			b'%' => Token { kind: TokenKind::Percent, span: parser.consume(1) },
			b'/' => Token { kind: TokenKind::Slash, span: parser.consume(1) },
			b'*' => Token { kind: TokenKind::Asterisk, span: parser.consume(1) },
			b'-' => Token { kind: TokenKind::Minus, span: parser.consume(1) },
			b'+' => Token { kind: TokenKind::Plus, span: parser.consume(1) },
			b'=' => Token { kind: TokenKind::Equal, span: parser.consume(1) },

			b'"' => parser.parse_string(),
			c if c.is_ascii_digit() => parser.parse_number(),
			c if c.is_ascii_alphabetic() => parser.parse_word(),

			c => {
				parser.consume(1);
				parser.error_ctx.unexpected_character(parser.span_offset, c as char);
				Token::error(parser.span_offset)
			}
		};

		tokens.push(token);

		parser.consume_whitespace();
	}

	Ok(TokenTree{ tokens })
}



struct Lexer<'s, 'e> {
	error_ctx: &'e ErrorContext<'e>,
	text: &'s [u8],
	span_offset: usize,
}

impl<'s, 'e> Lexer<'s, 'e> {
	fn is_eof(&self) -> bool {
		self.text.is_empty()
	}

	fn consume_whitespace(&mut self) {
		while !self.is_eof() {
			match self.peek() {
				c if c.is_ascii_whitespace() => {
					self.consume_until(|c| !c.is_ascii_whitespace());
				}

				b'/' => {
					match self.text.get(1) {
						Some(b'/') => {
							self.consume(2);
							self.consume_until(|c| [b'\n', b'\r'].contains(&c));
						}

						Some(b'*') => {
							self.consume(2);
							self.consume_pairs_until(|cs| cs == b"*/");
							self.consume(2);
						}

						_ => return
					}
				}

				_ => return
			}
		}
	}

	fn consume_until(&mut self, mut f: impl FnMut(u8) -> bool) -> Span {
		let index = self.text.iter()
			.position(move |&c| f(c))
			.unwrap_or(self.text.len());

		self.consume(index)
	}

	fn consume_pairs_until(&mut self, mut f: impl FnMut(&[u8; 2]) -> bool) -> Span {
		let start_index = self.text.windows(2)
			.position(move |cs| {
				let &[a, b] = cs else { unreachable!() };
				f(&[a, b])
			});

		match start_index {
			Some(start_index) => self.consume(start_index),
			None => self.consume(self.text.len()),
		}
	}

	fn consume(&mut self, count: usize) -> Span {
		assert!(count <= self.text.len());

		let span = Span {
			begin: self.span_offset,
			end: self.span_offset + count,
		};

		self.span_offset += count;
		self.text = &self.text[count..];

		span
	}

	fn peek(&self) -> u8 {
		self.text[0]
	}

	fn take(&mut self) -> u8 {
		let c = self.text[0];
		self.consume(1);
		c
	}

	fn parse_number(&mut self) -> Token { unimplemented!() }

	fn parse_string(&mut self) -> Token {
		assert!(self.peek() == b'"');

		let span_start = self.span_offset;

		self.consume(1);

		let mut text = Vec::new();

		while !self.is_eof() {
			match self.take() {
				b'\\' => match self.take() {
					b'n' => text.push(b'\n'),
					b't' => text.push(b'\t'),
					b'\\' => text.push(b'\\'),
					b'"' => text.push(b'"'),
					c => {
						self.error_ctx.unexpected_escape(span_start, c);
						return Token::error(span_start);
					},
				}

				b'"' => return Token {
					kind: TokenKind::LiteralString(String::from_utf8(text).unwrap()),
					span: Span {
						begin: span_start,
						end: self.span_offset,
					},
				},

				c => text.push(c)
			}
		}

		self.error_ctx.error(span_start, "Missing terminal '\"'");
		Token::error(span_start)
	}

	fn parse_word(&mut self) -> Token {
		assert!(self.peek().is_ascii_alphabetic());

		let end_index = self.text.iter()
			.position(|&c| !c.is_ascii_alphanumeric() && c != b'_')
			.unwrap_or(self.text.len());

		let kind = match &self.text[..end_index] {
			b"event" => TokenKind::Event,
			b"fn" => TokenKind::Fn,

			b"true" => TokenKind::LiteralBool(true),
			b"false" => TokenKind::LiteralBool(false),

			text => {
				let text = String::from_utf8(text.to_vec()).unwrap();
				TokenKind::Word(text)
			}
		};

		Token {
			kind,
			span: self.consume(end_index),
		}
	}
}
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

	PercentAssign,
	SlashAssign,
	AsteriskAssign,
	MinusAssign,
	PlusAssign,

	Assign,

	Lesser,
	Greater,
	LesserEqual,
	GreaterEqual,
	Equal,
	NotEqual,

	Word(String),
	LiteralString(String),
	LiteralInt(i64),
	LiteralFloat(f64),
	LiteralBool(bool),

	Event,
	Fn,

	Let,
	If,
	Else,
	Loop,
	While,
	For,
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
		let token = match parser.text {
			[b'(', ..] => Token { kind: TokenKind::LeftParen, span: parser.consume(1) },
			[b')', ..] => Token { kind: TokenKind::RightParen, span: parser.consume(1) },
			[b'{', ..] => Token { kind: TokenKind::LeftBrace, span: parser.consume(1) },
			[b'}', ..] => Token { kind: TokenKind::RightBrace, span: parser.consume(1) },
			[b'[', ..] => Token { kind: TokenKind::LeftSquare, span: parser.consume(1) },
			[b']', ..] => Token { kind: TokenKind::RightSquare, span: parser.consume(1) },
			[b';', ..] => Token { kind: TokenKind::Semicolon, span: parser.consume(1) },
			[b':', ..] => Token { kind: TokenKind::Colon, span: parser.consume(1) },
			[b',', ..] => Token { kind: TokenKind::Comma, span: parser.consume(1) },
			[b'.', ..] => Token { kind: TokenKind::Dot, span: parser.consume(1) },

			[b'%', b'=', ..] => Token { kind: TokenKind::PercentAssign, span: parser.consume(2) },
			[b'/', b'=', ..] => Token { kind: TokenKind::SlashAssign, span: parser.consume(2) },
			[b'*', b'=', ..] => Token { kind: TokenKind::AsteriskAssign, span: parser.consume(2) },
			[b'-', b'=', ..] => Token { kind: TokenKind::MinusAssign, span: parser.consume(2) },
			[b'+', b'=', ..] => Token { kind: TokenKind::PlusAssign, span: parser.consume(2) },

			[b'=', b'=', ..] => Token { kind: TokenKind::Equal, span: parser.consume(2) },
			[b'!', b'=', ..] => Token { kind: TokenKind::NotEqual, span: parser.consume(2) },
			[b'<', b'=', ..] => Token { kind: TokenKind::LesserEqual, span: parser.consume(2) },
			[b'>', b'=', ..] => Token { kind: TokenKind::GreaterEqual, span: parser.consume(2) },

			[b'<', ..] => Token { kind: TokenKind::Lesser, span: parser.consume(1) },
			[b'>', ..] => Token { kind: TokenKind::Greater, span: parser.consume(1) },

			[b'!', ..] => Token { kind: TokenKind::Bang, span: parser.consume(1) },
			[b'%', ..] => Token { kind: TokenKind::Percent, span: parser.consume(1) },
			[b'/', ..] => Token { kind: TokenKind::Slash, span: parser.consume(1) },
			[b'*', ..] => Token { kind: TokenKind::Asterisk, span: parser.consume(1) },
			[b'-', ..] => Token { kind: TokenKind::Minus, span: parser.consume(1) },
			[b'+', ..] => Token { kind: TokenKind::Plus, span: parser.consume(1) },
			[b'=', ..] => Token { kind: TokenKind::Assign, span: parser.consume(1) },

			[b'"', ..] => parser.parse_string(),
			[c, ..] if c.is_ascii_digit() => parser.parse_number(),
			[c, ..] if c.is_ascii_alphabetic() => parser.parse_word(),

			[c, ..] => {
				parser.consume(1);
				parser.error_ctx.unexpected_character(parser.span_offset, *c as char);
				Token::error(parser.span_offset)
			}

			[] => unreachable!(),
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
							let start_span = self.consume(2);
							self.consume_pairs_until(|cs| cs == b"*/");
							if !self.text.starts_with(b"*/") {
								self.error_ctx.error(start_span, format!("Comment block not terminated"));
								return
							}

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

	fn find_word_boundary_from(&self, start: usize) -> usize {
		if start >= self.text.len() {
			return self.text.len()
		}

		self.text[start..].iter()
			.position(|c| !c.is_ascii_alphanumeric())
			.map(|index| index + start)
			.unwrap_or(self.text.len())
	}

	fn parse_float(&mut self, dot_index: usize) -> Token {
		let end_index = self.find_word_boundary_from(dot_index + 1);
		let text = str::from_utf8(&self.text[..end_index]).unwrap();

		if end_index == dot_index + 1 {
			let span = self.consume(end_index);
			self.error_ctx.error(span, format!("'{text}' is not a valid float literal"));
			return Token::error(span.begin);
		}

		let Ok(value) = text.parse() else {
			let span = self.consume(end_index);
			self.error_ctx.error(span, format!("'{text}' is not a valid float literal"));
			return Token::error(span.begin);
		};

		Token {
			kind: TokenKind::LiteralFloat(value),
			span: self.consume(end_index),
		}
	}

	fn parse_radix_int_of_length(&mut self, radix: u32, start: usize, end: usize) -> Token {
		let text = str::from_utf8(&self.text[start..end]).unwrap();
		let Ok(value) = i64::from_str_radix(text, radix) else {
			let span = self.consume(end);
			self.error_ctx.error(span, format!("'{text}' is not a valid radix-{radix} integer"));
			return Token::error(start);
		};

		Token {
			kind: TokenKind::LiteralInt(value),
			span: self.consume(end),
		}
	}

	fn parse_number(&mut self) -> Token {
		assert!(self.peek().is_ascii_digit());

		let first_boundary = self.find_word_boundary_from(0);
		if self.text.get(first_boundary) == Some(&b'.') {
			return self.parse_float(first_boundary)
		}

		let mut radix = 10;
		let mut skip = 0;

		if self.text.starts_with(b"0x") {
			radix = 16;
			skip = 2;
		} else if self.text.starts_with(b"0b") {
			radix = 2;
			skip = 2;
		}

		self.parse_radix_int_of_length(radix, skip, first_boundary)
	}

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

			b"let" => TokenKind::Let,
			b"if" => TokenKind::If,
			b"else" => TokenKind::Else,
			b"loop" => TokenKind::Loop,
			b"while" => TokenKind::While,
			b"for" => TokenKind::For,

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
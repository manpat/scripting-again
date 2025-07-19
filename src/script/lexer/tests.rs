use super::*;

use crate::script::*;

fn test_lex(text: &str) -> Result<Vec<Token>, Vec<SpannedString>> {
	let error_ctx = ErrorContext::new(text);
	let token_tree = lexer::parse(&error_ctx, text).unwrap();
	if error_ctx.has_errors() {
		Err(error_ctx.errors.into_inner())
	} else {
		Ok(token_tree.tokens)
	}
}


macro_rules! assert_lexes {
	($text:expr, Ok( $($kinds:expr),* )) => {
		let text = $text;
		let expected_str = stringify!( $($kinds),* );

		let expected: &[TokenKind] = &[ $( $kinds ),* ];

		match test_lex(text) {
			Ok(tokens) => {
				let actual: Vec<_> = tokens.into_iter().map(|token| token.kind).collect();
				assert_eq!(&expected[..], &actual[..]);
			}

			Err(_) => panic!("'{text}' failed to produce [{expected_str}] - produced an error instead")
		};
	};

	($text:expr, Err) => {
		let text = $text;
		match test_lex(text) {
			Err(_) => {},
			x => panic!("'{text}' failed to produce an error. got '{x:?}'"),
		}
	};
}


#[test]
fn empty() {
	assert_lexes!("", Ok());
}

#[test]
fn basic() {
	assert_lexes!("(", Ok(TokenKind::LeftParen));
	assert_lexes!(")", Ok(TokenKind::RightParen));
	assert_lexes!("{", Ok(TokenKind::LeftBrace));
	assert_lexes!("}", Ok(TokenKind::RightBrace));
	assert_lexes!("[", Ok(TokenKind::LeftSquare));
	assert_lexes!("]", Ok(TokenKind::RightSquare));

	assert_lexes!(";", Ok(TokenKind::Semicolon));
	assert_lexes!(":", Ok(TokenKind::Colon));
	assert_lexes!(",", Ok(TokenKind::Comma));
	assert_lexes!(".", Ok(TokenKind::Dot));

	assert_lexes!("!", Ok(TokenKind::Bang));
	assert_lexes!("%", Ok(TokenKind::Percent));
	assert_lexes!("/", Ok(TokenKind::Slash));
	assert_lexes!("*", Ok(TokenKind::Asterisk));
	assert_lexes!("-", Ok(TokenKind::Minus));
	assert_lexes!("+", Ok(TokenKind::Plus));
	assert_lexes!("=", Ok(TokenKind::Equal));
}

#[test]
fn words() {
	assert_lexes!("event", Ok(TokenKind::Event));
	assert_lexes!("fn", Ok(TokenKind::Fn));
	assert_lexes!("true", Ok(TokenKind::LiteralBool(true)));
	assert_lexes!("false", Ok(TokenKind::LiteralBool(false)));
	assert_lexes!("fun", Ok(TokenKind::Word("fun".into())));
	assert_lexes!("efn", Ok(TokenKind::Word("efn".into())));
	assert_lexes!("e fn", Ok(TokenKind::Word("e".into()), TokenKind::Fn));
	assert_lexes!("e.fn", Ok(TokenKind::Word("e".into()), TokenKind::Dot, TokenKind::Fn));
}

#[test]
fn comments() {
	assert_lexes!("//", Ok());
	assert_lexes!("//fn", Ok());
	assert_lexes!("fn//fn", Ok(TokenKind::Fn));
	assert_lexes!("fn/*fn*/", Ok(TokenKind::Fn));
	assert_lexes!("fn/*fn", Err);
	assert_lexes!("fn/ *fn", Ok(TokenKind::Fn, TokenKind::Slash, TokenKind::Asterisk, TokenKind::Fn));
	assert_lexes!("/*\nfoobar*/", Ok());
	assert_lexes!("/*\nfoobar\n*/", Ok());
	assert_lexes!("/*/**/", Ok());
	assert_lexes!("/*//*/", Ok());
	assert_lexes!("// /*\n*/", Ok(TokenKind::Asterisk, TokenKind::Slash));
}

#[test]
fn strings() {
	assert_lexes!("\"\"", Ok(TokenKind::LiteralString("".into())));
	assert_lexes!("\"abc\"", Ok(TokenKind::LiteralString("abc".into())));
	assert_lexes!("\"abc\n\"", Ok(TokenKind::LiteralString("abc\n".into())));
	assert_lexes!("\"ab\\\"c\"", Ok(TokenKind::LiteralString("ab\"c".into())));
	assert_lexes!("\"ab\\nc\"", Ok(TokenKind::LiteralString("ab\nc".into())));
	assert_lexes!("\"ab\\tc\"", Ok(TokenKind::LiteralString("ab\tc".into())));
	assert_lexes!("\"ab\\\\c\"", Ok(TokenKind::LiteralString("ab\\c".into())));
	assert_lexes!("\"", Err);
	assert_lexes!("\"\\5\"", Err);
}

#[test]
fn numbers() {
	assert_lexes!("1", Ok(TokenKind::LiteralInt(1)));
	assert_lexes!("1a", Err);
	assert_lexes!("1 0", Ok(TokenKind::LiteralInt(1), TokenKind::LiteralInt(0)));
	assert_lexes!("1 .0", Ok(TokenKind::LiteralInt(1), TokenKind::Dot, TokenKind::LiteralInt(0)));
	assert_lexes!("1.", Err);
	assert_lexes!("1.0", Ok(TokenKind::LiteralFloat(1.0)));
	assert_lexes!("0xFF", Ok(TokenKind::LiteralInt(255)));
	assert_lexes!("0xff", Ok(TokenKind::LiteralInt(255)));
	assert_lexes!("0b101", Ok(TokenKind::LiteralInt(5)));
	assert_lexes!("0xFFFFFFFF", Ok(TokenKind::LiteralInt(0xffffffff)));
	assert_lexes!("0xFFFFFFFFffffffff", Err); // Too big
	assert_lexes!("0x7FFFFFFFffffffff", Ok(TokenKind::LiteralInt(0x7FFFFFFFffffffff)));

}
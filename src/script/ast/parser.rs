use crate::script::{Span, SpannedString, ErrorContext, ast, lexer};

use ast::*;
use lexer::{Token, TokenTree, TokenKind};

pub fn parse(error_ctx: &ErrorContext<'_>, tree: &TokenTree) -> anyhow::Result<SyntaxTree> {
	let mut parser = Parser { error_ctx, tokens: &tree.tokens, spans: Vec::new(), prev_span: Span::invalid() };
	let mut items = Vec::new();

	while !parser.error_ctx.has_errors() && !parser.tokens.is_empty() {
		items.push(parser.parse_top_level_item());
	}

	Ok(SyntaxTree { items })
}

// https://docs.python.org/3/reference/grammar.html
// https://www.lua.org/manual/5.3/manual.html

struct Parser<'e, 't> {
	error_ctx: &'e ErrorContext<'e>,
	tokens: &'t [Token],
	spans: Vec<Span>,
	prev_span: Span,
}

impl<'e, 't> Parser<'e, 't> {
	fn mark_prev_span(&mut self) {
		self.spans.push(self.prev_span);
	}

	fn extend_span(&mut self, span: Span) {
		self.prev_span = span;
		if let Some(top) = self.spans.last_mut() {
			top.extend(span);
		}
	}

	fn end_span(&mut self) -> Span {
		self.spans.pop().unwrap()
	}

	fn peek_next(&mut self) -> Option<&'t Token> {
		self.tokens.first()
	}

	fn take_next(&mut self) -> Option<&'t Token> {
		self.tokens.split_off_first()
			.inspect(|token| self.extend_span(token.span))
	}

	fn accept(&mut self, kind: &TokenKind) -> bool {
		let Some(token) = self.peek_next() else {
			return false;
		};

		if token.kind == *kind {
			self.take_next();
			true
		} else {
			false
		}
	}

	fn map_accept<T>(&mut self, f: impl FnOnce(&TokenKind) -> Option<T>) -> Option<T> {
		self.peek_next()
			.and_then(|token| f(&token.kind))
			.inspect(|_| { self.take_next(); })
	}

	fn expect(&mut self, kind: &TokenKind) -> bool {
		let Some(token) = self.take_next() else {
			self.error_ctx.unexpected_eof(format!("{kind:?}"));
			return false;
		};

		if token.kind != *kind {
			self.error_ctx.unexpected_token(token);
			return false;
		};

		true
	}

	fn expect_word(&mut self) -> SpannedString {
		let Some(token) = self.take_next() else {
			self.error_ctx.unexpected_eof("word");
			return SpannedString{ text: String::new(), span: Span::invalid() };
		};

		let TokenKind::Word(word) = &token.kind else {
			self.error_ctx.expected_token(token, "word");
			return SpannedString{ text: String::new(), span: Span::invalid() };
		};

		SpannedString::from((word.as_str(), token.span))
	}

	fn parse_top_level_item(&mut self) -> AstItem {
		let Some(token) = self.take_next() else {
			self.error_ctx.unexpected_eof("top level item");
			return AstItem::Error;
		};

		match token.kind {
			TokenKind::Event => {
				let name = self.expect_word();

				self.expect(&TokenKind::LeftParen);
				let parameters = self.parse_parameters();

				self.expect(&TokenKind::LeftBrace);
				let body = self.parse_block();

				AstItem::Event {
					name,
					parameters,
					body,
				}
			}

			TokenKind::Fn => {
				let name = self.expect_word();

				self.expect(&TokenKind::LeftParen);
				let parameters = self.parse_parameters();

				self.expect(&TokenKind::LeftBrace);
				let body = self.parse_block();

				AstItem::Fn {
					name,
					parameters,
					body,
				}
			}

			_ => {
				self.error_ctx.unexpected_token(token);
				AstItem::Error
			}
		}
	}

	fn parse_block(&mut self) -> AstBlock {
		self.mark_prev_span();

		let mut statements: Vec<AstExpression> = Vec::new();
		let mut has_unterminated_expression = false;

		while !self.accept(&TokenKind::RightBrace) && !self.error_ctx.has_errors() {
			// If the last expression is unterminated, and _wasn't_ something ending in a block, then we require a semicolon.
			if has_unterminated_expression
				&& let Some(expr) = statements.last()
				&& expr.requires_separator()
			{
				self.expect(&TokenKind::Semicolon);
				has_unterminated_expression = false;
				continue;
			}

			if self.accept(&TokenKind::Semicolon) {
				has_unterminated_expression = false;
				continue;
			}

			statements.push(self.parse_statement());
			has_unterminated_expression = true;
		}

		AstBlock {
			body: statements,
			has_tail_expression: has_unterminated_expression,
			span: self.end_span(),
		}
	}

	fn parse_parameters(&mut self) -> Vec<SpannedString> {
		let mut parameters = Vec::new();

		while !self.accept(&TokenKind::RightParen) && !self.error_ctx.has_errors() {
			parameters.push(self.expect_word());
			if !self.accept(&TokenKind::Comma) {
				self.expect(&TokenKind::RightParen);
				break
			}
		}

		parameters
	}

	fn parse_call_arguments(&mut self) -> Vec<AstExpression> {
		let mut expressions = Vec::new();

		while !self.accept(&TokenKind::RightParen) && !self.error_ctx.has_errors() {
			expressions.push(self.parse_expression());
			if !self.accept(&TokenKind::Comma) {
				self.expect(&TokenKind::RightParen);
				break
			}
		}

		expressions
	}

	fn parse_statement(&mut self) -> AstExpression {
		if self.accept(&TokenKind::Let) {
			let name = self.expect_word();
			self.expect(&TokenKind::Assign);
			return AstExpression::Let {
				name,
				value: Box::new(self.parse_expression()),
			};
		}

		// TODO(pat.m): should be expression
		if self.accept(&TokenKind::If) {
			let condition = Box::new(self.parse_expression());

			self.expect(&TokenKind::LeftBrace);
			let then_block = self.parse_block();
			let mut else_block = None;

			if self.accept(&TokenKind::Else) {
				self.expect(&TokenKind::LeftBrace);
				else_block = Some(self.parse_block());
			}
			// TODO(pat.m): else-if chains

			return AstExpression::If { condition, then_block, else_block };
		}

		if self.accept(&TokenKind::While) {
			let condition = Box::new(self.parse_expression());

			self.expect(&TokenKind::LeftBrace);

			return AstExpression::While {
				condition,
				body: self.parse_block(),
			};
		}

		// TODO(pat.m): for

		if self.accept(&TokenKind::Loop) {
			self.expect(&TokenKind::LeftBrace);
			return AstExpression::Loop(self.parse_block());
		}

		self.parse_expression()
	}

	fn parse_expression(&mut self) -> AstExpression {
		self.parse_expression_assignment()
	}

	// Not associative
	fn parse_expression_assignment(&mut self) -> AstExpression {
		let left = self.parse_expression_logical_or();

		if let Some(op_kind) = self.map_accept(|kind| match kind {
			TokenKind::Assign => Some(AssignOpKind::Assign),
			TokenKind::PlusAssign => Some(AssignOpKind::AddAssign),
			TokenKind::MinusAssign => Some(AssignOpKind::SubtractAssign),
			TokenKind::AsteriskAssign => Some(AssignOpKind::MultiplyAssign),
			TokenKind::SlashAssign => Some(AssignOpKind::DivideAssign),
			TokenKind::PercentAssign => Some(AssignOpKind::RemainderAssign),
			_ => None,
		})
		{
			return AstExpression::AssignOp {
				kind: op_kind,
				left: Box::new(left),
				right: Box::new(self.parse_expression_logical_or()),
			}
		}

		left
	}

	fn parse_expression_logical_or(&mut self) -> AstExpression {
		self.parse_expression_logical_and()
	}

	fn parse_expression_logical_and(&mut self) -> AstExpression {
		self.parse_expression_comparison()
	}

	// Not associative
	fn parse_expression_comparison(&mut self) -> AstExpression {
		let left = self.parse_expression_binary_add();

		if let Some(op_kind) = self.map_accept(|kind| match kind {
			TokenKind::Equal => Some(BinaryOpKind::Equal),
			TokenKind::NotEqual => Some(BinaryOpKind::NotEqual),
			TokenKind::Lesser => Some(BinaryOpKind::Lesser),
			TokenKind::Greater => Some(BinaryOpKind::Greater),
			TokenKind::LesserEqual => Some(BinaryOpKind::LesserEqual),
			TokenKind::GreaterEqual => Some(BinaryOpKind::GreaterEqual),
			_ => None,
		})
		{
			return AstExpression::BinaryOp {
				kind: op_kind,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_add()),
			}
		};

		left
	}

	fn parse_expression_binary_add(&mut self) -> AstExpression {
		let left = self.parse_expression_binary_multiply();

		if self.accept(&TokenKind::Plus) {
			return AstExpression::BinaryOp {
				kind: BinaryOpKind::Add,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_add()),
			}
		}

		if self.accept(&TokenKind::Minus) {
			return AstExpression::BinaryOp {
				kind: BinaryOpKind::Subtract,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_add()),
			}
		}

		left
	}

	fn parse_expression_binary_multiply(&mut self) -> AstExpression {
		let left = self.parse_expression_unary();

		if self.accept(&TokenKind::Asterisk) {
			return AstExpression::BinaryOp {
				kind: BinaryOpKind::Multiply,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_multiply()),
			}
		}

		if self.accept(&TokenKind::Slash) {
			return AstExpression::BinaryOp {
				kind: BinaryOpKind::Divide,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_multiply()),
			}
		}

		if self.accept(&TokenKind::Percent) {
			return AstExpression::BinaryOp {
				kind: BinaryOpKind::Remainder,
				left: Box::new(left),
				right: Box::new(self.parse_expression_binary_multiply()),
			}
		}

		left
	}

	fn parse_expression_unary(&mut self) -> AstExpression {
		if self.accept(&TokenKind::Bang) {
			return AstExpression::UnaryOp {
				kind: UnaryOpKind::Not,
				argument: Box::new(self.parse_expression_unary())
			}
		}

		if self.accept(&TokenKind::Minus) {
			return AstExpression::UnaryOp {
				kind: UnaryOpKind::Negate,
				argument: Box::new(self.parse_expression_unary())
			}
		}

		self.parse_expression_primary()
	}

	fn parse_expression_primary(&mut self) -> AstExpression {
		let mut expr = self.parse_expression_leaf();

		loop {
			if self.accept(&TokenKind::Dot) {
				expr = AstExpression::Lookup {
					parent: Box::new(expr),
					key: self.expect_word(),
				};

				continue
			}

			if self.accept(&TokenKind::LeftParen) {
				expr = AstExpression::Call( AstCall {
					name: Box::new(expr),
					arguments: self.parse_call_arguments()
				});

				continue
			}

			// array indexing

			break
		}

		expr
	}

	fn parse_expression_leaf(&mut self) -> AstExpression {
		let Some(token) = self.take_next() else {
			self.error_ctx.unexpected_eof("expression");
			return AstExpression::Error;
		};

		match &token.kind {
			TokenKind::LeftParen => {
				let expression = self.parse_expression();
				self.expect(&TokenKind::RightParen);
				expression
			}

			TokenKind::LeftBrace => {
				AstExpression::Block(self.parse_block())
			}

			TokenKind::Word(word) => {
				AstExpression::Name((word.clone(), token.span).into())
			}

			TokenKind::LiteralInt(value) => AstExpression::LiteralInt(*value, token.span),
			TokenKind::LiteralFloat(value) => AstExpression::LiteralFloat(*value, token.span),
			TokenKind::LiteralString(value) => AstExpression::LiteralString((value.clone(), token.span).into()),
			TokenKind::LiteralBool(value) => AstExpression::LiteralBool(*value, token.span),

			_ => {
				self.error_ctx.unexpected_token(token);
				AstExpression::Error
			}
		}
	}
}
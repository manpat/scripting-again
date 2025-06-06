use super::{SpannedString, Span};


#[derive(Debug)]
pub struct SyntaxTree {
	pub items: Vec<AstItem>
}


#[derive(Debug)]
pub enum AstItem {
	Error,

	Fn {
		name: SpannedString,
		parameters: Vec<SpannedString>,
		body: AstBlock,
	},

	Event {
		name: SpannedString,
		parameters: Vec<SpannedString>,
		body: AstBlock,
	},
}

#[derive(Debug)]
pub struct AstBlock {
	pub body: Vec<AstExpression>,
	pub span: Span,
	pub has_tail_expression: bool,
}

#[derive(Debug)]
pub enum UnaryOpKind {
	Not,
}

#[derive(Debug)]
pub enum BinaryOpKind {
	Add,
	Subtract,
	Multiply,
	Divide,
	Remainder,
}


#[derive(Debug)]
pub enum AstExpression {
	Error,

	Name(SpannedString),
	Lookup {
		parent: Box<AstExpression>,
		key: SpannedString,
	},

	LiteralString(SpannedString),
	LiteralInt(i64, Span),
	LiteralFloat(f64, Span),
	LiteralBool(bool, Span),

	Block(AstBlock),

	UnaryOp {
		kind: UnaryOpKind,
		argument: Box<AstExpression>,
	},

	BinaryOp {
		kind: BinaryOpKind,
		left: Box<AstExpression>,
		right: Box<AstExpression>,
	},

	Call {
		name: Box<AstExpression>,
		arguments: Vec<AstExpression>,
	},
}

impl AstExpression {
	pub fn span(&self) -> Span {
		use AstExpression::*;

		match self {
			Error => Span::invalid(),

			Name(s) => s.span,
			Lookup{parent, key} => Span::join(parent.span(), key.span),

			LiteralString(s) => s.span,
			LiteralInt(_, span) => *span,
			LiteralFloat(_, span) => *span,
			LiteralBool(_, span) => *span,

			Block(block) => block.span,

			UnaryOp{..} => unimplemented!(),

			BinaryOp{left, right, ..} => Span::join(left.span(), right.span()),
			Call{name, arguments} if arguments.is_empty() => name.span(),
			Call{name, arguments} => Span::join(name.span(), arguments.last().unwrap().span()),
		}
	}
}
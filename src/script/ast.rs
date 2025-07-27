mod parser;

pub use parser::parse;

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
pub struct AstCall {
	pub name: Box<AstExpression>,
	pub arguments: Vec<AstExpression>,
}

#[derive(Debug)]
pub enum UnaryOpKind {
	Not,
	Negate,
}

#[derive(Debug)]
pub enum BinaryOpKind {
	Add,
	Subtract,
	Multiply,
	Divide,
	Remainder,

	Lesser,
	Greater,
	LesserEqual,
	GreaterEqual,
	Equal,
	NotEqual,
}

#[derive(Debug)]
pub enum AssignOpKind {
	Assign,
	AddAssign,
	SubtractAssign,
	MultiplyAssign,
	DivideAssign,
	RemainderAssign,
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

	AssignOp {
		kind: AssignOpKind,
		left: Box<AstExpression>,
		right: Box<AstExpression>,
	},

	Call(AstCall),

	Let {
		name: SpannedString,
		value: Box<AstExpression>,
	},

	If {
		condition: Box<AstExpression>,
		then_block: AstBlock,
		else_block: Option<AstBlock>,
	},

	Loop(AstBlock),

	While {
		condition: Box<AstExpression>,
		body: AstBlock,
	}

	// TODO(pat.m): For
}

impl AstExpression {
	pub fn requires_separator(&self) -> bool {
		use AstExpression::*;

		match self {
			// All expressions terminating in a block should be unambiguous
			Block{..} | If{..} | Loop{..} | While{..} => false,
			_ => true,
		}
	}

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
			Call(AstCall{name, arguments}) if arguments.is_empty() => name.span(),
			Call(AstCall{name, arguments}) => Span::join(name.span(), arguments.last().unwrap().span()),

			_ => unimplemented!(),
		}
	}
}
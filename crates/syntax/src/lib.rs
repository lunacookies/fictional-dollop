use std::{fmt, mem};

pub type SyntaxTree = eventree::SyntaxTree<TreeConfig>;
pub type SyntaxBuilder = eventree::SyntaxBuilder<TreeConfig>;
pub type SyntaxNode = eventree::SyntaxNode<TreeConfig>;
pub type SyntaxToken = eventree::SyntaxToken<TreeConfig>;
pub type SyntaxElement = eventree::SyntaxElement<TreeConfig>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum NodeKind {
	SourceFile,
	Strukt,
	Field,
	Function,
	VarStmt,
	BinaryExpr,
	BlockExpr,
	CallExpr,
	VariableExpr,
	IntegerExpr,
	NamedTy,
	PointerTy,
	PrimitiveTy,
	ForeignPath,
	LocalPath,
	Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum TokenKind {
	StructKw,
	FnKw,
	VarKw,
	U32Kw,
	Ident,
	Integer,
	LParen,
	RParen,
	LBrace,
	RBrace,
	Eq,
	EqEq,
	BangEq,
	Plus,
	Hyphen,
	Star,
	Slash,
	Percent,
	Caret,
	Lt,
	Gt,
	LtEq,
	GtEq,
	LtLt,
	GtGt,
	Pipe,
	And,
	PipePipe,
	AndAnd,
	Comma,
	Dot,
	Whitespace,
	Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TreeConfig {}

unsafe impl eventree::SyntaxKind for NodeKind {
	fn to_raw(self) -> u16 {
		self as u16
	}

	unsafe fn from_raw(raw: u16) -> Self {
		mem::transmute(raw as u8)
	}
}

unsafe impl eventree::SyntaxKind for TokenKind {
	fn to_raw(self) -> u16 {
		self as u16
	}

	unsafe fn from_raw(raw: u16) -> Self {
		mem::transmute(raw as u8)
	}
}

impl eventree::TreeConfig for TreeConfig {
	type NodeKind = NodeKind;
	type TokenKind = TokenKind;
}

impl fmt::Display for TokenKind {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let s = match self {
			TokenKind::StructKw => "`struct`",
			TokenKind::FnKw => "`fn`",
			TokenKind::VarKw => "`var`",
			TokenKind::U32Kw => "`u32`",
			TokenKind::Ident => "identifier",
			TokenKind::Integer => "integer literal",
			TokenKind::LParen => "`(`",
			TokenKind::RParen => "`)`",
			TokenKind::LBrace => "`{`",
			TokenKind::RBrace => "`}`",
			TokenKind::Eq => "`=`",
			TokenKind::EqEq => "`==`",
			TokenKind::BangEq => "`!=`",
			TokenKind::Plus => "`+`",
			TokenKind::Hyphen => "`-`",
			TokenKind::Star => "`*`",
			TokenKind::Slash => "`/`",
			TokenKind::Percent => "`%`",
			TokenKind::Caret => "`^`",
			TokenKind::Lt => "`<`",
			TokenKind::Gt => "`>`",
			TokenKind::LtEq => "`<=`",
			TokenKind::GtEq => "`>=`",
			TokenKind::LtLt => "`<<`",
			TokenKind::GtGt => "`>>`",
			TokenKind::Pipe => "`|`",
			TokenKind::And => "`&`",
			TokenKind::PipePipe => "`||`",
			TokenKind::AndAnd => "`&&`",
			TokenKind::Comma => "`,`",
			TokenKind::Dot => "`.`",
			TokenKind::Whitespace => "whitespace",
			TokenKind::Error => "unrecognized token",
		};
		write!(f, "{s}")
	}
}

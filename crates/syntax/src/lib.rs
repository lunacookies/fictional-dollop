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
	Function,
	Field,
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
	U32Kw,
	Ident,
	LParen,
	RParen,
	LBrace,
	RBrace,
	Star,
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
			TokenKind::U32Kw => "`u32`",
			TokenKind::Ident => "identifier",
			TokenKind::LParen => "`(`",
			TokenKind::RParen => "`)`",
			TokenKind::LBrace => "`{`",
			TokenKind::RBrace => "`}`",
			TokenKind::Star => "`*`",
			TokenKind::Comma => "`,`",
			TokenKind::Dot => "`.`",
			TokenKind::Whitespace => "whitespace",
			TokenKind::Error => "unrecognized token",
		};
		write!(f, "{s}")
	}
}

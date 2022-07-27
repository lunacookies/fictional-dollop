use std::mem;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
	StructKw,
	Ident,
	LBrace,
	RBrace,
	Comma,
	Whitespace,
	Error,
}

unsafe impl eventree::SyntaxKind for TokenKind {
	fn to_raw(self) -> u16 {
		self as u16
	}

	unsafe fn from_raw(raw: u16) -> Self {
		mem::transmute(raw as u8)
	}
}

mod headers;
pub use headers::*;

use std::fmt;
use syntax::{SyntaxNode, SyntaxTree, TokenKind};

pub enum Ty {
	Named(String),
	Missing,
}

impl fmt::Display for Ty {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Ty::Named(n) => write!(f, "{n}"),
			Ty::Missing => write!(f, "<missing>"),
		}
	}
}

pub fn gen_ty(node: SyntaxNode, tree: &SyntaxTree) -> Ty {
	let name = match node
		.child_tokens(tree)
		.find(|t| t.kind(tree) == TokenKind::Ident)
	{
		Some(t) => t.text(tree),
		None => return Ty::Missing,
	};

	Ty::Named(name.to_string())
}

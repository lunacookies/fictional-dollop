mod headers;
pub use headers::*;

use arena::{Arena, ArenaMap, Id};
use syntax::{NodeKind, SyntaxNode, SyntaxTree, TokenKind};
use text_size::TextRange;

pub enum Ty {
	Named(String),
	Pointer(Id<Ty>),
	Missing,
}

fn gen_ty(
	node: SyntaxNode,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Ty {
	match node.kind(tree) {
		NodeKind::NamedTy => {
			let name = match node
				.child_tokens(tree)
				.find(|t| t.kind(tree) == TokenKind::Ident)
			{
				Some(t) => t.text(tree),
				None => return Ty::Missing,
			};

			Ty::Named(name.to_string())
		}
		NodeKind::PointerTy => {
			let ty = match node.child_nodes(tree).find(|n| {
				matches!(n.kind(tree), NodeKind::NamedTy | NodeKind::PointerTy)
			}) {
				Some(ty) => {
					let generated_ty = gen_ty(ty, tree, tys, ty_ranges);
					let id = tys.alloc(generated_ty);
					ty_ranges.insert(id, ty.range(tree));
					id
				}
				None => tys.alloc(Ty::Missing),
			};

			Ty::Pointer(ty)
		}
		_ => unreachable!(),
	}
}

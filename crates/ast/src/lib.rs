mod headers;
pub use headers::*;

use arena::{Arena, ArenaMap, Id};
use cst::{CstNode, CstToken};
use syntax::SyntaxTree;
use text_size::TextRange;

pub enum Ty {
	Named(String),
	Pointer(Id<Ty>),
	Missing,
}

fn gen_ty(
	ty: Option<cst::Ty>,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Id<Ty> {
	let ty = match ty {
		Some(t) => t,
		None => return tys.alloc(Ty::Missing),
	};

	let id = match ty {
		cst::Ty::NamedTy(ty) => {
			let name = match ty.name(tree) {
				Some(n) => n.text(tree),
				None => return tys.alloc(Ty::Missing),
			};

			tys.alloc(Ty::Named(name.to_string()))
		}
		cst::Ty::PointerTy(ty) => {
			let pointee = gen_ty(ty.pointee(tree), tree, tys, ty_ranges);
			tys.alloc(Ty::Pointer(pointee))
		}
	};

	ty_ranges.insert(id, ty.range(tree));

	id
}

use crate::Ty;
use arena::{Arena, ArenaMap, Id};
use cst::{CstNode, CstToken};
use std::collections::HashMap;
use syntax::SyntaxTree;
use text_size::TextRange;

pub struct World {
	pub headers: HashMap<String, Header>,
}

pub struct Header {
	pub items: HashMap<String, Item>,
	pub tys: Arena<Ty>,
	pub ty_ranges: ArenaMap<Ty, TextRange>,
}

pub enum Item {
	Strukt { fields: Vec<(String, Id<Ty>)> },
}

pub fn gen_header(source_file: cst::SourceFile, tree: &SyntaxTree) -> Header {
	GenHeaderCtx { tree, tys: Arena::new(), ty_ranges: ArenaMap::new() }
		.gen_header(source_file)
}

struct GenHeaderCtx<'a> {
	tree: &'a SyntaxTree,
	tys: Arena<Ty>,
	ty_ranges: ArenaMap<Ty, TextRange>,
}

impl GenHeaderCtx<'_> {
	fn gen_header(mut self, source_file: cst::SourceFile) -> Header {
		let mut items = HashMap::new();

		for item in source_file.items(self.tree) {
			if let Some((name, item)) = self.gen_item(item) {
				items.insert(name, item);
			}
		}

		Header { items, tys: self.tys, ty_ranges: self.ty_ranges }
	}

	fn gen_item(&mut self, item: cst::Item) -> Option<(String, Item)> {
		match item {
			cst::Item::Strukt(s) => self.gen_strukt(s),
		}
	}

	fn gen_strukt(&mut self, strukt: cst::Strukt) -> Option<(String, Item)> {
		let name = strukt.name(self.tree)?.text(self.tree);

		let mut fields = Vec::new();

		for field in strukt.fields(self.tree) {
			let name = match field.name(self.tree) {
				Some(name) => name.text(self.tree),
				None => continue,
			};

			let ty = self.gen_ty(field.ty(self.tree));

			fields.push((name.to_string(), ty));
		}

		Some((name.to_string(), Item::Strukt { fields }))
	}

	fn gen_ty(&mut self, ty: Option<cst::Ty>) -> Id<Ty> {
		let ty = match ty {
			Some(t) => t,
			None => return self.tys.alloc(Ty::Missing),
		};

		let id = match ty {
			cst::Ty::NamedTy(ty) => {
				let name = match ty.name(self.tree) {
					Some(n) => n.text(self.tree),
					None => return self.tys.alloc(Ty::Missing),
				};
				self.tys.alloc(Ty::Named(name.to_string()))
			}
			cst::Ty::PointerTy(ty) => {
				let pointee = self.gen_ty(ty.pointee(self.tree));
				self.tys.alloc(Ty::Pointer(pointee))
			}
		};

		self.ty_ranges.insert(id, ty.range(self.tree));

		id
	}
}

pub fn pretty_print_header(header: &Header) -> String {
	let mut s = String::new();

	let mut items: Vec<_> = header.items.iter().collect();
	items.sort_by_key(|(name, _)| *name);

	for (i, (name, item)) in items.into_iter().enumerate() {
		if i != 0 {
			s.push_str("\n\n");
		}

		match item {
			Item::Strukt { fields } => {
				s.push_str("struct ");
				s.push_str(name);
				s.push(' ');

				if fields.is_empty() {
					s.push_str("{}");
				} else {
					s.push('{');
					for (field_name, field_ty) in fields {
						s.push_str("\n\t");
						s.push_str(field_name);
						s.push(' ');
						pretty_print_ty(
							header.tys.get(*field_ty),
							&header.tys,
							&mut s,
						);
						s.push(',');
					}
					s.push_str("\n}");
				}
			}
		}
	}

	return s;

	fn pretty_print_ty(ty: &Ty, tys: &Arena<Ty>, s: &mut String) {
		match ty {
			Ty::Named(n) => s.push_str(n),
			Ty::Pointer(pointee) => {
				s.push('*');
				pretty_print_ty(tys.get(*pointee), tys, s);
			}
			Ty::Missing => s.push_str("<missing>"),
		}
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	test_utils::run_tests(|input| {
		let parse = parser::parse(input);
		let header = gen_header(parse.node, &parse.tree);
		pretty_print_header(&header)
	});
}

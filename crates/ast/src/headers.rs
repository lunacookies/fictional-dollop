use crate::{Path, Ty};
use arena::{Arena, ArenaMap, Id};
use cst::{CstNode, CstToken};
use std::collections::HashMap;
use syntax::SyntaxTree;
use text_size::TextRange;

pub struct World {
	pub headers: HashMap<String, Header>,
}

#[derive(Default)]
pub struct Header {
	pub items: HashMap<String, Item>,
	pub tys: Arena<Ty>,
	pub ty_ranges: ArenaMap<Ty, TextRange>,
	pub path_segments: Arena<String>,
	pub path_segment_ranges: ArenaMap<String, TextRange>,
}

pub enum Item {
	Strukt { fields: Vec<(String, Id<Ty>)> },
	Function,
}

pub fn gen_header(source_file: cst::SourceFile, tree: &SyntaxTree) -> Header {
	GenHeaderCtx { tree, header: Header::default() }.gen_header(source_file)
}

struct GenHeaderCtx<'a> {
	tree: &'a SyntaxTree,
	header: Header,
}

impl GenHeaderCtx<'_> {
	fn gen_header(mut self, source_file: cst::SourceFile) -> Header {
		for item in source_file.items(self.tree) {
			if let Some((name, item)) = self.gen_item(item) {
				self.header.items.insert(name, item);
			}
		}

		self.header
	}

	fn gen_item(&mut self, item: cst::Item) -> Option<(String, Item)> {
		match item {
			cst::Item::Strukt(s) => self.gen_strukt(s),
			cst::Item::Function(f) => self.gen_function(f),
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

	fn gen_function(
		&mut self,
		function: cst::Function,
	) -> Option<(String, Item)> {
		let name = function.name(self.tree)?.text(self.tree);
		Some((name.to_string(), Item::Function))
	}

	fn gen_ty(&mut self, ty: Option<cst::Ty>) -> Id<Ty> {
		let ty = match ty {
			Some(t) => t,
			None => return self.header.tys.alloc(Ty::Missing),
		};

		let id = match ty {
			cst::Ty::NamedTy(ty) => {
				let path =
					match ty.path(self.tree).and_then(|p| self.gen_path(p)) {
						Some(p) => p,
						None => return self.header.tys.alloc(Ty::Missing),
					};
				self.header.tys.alloc(Ty::Named(path))
			}
			cst::Ty::PointerTy(ty) => {
				let pointee = self.gen_ty(ty.pointee(self.tree));
				self.header.tys.alloc(Ty::Pointer(pointee))
			}
			cst::Ty::PrimitiveTy(ty) => match ty.kind(self.tree) {
				Some(cst::PrimitiveTyKind::U32) => {
					self.header.tys.alloc(Ty::U32)
				}
				None => self.header.tys.alloc(Ty::Missing),
			},
		};

		self.header.ty_ranges.insert(id, ty.range(self.tree));

		id
	}

	fn gen_path(&mut self, path: cst::Path) -> Option<Path> {
		let p = match path {
			cst::Path::ForeignPath(p) => Path::Foreign {
				module: self.gen_path_segment(p.module_name(self.tree)?),
				item: self.gen_path_segment(p.item_name(self.tree)?),
			},
			cst::Path::LocalPath(p) => Path::Local {
				item: self.gen_path_segment(p.item_name(self.tree)?),
			},
		};

		Some(p)
	}

	fn gen_path_segment(&mut self, ident: cst::Ident) -> Id<String> {
		let s = ident.text(self.tree).to_string();
		let id = self.header.path_segments.alloc(s);
		self.header.path_segment_ranges.insert(id, ident.range(self.tree));
		id
	}
}

pub fn pretty_print_header(header: &Header) -> String {
	PrettyPrintHeaderCtx { header, s: String::new() }.pretty_print()
}

struct PrettyPrintHeaderCtx<'a> {
	header: &'a Header,
	s: String,
}

impl PrettyPrintHeaderCtx<'_> {
	fn pretty_print(mut self) -> String {
		let mut items: Vec<_> = self.header.items.iter().collect();
		items.sort_by_key(|(name, _)| *name);

		for (i, (name, item)) in items.into_iter().enumerate() {
			if i != 0 {
				self.s.push_str("\n\n");
			}
			self.item(name, item);
		}

		self.s
	}

	fn item(&mut self, name: &str, item: &Item) {
		match item {
			Item::Strukt { fields } => {
				self.s.push_str("struct ");
				self.s.push_str(name);
				self.s.push(' ');

				if fields.is_empty() {
					self.s.push_str("{}");
				} else {
					self.s.push('{');
					for (field_name, field_ty) in fields {
						self.s.push_str("\n\t");
						self.s.push_str(field_name);
						self.s.push(' ');
						self.ty(*self.header.tys.get(*field_ty));
						self.s.push(',');
					}
					self.s.push_str("\n}");
				}
			}
			Item::Function => {
				self.s.push_str("fn ");
				self.s.push_str(name);
				self.s.push_str("() {}");
			}
		}
	}

	fn ty(&mut self, ty: Ty) {
		match ty {
			Ty::Named(path) => self.path(path),
			Ty::Pointer(pointee) => {
				self.s.push('*');
				self.ty(*self.header.tys.get(pointee));
			}
			Ty::U32 => self.s.push_str("u32"),
			Ty::Missing => self.s.push_str("<missing>"),
		}
	}

	fn path(&mut self, path: Path) {
		match path {
			Path::Local { item } => {
				self.s.push_str(self.header.path_segments.get(item))
			}
			Path::Foreign { module, item } => {
				self.s.push_str(self.header.path_segments.get(module));
				self.s.push('.');
				self.s.push_str(self.header.path_segments.get(item));
			}
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

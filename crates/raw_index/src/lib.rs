use arena::{Arena, ArenaMap, Id};
use cst::{CstNode, CstToken};
use std::collections::HashMap;
use syntax::SyntaxTree;
use text_size::TextRange;

pub struct Index {
	pub stubs: HashMap<String, Stub>,
}

#[derive(Default)]
pub struct Stub {
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

#[derive(Clone, Copy)]
pub enum Ty {
	Named(Path),
	Pointer(Id<Ty>),
	U32,
	Missing,
}

#[derive(Clone, Copy)]
pub enum Path {
	Local { item: Id<String> },
	Foreign { module: Id<String>, item: Id<String> },
}

pub fn run_indexer(source_file: cst::SourceFile, tree: &SyntaxTree) -> Stub {
	Indexer { tree, stub: Stub::default() }.run(source_file)
}

struct Indexer<'a> {
	tree: &'a SyntaxTree,
	stub: Stub,
}

impl Indexer<'_> {
	fn run(mut self, source_file: cst::SourceFile) -> Stub {
		for item in source_file.items(self.tree) {
			if let Some((name, item)) = self.item(item) {
				self.stub.items.insert(name, item);
			}
		}

		self.stub
	}

	fn item(&mut self, item: cst::Item) -> Option<(String, Item)> {
		match item {
			cst::Item::Strukt(s) => self.strukt(s),
			cst::Item::Function(f) => self.function(f),
		}
	}

	fn strukt(&mut self, strukt: cst::Strukt) -> Option<(String, Item)> {
		let name = strukt.name(self.tree)?.text(self.tree);

		let mut fields = Vec::new();

		for field in strukt.fields(self.tree) {
			let name = match field.name(self.tree) {
				Some(name) => name.text(self.tree),
				None => continue,
			};

			let ty = self.ty(field.ty(self.tree));

			fields.push((name.to_string(), ty));
		}

		Some((name.to_string(), Item::Strukt { fields }))
	}

	fn function(&mut self, function: cst::Function) -> Option<(String, Item)> {
		let name = function.name(self.tree)?.text(self.tree);
		Some((name.to_string(), Item::Function))
	}

	fn ty(&mut self, ty: Option<cst::Ty>) -> Id<Ty> {
		let ty = match ty {
			Some(t) => t,
			None => return self.stub.tys.alloc(Ty::Missing),
		};

		let id = match ty {
			cst::Ty::NamedTy(ty) => {
				let path = match ty.path(self.tree).and_then(|p| self.path(p))
				{
					Some(p) => p,
					None => return self.stub.tys.alloc(Ty::Missing),
				};
				self.stub.tys.alloc(Ty::Named(path))
			}
			cst::Ty::PointerTy(ty) => {
				let pointee = self.ty(ty.pointee(self.tree));
				self.stub.tys.alloc(Ty::Pointer(pointee))
			}
			cst::Ty::PrimitiveTy(ty) => match ty.kind(self.tree) {
				Some(cst::PrimitiveTyKind::U32) => {
					self.stub.tys.alloc(Ty::U32)
				}
				None => self.stub.tys.alloc(Ty::Missing),
			},
		};

		self.stub.ty_ranges.insert(id, ty.range(self.tree));

		id
	}

	fn path(&mut self, path: cst::Path) -> Option<Path> {
		let p = match path {
			cst::Path::ForeignPath(p) => Path::Foreign {
				module: self.path_segment(p.module_name(self.tree)?),
				item: self.path_segment(p.item_name(self.tree)?),
			},
			cst::Path::LocalPath(p) => Path::Local {
				item: self.path_segment(p.item_name(self.tree)?),
			},
		};

		Some(p)
	}

	fn path_segment(&mut self, ident: cst::Ident) -> Id<String> {
		let s = ident.text(self.tree).to_string();
		let id = self.stub.path_segments.alloc(s);
		self.stub.path_segment_ranges.insert(id, ident.range(self.tree));
		id
	}
}

pub fn pretty_print_stub(stub: &Stub) -> String {
	PrettyPrintStubCtx { stub, s: String::new() }.pretty_print()
}

struct PrettyPrintStubCtx<'a> {
	stub: &'a Stub,
	s: String,
}

impl PrettyPrintStubCtx<'_> {
	fn pretty_print(mut self) -> String {
		let mut items: Vec<_> = self.stub.items.iter().collect();
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
						self.ty(*self.stub.tys.get(*field_ty));
						self.s.push(',');
					}
					self.s.push_str("\n}");
				}
			}
			Item::Function => {
				self.s.push_str("fn ");
				self.s.push_str(name);
				self.s.push_str("()");
			}
		}
	}

	fn ty(&mut self, ty: Ty) {
		match ty {
			Ty::Named(path) => self.path(path),
			Ty::Pointer(pointee) => {
				self.s.push('*');
				self.ty(*self.stub.tys.get(pointee));
			}
			Ty::U32 => self.s.push_str("u32"),
			Ty::Missing => self.s.push_str("<missing>"),
		}
	}

	fn path(&mut self, path: Path) {
		match path {
			Path::Local { item } => {
				self.s.push_str(self.stub.path_segments.get(item))
			}
			Path::Foreign { module, item } => {
				self.s.push_str(self.stub.path_segments.get(module));
				self.s.push('.');
				self.s.push_str(self.stub.path_segments.get(item));
			}
		}
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	test_utils::run_tests(|input| {
		let parse = parser::parse(input);
		let stub = run_indexer(parse.node, &parse.tree);
		pretty_print_stub(&stub)
	});
}

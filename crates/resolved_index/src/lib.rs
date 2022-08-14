use arena::{Arena, Id};
use std::collections::HashMap;
use text_size::TextRange;

pub struct Index {
	pub stubs: HashMap<String, Stub>,
}

#[derive(Default)]
pub struct Stub {
	pub items: HashMap<String, Item>,
	pub tys: Arena<Ty>,
}

pub enum Item {
	Strukt { fields: Vec<(String, Id<Ty>)> },
	Function,
}

pub enum Ty {
	Named(Path),
	Pointer(Id<Ty>),
	U32,
	Unknown,
}

#[derive(Clone)]
pub struct Path {
	pub module: String,
	pub item: String,
}

pub fn resolve_raw_stub(
	raw_stub: &raw_index::Stub,
	raw_stub_name: &str,
	raw_index: &raw_index::Index,
) -> (Stub, Vec<Error>) {
	Resolver {
		raw_stub,
		raw_stub_name,
		raw_index,
		resolved_stub: Stub { items: HashMap::new(), tys: Arena::new() },
		errors: Vec::new(),
	}
	.run()
}

pub struct Error {
	pub range: TextRange,
	pub kind: ErrorKind,
}

pub enum ErrorKind {
	UndefinedItem,
	UndefinedModule,
	ExpectedTyFoundFunction,
}

struct Resolver<'a> {
	raw_stub: &'a raw_index::Stub,
	raw_stub_name: &'a str,
	raw_index: &'a raw_index::Index,
	resolved_stub: Stub,
	errors: Vec<Error>,
}

impl Resolver<'_> {
	fn run(mut self) -> (Stub, Vec<Error>) {
		for (name, item) in &self.raw_stub.items {
			self.item(name, item);
		}
		(self.resolved_stub, self.errors)
	}

	fn item(&mut self, name: &str, item: &raw_index::Item) {
		let resolved_item = match item {
			raw_index::Item::Strukt { fields } => self.strukt(fields),
			raw_index::Item::Function => self.function(),
		};
		self.resolved_stub.items.insert(name.to_string(), resolved_item);
	}

	fn strukt(&mut self, fields: &[(String, Id<raw_index::Ty>)]) -> Item {
		let mut resolved_fields = Vec::new();
		for (name, ty) in fields {
			let resolved_ty = self.ty(*ty);
			resolved_fields.push((name.clone(), resolved_ty))
		}
		Item::Strukt { fields: resolved_fields }
	}

	fn function(&mut self) -> Item {
		Item::Function
	}

	fn ty(&mut self, ty: Id<raw_index::Ty>) -> Id<Ty> {
		match *self.raw_stub.tys.get(ty) {
			raw_index::Ty::Named(path) => match self.path(path) {
				Some((path, item)) => match item {
					raw_index::Item::Strukt { .. } => {
						self.resolved_stub.tys.alloc(Ty::Named(path))
					}
					raw_index::Item::Function => {
						self.errors.push(Error {
							range: *self.raw_stub.ty_ranges.get(ty),
							kind: ErrorKind::ExpectedTyFoundFunction,
						});
						self.resolved_stub.tys.alloc(Ty::Unknown)
					}
				},
				None => self.resolved_stub.tys.alloc(Ty::Unknown),
			},
			raw_index::Ty::Pointer(pointee) => {
				let pointee = self.ty(pointee);
				self.resolved_stub.tys.alloc(Ty::Pointer(pointee))
			}
			raw_index::Ty::U32 => self.resolved_stub.tys.alloc(Ty::U32),
			raw_index::Ty::Missing => {
				self.resolved_stub.tys.alloc(Ty::Unknown)
			}
		}
	}

	fn path(
		&mut self,
		path: raw_index::Path,
	) -> Option<(Path, &raw_index::Item)> {
		match path {
			raw_index::Path::Local { item } => self.local_path(item),
			raw_index::Path::Foreign { module, item } => {
				self.foreign_path(module, item)
			}
		}
	}

	fn local_path(
		&mut self,
		item: Id<String>,
	) -> Option<(Path, &raw_index::Item)> {
		let item_text = self.raw_stub.path_segments.get(item);
		match self.raw_stub.items.get(item_text) {
			Some(item) => {
				let path = Path {
					module: self.raw_stub_name.to_string(),
					item: item_text.clone(),
				};
				Some((path, item))
			}
			None => {
				self.errors.push(Error {
					range: *self.raw_stub.path_segment_ranges.get(item),
					kind: ErrorKind::UndefinedItem,
				});
				None
			}
		}
	}

	fn foreign_path(
		&mut self,
		module: Id<String>,
		item: Id<String>,
	) -> Option<(Path, &raw_index::Item)> {
		let module_text = self.raw_stub.path_segments.get(module);

		let raw_stub_of_module = match self.raw_index.stubs.get(module_text) {
			Some(s) => s,
			None => {
				self.errors.push(Error {
					range: *self.raw_stub.path_segment_ranges.get(module),
					kind: ErrorKind::UndefinedModule,
				});
				return None;
			}
		};

		let item_text = self.raw_stub.path_segments.get(item);
		match raw_stub_of_module.items.get(item_text) {
			Some(item) => {
				let path = Path {
					module: module_text.clone(),
					item: item_text.clone(),
				};
				Some((path, item))
			}
			None => {
				self.errors.push(Error {
					range: *self.raw_stub.path_segment_ranges.get(item),
					kind: ErrorKind::UndefinedItem,
				});
				None
			}
		}
	}
}

pub fn pretty_print_index(index: &Index) -> String {
	let mut output = String::new();
	let mut stubs: Vec<_> = index.stubs.iter().collect();
	stubs.sort_by_key(|(name, _)| *name);

	for (i, (name, stub)) in stubs.into_iter().enumerate() {
		if i != 0 {
			output.push('\n');
		}
		output.push_str(name);
		output.push(':');
		for line in pretty_print_stub(stub).lines() {
			output.push('\n');
			if !line.is_empty() {
				output.push('\t');
				output.push_str(line);
			}
		}
		output.push('\n');
	}

	output
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
						self.ty(self.stub.tys.get(*field_ty));
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

	fn ty(&mut self, ty: &Ty) {
		match ty {
			Ty::Named(path) => self.path(path),
			Ty::Pointer(pointee) => {
				self.s.push('*');
				self.ty(self.stub.tys.get(*pointee));
			}
			Ty::U32 => self.s.push_str("u32"),
			Ty::Unknown => self.s.push_str("<unknown>"),
		}
	}

	fn path(&mut self, path: &Path) {
		self.s.push_str(&path.module);
		self.s.push('.');
		self.s.push_str(&path.item);
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	use std::collections::HashMap;
	use std::fmt::Write;

	test_utils::run_tests(|input| {
		let files = test_utils::split_multi_file_fixture(input);

		let mut raw_index = raw_index::Index { stubs: HashMap::new() };
		for (file_name, input) in files {
			let parse = parser::parse(input);
			let stub = raw_index::run_indexer(parse.node, &parse.tree);
			raw_index.stubs.insert(file_name.to_string(), stub);
		}

		let mut resolved_index = Index { stubs: HashMap::new() };
		let mut errors = HashMap::new();
		for (file_name, raw_stub) in &raw_index.stubs {
			let (resolved_stub, e) =
				resolve_raw_stub(raw_stub, file_name, &raw_index);
			resolved_index.stubs.insert(file_name.clone(), resolved_stub);
			errors.insert(file_name, e);
		}

		let mut output = pretty_print_index(&resolved_index);

		if errors.values().all(|e| e.is_empty()) {
			return output;
		}

		for (file_name, errors) in errors {
			for error in errors {
				write!(
					output,
					"\nerror in {file_name} at {:?}: ",
					error.range
				)
				.unwrap();
				match error.kind {
					ErrorKind::UndefinedItem => {
						output.push_str("undefined item")
					}
					ErrorKind::UndefinedModule => {
						output.push_str("undefined module")
					}
					ErrorKind::ExpectedTyFoundFunction => {
						output.push_str("expected type, found function")
					}
				}
			}
		}

		output
	});
}

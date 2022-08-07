use crate::{gen_ty, Ty};
use arena::{Arena, ArenaMap, Id};
use cst::CstToken;
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
	let mut items = HashMap::new();
	let mut tys = Arena::new();
	let mut ty_ranges = ArenaMap::new();

	for item in source_file.items(tree) {
		if let Some((name, item)) =
			gen_item(item, tree, &mut tys, &mut ty_ranges)
		{
			items.insert(name, item);
		}
	}

	Header { items, tys, ty_ranges }
}

fn gen_item(
	item: cst::Item,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Option<(String, Item)> {
	match item {
		cst::Item::Strukt(s) => gen_strukt(s, tree, tys, ty_ranges),
	}
}

fn gen_strukt(
	strukt: cst::Strukt,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Option<(String, Item)> {
	let name = strukt.name(tree)?.text(tree);

	let mut fields = Vec::new();

	for field in strukt.fields(tree) {
		let name = match field.name(tree) {
			Some(name) => name.text(tree),
			None => continue,
		};

		let ty = gen_ty(field.ty(tree), tree, tys, ty_ranges);

		fields.push((name.to_string(), ty));
	}

	Some((name.to_string(), Item::Strukt { fields }))
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

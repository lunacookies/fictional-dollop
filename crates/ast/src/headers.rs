use crate::{gen_ty, Ty};
use arena::{Arena, ArenaMap, Id};
use std::collections::HashMap;
use syntax::{NodeKind, SyntaxNode, SyntaxTree, TokenKind};
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

pub fn gen_header(tree: &SyntaxTree) -> Header {
	let root = tree.root();
	let mut items = HashMap::new();
	let mut tys = Arena::new();
	let mut ty_ranges = ArenaMap::new();

	for child in root.child_nodes(tree) {
		if let Some((name, item)) =
			gen_item(child, tree, &mut tys, &mut ty_ranges)
		{
			items.insert(name, item);
		}
	}

	Header { items, tys, ty_ranges }
}

fn gen_item(
	node: SyntaxNode,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Option<(String, Item)> {
	match node.kind(tree) {
		NodeKind::Strukt => gen_strukt(node, tree, tys, ty_ranges),
		_ => None,
	}
}

fn gen_strukt(
	node: SyntaxNode,
	tree: &SyntaxTree,
	tys: &mut Arena<Ty>,
	ty_ranges: &mut ArenaMap<Ty, TextRange>,
) -> Option<(String, Item)> {
	let name = node
		.child_tokens(tree)
		.find(|t| t.kind(tree) == TokenKind::Ident)
		.map(|t| t.text(tree))?;

	let mut fields = Vec::new();

	for child in node.child_nodes(tree) {
		if child.kind(tree) != NodeKind::Field {
			continue;
		}

		let name = match child
			.child_tokens(tree)
			.find(|t| t.kind(tree) == TokenKind::Ident)
		{
			Some(name) => name.text(tree),
			None => continue,
		};

		let ty = match child.child_nodes(tree).find(|n| {
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
		let header = gen_header(&parser::parse(input).tree);
		pretty_print_header(&header)
	});
}

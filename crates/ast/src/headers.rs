use crate::{gen_ty, Ty};
use std::collections::HashMap;
use std::fmt::Write;
use syntax::{NodeKind, SyntaxNode, SyntaxTree, TokenKind};

pub enum Item {
	Strukt { fields: Vec<(String, Ty)> },
}

pub fn gen_header(tree: &SyntaxTree) -> HashMap<String, Item> {
	let root = tree.root();
	let mut items = HashMap::new();

	for child in root.child_nodes(tree) {
		if let Some((name, item)) = gen_item(child, tree) {
			items.insert(name, item);
		}
	}

	items
}

fn gen_item(node: SyntaxNode, tree: &SyntaxTree) -> Option<(String, Item)> {
	match node.kind(tree) {
		NodeKind::Strukt => gen_strukt(node, tree),
		_ => None,
	}
}

fn gen_strukt(node: SyntaxNode, tree: &SyntaxTree) -> Option<(String, Item)> {
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

		let ty = match child
			.child_nodes(tree)
			.find(|n| n.kind(tree) == NodeKind::Ty)
		{
			Some(ty) => gen_ty(ty, tree),
			None => Ty::Missing,
		};

		fields.push((name.to_string(), Ty::Named(ty.to_string())));
	}

	Some((name.to_string(), Item::Strukt { fields }))
}

pub fn pretty_print_header(header: &HashMap<String, Item>) -> String {
	let mut s = String::new();

	let mut header: Vec<_> = header.iter().collect();
	header.sort_by_key(|(name, _)| *name);

	for (i, (name, item)) in header.into_iter().enumerate() {
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
						write!(s, "{field_ty}").unwrap();
						s.push(',');
					}
					s.push_str("\n}");
				}
			}
		}
	}

	s
}

#[cfg(test)]
#[test]
fn run_tests() {
	use expect_test::expect_file;
	use parser::parse;

	for entry in std::fs::read_dir("test_data").unwrap() {
		let test_path = entry.unwrap().path().canonicalize().unwrap();

		println!(
			"\n==== RUNNING HEADER GEN TEST {:?} ====",
			test_path.file_stem().unwrap()
		);

		let test_text = std::fs::read_to_string(&test_path).unwrap();
		let input = &test_text[..test_text.find("# ---\n").unwrap()];
		let header = gen_header(&parse(input).tree);

		let mut actual = input.to_string();
		actual.push_str("# ---\n");
		for line in pretty_print_header(&header).lines() {
			actual.push('#');
			if !line.is_empty() {
				actual.push(' ');
			}
			actual.push_str(line);
			actual.push('\n');
		}

		expect_file![test_path].assert_eq(&actual);
	}
}

use arena::Id;
use ast::{Header, Item, Path, Ty, World};
use text_size::TextRange;

pub fn nameres(header: &Header, world: &World) -> Vec<Error> {
	let mut errors = Vec::new();

	for item in header.items.values() {
		match item {
			Item::Strukt { fields } => {
				for (_, field_ty) in fields {
					nameres_ty(*field_ty, header, world, &mut errors);
				}
			}
		}
	}

	errors
}

pub struct Error {
	pub range: TextRange,
	pub kind: ErrorKind,
}

pub enum ErrorKind {
	UndefinedItem,
	UndefinedModule,
}

fn nameres_ty(
	ty: Id<Ty>,
	header: &Header,
	world: &World,
	errors: &mut Vec<Error>,
) {
	match *header.tys.get(ty) {
		Ty::Named(path) => match nameres_path(path, header, world, errors) {
			Some(item) => match item {
				Item::Strukt { .. } => {}
			},
			None => {}
		},
		Ty::Pointer(pointee) => nameres_ty(pointee, header, world, errors),
		Ty::U32 | Ty::Missing => {}
	}
}

fn nameres_path<'a>(
	path: Path,
	header: &'a Header,
	world: &'a World,
	errors: &mut Vec<Error>,
) -> Option<&'a Item> {
	let (h, item) = match path {
		Path::Local { item } => (header, item),
		Path::Foreign { module, item } => {
			let module_text = header.path_segments.get(module);
			match world.headers.get(module_text) {
				Some(h) => (h, item),
				None => {
					errors.push(Error {
						range: *header.path_segment_ranges.get(module),
						kind: ErrorKind::UndefinedModule,
					});
					return None;
				}
			}
		}
	};

	let item_text = header.path_segments.get(item);
	match h.items.get(item_text) {
		Some(i) => Some(i),
		None => {
			errors.push(Error {
				range: *header.path_segment_ranges.get(item),
				kind: ErrorKind::UndefinedItem,
			});
			None
		}
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	use std::collections::HashMap;
	use std::fmt::Write;

	test_utils::run_tests(|input| {
		let files = test_utils::split_multi_file_fixture(input);
		let mut world = World { headers: HashMap::new() };

		for (file_name, input) in files {
			let parse = parser::parse(input);
			let header = ast::gen_header(parse.node, &parse.tree);
			world.headers.insert(file_name.to_string(), header);
		}

		let mut output = String::new();

		for (file_name, header) in &world.headers {
			for error in nameres(header, &world) {
				write!(output, "error in {file_name} at {:?}: ", error.range)
					.unwrap();
				match error.kind {
					ErrorKind::UndefinedItem => {
						output.push_str("undefined item")
					}
					ErrorKind::UndefinedModule => {
						output.push_str("undefined module")
					}
				}
				output.push('\n');
			}
		}

		output
	});
}

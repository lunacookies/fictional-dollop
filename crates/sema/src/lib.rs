use arena::Id;
use ast::{Header, Item, Ty, World};
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
	UndefinedTy,
}

fn nameres_ty(
	ty: Id<Ty>,
	header: &Header,
	world: &World,
	errors: &mut Vec<Error>,
) {
	match header.tys.get(ty) {
		Ty::Named(n) => {
			for header in world.headers.values() {
				match header.items.get(n) {
					Some(Item::Strukt { .. }) => return,
					None => {}
				}
			}

			errors.push(Error {
				range: *header.ty_ranges.get(ty),
				kind: ErrorKind::UndefinedTy,
			});
		}
		Ty::Pointer(pointee) => nameres_ty(*pointee, header, world, errors),
		Ty::Missing => {}
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
			let header = ast::gen_header(&parser::parse(input).tree);
			world.headers.insert(file_name.to_string(), header);
		}

		let mut output = String::new();

		for (file_name, header) in &world.headers {
			for error in nameres(header, &world) {
				write!(output, "error in {file_name} at {:?}: ", error.range)
					.unwrap();
				match error.kind {
					ErrorKind::UndefinedTy => {
						output.push_str("undefined type")
					}
				}
				output.push('\n');
			}
		}

		output
	});
}

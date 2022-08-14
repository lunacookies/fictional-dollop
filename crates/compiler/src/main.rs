use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use text_size::TextSize;

fn main() -> anyhow::Result<()> {
	let mut world = ast::World { headers: HashMap::new() };
	let mut line_starts_map = HashMap::new();

	for entry in fs::read_dir(".")? {
		let path = entry?.path();
		if path.extension() != Some(OsStr::new("fd")) {
			continue;
		}
		let name = path.file_stem().unwrap().to_str().unwrap();

		let content = fs::read_to_string(&path)?;
		let line_starts = line_starts(&content);

		let parse = parser::parse(&content);
		for error in parse.errors {
			let (offset, message) = match error {
				parser::Error::Expected { range, message } => {
					(range.start(), format!("expected {message}"))
				}
				parser::Error::Missing { offset, message } => {
					(offset, format!("missing {message}"))
				}
			};
			let (line, column) = offset_to_line_column(offset, &line_starts);
			println!(
				"{name}.fd:{}:{}: error: {message}",
				line + 1,
				column + 1
			);
		}

		let header = ast::gen_header(parse.node, &parse.tree);
		world.headers.insert(name.to_string(), header);
		line_starts_map.insert(name.to_string(), (content, line_starts));
	}

	for (name, header) in &world.headers {
		let (content, line_starts) = &line_starts_map[name];
		let errors = sema::nameres(header, &world);

		for error in errors {
			let message = match error.kind {
				sema::ErrorKind::UndefinedItem => {
					format!("undefined item `{}`", &content[error.range])
				}
				sema::ErrorKind::UndefinedModule => {
					format!("undefined module `{}`", &content[error.range])
				}
				sema::ErrorKind::ExpectedTyFoundFunction => {
					format!(
						"expected type, found function `{}`",
						&content[error.range]
					)
				}
			};
			let (line, column) =
				offset_to_line_column(error.range.start(), line_starts);
			println!(
				"{name}.fd:{}:{}: error: {message}",
				line + 1,
				column + 1
			);
		}
	}

	Ok(())
}

fn offset_to_line_column(offset: TextSize, line_starts: &[u32]) -> (u32, u32) {
	let offset = u32::from(offset);
	let line = line_starts.partition_point(|&i| i <= offset) - 1;
	let column = offset - line_starts[line];
	(line as u32, column)
}

fn line_starts(input: &str) -> Vec<u32> {
	let mut line_starts = vec![0];
	for (i, _) in input.match_indices('\n') {
		line_starts.push(i as u32 + 1);
	}
	line_starts
}

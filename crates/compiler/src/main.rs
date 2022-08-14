use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use text_size::TextSize;

fn main() -> anyhow::Result<()> {
	let files = read_files()?;
	let parses = parse(&files);
	let raw_index = index(&parses);
	let _resolved_index = resolve(raw_index, &files);
	Ok(())
}

fn read_files() -> anyhow::Result<HashMap<String, (String, Vec<u32>)>> {
	let mut map = HashMap::new();

	for entry in fs::read_dir(".")? {
		let path = entry?.path();
		if path.extension() != Some(OsStr::new("fd")) {
			continue;
		}
		let name = path.file_stem().unwrap().to_str().unwrap();
		let content = fs::read_to_string(&path)?;
		let line_starts = line_starts(&content);
		map.insert(name.to_string(), (content, line_starts));
	}

	Ok(map)
}

fn parse(
	files: &HashMap<String, (String, Vec<u32>)>,
) -> HashMap<String, (cst::SourceFile, syntax::SyntaxTree)> {
	let mut map = HashMap::new();

	for (name, (content, line_starts)) in files {
		let parse = parser::parse(content);
		map.insert(name.clone(), (parse.node, parse.tree));

		for error in parse.errors {
			let (offset, message) = match error {
				parser::Error::Expected { range, message } => {
					(range.start(), format!("expected {message}"))
				}
				parser::Error::Missing { offset, message } => {
					(offset, format!("missing {message}"))
				}
			};
			let (line, column) = offset_to_line_column(offset, line_starts);
			println!(
				"{name}.fd:{}:{}: error: {message}",
				line + 1,
				column + 1
			);
		}
	}

	map
}

fn index(
	parses: &HashMap<String, (cst::SourceFile, syntax::SyntaxTree)>,
) -> raw_index::Index {
	let mut index = raw_index::Index { stubs: HashMap::new() };
	for (name, (source_file, tree)) in parses {
		let stub = raw_index::run_indexer(*source_file, tree);
		index.stubs.insert(name.clone(), stub);
	}
	index
}

fn resolve(
	raw_index: raw_index::Index,
	files: &HashMap<String, (String, Vec<u32>)>,
) -> resolved_index::Index {
	let mut resolved_index = resolved_index::Index { stubs: HashMap::new() };

	for (name, raw_stub) in &raw_index.stubs {
		let (stub, errors) =
			resolved_index::resolve_raw_stub(raw_stub, name, &raw_index);
		resolved_index.stubs.insert(name.clone(), stub);

		let (content, line_starts) = &files[name];
		for error in errors {
			let message = match error.kind {
				resolved_index::ErrorKind::UndefinedItem => {
					format!("undefined item `{}`", &content[error.range])
				}
				resolved_index::ErrorKind::UndefinedModule => {
					format!("undefined module `{}`", &content[error.range])
				}
				resolved_index::ErrorKind::ExpectedTyFoundFunction => {
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

	resolved_index
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

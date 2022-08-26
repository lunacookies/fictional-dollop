use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use text_size::TextSize;

fn main() -> anyhow::Result<()> {
	let files = read_files()?;
	let parses = parse(&files);
	let raw_index = index(&parses);
	let resolved_index = resolve(raw_index, &files);
	let hirs = lower(&resolved_index, &files, &parses);
	let mirs = build_mirs(&hirs);
	let asm = codegen(&mirs, &resolved_index);

	println!("{asm}");

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

fn lower(
	index: &resolved_index::Index,
	files: &HashMap<String, (String, Vec<u32>)>,
	parses: &HashMap<String, (cst::SourceFile, syntax::SyntaxTree)>,
) -> HashMap<String, hir::Hir> {
	let mut hirs = HashMap::new();

	for (name, stub) in &index.stubs {
		let (source_file, tree) = &parses[name];
		let (hir, errors) = hir::lower(stub, *source_file, tree);
		hirs.insert(name.clone(), hir);

		let (content, line_starts) = &files[name];
		for error in errors {
			let message = match error.kind {
				hir::ErrorKind::UndefinedVariable => {
					format!("undefined variable `{}`", &content[error.range])
				}
				hir::ErrorKind::TyMismatch { expected, actual } => format!(
					"expected type `{expected}`, found type `{actual}`"
				),
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

	hirs
}

fn build_mirs(hirs: &HashMap<String, hir::Hir>) -> HashMap<String, mir::Mir> {
	let mut mirs = HashMap::new();
	for (name, hir) in hirs {
		let mir = mir::build_mir(hir);
		mirs.insert(name.clone(), mir);
	}
	mirs
}

fn codegen(
	mirs: &HashMap<String, mir::Mir>,
	index: &resolved_index::Index,
) -> String {
	let mut asm = String::new();

	for (i, (name, mir)) in mirs.iter().enumerate() {
		if i != 0 {
			asm.push('\n');
		}

		let stub = &index.stubs[name];
		for (i, (_function, a)) in
			codegen_aarch64::codegen(mir, stub, name).into_iter().enumerate()
		{
			if i != 0 {
				asm.push('\n');
			}

			asm.push_str(&a);
		}
	}

	asm
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

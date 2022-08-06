use expect_test::expect_file;
use std::collections::HashMap;
use std::panic::RefUnwindSafe;

pub fn run_tests(test_fn: impl Fn(&str) -> String + RefUnwindSafe) {
	let mut did_any_test_fail = false;

	for entry in std::fs::read_dir("test_data").unwrap() {
		let test_path = entry.unwrap().path().canonicalize().unwrap();

		println!(
			"\n==== RUNNING TEST {:?} ====",
			test_path.file_stem().unwrap()
		);

		let did_panic = std::panic::catch_unwind(|| {
			let test_text = std::fs::read_to_string(&test_path).unwrap();
			let input = &test_text[..test_text.find("# ---\n").unwrap()];

			let mut actual = input.to_string();
			actual.push_str("# ---\n");
			for line in test_fn(input).lines() {
				actual.push('#');
				if !line.is_empty() {
					actual.push(' ');
				}
				actual.push_str(line);
				actual.push('\n');
			}

			expect_file![test_path].assert_eq(&actual);
		})
		.is_err();

		if did_panic {
			did_any_test_fail = true;
		}
	}

	assert!(!did_any_test_fail);
}

pub fn split_multi_file_fixture(input: &str) -> HashMap<&str, &str> {
	const FILE_DIVIDER: &str = "#- ";
	let mut files = HashMap::new();

	assert!(input.starts_with(FILE_DIVIDER));

	for (idx, _) in input.match_indices(FILE_DIVIDER) {
		let (file_name, contents) =
			input[idx + FILE_DIVIDER.len()..].split_once('\n').unwrap();
		files.insert(file_name, contents);
	}

	files
}

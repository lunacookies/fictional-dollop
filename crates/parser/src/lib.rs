mod grammar;

use lexer::Token;
use std::fmt;
use syntax::{NodeKind, SyntaxBuilder, SyntaxTree};
use text_size::TextRange;

pub fn parse(input: &str) -> Parse {
	let tokens = lexer::lex(input);
	let mut parser =
		Parser { tokens: &tokens, events: Vec::new(), errors: Vec::new() };
	grammar::source_file(&mut parser);
	let tree = process_events(input, &parser.events, &tokens);
	Parse { tree, errors: parser.errors }
}

pub struct Parse {
	pub tree: SyntaxTree,
	pub errors: Vec<Error>,
}

pub struct Error {
	range: TextRange,
}

struct Parser<'a> {
	tokens: &'a [Token],
	events: Vec<Event>,
	errors: Vec<Error>,
}

impl Parser<'_> {
	fn start_node(&mut self, kind: NodeKind) {
		self.events.push(Event::StartNode(kind));
	}

	fn bump(&mut self) {
		self.events.push(Event::AddToken);
	}

	fn finish_node(&mut self) {
		self.events.push(Event::FinishNode);
	}
}

enum Event {
	StartNode(NodeKind),
	AddToken,
	FinishNode,
}

fn process_events(
	input: &str,
	events: &[Event],
	mut tokens: &[Token],
) -> SyntaxTree {
	let mut builder = SyntaxBuilder::new(input);

	for event in events {
		match *event {
			Event::StartNode(kind) => builder.start_node(kind),
			Event::AddToken => {
				let token = tokens[0];
				builder.add_token(token.kind, token.range);
				tokens = &tokens[1..];
			}
			Event::FinishNode => builder.finish_node(),
		}
	}

	builder.finish()
}

impl fmt::Debug for Parse {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:#?}", self.tree)?;

		for error in &self.errors {
			writeln!(f, "{error:?}")?;
		}

		Ok(())
	}
}

impl fmt::Debug for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "error at {:?}", self.range)
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	use expect_test::expect_file;

	for entry in std::fs::read_dir("test_data").unwrap() {
		let test_path = entry.unwrap().path().canonicalize().unwrap();
		let test_text = std::fs::read_to_string(&test_path).unwrap();
		let input = &test_text[..test_text.find("# ---\n").unwrap()];
		let parse = parse(input);

		let mut actual = input.to_string();
		actual.push_str("# ---\n");
		for line in format!("{parse:#?}").lines() {
			actual.push_str("# ");
			actual.push_str(line);
			actual.push('\n');
		}

		expect_file![test_path].assert_eq(&actual);
	}
}

mod grammar;

use cst::CstNode;
use lexer::Token;
use std::fmt;
use syntax::{NodeKind, SyntaxBuilder, SyntaxTree, TokenKind};
use text_size::{TextRange, TextSize};

pub fn parse(input: &str) -> Parse<cst::SourceFile> {
	let tokens = lexer::lex(input);
	let mut parser = Parser::new(&tokens);
	grammar::source_file(&mut parser);
	let tree = process_events(input, &parser.events, &tokens);
	let node = cst::SourceFile::cast(tree.root(), &tree).unwrap();
	Parse { tree, node, errors: parser.errors }
}

pub struct Parse<N: CstNode> {
	pub tree: SyntaxTree,
	pub node: N,
	pub errors: Vec<Error>,
}

pub enum Error {
	Expected { range: TextRange, message: String },
	Missing { offset: TextSize, message: String },
}

struct Parser {
	tokens: Vec<Token>,
	events: Vec<Event>,
	errors: Vec<Error>,
	cursor: usize,
}

impl Parser {
	fn new(tokens: &[Token]) -> Parser {
		let mut t = Vec::new();

		for token in tokens {
			if token.kind != TokenKind::Whitespace {
				t.push(*token);
			}
		}

		Parser { tokens: t, events: Vec::new(), errors: Vec::new(), cursor: 0 }
	}

	fn expect(&mut self, kind: TokenKind) {
		self.expect_with_name(kind, &format!("{kind}"));
	}

	fn expect_with_name(&mut self, kind: TokenKind, name: &str) {
		if self.at(kind) {
			self.bump_any();
			return;
		}

		self.error(name);
	}

	fn bump(&mut self, kind: TokenKind) {
		assert!(self.at(kind));
		self.bump_any();
	}

	fn at(&self, kind: TokenKind) -> bool {
		self.peek() == Some(kind)
	}

	fn peek(&self) -> Option<TokenKind> {
		if self.eof() {
			return None;
		}
		Some(self.tokens[self.cursor].kind)
	}

	fn lookahead(&self) -> Option<TokenKind> {
		self.tokens.get(self.cursor + 1).map(|t| t.kind)
	}

	fn error(&mut self, message: &str) {
		if self.at_recovery() {
			self.errors.push(Error::Missing {
				offset: self.tokens[self.cursor - 1].range.end(),
				message: message.to_string(),
			});
			return;
		}

		self.errors.push(Error::Expected {
			range: self.tokens[self.cursor].range,
			message: message.to_string(),
		});
		self.start_node(NodeKind::Error);
		self.bump_any();
		self.finish_node();
	}

	fn error_without_recovery(&mut self, message: &str) {
		self.errors.push(Error::Expected {
			range: self.tokens[self.cursor].range,
			message: message.to_string(),
		});
		self.start_node(NodeKind::Error);
		self.bump_any();
		self.finish_node();
	}

	fn at_recovery(&self) -> bool {
		self.eof()
			|| self.at(TokenKind::LBrace)
			|| self.at(TokenKind::RBrace)
			|| self.at(TokenKind::StructKw)
	}

	fn start_node(&mut self, kind: NodeKind) {
		self.events.push(Event::StartNode(kind));
	}

	fn bump_any(&mut self) {
		assert!(!self.eof());
		self.events.push(Event::AddToken);
		self.cursor += 1;
	}

	fn finish_node(&mut self) {
		self.events.push(Event::FinishNode);
	}

	fn eof(&self) -> bool {
		self.cursor == self.tokens.len()
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Event {
	StartNode(NodeKind),
	AddToken,
	FinishNode,
}

fn process_events(
	input: &str,
	events: &[Event],
	tokens: &[Token],
) -> SyntaxTree {
	Sink { builder: SyntaxBuilder::new(input), tokens, cursor: 0 }
		.process_events(events)
}

struct Sink<'a> {
	builder: SyntaxBuilder,
	tokens: &'a [Token],
	cursor: usize,
}

impl Sink<'_> {
	fn process_events(mut self, events: &[Event]) -> SyntaxTree {
		assert_eq!(events[events.len() - 1], Event::FinishNode);

		for idx in 0..events.len() - 1 {
			let event = events[idx];
			let next = events[idx + 1];

			match event {
				Event::StartNode(kind) => self.builder.start_node(kind),
				Event::AddToken => self.add_token(),
				Event::FinishNode => self.builder.finish_node(),
			}

			match next {
				Event::StartNode(_) | Event::AddToken => self.skip_trivia(),
				Event::FinishNode => {}
			}
		}

		self.skip_trivia();
		self.builder.finish_node();

		self.builder.finish()
	}

	fn skip_trivia(&mut self) {
		if self.cursor < self.tokens.len()
			&& self.tokens[self.cursor].kind == TokenKind::Whitespace
		{
			self.add_token();
		}
	}

	fn add_token(&mut self) {
		let token = self.tokens[self.cursor];
		self.builder.add_token(token.kind, token.range);
		self.cursor += 1;
	}
}

impl<N: CstNode> fmt::Debug for Parse<N> {
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
		match self {
			Error::Expected { range, message } => {
				write!(f, "error at {:?}: expected {}", range, message)
			}
			Error::Missing { offset, message } => {
				write!(f, "error at {:?}: missing {}", offset, message)
			}
		}
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	test_utils::run_tests(|input| format!("{:?}", parse(input)));
}

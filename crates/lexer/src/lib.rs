use logos::Logos;
use std::{fmt, mem};
use text_size::TextRange;

pub fn lex(input: &str) -> Vec<Token> {
	assert!(input.len() < u32::MAX as usize);

	let mut lexer = LogosTokenKind::lexer(input);
	let mut tokens = Vec::new();

	while let Some(kind) = lexer.next() {
		let range = lexer.span();
		let range = TextRange::new(
			(range.start as u32).into(),
			(range.end as u32).into(),
		);
		let token = Token { kind: unsafe { mem::transmute(kind) }, range };
		tokens.push(token);
	}

	tokens
}

pub struct Token {
	pub kind: TokenKind,
	pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
	StructKw,
	Ident,
	LBrace,
	RBrace,
	Comma,
	Whitespace,
	Error,
}

#[derive(Logos)]
enum LogosTokenKind {
	#[token("struct")]
	StructKw,

	#[regex("[a-z][a-z0-9]*")]
	Ident,

	#[token("{")]
	LBrace,

	#[token("}")]
	RBrace,

	#[token(",")]
	Comma,

	#[regex("[ \t\n]+")]
	Whitespace,

	#[error]
	Error,
}

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}@{:?}", self.kind, self.range)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use expect_test::{expect, Expect};

	fn check(input: &str, expect: Expect) {
		let tokens = lex(input);

		expect.assert_eq(
			&tokens
				.into_iter()
				.map(|t| format!("{t:?}"))
				.collect::<Vec<_>>()
				.join("\n"),
		);
	}

	#[test]
	fn empty() {
		check("", expect![[""]]);
	}

	#[test]
	fn whitespace() {
		check(
			" \t  \n\t  ",
			expect![["
				Whitespace@0..8"]],
		);
	}

	#[test]
	fn ident() {
		check(
			"a abc a123",
			expect![["
				Ident@0..1
				Whitespace@1..2
				Ident@2..5
				Whitespace@5..6
				Ident@6..10"]],
		);
	}
}

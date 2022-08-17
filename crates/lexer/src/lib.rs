use logos::Logos;
use std::{fmt, mem};
use syntax::TokenKind;
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

#[derive(Clone, Copy)]
pub struct Token {
	pub kind: TokenKind,
	pub range: TextRange,
}

#[derive(Logos)]
enum LogosTokenKind {
	#[token("struct")]
	StructKw,

	#[token("fn")]
	FnKw,

	#[token("var")]
	VarKw,

	#[token("u32")]
	U32Kw,

	#[regex("[a-z][a-z0-9]*")]
	Ident,

	#[regex("[0-9]+")]
	Integer,

	#[token("(")]
	LParen,

	#[token(")")]
	RParen,

	#[token("{")]
	LBrace,

	#[token("}")]
	RBrace,

	#[token("=")]
	Eq,

	#[token("==")]
	EqEq,

	#[token("!=")]
	BangEq,

	#[token("+")]
	Plus,

	#[token("-")]
	Hyphen,

	#[token("*")]
	Star,

	#[token("/")]
	Slash,

	#[token("%")]
	Percent,

	#[token("^")]
	Caret,

	#[token("<")]
	Lt,

	#[token(">")]
	Gt,

	#[token("<=")]
	LtEq,

	#[token(">=")]
	GtEq,

	#[token("<<")]
	LtLt,

	#[token(">>")]
	GtGt,

	#[token("|")]
	Pipe,

	#[token("&")]
	And,

	#[token("||")]
	PipePipe,

	#[token("&&")]
	AndAnd,

	#[token(",")]
	Comma,

	#[token(".")]
	Dot,

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
	fn keywords() {
		check(
			"struct fn var u32",
			expect![["
				StructKw@0..6
				Whitespace@6..7
				FnKw@7..9
				Whitespace@9..10
				VarKw@10..13
				Whitespace@13..14
				U32Kw@14..17"]],
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

	#[test]
	fn integer() {
		check(
			"1 1234",
			expect![["
				Integer@0..1
				Whitespace@1..2
				Integer@2..6"]],
		);
	}

	#[test]
	fn delimiters() {
		check(
			"(){}",
			expect![["
				LParen@0..1
				RParen@1..2
				LBrace@2..3
				RBrace@3..4"]],
		);
	}

	#[test]
	fn symbols() {
		check(
			"= == != + - * / % ^ < > <= >= << >> | & || && , .",
			expect![["
				Eq@0..1
				Whitespace@1..2
				EqEq@2..4
				Whitespace@4..5
				BangEq@5..7
				Whitespace@7..8
				Plus@8..9
				Whitespace@9..10
				Hyphen@10..11
				Whitespace@11..12
				Star@12..13
				Whitespace@13..14
				Slash@14..15
				Whitespace@15..16
				Percent@16..17
				Whitespace@17..18
				Caret@18..19
				Whitespace@19..20
				Lt@20..21
				Whitespace@21..22
				Gt@22..23
				Whitespace@23..24
				LtEq@24..26
				Whitespace@26..27
				GtEq@27..29
				Whitespace@29..30
				LtLt@30..32
				Whitespace@32..33
				GtGt@33..35
				Whitespace@35..36
				Pipe@36..37
				Whitespace@37..38
				And@38..39
				Whitespace@39..40
				PipePipe@40..42
				Whitespace@42..43
				AndAnd@43..45
				Whitespace@45..46
				Comma@46..47
				Whitespace@47..48
				Dot@48..49"]],
		);
	}
}

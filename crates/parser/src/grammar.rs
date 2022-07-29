use crate::Parser;
use syntax::{NodeKind, TokenKind};

pub(crate) fn source_file(p: &mut Parser) {
	p.start_node(NodeKind::SourceFile);

	while !p.eof() {
		opt_item(p);
	}

	p.finish_node();
}

fn opt_item(p: &mut Parser) {
	match p.peek() {
		Some(TokenKind::StructKw) => strukt(p),
		Some(_) => p.error_without_recovery("item"),
		None => unreachable!(),
	}
}

fn strukt(p: &mut Parser) {
	p.start_node(NodeKind::Strukt);
	p.bump(TokenKind::StructKw);
	p.expect_with_name(TokenKind::Ident, "struct name");
	p.expect(TokenKind::LBrace);

	while !p.at_recovery() && !p.at(TokenKind::RBrace) {
		p.start_node(NodeKind::Field);
		p.expect_with_name(TokenKind::Ident, "field name");
		opt_ty(p);
		p.finish_node();

		if p.at(TokenKind::Comma) {
			p.bump(TokenKind::Comma);
		}
	}

	p.expect(TokenKind::RBrace);
	p.finish_node();
}

fn opt_ty(p: &mut Parser) {
	if !p.at(TokenKind::Ident) {
		p.error("type");
		return;
	}

	p.start_node(NodeKind::Ty);
	p.bump(TokenKind::Ident);
	p.finish_node();
}

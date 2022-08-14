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
		Some(TokenKind::FnKw) => function(p),
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

fn function(p: &mut Parser) {
	p.start_node(NodeKind::Function);
	p.bump(TokenKind::FnKw);
	p.expect(TokenKind::Ident);
	p.expect(TokenKind::LParen);
	p.expect(TokenKind::RParen);

	if p.at(TokenKind::LBrace) {
		block(p);
	} else {
		p.error("function body")
	}

	p.finish_node();
}

fn block(p: &mut Parser) {
	p.start_node(NodeKind::Block);
	p.bump(TokenKind::LBrace);

	while !p.at_recovery() && !p.at(TokenKind::RBrace) {
		stmt(p);
	}

	p.expect(TokenKind::RBrace);
	p.finish_node();
}

fn stmt(p: &mut Parser) {
	match p.peek() {
		Some(TokenKind::VarKw) => var_stmt(p),
		_ => p.error("statement"),
	}
}

fn var_stmt(p: &mut Parser) {
	p.start_node(NodeKind::VarStmt);
	p.bump(TokenKind::VarKw);
	p.expect(TokenKind::Ident);
	p.expect(TokenKind::Eq);
	expr(p);
	p.finish_node();
}

fn expr(p: &mut Parser) {
	p.start_node(NodeKind::IntegerExpr);
	p.expect(TokenKind::Integer);
	p.finish_node();
}

fn opt_ty(p: &mut Parser) {
	match p.peek() {
		Some(TokenKind::Ident) => {
			p.start_node(NodeKind::NamedTy);
			path(p);
			p.finish_node();
		}
		Some(TokenKind::Star) => {
			p.start_node(NodeKind::PointerTy);
			p.bump(TokenKind::Star);
			opt_ty(p);
			p.finish_node();
		}
		Some(TokenKind::U32Kw) => {
			p.start_node(NodeKind::PrimitiveTy);
			p.bump(TokenKind::U32Kw);
			p.finish_node();
		}
		_ => p.error("type"),
	}
}

fn path(p: &mut Parser) {
	if p.lookahead() != Some(TokenKind::Dot) {
		p.start_node(NodeKind::LocalPath);
		p.bump(TokenKind::Ident);
		p.finish_node();
		return;
	}

	p.start_node(NodeKind::ForeignPath);
	p.bump(TokenKind::Ident);
	p.bump(TokenKind::Dot);
	p.expect_with_name(TokenKind::Ident, "path segment");
	p.finish_node();
}

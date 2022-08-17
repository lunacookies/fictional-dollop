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
	expr_bp(p, 0);
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> Option<usize> {
	let lhs = lhs(p)?;

	while let Some(k) = p.peek() {
		let bp = match k {
			TokenKind::Eq => todo!("assignment"),
			TokenKind::PipePipe => 1,
			TokenKind::AndAnd => 2,
			TokenKind::EqEq
			| TokenKind::BangEq
			| TokenKind::Lt
			| TokenKind::Gt
			| TokenKind::LtEq
			| TokenKind::GtEq => 3,
			TokenKind::Pipe => 4,
			TokenKind::Caret => 5,
			TokenKind::And => 6,
			TokenKind::LtLt | TokenKind::GtGt => 7,
			TokenKind::Plus | TokenKind::Hyphen => 8,
			TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 9,
			_ => break,
		};

		if bp < min_bp {
			break;
		}

		p.events.insert(lhs, crate::Event::StartNode(NodeKind::BinaryExpr));
		p.bump_any();
		expr_bp(p, bp + 1);
		p.finish_node();
	}

	Some(lhs)
}

fn lhs(p: &mut Parser) -> Option<usize> {
	let idx = p.events.len();

	match p.peek() {
		Some(TokenKind::Integer) => {
			p.start_node(NodeKind::IntegerExpr);
			p.bump(TokenKind::Integer);
			p.finish_node();
		}
		Some(TokenKind::Ident) => {
			p.start_node(NodeKind::VariableExpr);
			p.bump(TokenKind::Ident);
			p.finish_node();
		}
		Some(TokenKind::LBrace) => block(p),
		_ => {
			p.error("expression");
			return None;
		}
	}

	Some(idx)
}

fn block(p: &mut Parser) {
	p.start_node(NodeKind::BlockExpr);
	p.bump(TokenKind::LBrace);

	while !p.at_recovery() && !p.at(TokenKind::RBrace) {
		stmt(p);
	}

	p.expect(TokenKind::RBrace);
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

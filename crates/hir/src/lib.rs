use arena::{Arena, Id};
use cst::{CstNode, CstToken};
use resolved_index::{Stub, Ty};
use std::collections::HashMap;
use std::fmt::Write;
use syntax::SyntaxTree;
use text_size::TextRange;

pub fn lower(
	stub: &Stub,
	source_file: cst::SourceFile,
	tree: &SyntaxTree,
) -> (Hir, Vec<Error>) {
	let mut ctx = LowerCtx {
		stub,
		tree,
		hir: Hir::default(),
		scopes: Vec::new(),
		errors: Vec::new(),
	};

	for item in source_file.items(tree) {
		let (name, body) = match item {
			cst::Item::Strukt(_) => continue,
			cst::Item::Function(f) => match f.name(tree) {
				Some(i) => {
					let body = f.body(tree).map(cst::Expr::BlockExpr);
					(i.text(tree), body)
				}
				None => continue,
			},
		};

		ctx.function(name, body);
	}

	(ctx.hir, ctx.errors)
}

#[derive(Default)]
pub struct Hir {
	pub map: HashMap<String, Id<Expr>>,
	pub exprs: Arena<Expr>,
	pub local_defs: Arena<LocalDef>,
	pub tys: Arena<Ty>,
}

#[derive(Clone, Copy)]
pub enum Stmt {
	LocalDef(Id<LocalDef>),
}

pub struct LocalDef {
	pub ty: Id<Ty>,
	pub value: Id<Expr>,
}

pub enum Expr {
	Missing,
	Integer(u32),
	Local(Id<LocalDef>),
	Block(Vec<Stmt>),
}

pub struct Error {
	pub kind: ErrorKind,
	pub range: TextRange,
}

pub enum ErrorKind {
	UndefinedVariable,
}

struct LowerCtx<'a> {
	stub: &'a Stub,
	tree: &'a SyntaxTree,
	hir: Hir,
	scopes: Vec<HashMap<String, Id<LocalDef>>>,
	errors: Vec<Error>,
}

impl LowerCtx<'_> {
	fn function(&mut self, name: &str, body: Option<cst::Expr>) {
		let (expr, _ty) = self.expr(body);
		self.hir.map.insert(name.to_string(), expr);
	}

	fn stmt(&mut self, stmt: cst::Stmt) -> Stmt {
		match stmt {
			cst::Stmt::VarStmt(var) => {
				let (value, ty) = self.expr(var.value(self.tree));
				let local_def_id =
					self.hir.local_defs.alloc(LocalDef { ty, value });

				if let Some(ident) = var.name(self.tree) {
					let name = ident.text(self.tree);
					self.scopes
						.last_mut()
						.unwrap()
						.insert(name.to_string(), local_def_id);
				}

				Stmt::LocalDef(local_def_id)
			}
		}
	}

	fn expr(&mut self, expr: Option<cst::Expr>) -> (Id<Expr>, Id<Ty>) {
		let expr = match expr {
			Some(e) => e,
			None => {
				return (
					self.hir.exprs.alloc(Expr::Missing),
					self.hir.tys.alloc(Ty::Unknown),
				)
			}
		};

		let (expr, ty) = match expr {
			cst::Expr::BlockExpr(block) => self.block_expr(block),
			cst::Expr::VariableExpr(variable) => self.variable_expr(variable),
			cst::Expr::IntegerExpr(integer) => self.integer_expr(integer),
		};
		(self.hir.exprs.alloc(expr), ty)
	}

	fn block_expr(&mut self, block: cst::BlockExpr) -> (Expr, Id<Ty>) {
		self.scopes.push(HashMap::new());
		let mut stmts = Vec::new();
		for stmt in block.stmts(self.tree) {
			stmts.push(self.stmt(stmt));
		}
		self.scopes.pop();
		(Expr::Block(stmts), self.hir.tys.alloc(Ty::Void))
	}

	fn variable_expr(
		&mut self,
		variable: cst::VariableExpr,
	) -> (Expr, Id<Ty>) {
		let name = variable.text(self.tree);

		for scope in self.scopes.iter().rev() {
			let local_def_id = match scope.get(name) {
				Some(id) => *id,
				None => continue,
			};

			return (
				Expr::Local(local_def_id),
				self.hir.local_defs.get(local_def_id).ty,
			);
		}

		self.errors.push(Error {
			kind: ErrorKind::UndefinedVariable,
			range: variable.range(self.tree),
		});
		(Expr::Missing, self.hir.tys.alloc(Ty::Unknown))
	}

	fn integer_expr(&mut self, integer: cst::IntegerExpr) -> (Expr, Id<Ty>) {
		let value = integer.text(self.tree).parse().unwrap();
		(Expr::Integer(value), self.hir.tys.alloc(Ty::U32))
	}
}

pub fn pretty_print(hir: &Hir) -> String {
	let mut ctx =
		PrettyPrintCtx { hir, output: String::new(), indentation: 0 };

	let mut map: Vec<_> = hir.map.iter().collect();
	map.sort_by_key(|(name, _)| *name);

	for (i, (name, body)) in map.into_iter().enumerate() {
		if i != 0 {
			ctx.output.push_str("\n\n");
		}
		ctx.item(name, *body);
	}

	ctx.output
}

struct PrettyPrintCtx<'a> {
	hir: &'a Hir,
	output: String,
	indentation: usize,
}

impl PrettyPrintCtx<'_> {
	fn item(&mut self, name: &str, body: Id<Expr>) {
		self.output.push_str(name);
		self.output.push_str(" = ");
		self.expr(body);
	}

	fn stmt(&mut self, stmt: Stmt) {
		match stmt {
			Stmt::LocalDef(local_def_id) => {
				let local_def = self.hir.local_defs.get(local_def_id);
				self.output.push_str("var l");
				self.output.push_str(&local_def_id.to_raw().to_string());
				self.output.push(' ');
				self.output.push_str(&resolved_index::pretty_print_ty(
					local_def.ty,
					&self.hir.tys,
				));
				self.output.push_str(" = ");
				self.expr(local_def.value);
			}
		}
	}

	fn expr(&mut self, expr: Id<Expr>) {
		match self.hir.exprs.get(expr) {
			Expr::Missing => self.output.push('?'),
			Expr::Integer(n) => self.output.push_str(&n.to_string()),
			Expr::Local(local_def_id) => {
				write!(self.output, "l{}", local_def_id.to_raw()).unwrap()
			}
			Expr::Block(stmts) => self.block_expr(stmts),
		}
	}

	fn block_expr(&mut self, stmts: &[Stmt]) {
		if stmts.is_empty() {
			self.output.push_str("{}");
			return;
		}

		self.output.push('{');
		self.indentation += 1;
		for stmt in stmts {
			self.newline();
			self.stmt(*stmt);
		}
		self.indentation -= 1;
		self.newline();
		self.output.push('}');
	}

	fn newline(&mut self) {
		self.output.push('\n');
		for _ in 0..self.indentation {
			self.output.push('\t');
		}
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
	use std::fmt::Write;

	test_utils::run_tests(|input| {
		let files = test_utils::split_multi_file_fixture(input);

		let mut syntax_trees = HashMap::new();
		for (file_name, input) in files {
			let parse = parser::parse(input);
			syntax_trees
				.insert(file_name.to_string(), (parse.node, parse.tree));
		}

		let mut raw_index = raw_index::Index { stubs: HashMap::new() };
		for (file_name, (source_file, tree)) in &syntax_trees {
			let stub = raw_index::run_indexer(*source_file, tree);
			raw_index.stubs.insert(file_name.to_string(), stub);
		}

		let mut resolved_index =
			resolved_index::Index { stubs: HashMap::new() };
		for (file_name, raw_stub) in &raw_index.stubs {
			let (resolved_stub, _) = resolved_index::resolve_raw_stub(
				raw_stub, file_name, &raw_index,
			);
			resolved_index.stubs.insert(file_name.clone(), resolved_stub);
		}

		let mut hirs = HashMap::new();
		let mut errors = HashMap::new();
		for (file_name, resolved_stub) in &resolved_index.stubs {
			let (source_file, tree) = &syntax_trees[file_name];
			let (hir, e) = lower(resolved_stub, *source_file, tree);
			hirs.insert(file_name, hir);
			errors.insert(file_name, e);
		}

		let mut output = String::new();

		let mut hirs: Vec<_> = hirs.into_iter().collect();
		hirs.sort_by_key(|(name, _)| *name);
		for (i, (file_name, hir)) in hirs.iter().enumerate() {
			if i != 0 {
				output.push_str("\n\n");
			}
			output.push_str("== ");
			output.push_str(file_name);
			output.push_str(" ==\n");
			output.push_str(&pretty_print(hir));
		}

		if errors.values().all(|e| e.is_empty()) {
			return output;
		}

		let mut errors: Vec<_> = errors.into_iter().collect();
		errors.sort_by_key(|(name, _)| *name);
		for (file_name, errors) in errors {
			for error in errors {
				write!(
					output,
					"\nerror in {file_name} at {:?}: ",
					error.range
				)
				.unwrap();
				match error.kind {
					ErrorKind::UndefinedVariable => {
						output.push_str("undefined variable")
					}
				}
			}
		}

		output
	});
}

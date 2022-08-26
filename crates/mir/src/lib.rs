use arena::{ArenaMap, Id};
use hir::{BinaryOp, Expr, Hir, LocalDef, Stmt};
use std::collections::HashMap;
use std::fmt::Write;

pub struct Mir {
	pub map: HashMap<String, (u32, u32)>,
	pub instrs: Vec<Instr>,
}

#[derive(Clone, Copy)]
pub enum Instr {
	MovImm { dst: u32, imm: u32 },
	MovReg { dst: u32, src: u32 },
	Add { dst: u32, lhs: u32, rhs: u32 },
	Sub { dst: u32, lhs: u32, rhs: u32 },
	Mul { dst: u32, lhs: u32, rhs: u32 },
	Div { dst: u32, lhs: u32, rhs: u32 },
	Mod { dst: u32, lhs: u32, rhs: u32 },
	BitAnd { dst: u32, lhs: u32, rhs: u32 },
	BitOr { dst: u32, lhs: u32, rhs: u32 },
	Xor { dst: u32, lhs: u32, rhs: u32 },
	Shl { dst: u32, lhs: u32, rhs: u32 },
	Shr { dst: u32, lhs: u32, rhs: u32 },
	Eq { dst: u32, lhs: u32, rhs: u32 },
	NEq { dst: u32, lhs: u32, rhs: u32 },
	Lt { dst: u32, lhs: u32, rhs: u32 },
	Gt { dst: u32, lhs: u32, rhs: u32 },
	LtEq { dst: u32, lhs: u32, rhs: u32 },
	GtEq { dst: u32, lhs: u32, rhs: u32 },
	And { dst: u32, lhs: u32, rhs: u32 },
	Or { dst: u32, lhs: u32, rhs: u32 },
}

pub fn build_mir(hir: &Hir) -> Mir {
	let mut ctx = BuildCtx {
		mir: Mir { map: HashMap::new(), instrs: Vec::new() },
		hir,
		current_reg: 0,
		locals: ArenaMap::new(),
	};

	for (name, body) in &hir.map {
		ctx.item(name, *body);
	}
	ctx.mir
}

struct BuildCtx<'a> {
	mir: Mir,
	hir: &'a Hir,
	current_reg: u32,
	locals: ArenaMap<LocalDef, u32>,
}

impl BuildCtx<'_> {
	fn item(&mut self, name: &str, body: Id<Expr>) {
		self.current_reg = 0;
		let start = self.mir.instrs.len() as u32;
		self.expr(body);
		let end = self.mir.instrs.len() as u32;
		self.mir.map.insert(name.to_string(), (start, end));
	}

	fn stmt(&mut self, stmt: Stmt) {
		match stmt {
			Stmt::LocalDef(local_def_id) => {
				let local_def = self.hir.local_defs.get(local_def_id);
				match self.expr(local_def.value) {
					BuildExprResult::UsedNewReg(reg) => {
						self.locals.insert(local_def_id, reg);
					}
					BuildExprResult::ReferenceExistingReg(reg) => {
						let new_reg = self.reg();
						self.emit(Instr::MovReg { dst: new_reg, src: reg });
						self.locals.insert(local_def_id, new_reg);
					}
					BuildExprResult::ZeroSized => {}
				}
			}
		}
	}

	fn expr(&mut self, expr: Id<Expr>) -> BuildExprResult {
		match self.hir.exprs.get(expr) {
			Expr::Missing => BuildExprResult::ZeroSized,
			Expr::Integer(i) => {
				let reg = self.reg();
				self.emit(Instr::MovImm { dst: reg, imm: *i });
				BuildExprResult::UsedNewReg(reg)
			}
			Expr::Local(local_def_id) => {
				match self.locals.get(*local_def_id) {
					Some(reg) => BuildExprResult::ReferenceExistingReg(*reg),
					None => BuildExprResult::ZeroSized,
				}
			}
			Expr::Block(stmts) => {
				for stmt in stmts {
					self.stmt(*stmt);
				}
				BuildExprResult::ZeroSized
			}
			Expr::Binary { lhs, rhs, op } => {
				let lhs = match self.expr(*lhs) {
					BuildExprResult::UsedNewReg(r)
					| BuildExprResult::ReferenceExistingReg(r) => Some(r),
					BuildExprResult::ZeroSized => None,
				};
				let rhs = match self.expr(*rhs) {
					BuildExprResult::UsedNewReg(r)
					| BuildExprResult::ReferenceExistingReg(r) => Some(r),
					BuildExprResult::ZeroSized => None,
				};

				let (lhs, rhs) = match (lhs, rhs) {
					(Some(lhs), Some(rhs)) => (lhs, rhs),
					_ => return BuildExprResult::ZeroSized,
				};

				let dst = self.reg();
				let instr = match *op {
					BinaryOp::Add => Instr::Add { dst, lhs, rhs },
					BinaryOp::Sub => Instr::Sub { dst, lhs, rhs },
					BinaryOp::Mul => Instr::Mul { dst, lhs, rhs },
					BinaryOp::Div => Instr::Div { dst, lhs, rhs },
					BinaryOp::Mod => Instr::Mod { dst, lhs, rhs },
					BinaryOp::BitAnd => Instr::BitAnd { dst, lhs, rhs },
					BinaryOp::BitOr => Instr::BitOr { dst, lhs, rhs },
					BinaryOp::BitXor => Instr::Xor { dst, lhs, rhs },
					BinaryOp::Shl => Instr::Shl { dst, lhs, rhs },
					BinaryOp::Shr => Instr::Shr { dst, lhs, rhs },
					BinaryOp::Eq => Instr::Eq { dst, lhs, rhs },
					BinaryOp::NEq => Instr::NEq { dst, lhs, rhs },
					BinaryOp::Lt => Instr::Lt { dst, lhs, rhs },
					BinaryOp::Gt => Instr::Gt { dst, lhs, rhs },
					BinaryOp::LtEq => Instr::LtEq { dst, lhs, rhs },
					BinaryOp::GtEq => Instr::GtEq { dst, lhs, rhs },
					BinaryOp::And => Instr::And { dst, lhs, rhs },
					BinaryOp::Or => Instr::Or { dst, lhs, rhs },
				};
				self.emit(instr);
				BuildExprResult::UsedNewReg(dst)
			}
		}
	}

	fn emit(&mut self, instr: Instr) {
		self.mir.instrs.push(instr);
	}

	fn reg(&mut self) -> u32 {
		let r = self.current_reg;
		self.current_reg += 1;
		r
	}
}

enum BuildExprResult {
	UsedNewReg(u32),
	ReferenceExistingReg(u32),
	ZeroSized,
}

pub fn pretty_print(mir: &Mir) -> String {
	let mut ctx = PrettyPrintCtx { mir, output: String::new() };

	let mut map: Vec<_> = mir.map.iter().collect();
	map.sort_by_key(|(name, _)| *name);

	for (i, (name, range)) in map.into_iter().enumerate() {
		if i != 0 {
			ctx.output.push('\n');
		}
		ctx.item(name, *range);
	}

	ctx.output
}

struct PrettyPrintCtx<'a> {
	mir: &'a Mir,
	output: String,
}

impl PrettyPrintCtx<'_> {
	fn item(&mut self, name: &str, range: (u32, u32)) {
		self.output.push_str(name);
		self.output.push(':');

		for i in range.0..range.1 {
			self.output.push_str("\n\t");
			self.instr(self.mir.instrs[i as usize]);
		}
	}

	fn instr(&mut self, instr: Instr) {
		match instr {
			Instr::MovImm { dst, imm } => {
				write!(self.output, "mov\tr{dst}, {imm}")
			}
			Instr::MovReg { dst, src } => {
				write!(self.output, "mov\tr{dst}, r{src}")
			}
			Instr::Add { dst, lhs, rhs } => {
				write!(self.output, "add\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Sub { dst, lhs, rhs } => {
				write!(self.output, "sub\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Mul { dst, lhs, rhs } => {
				write!(self.output, "mul\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Div { dst, lhs, rhs } => {
				write!(self.output, "div\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Mod { dst, lhs, rhs } => {
				write!(self.output, "mod\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::BitAnd { dst, lhs, rhs } => {
				write!(self.output, "bitand\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::BitOr { dst, lhs, rhs } => {
				write!(self.output, "bitor\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Xor { dst, lhs, rhs } => {
				write!(self.output, "xor\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Shl { dst, lhs, rhs } => {
				write!(self.output, "shl\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Shr { dst, lhs, rhs } => {
				write!(self.output, "shr\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Eq { dst, lhs, rhs } => {
				write!(self.output, "eq\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::NEq { dst, lhs, rhs } => {
				write!(self.output, "neq\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Lt { dst, lhs, rhs } => {
				write!(self.output, "lt\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Gt { dst, lhs, rhs } => {
				write!(self.output, "gt\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::LtEq { dst, lhs, rhs } => {
				write!(self.output, "lteq\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::GtEq { dst, lhs, rhs } => {
				write!(self.output, "gteq\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::And { dst, lhs, rhs } => {
				write!(self.output, "and\tr{dst}, r{lhs}, r{rhs}")
			}
			Instr::Or { dst, lhs, rhs } => {
				write!(self.output, "or\tr{dst}, r{lhs}, r{rhs}")
			}
		}
		.unwrap()
	}
}

#[cfg(test)]
#[test]
fn run_tests() {
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
		for (file_name, resolved_stub) in resolved_index.stubs {
			let (source_file, tree) = &syntax_trees[&file_name];
			let (hir, _) = hir::lower(&resolved_stub, *source_file, tree);
			hirs.insert(file_name.clone(), hir);
		}

		let mut mirs = HashMap::new();
		for (file_name, hir) in &hirs {
			mirs.insert(file_name, build_mir(hir));
		}

		let mut output = String::new();

		let mut mirs: Vec<_> = mirs.into_iter().collect();
		mirs.sort_by_key(|(name, _)| *name);
		for (i, (file_name, mir)) in mirs.into_iter().enumerate() {
			if i != 0 {
				output.push('\n');
			}
			output.push_str("== ");
			output.push_str(file_name);
			output.push_str(" ==\n");
			output.push_str(&pretty_print(&mir));
		}

		output
	});
}

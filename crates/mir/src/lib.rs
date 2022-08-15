use arena::Id;
use hir::{Expr, Hir, Stmt};
use std::collections::HashMap;
use std::fmt::Write;

pub struct Mir {
	pub map: HashMap<String, (u32, u32)>,
	pub instrs: Vec<Instr>,
}

#[derive(Clone, Copy)]
pub enum Instr {
	MovImm { reg: u32, value: u32 },
}

pub fn build_mir(hir: &Hir) -> Mir {
	let mut ctx = BuildCtx {
		mir: Mir { map: HashMap::new(), instrs: Vec::new() },
		hir,
		current_reg: 0,
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
				self.expr(local_def.value);
			}
		}
	}

	fn expr(&mut self, expr: Id<Expr>) -> Option<u32> {
		match self.hir.exprs.get(expr) {
			Expr::Missing => None,
			Expr::Integer(i) => {
				let reg = self.reg();
				self.emit(Instr::MovImm { reg, value: *i });
				Some(reg)
			}
			Expr::Block(stmts) => {
				for stmt in stmts {
					self.stmt(*stmt);
				}
				None
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
			Instr::MovImm { reg: register, value } => {
				write!(self.output, "mov\tr{register}, {value}")
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

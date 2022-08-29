use mir::{Instr, Mir};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Write;

pub fn codegen(
	mir: &Mir,
	stub: &resolved_index::Stub,
	stub_name: &str,
) -> HashMap<String, String> {
	let mut asms = HashMap::new();
	for (function_name, &(start, end)) in &mir.map {
		let ctx = Ctx { mir, stub, stub_name, asm: String::new() };
		let asm = ctx.function(function_name, start, end);
		asms.insert(function_name.clone(), asm);
	}
	asms
}

struct Ctx<'a> {
	mir: &'a Mir,
	stub: &'a resolved_index::Stub,
	stub_name: &'a str,
	asm: String,
}

impl Ctx<'_> {
	fn function(
		mut self,
		function_name: &str,
		start: u32,
		end: u32,
	) -> String {
		write!(self.asm, "_{}_{function_name}:", self.stub_name).unwrap();

		let instrs = &self.mir.instrs[start as usize..end as usize];

		let (reg_map, largest_offset_used) = stack_alloc(instrs);

		let stack_space_needed = nearest_multiple_of(
			// 8 bytes for link address
			largest_offset_used + 8,
			// aarch64 stack pointer must have alignment of 16
			16,
		);

		self.sub_stack_pointer(stack_space_needed);
		write!(self.asm, "\n\tstr\tx30, [sp, #{largest_offset_used}]")
			.unwrap();
		for instr in instrs {
			self.instr(instr, &reg_map);
		}
		write!(self.asm, "\n\tldr\tx30, [sp, #{largest_offset_used}]")
			.unwrap();
		self.add_stack_pointer(stack_space_needed);
		write!(self.asm, "\n\tret").unwrap();

		self.asm
	}

	fn instr(&mut self, instr: &Instr, reg_map: &HashMap<u32, u32>) {
		match *instr {
			Instr::MovImm { dst, imm } => {
				self.write_imm_to_scratch(imm, 0);
				self.store_scratch_to_stack(reg_map[&dst], 0);
			}
			Instr::MovReg { dst, src } => {
				self.load_stack_to_scratch(reg_map[&src], 0);
				self.store_scratch_to_stack(reg_map[&dst], 0);
			}
			Instr::Add { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tadd\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Sub { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tsub\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Mul { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tmul\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Div { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tudiv\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Mod { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tudiv\tx10, x8, x9").unwrap();
				write!(self.asm, "\n\tmsub\tx11, x10, x9, x8").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 3);
			}
			Instr::BitAnd { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tand\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::BitOr { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\torr\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Xor { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\teor\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Shl { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tlsl\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Shr { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tlsr\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Eq { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, eq").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::NEq { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, ne").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Lt { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, lo").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Gt { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, hi").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::LtEq { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, ls").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::GtEq { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tcmp\tx8, x9").unwrap();
				write!(self.asm, "\n\tcset\tx10, hs").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::And { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\tand\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
			Instr::Or { dst, lhs, rhs } => {
				self.load_stack_to_scratch(reg_map[&lhs], 0);
				self.load_stack_to_scratch(reg_map[&rhs], 1);
				write!(self.asm, "\n\torr\tx10, x8, x9").unwrap();
				self.store_scratch_to_stack(reg_map[&dst], 2);
			}
		}
	}

	fn load_stack_to_scratch(&mut self, offset: u32, scratch: u32) {
		write!(self.asm, "\n\tldr\tx{}, [sp, #{offset}]", scratch + 8)
			.unwrap();
	}

	fn store_scratch_to_stack(&mut self, offset: u32, scratch: u32) {
		write!(self.asm, "\n\tstr\tx{}, [sp, #{offset}]", 8 + scratch)
			.unwrap();
	}

	fn write_imm_to_scratch(&mut self, imm: u32, scratch: u32) {
		write!(self.asm, "\n\tmov\tx{}, #{imm}", 8 + scratch).unwrap();
	}

	fn sub_stack_pointer(&mut self, offset: u32) {
		write!(self.asm, "\n\tsub\tsp, sp, #{offset}").unwrap();
	}

	fn add_stack_pointer(&mut self, offset: u32) {
		write!(self.asm, "\n\tadd\tsp, sp, #{offset}").unwrap();
	}
}

// returns map from MIR registers to stack pointer offsets
// and largest offset used
fn stack_alloc(instrs: &[Instr]) -> (HashMap<u32, u32>, u32) {
	let mut map = HashMap::new();
	let mut current_offset = 0;

	let mut handle_reg = |reg| {
		if let Entry::Vacant(e) = map.entry(reg) {
			e.insert(current_offset);
			current_offset += 8;
		}
	};

	for instr in instrs {
		match *instr {
			Instr::MovImm { dst, imm: _ } => handle_reg(dst),
			Instr::MovReg { dst, src } => {
				handle_reg(dst);
				handle_reg(src);
			}
			Instr::Add { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Sub { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Mul { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Div { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Mod { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::BitAnd { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::BitOr { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Xor { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Shl { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Shr { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Eq { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::NEq { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Lt { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Gt { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::LtEq { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::GtEq { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::And { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
			Instr::Or { dst, lhs, rhs } => {
				handle_reg(dst);
				handle_reg(lhs);
				handle_reg(rhs);
			}
		}
	}

	(map, current_offset)
}

fn nearest_multiple_of(x: u32, y: u32) -> u32 {
	div_ceil(x, y) * y
}

fn div_ceil(x: u32, y: u32) -> u32 {
	let d = x / y;
	let r = x % y;
	if r > 0 && y > 0 {
		d + 1
	} else {
		d
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
		for (file_name, resolved_stub) in &resolved_index.stubs {
			let (source_file, tree) = &syntax_trees[file_name];
			let (hir, _) = hir::lower(
				resolved_stub,
				file_name,
				&resolved_index,
				*source_file,
				tree,
			);
			hirs.insert(file_name.clone(), hir);
		}

		let mut mirs = HashMap::new();
		for (file_name, hir) in &hirs {
			mirs.insert(file_name.clone(), mir::build_mir(hir));
		}

		let mut asms = HashMap::new();

		for (file_name, mir) in &mirs {
			asms.insert(
				file_name,
				codegen(mir, &resolved_index.stubs[file_name], file_name),
			);
		}

		let mut output = String::new();
		let mut asms: Vec<_> = asms.into_iter().collect();
		asms.sort_by_key(|(name, _)| *name);
		for (i, (_file_name, asm)) in asms.into_iter().enumerate() {
			if i != 0 {
				output.push('\n');
			}

			let mut asm: Vec<_> = asm.iter().collect();
			asm.sort_by_key(|(name, _)| *name);
			for (i, (_function_name, asm)) in asm.into_iter().enumerate() {
				if i != 0 {
					output.push('\n');
				}
				output.push_str(asm);
			}
		}

		output
	});
}

use std::fmt;
use super::types::*;

use std::collections::{VecDeque, HashSet};


pub fn format_function(f: &mut fmt::Formatter, function: &Function) -> fmt::Result {
	f.write_str("{\n")?;

	let mut block_queue = VecDeque::from([function.entry]);
	let mut seen = HashSet::new();

	while let Some(block_key) = block_queue.pop_front() {
		if !seen.insert(block_key) {
			continue
		}

		let block = &function.blocks[block_key];

		write!(f, "\t{block_key:?}:\n")?;
		for &inst_key in block.insts.iter() {
			f.write_str("\t\t")?;
			format_inst(f, function, inst_key)?;
			f.write_str("\n")?;
		}

		// if !block.successors.is_empty() {
		// 	f.write_str("\t\tsucc: ")?;
		// }

		for &successor in block.successors.iter() {
			// write!(f, "{successor:?} ")?;
			if !seen.contains(&successor) {
				block_queue.push_back(successor);
			}
		}

		f.write_str("\n")?;
	} 

	f.write_str("}\n")?;
	Ok(())
}



fn format_inst(f: &mut fmt::Formatter, function: &Function, inst_key: InstKey) -> fmt::Result {
	let inst = &function.insts[inst_key];

	write!(f, "{} = ", inst.name)?;

	match inst.data {
		InstData::ConstInt(v) => write!(f, "ConstInt({v})"),
		InstData::ConstFloat(v) => write!(f, "ConstFloat({v})"),
		InstData::LoadGlobal(ref name) => write!(f, "LoadGlobal({name})"),

		InstData::Jump(to) => write!(f, "Jump({to:?})"),
		InstData::JumpIf{condition, then_block, else_block} =>
			write!(f, "JumpIf({}, {then_block:?}, {else_block:?})", function.get_inst_name(condition)),

		InstData::Add(l, r) => {
			write!(f, "{} + {}", function.get_inst_name(l), function.get_inst_name(r))
		},
	}
}
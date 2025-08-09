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
		f.write_str("\t\t- from: ")?;

		for pred_id in block.predecessors.iter() {
			write!(f, "{pred_id:?} ")?;
		}

		f.write_str("\n")?;

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


	let unseen: Vec<_> = function.blocks.keys()
		.filter(|key| !seen.contains(&key))
		.collect();

	if !unseen.is_empty() {
		f.write_str("orphaned blocks:\n")?;

		for block_key in unseen {
			let block = &function.blocks[block_key];

			write!(f, "\t{block_key:?}:\n")?;
			for &inst_key in block.insts.iter() {
				f.write_str("\t\t")?;
				format_inst(f, function, inst_key)?;
				f.write_str("\n")?;
			}
		}
	}

	f.write_str("}\n")?;
	Ok(())
}



fn format_inst(f: &mut fmt::Formatter, function: &Function, inst_key: InstKey) -> fmt::Result {
	let inst = &function.insts[inst_key];

	if !inst.data.is_terminator() {
		write!(f, "{} = ", inst.name)?;
	}

	match inst.data {
		InstData::ConstUnit => write!(f, "ConstUnit"),
		InstData::ConstInt(v) => write!(f, "ConstInt({v})"),
		InstData::ConstFloat(v) => write!(f, "ConstFloat({})", v.0),
		InstData::ConstBool(v) => write!(f, "ConstBool({v})"),
		InstData::ConstString(ref v) => write!(f, "ConstString(\"{v}\")"),

		InstData::LoadGlobal(ref name) => write!(f, "LoadGlobal({name})"),

		InstData::Jump(to) => write!(f, "Jump({to:?})"),
		InstData::JumpIf{condition, then_block, else_block} =>
			write!(f, "JumpIf({}, then: {then_block:?}, else: {else_block:?})", function.get_inst_name(condition)),

		InstData::Return{value} => {
			write!(f, "Return({})", function.get_inst_name(value))
		}

		InstData::Call{target, ref arguments} => {
			write!(f, "{}( ", function.get_inst_name(target))?;

			for arg in arguments {
				write!(f, "{}, ", function.get_inst_name(*arg))?;
			}

			f.write_str(")")
		}

		InstData::Phi{ref arguments} => {
			write!(f, "phi(")?;

			for arg in arguments {
				write!(f, "{}, ", function.get_inst_name(*arg))?;
			}

			f.write_str(")")
		}

		InstData::Not(value) => write!(f, "!{}", function.get_inst_name(value)),
		InstData::Negate(value) => write!(f, "-{}", function.get_inst_name(value)),

		InstData::Add(l, r) => write!(f, "{} + {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::Sub(l, r) => write!(f, "{} - {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::Mul(l, r) => write!(f, "{} * {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::Div(l, r) => write!(f, "{} / {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::Rem(l, r) => write!(f, "{} % {}", function.get_inst_name(l), function.get_inst_name(r)),

		InstData::CmpEqual(l, r) => write!(f, "{} == {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::CmpNotEqual(l, r) => write!(f, "{} != {}", function.get_inst_name(l), function.get_inst_name(r)),

		InstData::CmpLesser(l, r) => write!(f, "{} < {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::CmpGreater(l, r) => write!(f, "{} > {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::CmpLesserEqual(l, r) => write!(f, "{} <= {}", function.get_inst_name(l), function.get_inst_name(r)),
		InstData::CmpGreaterEqual(l, r) => write!(f, "{} >= {}", function.get_inst_name(l), function.get_inst_name(r)),
	}
}
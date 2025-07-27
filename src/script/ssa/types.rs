use slotmap::SlotMap;
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::fmt;

use super::formatting::*;


slotmap::new_key_type! {
	pub struct InstKey;
	pub struct BasicBlockKey;
}


#[derive(Clone, Debug)]
pub enum InstData {
	ConstUnit,
	ConstInt(i64),
	ConstFloat(f64),
	ConstBool(bool),
	ConstString(String),

	LoadGlobal(String),

	Jump(BasicBlockKey),

	JumpIf {
		condition: InstKey,
		then_block: BasicBlockKey,
		else_block: BasicBlockKey,
	},

	Return {
		value: InstKey,
	},

	Call {
		target: InstKey,
		arguments: SmallVec<[InstKey; 4]>,
	},

	// Phi {

	// },

	Not(InstKey),
	Negate(InstKey),

	Add(InstKey, InstKey),
	Sub(InstKey, InstKey),
	Mul(InstKey, InstKey),
	Div(InstKey, InstKey),
	Rem(InstKey, InstKey),
}

impl InstData {
	pub fn is_terminator(&self) -> bool {
		match *self {
			InstData::Jump(..) | InstData::JumpIf{..} | InstData::Return{..} => true,
			_ => false,
		}
	}

	pub fn get_successors(&self) -> SmallVec<[BasicBlockKey; 2]> {
		match *self {
			InstData::Jump(key) => SmallVec::from_slice(&[key]),
			InstData::JumpIf{then_block, else_block, ..}
				=> SmallVec::from([then_block, else_block]),

			_ => SmallVec::default(),
		}
	}
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InstName(SmolStr, u32);

impl fmt::Display for InstName {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let InstName(string, number) = self;
		write!(f, "${string}.{number}")
	}
}


#[derive(Clone, Debug)]
pub struct Inst {
	pub name: InstName,
	pub data: InstData,
	pub users: SmallVec<[InstKey; 2]>,
}


#[derive(Debug)]
pub struct BasicBlock {
	pub id: BasicBlockKey,
	pub insts: SmallVec<[InstKey; 8]>,
	pub successors: SmallVec<[BasicBlockKey; 2]>,
}

impl BasicBlock {
	pub fn new(id: BasicBlockKey) -> BasicBlock {
		BasicBlock {
			id,
			insts: SmallVec::default(),
			successors: SmallVec::default(),
		}
	}
}


#[derive(Debug)]
pub struct Function {
	pub insts: SlotMap<InstKey, Inst>,
	pub blocks: SlotMap<BasicBlockKey, BasicBlock>,
	pub entry: BasicBlockKey,
	pub exit: BasicBlockKey,
	pub counter: u32,
}

impl Function {
	pub fn new() -> Function {
		let mut blocks = SlotMap::with_key();
		let entry = blocks.insert_with_key(BasicBlock::new);
		let exit = blocks.insert_with_key(BasicBlock::new);

		Function {
			insts: SlotMap::with_key(),
			blocks,
			entry,
			exit,
			counter: 0,
		}
	}

	pub fn new_block(&mut self) -> BasicBlockKey {
		self.blocks.insert_with_key(BasicBlock::new)
	}

	pub fn build_block(&mut self, id: BasicBlockKey) -> BlockBuilder<'_> {
		BlockBuilder { function: self, id }
	}

	pub fn get_inst_name(&self, key: InstKey) -> &InstName {
		&self.insts[key].name
	}

	pub fn new_inst(&mut self, name: &str, data: InstData) -> InstKey {
		let counter = self.counter;
		self.counter += 1;

		self.insts.insert(Inst {
			name: InstName(SmolStr::from(name), counter),
			data,
			users: SmallVec::default(),
		})
	}

	pub fn update_inst_name(&mut self, key: InstKey, name: &str) {
		self.insts[key].name.0 = SmolStr::from(name);
	}
}

impl fmt::Display for Function {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		format_function(f, self)
	}
}



pub struct BlockBuilder<'f> {
	pub function: &'f mut Function,
	pub id: BasicBlockKey,
}

impl BlockBuilder<'_> {
	pub fn add_named_inst(&mut self, name: &str, data: InstData) -> InstKey {
		let successors = data.get_successors();
		let key = self.function.new_inst(name, data);
		let block = &mut self.function.blocks[self.id];
		block.insts.push(key);

		if !successors.is_empty() {
			block.successors = successors;
		}

		key
	}

	pub fn add_inst(&mut self, data: InstData) -> InstKey {
		self.add_named_inst("", data)
	}

	pub fn const_unit(&mut self) -> InstKey {
		self.add_inst(InstData::ConstUnit)
	}
	pub fn const_int(&mut self, value: i64) -> InstKey {
		self.add_inst(InstData::ConstInt(value))
	}
	pub fn const_float(&mut self, value: f64) -> InstKey {
		self.add_inst(InstData::ConstFloat(value))
	}

	pub fn jump(&mut self, to: BasicBlockKey) -> InstKey {
		self.add_inst(InstData::Jump(to))
	}
}
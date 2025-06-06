


#[derive(Debug)]
pub enum Instruction {
	PushNull,
	PushBool(bool),
	PushInt(i64),
	PushFloat(f64),
	PushString(String),

	Lookup(String),
	TableLookup(String),

	Discard(u32),
	CallFunction(u32),
	Return(u32),

	Add, Subtract,
	Divide, Multiply,
	Remainder,
}

impl Instruction {
	pub fn pushes(&self) -> u32 {
		use Instruction::*;
		match self {
			PushNull | PushBool(_) | PushInt(_) | PushFloat(_) | PushString(_) | Lookup(_)  => 1,
			Add | Subtract | Divide | Multiply | Remainder  => 1,
			CallFunction(_) => 1,
			_ => 0
		}
	}

	pub fn pops(&self) -> u32 {
		use Instruction::*;
		match self {
			TableLookup(_) => 1,
			Add | Subtract | Divide | Multiply | Remainder  => 2,
			Discard(count) => *count,
			CallFunction(arguments) => *arguments + 1,
			_ => 0
		}
	}

	pub fn adjusts_stack(&self) -> i64 {
		self.pushes() as i64 - self.pops() as i64
	}

	pub fn has_side_effects(&self) -> bool {
		match self {
			Instruction::CallFunction(_) => true,
			Instruction::Return(_) => true,
			_ => false
		}
	}
}

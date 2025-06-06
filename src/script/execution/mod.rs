pub mod instructions;

#[cfg(test)]
mod tests;

use anyhow::Context;



#[derive(Clone, Debug, PartialEq)]
pub struct FunctionRef;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
	Null,
	Bool(bool),
	Int(i64),
	Float(f64),
	String(String),
	Function(FunctionRef),
}

#[derive(Default)]
pub struct Environment {

}

#[derive(Debug)]
pub struct Frame {
	pub function: FunctionRef,
	pub instruction_index: usize,
	pub stack_base: usize, // Size of stack before arguments
}

#[derive(Default)]
pub struct ExecutionState {
	pub environment: Environment,
	pub stack: Vec<Value>,
	pub frames: Vec<Frame>,
}

impl ExecutionState {
	pub fn enter_global_frame(&mut self) {
		self.frames.push(Frame {
			function: FunctionRef,
			instruction_index: 0,
			stack_base: self.stack.len(),
		});
	}

	pub fn enter_function_frame(&mut self, function: FunctionRef, num_args: u32) {
		self.frames.push(Frame {
			function,
			instruction_index: 0,
			stack_base: self.stack.len() - num_args as usize,
		});
	}

	pub fn leave_frame(&mut self) -> anyhow::Result<()> {
		anyhow::ensure!(!self.frames.is_empty());
		self.frames.pop();
		Ok(())
	}

	pub fn advance(&mut self) -> anyhow::Result<()> {
		self.frames.last_mut()
			.context("Not currently in an execution frame")?
			.instruction_index += 1;

		Ok(())
	}

	pub fn pop(&mut self) -> anyhow::Result<Value> {
		self.stack.pop()
			.context("Tried to pop but stack is empty")
	}

	pub fn pop_function(&mut self) -> anyhow::Result<FunctionRef> {
		match self.pop()? {
			Value::Function(function) => Ok(function),
			value => anyhow::bail!("Tried to pop function but got {value:?}"),
		}
	}

	pub fn discard(&mut self, count: u32) -> anyhow::Result<()> {
		let Some(new_size) = self.stack.len().checked_sub(count as usize) else {
			anyhow::bail!("Tried to discard more elements than were in stack");
		};

		self.stack.truncate(new_size);

		Ok(())
	}
}

use instructions::Instruction;

pub fn execute_instruction(state: &mut ExecutionState, inst: &Instruction) -> anyhow::Result<()> {
	use Value::*;

	match inst {
		Instruction::PushNull => {
			state.stack.push(Value::Null);
			state.advance()?;
		}
		Instruction::PushBool(value) => {
			state.stack.push(Value::Bool(*value));
			state.advance()?;
		}
		Instruction::PushInt(value) => {
			state.stack.push(Value::Int(*value));
			state.advance()?;
		}
		Instruction::PushFloat(value) => {
			state.stack.push(Value::Float(*value));
			state.advance()?;
		}
		Instruction::PushString(value) => {
			state.stack.push(Value::String(value.clone()));
			state.advance()?;
		}

		Instruction::Lookup(_name) => unimplemented!(),
		Instruction::TableLookup(_name) => unimplemented!(),

		Instruction::Discard(count) => {
			state.discard(*count)?;
			state.advance()?;
		}

		Instruction::CallFunction(arg_count) => {
			let Some(first_index) = state.stack.len().checked_sub(*arg_count as usize) else {
				anyhow::bail!("Invalid function call: not enough items in stack");
			};

			anyhow::ensure!(first_index > 0);

			let function = state.pop_function()?;

			state.advance()?;
			state.enter_function_frame(function, *arg_count);
		}

		Instruction::Return(_) => {
			// TODO(pat.m): return values?
			// 
			state.leave_frame()?;
		}


		Instruction::Add => {
			let right = state.pop()?;
			let left = state.pop()?;

			match (left, right) {
				(Int(left), Int(right)) => state.stack.push(Int(left + right)),
				(Float(left), Float(right)) => state.stack.push(Float(left + right)),
				(left, right) => anyhow::bail!("Invalid arguments to Add: {left:?} and {right:?}"),
			}

			state.advance()?;
		}
		Instruction::Subtract => {
			let right = state.pop()?;
			let left = state.pop()?;

			match (left, right) {
				(Int(left), Int(right)) => state.stack.push(Int(left - right)),
				(Float(left), Float(right)) => state.stack.push(Float(left - right)),
				(left, right) => anyhow::bail!("Invalid arguments to Subtract: {left:?} and {right:?}"),
			}

			state.advance()?;
		}
		Instruction::Divide => {
			let right = state.pop()?;
			let left = state.pop()?;

			match (left, right) {
				(Int(_), Int(0)) => anyhow::bail!("Attempting to divide by zero"),
				(Int(left), Int(right)) => state.stack.push(Int(left / right)),
				(Float(left), Float(right)) => state.stack.push(Float(left / right)),
				(left, right) => anyhow::bail!("Invalid arguments to Divide: {left:?} and {right:?}"),
			}

			state.advance()?;
		}
		Instruction::Multiply => {
			let right = state.pop()?;
			let left = state.pop()?;

			match (left, right) {
				(Int(left), Int(right)) => state.stack.push(Int(left * right)),
				(Float(left), Float(right)) => state.stack.push(Float(left * right)),
				(left, right) => anyhow::bail!("Invalid arguments to Multiply: {left:?} and {right:?}"),
			}

			state.advance()?;
		}
		Instruction::Remainder => {
			let right = state.pop()?;
			let left = state.pop()?;

			match (left, right) {
				(Int(_), Int(0)) => anyhow::bail!("Attempting to mod by zero"),
				(Int(left), Int(right)) => state.stack.push(Int(left % right)),
				(Float(left), Float(right)) => state.stack.push(Float(left % right)),
				(left, right) => anyhow::bail!("Invalid arguments to Remainder: {left:?} and {right:?}"),
			}

			state.advance()?;
		}
	}

	Ok(())
}
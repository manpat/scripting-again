use super::*;
use instructions::Instruction::*;

fn make_state(stack: &[Value]) -> ExecutionState {
	let mut state = ExecutionState::default();
	state.enter_global_frame();
	state.stack = stack.to_vec();
	state
}

#[test]
fn test_push() -> anyhow::Result<()> {
	let mut state = ExecutionState::default();
	state.enter_global_frame();

	execute_instruction(&mut state, &PushNull)?;
	execute_instruction(&mut state, &PushBool(false))?;
	execute_instruction(&mut state, &PushInt(69))?;
	execute_instruction(&mut state, &PushFloat(32.0))?;
	execute_instruction(&mut state, &PushString("foo".into()))?;

	assert_eq!(&state.stack, &[
		Value::Null,
		Value::Bool(false),
		Value::Int(69),
		Value::Float(32.0),
		Value::String("foo".into()),
	]);

	execute_instruction(&mut state, &Discard(4))?;

	assert_eq!(&state.stack, &[
		Value::Null,
	]);

	execute_instruction(&mut state, &Discard(1))?;
	assert!(state.stack.is_empty());

	assert!(execute_instruction(&mut state, &Discard(1)).is_err());

	Ok(())
}

#[test]
fn test_add() -> anyhow::Result<()> {
	let mut state = make_state(&[Value::Int(1), Value::Int(2)]);
	execute_instruction(&mut state, &Add)?;
	assert_eq!(&state.stack, &[Value::Int(3)]);

	let mut state = make_state(&[Value::Float(1.0), Value::Float(2.0)]);
	execute_instruction(&mut state, &Add)?;
	assert_eq!(&state.stack, &[Value::Float(3.0)]);

	// Int + Float
	let mut state = make_state(&[Value::Int(1), Value::Float(2.0)]);
	assert!(execute_instruction(&mut state, &Add).is_err());

	// Empty
	assert!(state.stack.is_empty());
	assert!(execute_instruction(&mut state, &Add).is_err());

	Ok(())
}

#[test]
fn test_subtract() -> anyhow::Result<()> {
	let mut state = ExecutionState::default();
	state.enter_global_frame();

	execute_instruction(&mut state, &PushInt(1))?;
	execute_instruction(&mut state, &PushInt(2))?;
	execute_instruction(&mut state, &Subtract)?;
	execute_instruction(&mut state, &PushFloat(1.0))?;
	execute_instruction(&mut state, &PushFloat(2.0))?;
	execute_instruction(&mut state, &Subtract)?;

	assert_eq!(&state.stack, &[
		Value::Int(-1),
		Value::Float(-1.0),
	]);

	// Int + Float
	assert!(execute_instruction(&mut state, &Subtract).is_err());

	// Empty
	assert!(state.stack.is_empty());
	assert!(execute_instruction(&mut state, &Subtract).is_err());

	Ok(())
}

#[test]
fn test_divide() -> anyhow::Result<()> {
	let mut state = ExecutionState::default();
	state.enter_global_frame();

	execute_instruction(&mut state, &PushInt(10))?;
	execute_instruction(&mut state, &PushInt(5))?;
	execute_instruction(&mut state, &Divide)?;
	execute_instruction(&mut state, &PushFloat(1.0))?;
	execute_instruction(&mut state, &PushFloat(2.0))?;
	execute_instruction(&mut state, &Divide)?;

	assert_eq!(&state.stack, &[
		Value::Int(2),
		Value::Float(0.5),
	]);

	// Int + Float
	assert!(execute_instruction(&mut state, &Divide).is_err());

	// Empty
	assert!(state.stack.is_empty());
	assert!(execute_instruction(&mut state, &Divide).is_err());

	// Divide by zero
	state.stack = [Value::Int(1), Value::Int(0)].to_vec();
	assert!(execute_instruction(&mut state, &Divide).is_err());

	Ok(())
}

#[test]
fn test_remainder() -> anyhow::Result<()> {
	let mut state = ExecutionState::default();
	state.enter_global_frame();

	execute_instruction(&mut state, &PushInt(7))?;
	execute_instruction(&mut state, &PushInt(5))?;
	execute_instruction(&mut state, &Remainder)?;
	execute_instruction(&mut state, &PushFloat(3.5))?;
	execute_instruction(&mut state, &PushFloat(2.0))?;
	execute_instruction(&mut state, &Remainder)?;

	assert_eq!(&state.stack, &[
		Value::Int(2),
		Value::Float(1.5),
	]);

	// Int + Float
	assert!(execute_instruction(&mut state, &Remainder).is_err());

	// Empty
	assert!(state.stack.is_empty());
	assert!(execute_instruction(&mut state, &Remainder).is_err());

	// Remainder by zero
	state.stack = [Value::Int(1), Value::Int(0)].to_vec();
	assert!(execute_instruction(&mut state, &Remainder).is_err());

	Ok(())
}

#[test]
fn test_call() -> anyhow::Result<()> {
	let mut state = ExecutionState::default();
	state.enter_global_frame();

	state.stack.extend([Value::Null, Value::Null, Value::Function(FunctionRef)]);
	execute_instruction(&mut state, &CallFunction(2))?;

	assert_eq!(state.frames.len(), 2);
	assert_eq!(state.frames[1].instruction_index, 0);

	execute_instruction(&mut state, &Return(0))?;

	assert_eq!(state.frames.len(), 1);
	assert_eq!(state.frames[0].instruction_index, 1);

	Ok(())
}
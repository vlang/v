module main

pub struct Registers {
pub mut:
	x [31]u64
}

// new creates zeroed general purpose registers.
pub fn Registers.new() Registers {
	return Registers{}
}

// read returns x0 through x30, or zero for x31 (XZR).
pub fn (r &Registers) read(index int) u64 {
	if index < 0 || index > 31 {
		panic('Invalid register index: ${index}')
	}
	if index == 31 {
		return 0
	}
	return r.x[index]
}

// write updates x0 through x30 and discards writes to x31 (XZR).
pub fn (mut r Registers) write(index int, new_value u64) {
	if index < 0 || index > 31 {
		panic('Invalid register index: ${index}')
	}
	if index == 31 {
		return
	}
	r.x[index] = new_value
}

pub struct Flags {
pub mut:
	zero_flag     bool // Z: the result was zero.
	negative_flag bool // N: the result's most significant bit was one.
	carry_flag    bool // C: carry on addition, no borrow on subtraction.
	overflow_flag bool // V: signed overflow.
}

// new creates cleared condition flags.
pub fn Flags.new() Flags {
	return Flags{}
}

pub struct State {
pub mut:
	is_running bool
}

// new creates a stopped execution state.
pub fn State.new() State {
	return State{}
}

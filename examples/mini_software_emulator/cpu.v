module main

pub struct CPU {
pub mut:
	registers Registers
	memory    []u8
	pc        u64
	flags     Flags
	state     State
mut:
	branch_taken bool
}

// new creates a CPU with the requested number of zeroed memory bytes.
pub fn CPU.new(memory_size int) CPU {
	if memory_size < 0 {
		panic('Memory size must be nonnegative')
	}
	return CPU{
		registers: Registers.new()
		memory:    []u8{len: memory_size}
		flags:     Flags.new()
		state:     State.new()
	}
}

// load_program_into_memory copies the program to the start of memory.
pub fn (mut c CPU) load_program_into_memory(program []u8) {
	if program.len > c.memory.len {
		panic('Program exceeds CPU memory')
	}
	for i, byte in program {
		c.memory[i] = byte
	}
}

// clear_memory clears the general purpose registers and memory.
pub fn (mut c CPU) clear_memory() {
	for i in 0 .. c.registers.x.len {
		c.registers.x[i] = 0
	}
	for i in 0 .. c.memory.len {
		c.memory[i] = 0
	}
}

// reset clears memory, registers, flags and execution state.
pub fn (mut c CPU) reset() {
	c.clear_memory()
	c.pc = 0
	c.flags = Flags.new()
	c.state = State.new()
	c.branch_taken = false
}

// fetch_instruction reads one aligned, little endian instruction at the program counter.
pub fn (c &CPU) fetch_instruction() u32 {
	if c.pc % 4 != 0 || c.pc > u64(c.memory.len) || u64(c.memory.len) - c.pc < 4 {
		panic('Invalid instruction address or truncated instruction')
	}
	p := int(c.pc)
	return u32(c.memory[p]) | (u32(c.memory[p + 1]) << 8) |
		(u32(c.memory[p + 2]) << 16) | (u32(c.memory[p + 3]) << 24)
}

fn (mut c CPU) jump(target u64) {
	c.pc = target
	c.branch_taken = true
}

// memory_address checks the full unsigned address before converting it to a host index.
fn (c &CPU) memory_address(base u64, offset u64, size u64) int {
	limit := u64(c.memory.len)
	if base > limit || offset > limit - base || size > limit - base - offset {
		panic('Memory address out of bounds')
	}
	return int(base + offset)
}

// execute_instruction executes one instruction and advances PC unless a branch was taken.
pub fn (mut c CPU) execute_instruction() {
	instruction := c.fetch_instruction()
	// The opcode occupies the low byte of this custom instruction encoding.
	opcode := int(instruction & 0xff)
	if opcode >= instruction_handlers.len {
		panic('Unknown or unimplemented instruction: ${opcode}.')
	}
	c.branch_taken = false
	handler := instruction_handlers[opcode]
	handler(mut c, instruction)
	if !c.branch_taken {
		c.pc += 4
	}
}

// execute_program runs from address zero, preserving registers and other memory.
// Execution stops at HALT or the end of the program; invalid branch targets panic.
pub fn (mut c CPU) execute_program(program []u8) {
	if program.len % 4 != 0 {
		panic('Program must contain complete four-byte instructions')
	}
	c.load_program_into_memory(program)
	c.pc = 0
	c.state.is_running = true
	defer {
		c.state.is_running = false
	}
	for c.state.is_running && c.pc < u64(program.len) {
		c.execute_instruction()
		if c.pc > u64(program.len) || c.pc % 4 != 0 {
			panic('Branch target outside program or unaligned')
		}
	}
}

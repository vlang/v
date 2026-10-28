module main

pub struct CPU {
pub mut:
    registers Registers
    memory    []u8
    pc        u64
    flags     Flags
    state     State
}

pub fn CPU.new(memory_size int) CPU {
    return CPU{
        registers: Registers.new()
        memory:    []u8{len: memory_size, init: 0}
        pc:        0
        flags:     Flags.new()
        state:     State.new()
    }
}

pub fn (mut c CPU) load_program_into_memory(program []u8) {
    for i, byte in program {
        c.memory[i] = byte
    }
}

pub fn (mut c CPU) clear_memory() {
    for i in 0..c.registers.x.len {
        c.registers.x[i] = 0
    }
    for i in 0..c.memory.len {
        c.memory[i] = 0
    }
}

pub fn (mut c CPU) reset() {
    c.clear_memory()
    c.pc = 0
    c.flags.zero_flag = false
    c.flags.negative_flag = false
    c.flags.carry_flag = false
    c.flags.overflow_flag = false
    c.state.is_running = false
}

pub fn (c &CPU) fetch_instruction() u32 {
    p := int(c.pc)
    // << standard ARM64 32-bit little endian instruction encoding >> \\
    unsafe {
        return u32(c.memory[p]) |
            (u32(c.memory[p + 1]) << 8) |
            (u32(c.memory[p + 2]) << 16) |
            (u32(c.memory[p + 3]) << 24)
    }
}

pub fn (mut c CPU) execute_instruction() {
    // << fetch >> \\
    instruction := c.fetch_instruction()

    // << decode: extract opcode (top 8 bits) >> \\
    opcode := uint(instruction & 0xFF)
    mut handler := instruction_handlers[0]
    // << execute >> \\
    if opcode < instruction_handlers.len {
        handler = instruction_handlers[opcode]
        handler(mut c, instruction)
    } else {
        panic('Unknown or unimplemented instruction: ${opcode}.')
    }
}

pub fn (mut c CPU) execute_program(program []u8) {
    // << load + setup >> \\
    c.load_program_into_memory(program)

    // << cached pc copy to compare branching >> \\
    mut old_pc := c.pc
    c.state.is_running = true
    
    for c.state.is_running && int(c.pc) < program.len {
        // << execute >> \\
        old_pc = c.pc
        c.execute_instruction()

        // << execute normally if branch didnt change pc >> \\
        if old_pc == c.pc {
            c.pc += 4
        }
    }
}

module main

import os
import v.cmdexec

fn instruction_bytes(instruction u32) []u8 {
	return [u8(instruction), u8(instruction >> 8), u8(instruction >> 16), u8(instruction >> 24)]
}

fn test_registers_and_memory_round_trip() {
	mut cpu := CPU.new(16)
	cpu.registers.write(31, 123)
	assert cpu.registers.read(31) == 0
	cpu.registers.write(0, 0x8877665544332211)
	cpu.registers.write(1, 8)
	str(mut cpu, 0x08 | (u32(1) << 16))
	assert cpu.memory[8..] == [u8(0x11), 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
	ldr(mut cpu, 0x07 | (u32(2) << 8) | (u32(1) << 16))
	assert cpu.registers.read(2) == cpu.registers.read(0)
	cpu.registers.write(1, 15)
	strb(mut cpu, 0x25 | (u32(1) << 16))
	ldrb(mut cpu, 0x24 | (u32(3) << 8) | (u32(1) << 16))
	assert cpu.registers.read(3) == 0x11
	cpu.reset()
	assert cpu.memory == []u8{len: 16}
	assert cpu.registers.read(0) == 0
}

fn check_arithmetic_flags(cpu CPU, a u64, b u64, subtract bool) {
	result := if subtract { a - b } else { a + b }
	a_negative := i64(a) < 0
	b_negative := i64(b) < 0
	result_negative := i64(result) < 0
	expected_overflow := if subtract {
		a_negative != b_negative && result_negative != a_negative
	} else {
		a_negative == b_negative && result_negative != a_negative
	}
	assert cpu.flags.zero_flag == (result == 0)
	assert cpu.flags.negative_flag == result_negative
	assert cpu.flags.carry_flag == (if subtract { a >= b } else { result < a })
	assert cpu.flags.overflow_flag == expected_overflow
}

fn test_flag_setting_instructions_at_signed_boundaries() {
	values := [u64(0), 1, 0x7fffffffffffffff, 0x8000000000000000, 0xffffffffffffffff]
	mut cpu := CPU.new(4)
	for a in values {
		for b in values {
			cpu.registers.write(0, a)
			cpu.registers.write(1, b)
			cmp(mut cpu, 0x09 | (u32(1) << 16))
			check_arithmetic_flags(cpu, a, b, true)
			subs(mut cpu, 0x29 | (u32(2) << 8) | (u32(1) << 24))
			check_arithmetic_flags(cpu, a, b, true)
			assert cpu.registers.read(2) == a - b
			adds(mut cpu, 0x28 | (u32(2) << 8) | (u32(1) << 24))
			check_arithmetic_flags(cpu, a, b, false)
			assert cpu.registers.read(2) == a + b
		}
		cpu.registers.write(0, a)
		cmpi(mut cpu, 0x27 | (u32(65535) << 16))
		check_arithmetic_flags(cpu, a, 65535, true)
		for immediate in [u32(0), 1, 255] {
			cpu.registers.write(0, a)
			cmpi(mut cpu, 0x27 | (immediate << 16))
			check_arithmetic_flags(cpu, a, u64(immediate), true)
			subis(mut cpu, 0x2b | (u32(2) << 8) | (immediate << 24))
			check_arithmetic_flags(cpu, a, u64(immediate), true)
			assert cpu.registers.read(2) == a - immediate
			addis(mut cpu, 0x2a | (u32(2) << 8) | (immediate << 24))
			check_arithmetic_flags(cpu, a, u64(immediate), false)
			assert cpu.registers.read(2) == a + immediate
		}
	}
}

fn test_branch_to_self_and_conditional_fallthrough() {
	assert instruction_handlers.len == 44
	mut cpu := CPU.new(8)
	for opcode in [u32(0x05), 0x06, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x22] {
		cpu.reset()
		cpu.load_program_into_memory(instruction_bytes(opcode))
		cpu.flags.zero_flag = opcode == 0x06 || opcode == 0x0c
		cpu.flags.negative_flag = opcode == 0x0b
		cpu.execute_instruction()
		assert cpu.pc == 0, 'taken branch ${opcode} must stay at address zero'
		cpu.execute_instruction()
		assert cpu.pc == 0
	}
	cpu.reset()
	cpu.load_program_into_memory(instruction_bytes(0x06))
	cpu.execute_instruction()
	assert cpu.pc == 4
	cpu.reset()
	cpu.load_program_into_memory(instruction_bytes(0x22))
	cpu.execute_instruction()
	assert cpu.registers.read(30) == 4
	cpu.load_program_into_memory(instruction_bytes(0x23))
	cpu.execute_instruction()
	assert cpu.pc == 4
}

fn test_execution_end_halt_and_signed_branch() {
	mut cpu := CPU.new(32)
	cpu.execute_program([u8(0x01), 0, 42, 0])
	assert cpu.pc == 4
	assert !cpu.state.is_running
	// A new run starts at zero and preserves registers.
	cpu.execute_program([u8(0x0f), 0, 0, 1])
	assert cpu.registers.read(0) == 43
	cpu.execute_program([u8(0), 0, 0, 0, 0x01, 0, 99, 0])
	assert cpu.pc == 4
	assert cpu.registers.read(0) == 43
	assert !cpu.state.is_running
	cpu.registers.write(0, 0x8000000000000000)
	cpu.registers.write(1, 1)
	cpu.execute_program([
		u8(0x09),
		0,
		1,
		0, // CMP minimum signed integer with one: N=0, V=1.
		0x0b,
		16,
		0,
		0, // B.LT must take the signed comparison branch.
		0x01,
		2,
		99,
		0,
		0x05,
		20,
		0,
		0,
		0x01,
		2,
		42,
		0,
	])
	assert cpu.registers.read(2) == 42
	assert !cpu.state.is_running
	cpu.execute_program([]u8{})
	assert cpu.pc == 0
	assert !cpu.state.is_running
}

fn test_demo_programs_and_sequence_boundaries() {
	run_rough_test()
	run_addition_test(1)
	run_addition_test(255)
	run_square_addition_test(1)
	run_square_addition_test(255)
	for n in [u8(0), 1, 2, 10] {
		run_fibonacci_test(n)
		run_pell_test(n)
	}
	run_heavy_addition_test(1, 1)
}

fn test_invalid_bytecode_panics_before_access() {
	dir := os.join_path(os.vtmp_dir(), 'emulator_invalid_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	for filename in ['cpu.v', 'cpu_instructions.v', 'state.v'] {
		os.cp(os.join_path(@DIR, filename), os.join_path(dir, filename)) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main
import os
fn main() {
    mut cpu := CPU.new(8)
    match os.args[1] {
        "read-negative" { cpu.registers.read(-1) }
        "read-large" { cpu.registers.read(255) }
        "write-large" { cpu.registers.write(32, 1) }
        "decoded-register" { cpu.execute_program([u8(1), 255, 0, 0]) }
        "truncated-fetch" { cpu.pc = 8; cpu.fetch_instruction() }
        "partial-fetch" { cpu.memory = []u8{len: 3}; cpu.fetch_instruction() }
        "huge-pc" { cpu.pc = 0xfffffffffffffffc; cpu.fetch_instruction() }
        "program-size" { cpu.load_program_into_memory([]u8{len: 12}) }
        "partial-program" { cpu.execute_program([u8(1)]) }
        "unaligned-branch" { cpu.execute_program([u8(5), 1, 0, 0]) }
        "outside-branch" { cpu.execute_program([u8(5), 8, 0, 0]) }
        "load-wrap" { cpu.registers.write(0, 0xffffffffffffffff); ldr(mut cpu, u32(1) << 24) }
        "store-wrap" { cpu.registers.write(0, 0xffffffffffffffff); str(mut cpu, u32(1) << 24) }
        "load-byte-wrap" { cpu.registers.write(0, 0xffffffffffffffff); ldrb(mut cpu, u32(1) << 24) }
        "store-byte-wrap" { cpu.registers.write(0, 0xffffffffffffffff); strb(mut cpu, u32(1) << 24) }
        else { panic("unknown case") }
    }
}
') or { panic(err) }
	binary := os.join_path(dir, 'invalid' + if os.user_os() == 'windows' { '.exe' } else { '' })
	built := cmdexec.run(@VEXE, ['-o', binary, dir])
	assert built.exit_code == 0, built.output
	cases := {
		'read-negative':    'Invalid register index'
		'read-large':       'Invalid register index'
		'write-large':      'Invalid register index'
		'decoded-register': 'Invalid register index'
		'truncated-fetch':  'Invalid instruction address'
		'partial-fetch':    'Invalid instruction address'
		'huge-pc':          'Invalid instruction address'
		'program-size':     'Program exceeds CPU memory'
		'partial-program':  'complete four-byte instructions'
		'unaligned-branch': 'Branch target outside program or unaligned'
		'outside-branch':   'Branch target outside program or unaligned'
		'load-wrap':        'Memory address out of bounds'
		'store-wrap':       'Memory address out of bounds'
		'load-byte-wrap':   'Memory address out of bounds'
		'store-byte-wrap':  'Memory address out of bounds'
	}
	for name, message in cases {
		result := cmdexec.run_with_timeout(binary, [name], 10000)
		assert result.exit_code != 0, name
		assert result.output.contains(message), '${name}: ${result.output}'
	}
}

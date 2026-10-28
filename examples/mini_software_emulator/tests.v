module main

import time

// run_rough_test checks arithmetic, loops and memory round trips.
pub fn run_rough_test() {
	mut my_cpu := CPU.new(1024)

	// Define a program:
	// 1. MOV x1, #42    -> [0x01, 0x01, 0x2A, 0x00] (opcode 1, reg 1, imm 42)
	// 2. MOV x2, #10    -> [0x01, 0x02, 0x0A, 0x00] (opcode 1, reg 2, imm 10)
	// 3. ADD x3, x1, x2 -> [0x02, 0x03, 0x01, 0x02] (opcode 2, rd 3, rn 1, rm 2)
	program1 := [
		u8(0x01),
		0x01,
		0x2A,
		0x00,
		0x01,
		0x02,
		0x0A,
		0x00,
		0x02,
		0x03,
		0x01,
		0x02,
	]

	my_cpu.execute_program(program1)

	assert my_cpu.registers.read(1) == 42
	assert my_cpu.registers.read(2) == 10
	assert my_cpu.registers.read(3) == 52

	// Check results
	println('x1 = ${my_cpu.registers.read(1)}') // Should be 42
	println('x2 = ${my_cpu.registers.read(2)}') // Should be 10
	println('x3 = ${my_cpu.registers.read(3)}') // Should be 52

	my_cpu.reset()

	// Define program2: A loop that counts down from 7 to 0 using SUB, CMP, and B.EQ
	program2 := [
		u8(0x01),
		0x00,
		0x00,
		0x00, // 0x00: MOV x0, #0  (comparison stop = 0)
		0x01,
		0x01,
		0x15,
		0x00, // 0x04: MOV x1, #21 (step value)
		0x01,
		0x02,
		0x07,
		0x00, // 0x08: MOV x2, #7  (counter = 7)
		0x01,
		0x03,
		0x00,
		0x00, // 0x0C: MOV x3, #0  (accumulator = 0)
		0x01,
		0x04,
		0x01,
		0x00, // 0x10: MOV x4, #1  (decrementer constant = 1)

		// --- Loop Start (Address 20 / 0x14) ---
		0x02,
		0x03,
		0x03,
		0x01, // 0x14: ADD x3, x3, x1 (x3 += 21)
		0x03,
		0x02,
		0x02,
		0x04, // 0x18: SUB x2, x2, x4 (x2 -= 1)
		0x09,
		0x02,
		0x00,
		0x00, // 0x1C: CMP x2, x0     (Compare x2 with x0/zero, sets flags)
		0x06,
		0x28,
		0x00,
		0x00, // 0x20: B.EQ 0x28      (If zero flag is true, jump OUT to 0x28 & exit program)
		0x05,
		0x14,
		0x00,
		0x00, // 0x24: B 0x14         (Else, jump back to loop start at 0x14 & re-iterate)

		// --- Loop Exit / End (Address 40 / 0x28) ---
	]

	my_cpu.execute_program(program2)
	assert my_cpu.registers.read(3) == 147
	println('x3 = ${my_cpu.registers.read(3)}') // Should output 21 × 7 = 147
	my_cpu.reset()

	// program3: demonstrate STR + LDR round trip.
	program3 := [
		u8(0x01),
		0x01,
		0xAB,
		0x00, // 0x00: MOV x1, #0xAB
		0x01,
		0x02,
		0x00,
		0x00, // 0x04: MOV x2, #0
		0x08,
		0x01,
		0x02,
		0x40, // 0x08: STR x1, [x2, #64]
		0x07,
		0x03,
		0x02,
		0x40, // 0x0C: LDR x3, [x2, #64]
	]

	my_cpu.execute_program(program3)
	assert my_cpu.registers.read(3) == 171
	assert my_cpu.memory[64..72] == [u8(0xab), 0, 0, 0, 0, 0, 0, 0]

	println('x1 = ${my_cpu.registers.read(1)}') // Should be 171
	println('x3 = ${my_cpu.registers.read(3)}') // Should be 171 (round-tripped via memory)

	// Show the raw bytes at address 64 to prove the store happened LE.
	stored := my_cpu.memory[64..72].clone()
	println('memory[64..72] = ${stored}') // Should be [0xAB, 0, 0, 0, 0, 0, 0, 0]
	my_cpu.reset()

	program4 := [
		u8(0x01),
		0x01,
		0x11,
		0x00, // 0x00: MOV x1, #0x11
		0x01,
		0x02,
		0x22,
		0x00, // 0x04: MOV x2, #0x22
		0x01,
		0x03,
		0x00,
		0x00, // 0x08: MOV x3, #0      (base = 0)
		0x08,
		0x01,
		0x03,
		0x40, // 0x0C: STR x1, [x3, #64]  -> mem[64]  = 0x11
		0x08,
		0x02,
		0x03,
		0x48, // 0x10: STR x2, [x3, #72]  -> mem[72]  = 0x22
		0x07,
		0x04,
		0x03,
		0x40, // 0x14: LDR x4, [x3, #64]  -> x4 = 0x11
		0x07,
		0x05,
		0x03,
		0x48, // 0x18: LDR x5, [x3, #72]  -> x5 = 0x22
	]

	my_cpu.execute_program(program4)
	assert my_cpu.registers.read(4) == 17
	assert my_cpu.registers.read(5) == 34
	println('x4: ${my_cpu.registers.read(4)}.') // should be 17
	println('x5: ${my_cpu.registers.read(5)}.') // should be 34
	my_cpu.reset()
}

// run_addition_test compares summation for a positive max with native V.
pub fn run_addition_test(max u8) {
	assert max > 0, 'max must be positive'
	// ---- V baseline ---- \\
	start_v := time.sys_mono_now()
	mut sum_v := u64(0)
	for i in 1 .. u64(max) + 1 {
		sum_v += i
	}
	time_v := time.sys_mono_now() - start_v
	println('Time for V to add up to ${max}: ${time_v}ns.')
	println('Sum up to ${max} by V: ${sum_v}.')

	program := [
		u8(0x01),
		0x00,
		0x00,
		0x00, // 0x00: MOV  x0, #0
		0x01,
		0x01,
		0x00,
		0x00, // 0x04: MOV  x1, #0
		0x01,
		0x02,
		max,
		0x00, // 0x08: MOV  x2, #max
		0x0F,
		0x01,
		0x01,
		0x01, // 0x0C: ADDI x1, x1,   #1
		0x02,
		0x00,
		0x00,
		0x01, // 0x10: ADD  x0, x0,   x1
		0x09,
		0x02,
		0x01,
		0x00, // 0x14: CMP  x2, x1
		0x0A,
		0x0C,
		0x00,
		0x00, // 0x18: B.NE #12
	]

	mut my_cpu := CPU.new(1024)
	start_vm := time.sys_mono_now()
	my_cpu.execute_program(program)
	time_vm := time.sys_mono_now() - start_vm
	gap := f32(time_vm) / f32(if time_v == 0 { u64(1) } else { time_v })
	println('Time for my custom VM to add up to ${max}: ${time_vm}ns.')
	println('Sum up to ${max} by my custom VM: ${my_cpu.registers.read(0)}.')
	println('Gap between my VM & V: ${gap}x.')

	// << Sanity check: VM and V agree? >> \\
	assert my_cpu.registers.read(0) == sum_v
}

// run_square_addition_test compares square summation for a positive max with native V.
pub fn run_square_addition_test(max u8) {
	assert max > 0, 'max must be positive'
	// ---- V baseline ---- \\
	start_v := time.sys_mono_now()
	mut sum_v := u64(0)
	for i in 1 .. u64(max) + 1 {
		sum_v += i * i
	}
	time_v := time.sys_mono_now() - start_v
	println('Time for V to add squares up to ${max}: ${time_v}ns.')
	println('Squares sum up to ${max} by V: ${sum_v}.')

	program := [
		u8(0x01),
		0x00,
		0x00,
		0x00, // 0x00: MOV  x0,   #0
		0x01,
		0x01,
		0x00,
		0x00, // 0x04: MOV  x1,   #0
		0x01,
		0x02,
		max,
		0x00, // 0x08: MOV  x2,   #max
		0x0F,
		0x01,
		0x01,
		0x01, // 0x0C: ADDI x1,   x1,   #1
		0x04,
		0x03,
		0x01,
		0x01, // 0x10: MUL  x3,   x1,   x1
		0x02,
		0x00,
		0x00,
		0x03, // 0x14: ADD  x0,   x0,   x3
		0x09,
		0x02,
		0x01,
		0x00, // 0x18: CMP  x2,   x1
		0x0A,
		0x0C,
		0x00,
		0x00, // 0x1C: B.NE #12
	]

	mut my_cpu := CPU.new(1024)
	start_vm := time.sys_mono_now()
	my_cpu.execute_program(program)
	time_vm := time.sys_mono_now() - start_vm
	gap := f32(time_vm) / f32(if time_v == 0 { u64(1) } else { time_v })
	println('Time for my custom VM to add squares up to ${max}: ${time_vm}ns.')
	println('Squares sum up to ${max} by my custom VM: ${my_cpu.registers.read(0)}.')
	println('Gap between my VM & V: ${gap}x.')

	// << Sanity check: VM and V agree? >> \\
	assert my_cpu.registers.read(0) == sum_v
}

// run_fibonacci_test compares Fibonacci numbers with native V.
pub fn run_fibonacci_test(n u8) {
	// ---- V baseline ---- \\
	start_v := time.sys_mono_now()
	mut a := u64(0)
	mut b_val := u64(1)
	mut fib := u64(0)

	if n == 0 {
		fib = 0
	} else if n == 1 {
		fib = 1
	} else {
		for _ in 2 .. int(n) + 1 {
			fib = a + b_val
			a = b_val
			b_val = fib
		}
	}
	time_v := time.sys_mono_now() - start_v
	println('Time for V to compute Fib(${n}): ${time_v}ns.')
	println('Fib(${n}) by V: ${fib}.')

	first := if n == 0 { u8(0) } else { u8(1) }

	// ---- VM Bytecode Program ---- \\
	program := [
		u8(0x01),
		0x00,
		0x00,
		0x00, // 0x00: MOV  x0, #0      (a = 0)
		0x01,
		0x01,
		first,
		0x00, // 0x04: MOV x1, #first (b = 0 when n = 0, else 1)
		0x01,
		0x03,
		n,
		0x00, // 0x08: MOV  x3, #n      (target n)
		0x01,
		0x04,
		first,
		0x00, // 0x0C: MOV x4, #first (counter = 0 when n = 0, else 1)
		0x01,
		0x05,
		0x01,
		0x00, // 0x10: MOV  x5, #1      (constant 1)

		// --- Loop Start (Address 20 / 0x14) ---
		0x09,
		0x03,
		0x04,
		0x00, // 0x14: CMP  x3, x4      (Compare target n with counter)
		0x06,
		0x30,
		0x00,
		0x00, // 0x18: B.EQ 0x30        (If equal, exit loop)
		0x02,
		0x02,
		0x00,
		0x01, // 0x1C: ADD  x2, x0, x1  (fib = a + b)
		0x26,
		0x00,
		0x01,
		0x00, // 0x20: MOVR x0, x1      (a = b)
		0x26,
		0x01,
		0x02,
		0x00, // 0x24: MOVR x1, x2      (b = fib)
		0x02,
		0x04,
		0x04,
		0x05, // 0x28: ADD  x4, x4, x5  (counter += 1)
		0x05,
		0x14,
		0x00,
		0x00, // 0x2C: B 0x14           (Loop back)
		// --- Loop Exit (Address 48 / 0x30) ---
	]

	mut my_cpu := CPU.new(1024)
	start_vm := time.sys_mono_now()
	my_cpu.execute_program(program)
	time_vm := time.sys_mono_now() - start_vm
	denom := if time_v == 0 { u64(1) } else { time_v }
	gap := f32(time_vm) / f32(denom)

	println('Time for my custom VM to compute Fib(${n}): ${time_vm}ns.')
	println('Fib(${n}) by my custom VM: ${my_cpu.registers.read(1)}.')
	println('Gap between my VM & V: ${gap}x.')

	assert my_cpu.registers.read(1) == fib
}

// run_pell_test compares Pell numbers with native V.
pub fn run_pell_test(n u8) {
	// ---- V baseline ---- \\
	start_v := time.sys_mono_now()
	mut p0 := u64(0)
	mut p1 := u64(1)
	mut p_curr := u64(0)

	if n == 0 {
		p_curr = 0
	} else if n == 1 {
		p_curr = 1
	} else {
		for _ in 2 .. int(n) + 1 {
			p_curr = 2 * p1 + p0
			p0 = p1
			p1 = p_curr
		}
	}
	time_v := time.sys_mono_now() - start_v
	println('Time for V to compute Pell(${n}) : ${time_v}ns.')
	println('Pell(${n}) by V: ${p_curr}.')
	first := if n == 0 { u8(0) } else { u8(1) }

	program := [
		u8(0x01),
		0x03,
		0x00,
		0x00, // 0x00: MOV  x3, #0      (p0 = 0)
		0x01,
		0x04,
		first,
		0x00, // 0x04: MOV x4, #first (p1 = 0 when n = 0, else 1)
		0x01,
		0x05,
		0x02,
		0x00, // 0x08: MOV  x5, #2      (multiplier 2)
		0x01,
		0x06,
		0x01,
		0x00, // 0x0C: MOV  x6, #1      (constant 1)
		0x01,
		0x02,
		first,
		0x00, // 0x10: MOV x2, #first (counter = 0 when n = 0, else 1)
		0x01,
		0x01,
		n,
		0x00, // 0x14: MOV  x1, #n      (target n)

		// --- Loop Start (Address 24 / 0x18) ---
		0x09,
		0x01,
		0x02,
		0x00, // 0x18: CMP  x1, x2      (Compare n with counter)
		0x06,
		0x38,
		0x00,
		0x00, // 0x1C: B.EQ 0x38        (If equal, jump out to exit)
		0x04,
		0x00,
		0x04,
		0x05, // 0x20: MUL  x0, x4, x5  (x0 = p1 * 2)
		0x02,
		0x00,
		0x00,
		0x03, // 0x24: ADD  x0, x0, x3  (x0 = 2*p1 + p0 -> new p_curr)
		0x26,
		0x03,
		0x04,
		0x00, // 0x28: MOVR x3, x4      (p0 = p1)
		0x26,
		0x04,
		0x00,
		0x00, // 0x2C: MOVR x4, x0      (p1 = p_curr)
		0x02,
		0x02,
		0x02,
		0x06, // 0x30: ADD  x2, x2, x6  (counter += 1)
		0x05,
		0x18,
		0x00,
		0x00, // 0x34: B 0x18           (Loop back)
		// --- Loop Exit (Address 56 / 0x38) ---
	]

	mut my_cpu := CPU.new(1024)
	start_vm := time.sys_mono_now()
	my_cpu.execute_program(program)
	time_vm := time.sys_mono_now() - start_vm
	denom := if time_v == 0 { u64(1) } else { time_v }
	gap := f32(time_vm) / f32(denom)

	println('Time for my custom VM to compute Pell(${n}): ${time_vm}ns.')
	println('Pell(${n}) by my custom VM: ${my_cpu.registers.read(4)}.')
	println('Gap between my VM & V: ${gap}x.')

	assert my_cpu.registers.read(4) == p_curr
}

// run_simple_tests runs the demonstration checks.
pub fn run_simple_tests() {
	run_rough_test()

	run_addition_test(100)
	run_addition_test(150)
	run_addition_test(200)

	run_square_addition_test(100)
	run_square_addition_test(150)
	run_square_addition_test(200)

	run_fibonacci_test(10)
	run_fibonacci_test(5)
	run_fibonacci_test(71)

	run_pell_test(7)
	run_pell_test(17)
	run_pell_test(21)
}

// run_heavy_addition_test benchmarks summation; max and times must be positive.
pub fn run_heavy_addition_test(max u8, times u32) {
	assert times > 0, 'times must be positive'
	assert max > 0, 'max must be positive'
	// -- V baseline -- \\
	mut total_v_time := u64(0)
	mut total_v_sum := u64(0)
	for _ in 0 .. times {
		total_v_sum = 0
		start_v := time.sys_mono_now()
		for i in 1 .. u64(max) + 1 {
			total_v_sum += i
		}
		time_v := time.sys_mono_now() - start_v
		total_v_time += time_v
	}
	average_v_time := total_v_time / u64(times)
	println('Average time for V to add upto ${max} across ${times} runs: ${average_v_time}ns.')

	// -- My VM -- \\
	program := [
		u8(0x01),
		0x00,
		0x00,
		0x00, // 0x00, mov x0 #0
		0x01,
		0x01,
		0x00,
		0x00, // 0x04, mov x1, #0
		0x0F,
		0x01,
		0x01,
		0x01, // 0x08, addi x1, x1, #1
		0x02,
		0x00,
		0x00,
		0x01, // 0x0C, add x0, x0, x1
		0x27,
		0x01,
		max,
		0x00, // 0x10, cmpi x1, #max
		0x0A,
		0x08,
		0x00,
		0x00, // 0x14, bne, #8
	]
	mut my_cpu := CPU.new(1024)

	mut total_vm_time := u64(0)
	for _ in 0 .. times {
		my_cpu.reset()
		start_vm := time.sys_mono_now()

		my_cpu.execute_program(program)

		time_vm := time.sys_mono_now() - start_vm
		assert my_cpu.registers.read(0) == total_v_sum
		total_vm_time += time_vm
	}
	average_vm_time := total_vm_time / u64(times)
	gap := f64(average_vm_time) / f64(if average_v_time == 0 { u64(1) } else { average_v_time })
	println('Average time for my VM to add upto ${max} across ${times} runs: ${average_vm_time}ns.')
	println('Gap between my VM & V: ${gap}x.')
}

// run_heavy_tests runs the repeated summation benchmark.
pub fn run_heavy_tests() {
	run_heavy_addition_test(100, 1000)
}

module runtime

import math

struct Frame {
	function int
	base     int
mut:
	pc int
}

fn check_values(values []Value, types []ValueType, functions int) ! {
	if values.len != types.len { return error('WebAssembly argument/result count mismatch') }
	for i, typ in types {
		if values[i].typ != typ { return error('WebAssembly argument/result type mismatch') }
		if typ == .funcref && values[i].bits > u64(functions) {
			return error('invalid function reference from host')
		}
	}
}

fn raw_value(value Value) u64 {
	return if value.typ in [.i32, .f32] { u64(u32(value.bits)) } else { value.bits }
}

// Move the top arity values down without allocating, including overlapping ranges.
fn keep_values(mut stack []u64, destination int, arity int) {
	source := stack.len - arity
	for i in 0 .. arity { stack[destination + i] = stack[source + i] }
	stack.trim(destination + arity)
}

fn (mut instance Instance) enter(index int, mut stack []u64, mut frames []Frame) ! {
	f := &instance.mod.functions[index]
	sig := instance.mod.types[f.typ]
	base := stack.len - sig.parameters.len
	if index < instance.mod.import_count {
		mut args := []Value{cap: sig.parameters.len}
		for i, typ in sig.parameters { args << Value{typ, stack[base + i]} }
		callback := instance.hosts[index]
		results := callback(mut instance, args)!
		check_values(results, sig.results, instance.mod.functions.len)!
		if base + results.len > instance.config.max_stack_values {
			return error('operand stack limit exceeded')
		}
		stack.trim(base)
		for value in results { stack << raw_value(value) }
		return
	}
	if frames.len >= instance.config.max_call_depth { return error('call stack limit exceeded') }
	if f.locals.len + f.stack_size > instance.config.max_stack_values - base {
		return error('operand stack limit exceeded')
	}
	for _ in sig.parameters.len .. f.locals.len { stack << u64(0) }
	frames << Frame{ function: index, base: base }
}

fn (mut instance Instance) invoke(index int, args []Value) ![]Value {
	if instance.running { return error('reentrant calls on the same instance are unsupported') }
	sig := instance.mod.types[instance.mod.functions[index].typ]
	check_values(args, sig.parameters, instance.mod.functions.len)!
	instance.running = true
	defer { instance.running = false }
	instance.remaining_fuel = instance.config.fuel
	mut stack := []u64{cap: 256}
	for arg in args { stack << raw_value(arg) }
	mut frames := []Frame{cap: 32}
	instance.enter(index, mut stack, mut frames)!
	for frames.len > 0 {
		frame := frames.last()
		f := &instance.mod.functions[frame.function]
		ins := f.code[frame.pc]
		frames[frames.len - 1].pc++
		if instance.config.fuel != 0 {
			if instance.remaining_fuel == 0 { return error('execution fuel exhausted') }
			instance.remaining_fuel--
		}
		match ins.op {
			0 { return error('unreachable instruction executed') }
			1, 2, 3 {}
			4 {
				if u32(stack.pop()) == 0 { frames[frames.len - 1].pc = ins.a }
			}
			0x0c, 0x0d, 0x0e {
				mut label := ins.a
				if ins.op == 0x0d && u32(stack.pop()) == 0 { continue }
				if ins.op == 0x0e {
					label = f.branch_table[ins.a + int(math.min(u64(u32(stack.pop())), u64(ins.b)))]
				}
				branch := f.branches[label]
				keep_values(mut stack, frame.base + f.locals.len + branch.height, branch.arity)
				frames[frames.len - 1].pc = branch.pc
			}
			0x0f {
				keep_values(mut stack, frame.base, instance.mod.types[f.typ].results.len)
				frames.pop()
			}
			0x10, 0x11 {
				mut callee := ins.a
				if ins.op == 0x11 {
					i := u64(u32(stack.pop()))
					if i >= u64(instance.table.len) { return error('undefined table element') }
					ref := instance.table[int(i)]
					if ref == 0 { return error('uninitialized table element') }
					callee = int(ref - 1)
					if instance.mod.types[instance.mod.functions[callee].typ] != instance.mod.types[ins.a] {
						return error('indirect call type mismatch')
					}
				}
				instance.enter(callee, mut stack, mut frames)!
			}
			0x1a { stack.pop() }
			0x1b, 0x1c {
				condition := u32(stack.pop())
				b := stack.pop()
				if condition == 0 { stack[stack.len - 1] = b }
			}
			0x20 { stack << stack[frame.base + ins.a] }
			0x21 { stack[frame.base + ins.a] = stack.pop() }
			0x22 { stack[frame.base + ins.a] = stack.last() }
			0x23 { stack << instance.globals[ins.a].value }
			0x24 { instance.globals[ins.a].value = stack.pop() }
			0x25, 0x26 {
				value := if ins.op == 0x26 { stack.pop() } else { u64(0) }
				i := u64(u32(stack.pop()))
				if i >= u64(instance.table.len) { return error('out of bounds table access') }
				if ins.op == 0x26 {
					instance.table[int(i)] = value
				} else {
					stack << instance.table[int(i)]
				}
			}
			0x28...0x3e {
				mut value := if ins.op >= 0x36 { stack.pop() } else { u64(0) }
				address := u64(u32(stack.pop())) + ins.value
				if address + u64(ins.a) > u64(instance.memory.len) {
					return error('out of bounds memory access')
				}
				if ins.op >= 0x36 {
					for i in 0 .. ins.a { instance.memory[int(address) + i] = u8(value >> (i * 8)) }
				} else {
					for i in 0 .. ins.a {
						value |= u64(instance.memory[int(address) + i]) << (i * 8)
					}
					value = match ins.op {
						0x2c { u64(u32(int(i8(value)))) }
						0x2e { u64(u32(int(i16(value)))) }
						0x30 { u64(i64(i8(value))) }
						0x32 { u64(i64(i16(value))) }
						0x34 { u64(i64(i32(u32(value)))) }
						else { value }
					}
					stack << value
				}
			}
			0x3f { stack << u64(instance.memory.len / 65536) }
			0x40 {
				growth := u64(u32(stack.pop()))
				old := instance.memory.len / 65536
				limits := instance.mod.memory or { return error('missing memory') }
				if growth + u64(old) > u64(math.min(limits.max, instance.config.max_memory_pages)) {
					stack << u64(0xffffffff)
				} else {
					instance.memory << []u8{len: int(growth) * 65536}
					stack << u64(old)
				}
			}
			0x41...0x44, 0xd0, 0xd2 { stack << ins.value }
			0xd1 { stack[stack.len - 1] = u64(stack.last() == 0) }
			0x108...0x111 { instance.bulk(ins, mut stack)! }
			else {
				b := if ins.a == 2 { stack.pop() } else { u64(0) }
				stack[stack.len - 1] = numeric(ins.op, stack.last(), b)!
			}
		}
	}
	mut results := []Value{cap: sig.results.len}
	for i, typ in sig.results { results << Value{typ, stack[i]} }
	return results
}

fn checked_range(start u64, count u64, size int) !int {
	if start + count > u64(size) { return error('out of bounds bulk access') }
	return int(start)
}

fn (mut instance Instance) bulk(ins Instruction, mut stack []u64) ! {
	match ins.op {
		0x109 {
			instance.data[ins.a] = []u8{}
			return
		}
		0x10d {
			instance.elements[ins.a] = []u64{}
			return
		}
		0x110 {
			stack << u64(instance.table.len)
			return
		}
		else {}
	}
	n := u64(u32(stack.pop()))
	source := stack.pop()
	if ins.op == 0x10f {
		limits := instance.mod.table or { return error('missing table') }
		old := instance.table.len
		if n + u64(old) > u64(math.min(limits.max, instance.config.max_table_elements)) {
			stack << u64(0xffffffff)
		} else {
			instance.table << []u64{len: int(n), init: source}
			stack << u64(old)
		}
		return
	}
	destination := u64(u32(stack.pop()))
	if ins.op in [u16(0x108), 0x10a, 0x10b] {
		d := checked_range(destination, n, instance.memory.len)!
		if ins.op == 0x108 {
			s := checked_range(u64(u32(source)), n, instance.data[ins.a].len)!
			copy(mut instance.memory[d..d + int(n)], instance.data[ins.a][s..s + int(n)])
		} else if ins.op == 0x10a {
			s := checked_range(u64(u32(source)), n, instance.memory.len)!
			copy(mut instance.memory[d..d + int(n)], instance.memory[s..s + int(n)])
		} else {
			for i in d .. d + int(n) { instance.memory[i] = u8(source) }
		}
	} else {
		d := checked_range(destination, n, instance.table.len)!
		if ins.op == 0x10c {
			s := checked_range(u64(u32(source)), n, instance.elements[ins.a].len)!
			copy(mut instance.table[d..d + int(n)], instance.elements[ins.a][s..s + int(n)])
		} else if ins.op == 0x10e {
			s := checked_range(u64(u32(source)), n, instance.table.len)!
			copy(mut instance.table[d..d + int(n)], instance.table[s..s + int(n)])
		} else {
			for i in d .. d + int(n) { instance.table[i] = source }
		}
	}
}

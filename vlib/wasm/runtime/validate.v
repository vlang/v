module runtime

import math

struct Control {
	op         u16
	height     int
	label      int
	parameters []ValueType
	results    []ValueType
	if_pc      int
mut:
	unreachable bool
	has_else    bool
}

struct Validator {
mut:
	stack     []int
	controls  []Control
	max_stack int
}

fn (mut v Validator) push(typ int) {
	v.stack << typ
	v.max_stack = math.max(v.max_stack, v.stack.len)
}

fn (mut v Validator) pop(expected int) !int {
	c := v.controls.last()
	if v.stack.len == c.height && c.unreachable { return expected }
	if v.stack.len <= c.height { return error('WebAssembly operand stack underflow') }
	actual := v.stack.pop()
	if expected >= 0 && actual >= 0 && actual != expected {
		return error('WebAssembly operand type mismatch')
	}
	return if actual < 0 { expected } else { actual }
}

fn (mut v Validator) pop_types(types []ValueType) ! {
	for i := types.len - 1; i >= 0; i-- { v.pop(int(types[i]))! }
}

fn (mut v Validator) push_types(types []ValueType) {
	for typ in types { v.push(int(typ)) }
}

fn (mut v Validator) unreachable() {
	v.stack.trim(v.controls.last().height)
	v.controls[v.controls.len - 1].unreachable = true
}

fn (v &Validator) label(depth int) Control { return v.controls[v.controls.len - 1 - depth] }

fn (c Control) branch_types() []ValueType {
	return if c.op == 3 { c.parameters } else { c.results }
}

fn (mut r Reader) block_type(types []Signature) !Signature {
	start := r.pos
	x := i64(r.leb(33, true)!)
	// Negative block types are single-byte value types, not signed type indices.
	if x < 0 && r.pos != start + 1 { return error('invalid block type') }
	if x == -64 { return Signature{} }
	if x < 0 { return Signature{ results: [value_type(u8(x & 0x7f))!] } }
	if x >= types.len { return error('invalid block type') }
	return types[int(x)]
}

// Numeric operators share one signature table between validation and dispatch.
fn numeric_signature(op u16) !(int, int, int) {
	if op == 0x45 { return 0x7f, 0x7f, 1 }
	if op >= 0x46 && op <= 0x4f { return 0x7f, 0x7f, 2 }
	if op == 0x50 { return 0x7e, 0x7f, 1 }
	if op >= 0x51 && op <= 0x5a { return 0x7e, 0x7f, 2 }
	if op >= 0x5b && op <= 0x60 { return 0x7d, 0x7f, 2 }
	if op >= 0x61 && op <= 0x66 { return 0x7c, 0x7f, 2 }
	if op >= 0x67 && op <= 0x78 { return 0x7f, 0x7f, if op <= 0x69 { 1 } else { 2 } }
	if op >= 0x79 && op <= 0x8a { return 0x7e, 0x7e, if op <= 0x7b { 1 } else { 2 } }
	if op >= 0x8b && op <= 0x98 { return 0x7d, 0x7d, if op <= 0x91 { 1 } else { 2 } }
	if op >= 0x99 && op <= 0xa6 { return 0x7c, 0x7c, if op <= 0x9f { 1 } else { 2 } }
	if op >= 0xa7 && op <= 0xc4 {
		inputs := [0x7e, 0x7d, 0x7d, 0x7c, 0x7c, 0x7f, 0x7f, 0x7d, 0x7d, 0x7c, 0x7c, 0x7f, 0x7f,
			0x7e, 0x7e, 0x7c, 0x7f, 0x7f, 0x7e, 0x7e, 0x7d, 0x7d, 0x7c, 0x7f, 0x7e, 0x7f, 0x7f,
			0x7e, 0x7e, 0x7e]!
		outputs := [0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e, 0x7d, 0x7d,
			0x7d, 0x7d, 0x7d, 0x7c, 0x7c, 0x7c, 0x7c, 0x7c, 0x7f, 0x7e, 0x7d, 0x7c, 0x7f, 0x7f,
			0x7e, 0x7e, 0x7e]!
		return inputs[int(op - 0xa7)], outputs[int(op - 0xa7)], 1
	}
	if op >= 0x100 && op <= 0x107 {
		i := int(op - 0x100)
		return if i & 2 == 0 { 0x7d } else { 0x7c }, if i < 4 { 0x7f } else { 0x7e }, 1
	}
	return error('unsupported WebAssembly opcode 0x${op:x}')
}

fn memory_shape(op u16) (int, int) {
	widths := [4, 8, 4, 8, 1, 1, 2, 2, 1, 1, 2, 2, 4, 4, 4, 8, 4, 8, 1, 2, 1, 2, 4]!
	types := [0x7f, 0x7e, 0x7d, 0x7c, 0x7f, 0x7f, 0x7f, 0x7f, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e, 0x7e,
		0x7f, 0x7e, 0x7d, 0x7c, 0x7f, 0x7f, 0x7e, 0x7e, 0x7e]!
	return widths[int(op - 0x28)], types[int(op - 0x28)]
}

fn (mut mod Module) decode_function(index int, mut r Reader, declared_refs []bool) ! {
	mut f := mod.functions[index]
	sig := mod.types[f.typ]
	mut v := Validator{ controls: [Control{ op: 0xff, results: sig.results }] }
	f.branches << Branch{ arity: sig.results.len }
	for v.controls.len > 0 {
		mod.decoded_instructions++
		if mod.decoded_instructions > 1000000 { return error('too many module instructions') }
		mut op := u16(r.byte()!)
		if op == 0xfc {
			sub := r.uint()!
			if sub > 17 { return error('unsupported extended opcode ${sub}') }
			op = u16(0x100 + sub)
		}
		mut a := 0
		mut b := 0
		mut value := u64(0)
		match op {
			0 { v.unreachable() }
			1 {}
			2, 3, 4 {
				if op == 4 { v.pop(0x7f)! }
				typ := r.block_type(mod.types)!
				v.pop_types(typ.parameters)!
				c := Control{
					op:         op
					height:     v.stack.len
					label:      f.branches.len
					parameters: typ.parameters
					results:    typ.results
					if_pc:      f.code.len
				}
				f.branches << Branch{ pc: f.code.len + 1, height: c.height, arity: c.branch_types().len }
				v.controls << c
				v.push_types(typ.parameters)
			}
			5, 0x0b {
				c := v.controls.last()
				v.pop_types(c.results)!
				if v.stack.len != c.height {
					return error('values left on WebAssembly block stack')
				}
				if op == 5 {
					if c.op != 4 || c.has_else { return error('unexpected else') }
					v.controls[v.controls.len - 1].has_else = true
					v.controls[v.controls.len - 1].unreachable = false
					f.code[c.if_pc] = Instruction{ ...f.code[c.if_pc], a: f.code.len + 1 }
					v.push_types(c.parameters)
					op = 0x0c
					a = c.label
				} else {
					if c.op == 4 && !c.has_else {
						if c.parameters != c.results {
							return error('if without else has incompatible result type')
						}
						f.code[c.if_pc] = Instruction{ ...f.code[c.if_pc], a: f.code.len }
					}
					if c.op != 3 { f.branches[c.label].pc = f.code.len }
					v.controls.pop()
					v.push_types(c.results)
					op = if v.controls.len == 0 { u16(0x0f) } else { u16(1) }
				}
			}
			0x0c, 0x0d {
				c := v.label(r.index(v.controls.len)!)
				a = c.label
				if op == 0x0d { v.pop(0x7f)! }
				v.pop_types(c.branch_types())!
				if op == 0x0c { v.unreachable() } else { v.push_types(c.branch_types()) }
			}
			0x0e {
				b = r.count()!
				a = f.branch_table.len
				mut types := []ValueType{}
				for i in 0 .. b + 1 {
					c := v.label(r.index(v.controls.len)!)
					if i == 0 {
						types = c.branch_types()
					} else if types != c.branch_types() {
						return error('branch table type mismatch')
					}
					f.branch_table << c.label
				}
				v.pop(0x7f)!
				v.pop_types(types)!
				v.unreachable()
			}
			0x0f {
				v.pop_types(sig.results)!
				v.unreachable()
			}
			0x10, 0x11 {
				mut typ := Signature{}
				if op == 0x10 {
					a = r.index(mod.functions.len)!
					typ = mod.types[mod.functions[a].typ]
				} else {
					a = r.index(mod.types.len)!
					if mod.table == none { return error('call_indirect without table') }
					r.index(1)!
					v.pop(0x7f)!
					typ = mod.types[a]
				}
				v.pop_types(typ.parameters)!
				v.push_types(typ.results)
			}
			0x1a { v.pop(-1)! }
			0x1b, 0x1c {
				mut typ := -1
				if op == 0x1c {
					if r.uint()! != 1 { return error('select requires exactly one type') }
					typ = int(value_type(r.byte()!)!)
				}
				v.pop(0x7f)!
				typ = v.pop(typ)!
				typ = v.pop(typ)!
				if op == 0x1b && typ == 0x70 {
					return error('reference select needs an explicit type')
				}
				v.push(typ)
			}
			0x20, 0x21, 0x22 {
				a = r.index(f.locals.len)!
				if op != 0x20 { v.pop(int(f.locals[a]))! }
				if op != 0x21 { v.push(int(f.locals[a])) }
			}
			0x23, 0x24 {
				a = r.index(mod.globals.len)!
				g := mod.globals[a]
				if op == 0x23 {
					v.push(int(g.typ))
				} else {
					if !g.mutable { return error('write to immutable global') }
					v.pop(int(g.typ))!
				}
			}
			0x25, 0x26 {
				if mod.table == none { return error('table instruction without table') }
				r.index(1)!
				if op == 0x26 { v.pop(0x70)! }
				v.pop(0x7f)!
				if op == 0x25 { v.push(0x70) }
			}
			0x28...0x3e {
				if mod.memory == none { return error('memory instruction without memory') }
				width, typ := memory_shape(op)
				alignment := r.uint()!
				if alignment > 3 || (1 << alignment) > width {
					return error('invalid memory alignment')
				}
				value = u64(r.uint()!)
				a = width
				if op >= 0x36 { v.pop(typ)! }
				v.pop(0x7f)!
				if op < 0x36 { v.push(typ) }
			}
			0x3f, 0x40 {
				if mod.memory == none { return error('memory instruction without memory') }
				r.index(1)!
				if op == 0x40 { v.pop(0x7f)! }
				v.push(0x7f)
			}
			0x41...0x44 {
				value = r.literal(op)!
				v.push(0x7f - int(op - 0x41))
			}
			0xd0 {
				if r.byte()! != 0x70 { return error('unsupported reference type') }
				v.push(0x70)
			}
			0xd1 {
				v.pop(0x70)!
				v.push(0x7f)
			}
			0xd2 {
				referenced := r.index(mod.functions.len)!
				if !declared_refs[referenced] { return error('undeclared function reference') }
				value = u64(referenced + 1)
				v.push(0x70)
			}
			0x108...0x111 {
				if op in [u16(0x108), 0x109] { a = r.index(math.max(0, mod.data_count))! }
				if op in [u16(0x10c), 0x10d] { a = r.index(mod.elements.len)! }
				if op in [u16(0x108), 0x10a, 0x10b] {
					if mod.memory == none { return error('memory instruction without memory') }
					r.index(1)!
					if op == 0x10a { r.index(1)! }
				}
				if op >= 0x10c && op != 0x10d {
					if mod.table == none { return error('table instruction without table') }
					r.index(1)!
					if op == 0x10e { r.index(1)! }
				}
				if op !in [u16(0x109), 0x10d, 0x110] {
					v.pop(0x7f)!
					v.pop(if op in [u16(0x10f), 0x111] { 0x70 } else { 0x7f })!
					if op != 0x10f { v.pop(0x7f)! }
				}
				if op in [u16(0x10f), 0x110] { v.push(0x7f) }
			}
			else {
				input, output, arity := numeric_signature(op)!
				for _ in 0 .. arity { v.pop(input)! }
				v.push(output)
				a = arity
			}
		}
		f.code << Instruction{op, a, b, value}
		if v.stack.len > 1000000 || v.controls.len > 100000 {
			return error('function validation limit exceeded')
		}
	}
	if r.pos != r.bytes.len { return error('trailing instructions after function end') }
	f.stack_size = v.max_stack
	mod.functions[index] = f
}

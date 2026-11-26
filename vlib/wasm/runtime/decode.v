module runtime

import encoding.utf8.validate

struct Reader {
	bytes []u8
mut:
	pos int
}

fn (mut r Reader) byte() !u8 {
	if r.pos == r.bytes.len { return error('unexpected end of WebAssembly binary') }
	b := r.bytes[r.pos]
	r.pos++
	return b
}

fn (mut r Reader) take(n u32) ![]u8 {
	if u64(n) > u64(r.bytes.len - r.pos) { return error('truncated WebAssembly section') }
	start := r.pos
	r.pos += int(n)
	return r.bytes[start..r.pos]
}

fn (mut r Reader) leb(width int, signed bool) !u64 {
	mut value := u64(0)
	for shift := 0; shift < width; shift += 7 {
		b := r.byte()!
		payload := u64(b & 0x7f)
		remaining := width - shift
		if remaining < 7 {
			mask := u64(0x7f) >> (7 - remaining)
			negative := signed && payload & (u64(1) << (remaining - 1)) != 0
			if payload & ~mask != (if negative { u64(0x7f) & ~mask } else { u64(0) }) {
				return error('integer representation out of range')
			}
		}
		value |= payload << shift
		if b & 0x80 == 0 {
			if signed && b & 0x40 != 0 && shift + 7 < 64 { value |= ~u64(0) << (shift + 7) }
			return value
		}
	}
	return error('integer representation too long')
}

fn (mut r Reader) uint() !u32 { return u32(r.leb(32, false)!) }

fn (mut r Reader) index(length int) !int {
	i := r.uint()!
	if u64(i) >= u64(length) { return error('invalid WebAssembly index ${i}') }
	return int(i)
}

fn (mut r Reader) count() !int {
	n := r.uint()!
	if n > 1000000 || u64(n) > u64(r.bytes.len - r.pos) {
		return error('WebAssembly vector is too large or truncated')
	}
	return int(n)
}

fn (mut r Reader) name() !string {
	data := r.take(r.uint()!)!
	s := data.bytestr()
	if !validate.utf8_string(s) { return error('invalid UTF-8 name') }
	return s
}

fn value_type(b u8) !ValueType {
	return match b {
		0x7f { .i32 }
		0x7e { .i64 }
		0x7d { .f32 }
		0x7c { .f64 }
		0x70 { .funcref }
		else { return error('unsupported WebAssembly value type 0x${b:02x}') }
	}
}

fn (mut r Reader) value_types() ![]ValueType {
	n := r.count()!
	mut types := []ValueType{cap: n}
	for _ in 0 .. n { types << value_type(r.byte()!)! }
	return types
}

fn (mut r Reader) limits(maximum int) !Limits {
	flag := r.byte()!
	if flag > 1 { return error('shared and 64-bit memories/tables are unsupported') }
	minimum := r.uint()!
	maximum_declared := if flag == 1 { r.uint()! } else { u32(maximum) }
	if minimum > maximum_declared || maximum_declared > u32(maximum) {
		return error('invalid WebAssembly limits')
	}
	return Limits{int(minimum), int(maximum_declared)}
}

fn (mut r Reader) literal(op u16) !u64 {
	if op == 0x41 { return u64(u32(r.leb(32, true)!)) }
	if op == 0x42 { return r.leb(64, true) }
	mut value := u64(0)
	n := if op == 0x43 { 4 } else { 8 }
	for i in 0 .. n { value |= u64(r.byte()!) << (i * 8) }
	return value
}

fn (mut r Reader) constant(expected ValueType, functions int) !u64 {
	op := r.byte()!
	mut value := u64(0)
	match op {
		0x41, 0x42, 0x43, 0x44 {
			if u8(expected) != 0x7f - (op - 0x41) { return error('constant type mismatch') }
			value = r.literal(op)!
		}
		0xd0 {
			if expected != .funcref || r.byte()! != 0x70 { return error('invalid ref.null') }
		}
		0xd2 {
			if expected != .funcref { return error('constant type mismatch') }
			value = u64(r.index(functions)! + 1)
		}
		else { return error('unsupported constant expression opcode 0x${op:02x}') }
	}
	if r.byte()! != 0x0b { return error('unterminated constant expression') }
	return value
}

// load decodes and validates a core WebAssembly binary, rejecting unsupported extensions.
pub fn load(bytes []u8) !&Module {
	if bytes.len > 64 * 1024 * 1024 {
		return error('WebAssembly binary exceeds 64 MiB loader limit')
	}
	mut r := Reader{ bytes: bytes }
	if r.take(8)! != [u8(0), 0x61, 0x73, 0x6d, 1, 0, 0, 0] {
		return error('invalid WebAssembly magic or version')
	}
	mut mod := &Module{}
	mut exports := []Export{}
	mut last_section := 0
	mut code_count := 0
	mut total_locals := 0
	mut declared_refs := []bool{}
	for r.pos < bytes.len {
		section := r.byte()!
		mut s := Reader{ bytes: r.take(r.uint()!)! }
		if section == 0 {
			s.name()!
			continue
		}
		rank := if section == 12 {
			10
		} else if section >= 10 {
			int(section) + 1
		} else {
			int(section)
		}
		if section > 12 || rank <= last_section {
			return error('invalid section order or duplicate section')
		}
		last_section = rank
		match section {
			1 {
				for _ in 0 .. s.count()! {
					if s.byte()! != 0x60 { return error('unsupported WebAssembly type definition') }
					mod.types << Signature{s.value_types()!, s.value_types()!}
				}
			}
			2 {
				for _ in 0 .. s.count()! {
					namespace := s.name()!
					name := s.name()!
					if s.byte()! != 0 {
						return error('only function imports are currently supported')
					}
					mod.functions << Function{ typ: s.index(mod.types.len)!, module: namespace, name: name }
					declared_refs << false
				}
				mod.import_count = mod.functions.len
			}
			3 {
				for _ in 0 .. s.count()! {
					mod.functions << Function{ typ: s.index(mod.types.len)! }
					declared_refs << false
				}
			}
			4 {
				n := s.uint()!
				if n > 1 { return error('multiple tables are unsupported') }
				if n == 1 {
					if s.byte()! != 0x70 { return error('expected a funcref table') }
					mod.table = s.limits(0x7fffffff)!
				}
			}
			5 {
				n := s.uint()!
				if n > 1 { return error('multiple memories are unsupported') }
				if n == 1 { mod.memory = s.limits(65536)! }
			}
			6 {
				for _ in 0 .. s.count()! {
					typ := value_type(s.byte()!)!
					mutable := s.byte()!
					if mutable > 1 { return error('invalid global mutability') }
					value := s.constant(typ, mod.functions.len)!
					if typ == .funcref && value != 0 { declared_refs[int(value - 1)] = true }
					mod.globals << Global{typ, mutable == 1, value}
				}
			}
			7 {
				mut names := map[string]bool{}
				for _ in 0 .. s.count()! {
					name := s.name()!
					if name in names { return error('duplicate export `${name}`') }
					names[name] = true
					kind := s.byte()!
					length := match kind {
						0 { mod.functions.len }
						1 {
							if mod.table != none { 1 } else { 0 }
						}
						2 {
							if mod.memory != none { 1 } else { 0 }
						}
						3 { mod.globals.len }
						else { return error('invalid export kind') }
					}
					index := s.index(length)!
					if kind == 0 { declared_refs[index] = true }
					exports << Export{name, kind, index}
				}
			}
			8 {
				mod.start = s.index(mod.functions.len)!
				if mod.types[mod.functions[mod.start].typ] != Signature{} {
					return error('start function must have type () -> ()')
				}
			}
			9 {
				for _ in 0 .. s.count()! {
					flags := s.uint()!
					if flags > 7 { return error('invalid element segment flags') }
					active := flags & 1 == 0
					if active && mod.table == none {
						return error('element segment without a table')
					}
					if flags in [u32(2), 6] { s.index(1)! }
					offset := if active {
						u32(s.constant(.i32, mod.functions.len)!)
					} else {
						u32(0)
					}
					if flags & 3 != 0 {
						if s.byte()! != (if flags < 4 { u8(0) } else { u8(0x70) }) {
							return error('unsupported element type')
						}
					}
					mut elements := []u64{}
					for _ in 0 .. s.count()! {
						elements << if flags < 4 {
							u64(s.index(mod.functions.len)! + 1)
						} else {
							s.constant(.funcref, mod.functions.len)!
						}
					}
					for ref in elements {
						if ref != 0 { declared_refs[int(ref - 1)] = true }
					}
					if flags & 3 == 3 { elements.clear() }
					mod.elements << Segment{ passive: !active, offset: offset, elements: elements }
				}
			}
			10 {
				code_count = s.count()!
				if code_count != mod.functions.len - mod.import_count {
					return error('function and code counts differ')
				}
				for i in mod.import_count .. mod.functions.len {
					mut body := Reader{ bytes: s.take(s.uint()!)! }
					total_locals += mod.types[mod.functions[i].typ].parameters.len
					if total_locals > 1000000 { return error('too many module locals') }
					mut locals := mod.types[mod.functions[i].typ].parameters.clone()
					for _ in 0 .. body.count()! {
						n := body.uint()!
						typ := value_type(body.byte()!)!
						if u64(total_locals) + n > 1000000 {
							return error('too many locals')
						}
						total_locals += int(n)
						for _ in 0 .. int(n) { locals << typ }
					}
					mod.functions[i].locals = locals
					mod.decode_function(i, mut body, declared_refs)!
				}
			}
			11 {
				for _ in 0 .. s.count()! {
					flags := s.uint()!
					if flags > 2 { return error('invalid data segment flags') }
					if flags != 1 && mod.memory == none {
						return error('data segment without memory')
					}
					if flags == 2 { s.index(1)! }
					offset := if flags == 1 {
						u32(0)
					} else {
						u32(s.constant(.i32, mod.functions.len)!)
					}
					mod.data << Segment{ passive: flags == 1, offset: offset, data: s.take(s.uint()!)!.clone() }
				}
			}
			12 {
				n := s.uint()!
				if n > 1000000 { return error('too many data segments') }
				mod.data_count = int(n)
			}
			else {}
		}
		if s.pos != s.bytes.len { return error('trailing bytes in WebAssembly section') }
	}
	if code_count != mod.functions.len - mod.import_count {
		return error('missing function bodies')
	}
	if mod.data_count >= 0 && mod.data_count != mod.data.len { return error('data count mismatch') }
	// Export names and all decoded storage are independent of the caller's byte buffer.
	return &Module{ ...mod, exports: exports }
}

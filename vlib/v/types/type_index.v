module types

pub fn (tc &TypeChecker) type_index(name string) int {
	mut base := name.trim_space()
	if base.len == 0 {
		return 0
	}
	mut indirections := 0
	for base.starts_with('&') {
		indirections++
		base = base[1..].trim_space()
	}
	builtin_idx := builtin_type_index(base)
	index := if builtin_idx > 0 {
		builtin_idx
	} else if isnil(tc) {
		stable_type_index(base)
	} else {
		tc.runtime_type_indexes[base] or { stable_type_index(base) }
	}
	return index | int(u32(indirections) << 16)
}

pub fn builtin_type_index(name string) int {
	return match name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'none' { 20 }
		'string' { 21 }
		'rune' { 22 }
		'float literal' { 27 }
		'int literal' { 28 }
		'thread' { 29 }
		'nil' { 31 }
		else { 0 }
	}
}

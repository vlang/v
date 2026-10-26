module types

import v.flat

const runtime_source_type_float_literal = flat.source_type_id('float literal')
const runtime_source_type_int_literal = flat.source_type_id('int literal')
const runtime_source_type_thread = flat.source_type_id('thread')

// type_index returns the runtime identity for an interned spelling, preserving pointer depth.
pub fn (tc &TypeChecker) type_index(name_id u32) int {
	mut base_id := trimmed_source_type_id(name_id)
	if base_id == 0 {
		return 0
	}
	mut indirections := 0
	for flat.source_type_shape(base_id).has(.pointer) {
		indirections++
		base_id = trimmed_source_type_id(flat.source_type_syntax(base_id).raw_base)
	}
	builtin_idx := builtin_type_index(base_id)
	index := if builtin_idx > 0 {
		builtin_idx
	} else if isnil(tc) {
		stable_type_index(base_id)
	} else {
		tc.runtime_type_indexes[base_id] or { stable_type_index(base_id) }
	}
	return index | int(u32(indirections) << 16)
}

// builtin_type_index returns V's stable runtime index for an interned builtin spelling.
pub fn builtin_type_index(name_id u32) int {
	return match name_id {
		flat.source_type_void { 1 }
		flat.source_type_voidptr { 2 }
		flat.source_type_byteptr { 3 }
		flat.source_type_charptr { 4 }
		flat.source_type_i8 { 5 }
		flat.source_type_i16 { 6 }
		flat.source_type_i32 { 7 }
		flat.source_type_int { 8 }
		flat.source_type_i64 { 9 }
		flat.source_type_isize { 10 }
		flat.source_type_u8 { 11 }
		flat.source_type_u16 { 12 }
		flat.source_type_u32 { 13 }
		flat.source_type_u64 { 14 }
		flat.source_type_uint, flat.source_type_usize { 15 }
		flat.source_type_f32 { 16 }
		flat.source_type_f64 { 17 }
		flat.source_type_char { 18 }
		flat.source_type_bool { 19 }
		flat.source_type_none { 20 }
		flat.source_type_string { 21 }
		flat.source_type_rune { 22 }
		runtime_source_type_float_literal { 27 }
		runtime_source_type_int_literal { 28 }
		runtime_source_type_thread { 29 }
		flat.source_type_nil { 31 }
		else { 0 }
	}
}

module fastc

import v.flat

// Primitive source IDs are assigned by interning, so classify their actual
// immutable values without assuming an order or a contiguous reserved range.
struct FastcPrimitiveTypeMask {
	base    u32
	bits    u64
	enabled bool
}

fn fastc_primitive_type_mask(ids []u32) FastcPrimitiveTypeMask {
	if ids.len == 0 { return FastcPrimitiveTypeMask{} }
	mut first := ids[0]
	mut last := first
	for id in ids {
		if id < first { first = id }
		if id > last { last = id }
	}
	if last - first >= 64 { return FastcPrimitiveTypeMask{} }
	mut bits := u64(0)
	for id in ids {
		bits |= u64(1) << (id - first)
	}
	return FastcPrimitiveTypeMask{ base: first, bits: bits, enabled: true }
}

@[inline]
fn (mask &FastcPrimitiveTypeMask) contains(typ u32) bool {
	if !mask.enabled || typ < mask.base { return false }
	offset := typ - mask.base
	return offset < 64 && (mask.bits & (u64(1) << offset)) != 0
}

const fastc_scalar_type_mask = fastc_primitive_type_mask([
	flat.source_type_bool,
	flat.source_type_char,
	flat.source_type_f32,
	flat.source_type_f64,
	flat.source_type_i8,
	flat.source_type_i16,
	flat.source_type_i32,
	flat.source_type_i64,
	flat.source_type_int,
	flat.source_type_isize,
	flat.source_type_rune,
	flat.source_type_string,
	flat.source_type_u8,
	flat.source_type_u16,
	flat.source_type_u32,
	flat.source_type_u64,
	flat.source_type_usize,
])

const fastc_comparison_type_mask = fastc_primitive_type_mask([
	flat.source_type_bool,
	flat.source_type_int,
	flat.source_type_i8,
	flat.source_type_i16,
	flat.source_type_i32,
	flat.source_type_i64,
	flat.source_type_isize,
	flat.source_type_u8,
	flat.source_type_u16,
	flat.source_type_u32,
	flat.source_type_u64,
	flat.source_type_usize,
	flat.source_type_f32,
	flat.source_type_f64,
])

const fastc_arithmetic_type_mask = fastc_primitive_type_mask([
	flat.source_type_int,
	flat.source_type_i8,
	flat.source_type_i16,
	flat.source_type_i32,
	flat.source_type_i64,
	flat.source_type_isize,
	flat.source_type_u8,
	flat.source_type_u16,
	flat.source_type_u32,
	flat.source_type_u64,
	flat.source_type_usize,
	flat.source_type_f32,
	flat.source_type_f64,
])

const fastc_integer_type_mask = fastc_primitive_type_mask([
	flat.source_type_char,
	flat.source_type_i8,
	flat.source_type_i16,
	flat.source_type_i32,
	flat.source_type_i64,
	flat.source_type_int,
	flat.source_type_isize,
	flat.source_type_rune,
	flat.source_type_u8,
	flat.source_type_u16,
	flat.source_type_u32,
	flat.source_type_u64,
	fastc_type_unsigned_int,
	flat.source_type_usize,
])

const fastc_unsigned_integer_type_mask = fastc_primitive_type_mask([
	flat.source_type_u8,
	flat.source_type_u16,
	flat.source_type_u32,
	flat.source_type_u64,
	fastc_type_unsigned_int,
	flat.source_type_usize,
])

const fastc_wide_unsigned_integer_type_mask = fastc_primitive_type_mask([
	flat.source_type_u32,
	flat.source_type_u64,
	fastc_type_unsigned_int,
	flat.source_type_usize,
])

const fastc_signed_integer_type_mask = fastc_primitive_type_mask([
	flat.source_type_char,
	flat.source_type_i8,
	flat.source_type_i16,
	flat.source_type_i32,
	flat.source_type_i64,
	flat.source_type_int,
	flat.source_type_isize,
	flat.source_type_rune,
])

@[inline]
fn fastc_plain_arithmetic_value_type(typ u32) bool {
	if fastc_arithmetic_type_mask.enabled { return fastc_arithmetic_type_mask.contains(typ) }
	return typ in [flat.source_type_int, flat.source_type_i8, flat.source_type_i16,
		flat.source_type_i32, flat.source_type_i64, flat.source_type_isize, flat.source_type_u8,
		flat.source_type_u16, flat.source_type_u32, flat.source_type_u64, flat.source_type_usize,
		flat.source_type_f32, flat.source_type_f64]
}

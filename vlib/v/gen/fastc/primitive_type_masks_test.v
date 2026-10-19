module fastc

import v.flat

struct FastcPrimitiveMaskTestCase {
	ids     []u32
	enabled bool
	probes  []u32
}

struct FastcInitializedPrimitiveMaskTestCase {
	ids  []u32
	mask FastcPrimitiveTypeMask
}

fn test_fastc_primitive_masks_preserve_sparse_identity_membership_and_shift_bounds() {
	cases := [
		FastcPrimitiveMaskTestCase{ ids: []u32{}, probes: [u32(0), 1, 63, 64] },
		FastcPrimitiveMaskTestCase{ ids: [u32(0)], enabled: true, probes: [u32(0), 1, 63, 64] },
		FastcPrimitiveMaskTestCase{
			ids:     [u32(17)]
			enabled: true
			probes:  [u32(0), 16, 17, 18, 80, 81]
		},
		FastcPrimitiveMaskTestCase{
			ids:     [u32(70), 7, 70, 10]
			enabled: true
			probes:  [u32(0), 6, 7, 8, 10, 69, 70, 71]
		},
		FastcPrimitiveMaskTestCase{ ids: [u32(7), 71], probes: [u32(0), 7, 8, 70, 71, 72] },
		FastcPrimitiveMaskTestCase{
			ids:    [u32(1), 0xffff_ffff]
			probes: [u32(0), 1, 2, 0xffff_fffe, 0xffff_ffff]
		},
		FastcPrimitiveMaskTestCase{
			ids:     [u32(0xffff_ffff), 0xffff_ffc0]
			enabled: true
			probes:  [u32(0), 0xffff_ffbf, 0xffff_ffc0, 0xffff_fffe, 0xffff_ffff]
		},
	]
	for item in cases {
		mask := fastc_primitive_type_mask(item.ids)
		assert mask.enabled == item.enabled, item.ids.str()
		for probe in item.probes {
			if mask.enabled {
				assert mask.contains(probe) == (probe in item.ids), '${item.ids}: ${probe}'
			} else {
				// Disabled masks must never claim a match: callers retain their
				// original ID predicates instead of using an overflowing shift.
				assert !mask.contains(probe), '${item.ids}: ${probe}'
			}
		}
	}
}

fn test_fastc_primitive_masks_leave_caller_identity_arrays_unchanged() {
	mut ids := [u32(50), 3, 50, 11]
	mask := fastc_primitive_type_mask(ids)
	assert ids == [u32(50), 3, 50, 11]
	ids[0] = 4
	assert mask.contains(50)
	assert !mask.contains(4)
}

fn test_fastc_primitive_classifiers_match_the_original_exact_source_id_sets() {
	scalar := [flat.source_type_bool, flat.source_type_char, flat.source_type_f32,
		flat.source_type_f64, flat.source_type_i8, flat.source_type_i16, flat.source_type_i32,
		flat.source_type_i64, flat.source_type_int, flat.source_type_isize, flat.source_type_rune,
		flat.source_type_string, flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_usize]
	comparison := [flat.source_type_bool, flat.source_type_int, flat.source_type_i8,
		flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
		flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		flat.source_type_usize, flat.source_type_f32, flat.source_type_f64]
	arithmetic := [flat.source_type_int, flat.source_type_i8, flat.source_type_i16,
		flat.source_type_i32, flat.source_type_i64, flat.source_type_isize, flat.source_type_u8,
		flat.source_type_u16, flat.source_type_u32, flat.source_type_u64, flat.source_type_usize,
		flat.source_type_f32, flat.source_type_f64]
	signed := [flat.source_type_char, flat.source_type_i8, flat.source_type_i16, flat.source_type_i32,
		flat.source_type_i64, flat.source_type_int, flat.source_type_isize, flat.source_type_rune]
	unsigned := [flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		fastc_type_unsigned_int, flat.source_type_usize]
	wide := [flat.source_type_u32, flat.source_type_u64, fastc_type_unsigned_int,
		flat.source_type_usize]
	mut integer := signed.clone()
	integer << unsigned
	for item in [
		FastcInitializedPrimitiveMaskTestCase{ ids: scalar, mask: fastc_scalar_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: comparison, mask: fastc_comparison_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: arithmetic, mask: fastc_arithmetic_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: integer, mask: fastc_integer_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: signed, mask: fastc_signed_integer_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: unsigned, mask: fastc_unsigned_integer_type_mask },
		FastcInitializedPrimitiveMaskTestCase{ ids: wide, mask: fastc_wide_unsigned_integer_type_mask },
	] {
		mut first := item.ids[0]
		mut last := first
		for id in item.ids {
			if id < first { first = id }
			if id > last { last = id }
		}
		// A wrongly uninitialized or disabled mask must not hide behind the
		// correct legacy fallback and make this optimization test pass.
		assert item.mask.enabled == (last - first < 64), item.ids.str()
		if item.mask.enabled {
			assert item.mask.base == first
			for id in item.ids {
				assert item.mask.contains(id), id.str()
			}
		}
	}
	mut probes := []u32{}
	for typ in 0 .. 512 { probes << u32(typ) }
	for typ in scalar {
		probes << typ
		if typ > 0 { probes << typ - 1 }
		if typ < u32(0xffff_ffff) { probes << typ + 1 }
	}
	probes << fastc_type_unsigned_int
	probes << fastc_type_integer_literal
	probes << fastc_type_negative_integer_literal
	probes << fastc_type_float_literal
	probes << [u32(0), u32(0xffff_ffff), flat.source_type_void, flat.source_type_voidptr,
		flat.source_type_byteptr, flat.source_type_charptr, flat.source_type_nil,
		flat.source_type_none, flat.source_type_id('PrimitiveAlias'), flat.source_type_id('int*'),
		flat.source_type_id('Array_int'), flat.source_type_id('Map_string_int'),
		flat.source_type_id('FixedArray_2_int')]
	for typ in probes {
		assert fastc_simple_call_scalar_type(typ) == (typ in scalar), typ.str()
		assert fastc_primitive_member_comparison_value_type(typ) == (typ in comparison), typ.str()
		assert fastc_plain_arithmetic_value_type(typ) == (typ in arithmetic), typ.str()
		assert fastc_is_integer_type(typ) == (typ in integer), typ.str()
		assert fastc_is_signed_integer_type(typ) == (typ in signed), typ.str()
		assert fastc_is_unsigned_integer_type(typ) == (typ in unsigned), typ.str()
		assert fastc_is_wide_unsigned_integer_type(typ) == (typ in wide), typ.str()
	}
}

module main

import cnumalias

fn test_c_numeric_alias_call_casts_convert_back_to_numbers() {
	a := cnumalias.make(3)
	assert u32(a) == 3
	assert int(a) == 3
	assert u64(C.CNA_U32(7)) == 7
	assert f64(C.CNA_U32(2)) / 4 == 0.5
	b := C.CNA_U32(u32(9))
	assert u32(b) == 9
	assert int(cnumalias.negative()) == -5
	assert cnumalias.row_count() == 42
}

fn test_c_numeric_alias_comparisons_and_arithmetic() {
	a := cnumalias.make(3)
	assert a == C.CNA_U32(3)
	assert a == cnumalias.make(3)
	assert a != C.CNA_U32(4)
	assert a < C.CNA_U32(4)
	assert cnumalias.same(a, C.CNA_U32(3))
	assert !cnumalias.same(a, C.CNA_U32(5))
	assert cnumalias.widen(a) == 4
	assert cnumalias.local_ops() == 8
	mut c := C.CNA_U32(a) + 1
	c *= C.CNA_U32(2)
	assert u32(c) == 8
}

fn test_c_numeric_alias_in_fields_arrays_and_strings() {
	mut h := cnumalias.Holder{
		value: cnumalias.make(5)
		small: cnumalias.negative()
		len:   C.CNA_LEN(1) << 40
	}
	h.list << h.value
	h.list << C.CNA_U32(9)
	assert u32(h.value) == 5
	assert int(h.small) == -5
	assert i64(h.len) == i64(1) << 40
	assert h.list.map(u32(it)) == [u32(5), 9]
	assert h.list[1] > h.list[0]
	fixed := [C.CNA_U32(1), C.CNA_U32(2)]!
	assert u32(fixed[0]) + u32(fixed[1]) == 3
	mut m := map[string]C.CNA_U32{}
	m['x'] = C.CNA_U32(11)
	assert u32(m['x']) == 11
	assert '${h.value} ${C.CNA_U32(6)}' == '5 6'
}

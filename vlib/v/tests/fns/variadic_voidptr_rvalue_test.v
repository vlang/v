import strconv

struct VariadicVoidptrValue {
mut:
	x u8
}

fn test_variadic_voidptr_rvalues_are_boxed_with_promotions() {
	mut value := VariadicVoidptrValue{
		x: 1
	}
	assert unsafe { strconv.v_sprintf('x=%02d', value.x) } == 'x=01'
	assert unsafe { strconv.v_sprintf('x=%02d', int(value.x)) } == 'x=01'
	assert unsafe { strconv.v_sprintf('%s %.1f', 'abc', f32(1.5)) } == 'abc 1.5'
}

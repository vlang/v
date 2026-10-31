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

fn verify_variadic_voidptr_storage(args ...voidptr) {
	assert args.len == 7
	// The voidptr tail erases storage types, so checking the promoted payloads requires casts.
	unsafe {
		assert *(&int(args[0])) == 1
		assert *(&int(args[1])) == 2
		assert *(&f64(args[2])) == 1.5
		assert *(&f64(args[3])) == 2.5
		assert *(&string(args[4])) == 'boxed'
		assert *(&int(args[5])) == 42
		assert args[6] == nil
	}
}

fn test_variadic_voidptr_values_use_one_promoted_storage_slot() {
	value := VariadicVoidptrValue{
		x: 1
	}
	float_value := f32(1.5)
	pointed_value := 42
	verify_variadic_voidptr_storage(value.x, i16(2), float_value, f32(2.5), 'boxed',
		&pointed_value, unsafe { nil })
}

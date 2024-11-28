const foo = u32(1)
const bar = u32(2)
const weapon_keys = [&foo, &bar]!

fn test_const_fixed_array_of_ref_value() {
	assert weapon_keys[0] == &foo
	assert weapon_keys[1] == &bar
}

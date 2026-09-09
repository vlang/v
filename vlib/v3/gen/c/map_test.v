module c

import v3.types

fn test_map_integer_callback_size_suffix_uses_target_pointer_width() {
	for key_type in [types.Type(types.voidptr_), types.Type(types.isize_), types.Type(types.usize_)] {
		assert map_integer_callback_size_suffix(key_type, 'void*', 32) == '4'
		assert map_integer_callback_size_suffix(key_type, 'void*', 64) == '8'
	}
}

fn test_map_integer_callback_size_suffix_uses_scalar_width() {
	assert map_integer_callback_size_suffix(types.Type(types.u8_), 'u8', 64) == '1'
	assert map_integer_callback_size_suffix(types.Type(types.u16_), 'u16', 64) == '2'
	assert map_integer_callback_size_suffix(types.Type(types.u32_), 'u32', 64) == '4'
	assert map_integer_callback_size_suffix(types.Type(types.u64_), 'u64', 32) == '8'
}

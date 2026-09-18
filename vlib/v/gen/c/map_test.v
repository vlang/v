module c

import v.flat
import v.pref
import v.types

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

fn map_codegen_callback_test_names(g &FlatGen, key types.Type) []string {
	hash_fn, eq_fn, clone_fn, free_fn := g.map_callback_names(key)
	return [hash_fn, eq_fn, clone_fn, free_fn]
}

fn test_map_codegen_callbacks_unwrap_alias_keys() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.target = pref.Target{
			pointer_bits: bits
		}
		for key in [types.Type(types.string_), types.Type(types.int_), types.Type(types.u8_),
			types.Type(types.u16_), types.Type(types.u32_), types.Type(types.u64_),
			types.Type(types.isize_), types.Type(types.usize_), types.Type(types.voidptr_),
			types.Type(types.ArrayFixed{ elem_type: types.Type(types.u64_), len: 2 })] {
			alias := types.Type(types.Alias{
				name:      'MapKey'
				base_type: key
			})
			nested := types.Type(types.Alias{
				name:      'NestedMapKey'
				base_type: alias
			})
			expected := map_codegen_callback_test_names(g, key)
			assert map_codegen_callback_test_names(g, alias) == expected, '${bits}: ${key.name()}'
			assert map_codegen_callback_test_names(g, nested) == expected, '${bits}: ${key.name()}'
		}
		assert map_codegen_callback_test_names(g, types.Type(types.string_)) == [
			'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string']
		size := (bits / 8).str()
		assert map_codegen_callback_test_names(g, types.Type(types.int_)) == [
			'map_hash_int_${size}', 'map_eq_int_${size}', 'map_clone_int_${size}', 'map_free_nop']
	}
}

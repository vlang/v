module transform

import v.flat
import v.types

fn map_callback_test_names(key string) []string {
	hash_fn, eq_fn, clone_fn, free_fn := map_callback_names(key)
	return [hash_fn, eq_fn, clone_fn, free_fn]
}

fn map_callback_test_integer_names(size string) []string {
	return ['map_hash_int_${size}', 'map_eq_int_${size}', 'map_clone_int_${size}', 'map_free_nop']
}

fn test_map_callbacks_follow_target_width_not_host_width() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		expected := map_callback_test_integer_names((bits / 8).str())
		for key in ['int', 'isize', 'usize', 'uint', 'voidptr', 'charptr', 'byteptr', '&int', '&Item',
			'&&Item'] {
			assert map_callback_test_names(key) == expected, '${bits}: ${key}'
		}
	}
}

fn test_map_callbacks_preserve_fixed_width_and_string_keys() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		for size, keys in {
			'1': ['u8', 'i8', 'byte', 'bool', 'char']
			'2': ['u16', 'i16']
			'4': ['u32', 'i32', 'f32', 'rune']
			'8': ['u64', 'i64', 'f64']
		} {
			for key in keys {
				assert map_callback_test_names(key) == map_callback_test_integer_names(size), '${bits}: ${key}'
			}
		}
		assert map_callback_test_names('string') == ['map_hash_string', 'map_eq_string',
			'map_clone_string', 'map_free_string']
	}
}

fn test_map_callbacks_resolve_aliases_and_keep_fixed_array_helpers() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		aliases := {
			'WordKey':   'int'
			'WideKey':   'u64'
			'TinyKey':   'u8'
			'TextKey':   'string'
			'IndexKey':  'usize'
			'PtrKey':    'voidptr'
			'NestedKey': 'TextKey'
			'BlockKey':  '[2]u64'
		}
		for name, base in aliases {
			tc.type_aliases[name] = base
			tc.type_alias_modules[name] = 'main'
		}
		t := Transformer{
			a:          &a
			tc:         &tc
			cur_module: 'main'
		}
		for name, base in aliases {
			hash_fn, eq_fn, clone_fn, free_fn := t.map_callback_names_for_type(name)
			actual := [hash_fn, eq_fn, clone_fn, free_fn]
			if name == 'BlockKey' {
				prefix := '${tc.c_type(tc.parse_type(base))}_map_key'
				assert actual == ['${prefix}_hash', '${prefix}_eq', '${prefix}_clone',
					'${prefix}_free']
			} else {
				expected := map_callback_test_names(if name == 'NestedKey' {
					'string'
				} else {
					base
				})
				assert actual == expected, '${bits}: ${name}'
			}
		}
	}
}

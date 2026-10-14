module transform

import v.flat
import v.types

fn voidptr_variadic_test_storage(mut t Transformer, typ string) string {
	t.set_var_type('arg', typ)
	return t.voidptr_variadic_storage_type(t.a.add_node(flat.Node{ kind: .ident, value: 'arg' }))
}

// The callee of a `...voidptr` parameter reads an integer argument as an `int`, so the
// temporary of an argument has to be as wide as the target's `int`, not the host's.
fn test_voidptr_variadic_storage_widens_integers_narrower_than_the_target_int() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for bits in [32, 64] {
		types.set_platform_int_bits(bits)
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		for name in ['Plain', 'Tiny', 'Short', 'Half', 'Whole', 'Wide', 'dep.Tiny'] {
			tc.enum_names[name] = true
		}
		tc.structs['Point'] = []types.StructField{}
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.enum_backing_types['Tiny'] = 'u8'
		t.enum_backing_types['Short'] = 'i16'
		t.enum_backing_types['Half'] = 'u32'
		t.enum_backing_types['Whole'] = 'int'
		t.enum_backing_types['Wide'] = 'u64'
		t.enum_backing_types['dep.Tiny'] = 'i64'
		for typ in ['char', 'i8', 'u8', 'i16', 'u16', 'Tiny', 'Short'] {
			assert voidptr_variadic_test_storage(mut t, typ) == 'int', '${bits}: ${typ}'
		}
		// As wide as a 32 bit `int`. An enum without a backing type is a C `int`.
		for typ in ['i32', 'u32', 'rune', 'Plain', 'Half'] {
			expected := if bits == 64 { 'int' } else { typ }
			assert voidptr_variadic_test_storage(mut t, typ) == expected, '${bits}: ${typ}'
		}
		for typ in ['int', 'i64', 'u64', 'isize', 'usize', 'bool', 'f64', 'string', 'Whole', 'Wide',
			'dep.Tiny', 'Point'] {
			assert voidptr_variadic_test_storage(mut t, typ) == typ, '${bits}: ${typ}'
		}
		assert voidptr_variadic_test_storage(mut t, 'f32') == 'f64'
	}
}

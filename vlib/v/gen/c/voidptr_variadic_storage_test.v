module c

import v.flat
import v.pref
import v.types

// The callee of a `...voidptr` parameter reads an integer argument as a V `int`, so the
// temporary of an argument has to be as wide as the target's `int`, not as C's `int`.
fn test_voidptr_variadic_storage_c_type_widens_integers_narrower_than_the_target_int() {
	original_bits := types.platform_int_bits()
	defer {
		types.set_platform_int_bits(original_bits)
	}
	for arch in ['arm32', 'amd64'] {
		target := pref.target_from('linux', arch) or { panic(err) }
		types.set_platform_int_bits(target.pointer_bits)
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.set_target(target)
		g.register_enum_backing_info('Tiny', 'u8')
		g.register_enum_backing_info('Whole', 'int')
		g.register_enum_backing_info('Wide', 'u64')
		int_ct := if target.pointer_bits == 64 { 'i64' } else { 'i32' }
		small_alias := types.Type(types.Alias{
			name:      'Small'
			base_type: types.Type(types.u8_)
		})
		for typ in [types.Type(types.char_), types.Type(types.i8_), types.Type(types.u8_),
			types.Type(types.i16_), types.Type(types.u16_), types.Type(types.int_),
			types.Type(types.Enum{ name: 'Tiny' }), types.Type(types.Enum{ name: 'Whole' }),
			small_alias] {
			assert g.voidptr_variadic_storage_c_type(typ) == int_ct, '${arch}: ${typ.name()}'
		}
		// As wide as a 32 bit `int`. An enum without a backing type is a C `int`.
		same_as_int32 := {
			'i32':   types.Type(types.i32_)
			'u32':   types.Type(types.u32_)
			'rune':  types.Type(types.rune_)
			'Plain': types.Type(types.Enum{
				name: 'Plain'
			})
		}
		for name, typ in same_as_int32 {
			expected := if target.pointer_bits == 64 { 'i64' } else { tc.c_type(typ) }
			assert g.voidptr_variadic_storage_c_type(typ) == expected, '${arch}: ${name}'
		}
		assert tc.c_type(types.Type(types.Enum{ name: 'Plain' })) == 'int'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.i64_)) == 'i64'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.u64_)) == 'u64'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.Enum{ name: 'Wide' })) == 'u64'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.bool_)) == 'bool'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.string_)) == 'string'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.f64_)) == 'double'
		assert g.voidptr_variadic_storage_c_type(types.Type(types.f32_)) == 'double'
	}
}

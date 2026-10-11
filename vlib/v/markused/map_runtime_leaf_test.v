module markused

import v.flat
import v.types

fn map_runtime_leaf_values() []types.Type {
	return [
		types.Type(types.bool_),
		types.Type(types.int_),
		types.Type(types.i8_),
		types.Type(types.i64_),
		types.Type(types.u8_),
		types.Type(types.u64_),
		types.Type(types.i128_),
		types.Type(types.u128_),
		types.Type(types.f32_),
		types.Type(types.f64_),
		types.Type(types.String{}),
		types.Type(types.Char{}),
		types.Type(types.Rune{}),
		types.Type(types.ISize{}),
		types.Type(types.USize{}),
		types.Type(types.Void{}),
		types.Type(types.Nil{}),
		types.Type(types.None{}),
	]
}

fn test_map_runtime_semantic_leaves_do_not_require_map_helpers() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	mut scan := RuntimeHelpersScan{}
	for typ in map_runtime_leaf_values() {
		assert !scan.type_needs_map_runtime(typ, &tc), typ.name()
	}
	assert scan.map_type_cache.len == 0
}

fn test_map_runtime_wrappers_preserve_leaf_and_map_results() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	for elem in [types.Type(types.int_), types.Type(types.Map{
		key_type:   types.Type(types.String{})
		value_type: types.Type(types.int_)
	})] {
		expected := elem is types.Map
		for typ in [
			types.Type(types.Alias{ name: 'Values', base_type: elem }),
			types.Type(types.Pointer{ base_type: elem }),
			types.Type(types.OptionType{ base_type: elem }),
			types.Type(types.ResultType{ base_type: elem }),
			types.Type(types.Array{ elem_type: elem }),
			types.Type(types.ArrayFixed{ elem_type: elem, len: 3 }),
			types.Type(types.Channel{ elem_type: elem }),
		] {
			mut scan := RuntimeHelpersScan{}
			assert scan.type_needs_map_runtime(typ, &tc) == expected, typ.name()
			assert scan.type_needs_map_runtime(typ, &tc) == expected, typ.name()
		}
	}
}

fn test_map_runtime_mixed_signatures_and_recursive_structs_keep_maps() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mapped := types.Type(types.Map{
		key_type:   types.Type(types.String{})
		value_type: types.Type(types.int_)
	})
	self_type := types.Type(types.Struct{ name: 'Recursive' })
	tc.structs['Recursive'] = [
		types.StructField{ name: 'marker', typ: types.Type(types.int_) },
		types.StructField{ name: 'next', typ: types.Type(types.Pointer{ base_type: self_type }) },
		types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: mapped, len: 2 }) },
	]
	for typ in [
		self_type,
		types.Type(types.FnType{
			params:      [types.Type(types.int_), types.Type(types.Pointer{ base_type: mapped })]
			return_type: types.Type(types.Void{})
		}),
		types.Type(types.FnType{
			params:      [types.Type(types.String{})]
			return_type: types.Type(types.ResultType{ base_type: mapped })
		}),
		types.Type(types.MultiReturn{
			types: [types.Type(types.int_), types.Type(types.OptionType{ base_type: mapped })]
		}),
	] {
		mut scan := RuntimeHelpersScan{}
		assert scan.type_needs_map_runtime(typ, &tc), typ.name()
		assert scan.type_needs_map_runtime(typ, &tc), typ.name()
	}
}

fn test_map_runtime_generic_placeholders_and_unknowns_keep_existing_results() {
	mut a := flat.FlatAst.new()
	tc := types.TypeChecker.new(&a)
	generic := types.Type(types.Unknown{ reason: 'generic placeholder T' })
	unknown := types.Type(types.Unknown{ reason: 'unresolved value' })
	mut scan := RuntimeHelpersScan{}
	assert scan.type_needs_map_runtime(generic, &tc)
	assert !scan.type_needs_map_runtime(unknown, &tc)
	assert scan.type_needs_map_runtime(types.Type(types.Alias{
		name:      'Deferred'
		base_type: generic
	}), &tc)
	assert scan.type_needs_map_runtime(generic, &tc)
	assert !scan.type_needs_map_runtime(unknown, &tc)
}

fn test_map_runtime_nominal_primitive_spelling_keeps_semantic_cache_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mapped := types.Type(types.Map{
		key_type:   types.Type(types.String{})
		value_type: types.Type(types.int_)
	})
	// Hand-built metadata can reuse a primitive spelling; its semantic tag differs.
	for leaf in [types.Type(types.int_), types.Type(types.String{})] {
		name := leaf.name()
		nominal := types.Type(types.Struct{ name: name })
		tc.structs[name] = [types.StructField{ name: 'values', typ: mapped }]
		for leaf_first in [false, true] {
			mut scan := RuntimeHelpersScan{}
			if leaf_first {
				assert !scan.type_needs_map_runtime(leaf, &tc)
			}
			assert scan.type_needs_map_runtime(nominal, &tc)
			assert !scan.type_needs_map_runtime(leaf, &tc)
			assert scan.type_needs_map_runtime(nominal, &tc)
		}
	}
}

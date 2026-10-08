module markused

import v.flat
import v.types

fn test_runtime_helper_queries_keep_wrapped_errors_after_scalar_comparisons() {
	mut a := flat.FlatAst{}
	mut tc := types.TypeChecker.new(&a)
	mut cache := map[string]int{}
	for scalar in [types.Type(types.int_), types.Type(types.f64_), types.Type(types.string_),
		types.Type(types.char_), types.Type(types.rune_), types.Type(types.isize_),
		types.Type(types.usize_), types.Type(types.void_), types.Type(types.nil_),
		types.Type(types.none_), types.Type(types.bool_)] {
		assert !markused_type_equality_uses_ierror(scalar, &tc, mut cache)
	}
	error_type := types.Type(types.Interface{ name: 'builtin.IError' })
	alias := types.Type(types.Alias{ name: 'support.int', base_type: error_type })
	for wrapped in [alias, types.Type(types.OptionType{ base_type: alias }),
		types.Type(types.ResultType{ base_type: alias }), types.Type(types.Array{ elem_type: alias }),
		types.Type(types.ArrayFixed{ elem_type: alias, len: 2 }),
		types.Type(types.Map{ key_type: types.string_, value_type: alias }),
		types.Type(types.MultiReturn{ types: [types.Type(types.int_), alias] })] {
		assert markused_type_equality_uses_ierror(wrapped, &tc, mut cache)
	}
	// Equality does not descend into pointers, even when their base is IError.
	assert !markused_type_equality_uses_ierror(types.Pointer{ base_type: error_type }, &tc,
		mut cache)
	assert !markused_type_equality_uses_ierror(types.Unknown{ reason: 'generic placeholder T' },
		&tc, mut cache)
}

fn test_channel_stringification_keeps_alias_module_context_after_scalar_prints() {
	mut a := flat.FlatAst{}
	mut tc := types.TypeChecker.new(&a)
	mut scan := RuntimeHelpersScan{}
	channel := types.Type(types.Channel{ elem_type: types.int_ })
	alias := types.Type(types.Alias{ name: 'Value', base_type: channel })
	tc.fn_ret_types['first.Value.str'] = types.string_
	for current_module in ['first', 'second'] {
		for scalar in [types.Type(types.string_), types.Type(types.int_), types.Type(types.nil_),
			types.Type(types.none_)] {
			assert !markused_type_stringifies_channel(scalar, current_module, &tc,
				mut scan.channel_stringify_cache)
		}
	}
	assert !markused_type_stringifies_channel(alias, 'first', &tc,
		mut scan.channel_stringify_cache)
	assert markused_type_stringifies_channel(alias, 'second', &tc,
		mut scan.channel_stringify_cache)
	assert !markused_type_stringifies_channel(alias, 'first', &tc,
		mut scan.channel_stringify_cache)
	assert markused_type_stringifies_channel(types.Pointer{ base_type: channel }, 'first', &tc,
		mut scan.channel_stringify_cache)
	assert !markused_type_stringifies_channel(types.Unknown{ reason: 'generic placeholder T' },
		'second', &tc, mut scan.channel_stringify_cache)
}

fn test_runtime_helper_queries_keep_recursive_nominal_payloads_and_custom_str() {
	mut a := flat.FlatAst{}
	mut tc := types.TypeChecker.new(&a)
	mut equality_cache := map[string]int{}
	mut channel_cache := map[string]int{}
	error_type := types.Type(types.Interface{ name: 'IError' })
	channel := types.Type(types.Channel{ elem_type: types.int_ })
	error_box := types.Type(types.Struct{ name: 'support.int' })
	channel_box := types.Type(types.Struct{ name: 'support.string' })
	tc.structs['support.int'] = [
		types.StructField{ name: 'recursive', typ: error_box },
		types.StructField{ name: 'code', typ: types.int_ },
		types.StructField{ name: 'cause', typ: error_type },
	]
	tc.structs['support.string'] = [
		types.StructField{ name: 'recursive', typ: channel_box },
		types.StructField{ name: 'label', typ: types.string_ },
		types.StructField{ name: 'events', typ: channel },
	]
	assert !markused_type_equality_uses_ierror(types.int_, &tc, mut equality_cache)
	assert !markused_type_stringifies_channel(types.string_, 'support', &tc, mut channel_cache)
	// Nominal payloads with builtin-like suffixes must retain their own dependency traversal.
	assert markused_type_equality_uses_ierror(error_box, &tc, mut equality_cache)
	assert markused_type_stringifies_channel(channel_box, 'support', &tc, mut channel_cache)
	assert markused_type_stringifies_channel(types.Map{
		key_type:   types.string_
		value_type: channel_box
	}, 'support', &tc, mut channel_cache)
	tc.fn_ret_types['support.Custom.str'] = types.string_
	tc.structs['support.Custom'] = [types.StructField{ name: 'events', typ: channel }]
	assert !markused_type_stringifies_channel(types.Struct{ name: 'support.Custom' }, 'support',
		&tc, mut channel_cache)
	assert markused_type_equality_uses_ierror(error_box, &tc, mut equality_cache)
}

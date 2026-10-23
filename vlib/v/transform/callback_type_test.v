module transform

import v.types

fn test_imported_type_payloads_keep_implicit_references() {
	fixed := types.ArrayFixed{ elem_type: types.Type(types.string_), len: 2 }
	channel := types.Channel{ elem_type: types.Type(types.int_), is_mut: true }
	mapping := types.Map{
		key_type:   types.Type(types.string_)
		value_type: types.Type(types.int_)
	}
	signature := types.FnType{ return_type: types.Type(types.string_) }
	alias := types.Alias{ name_id: 123, base_type: types.Type(types.int_) }
	assert fixed.elem_type is types.String
	assert fixed.len == 2
	assert channel.elem_type is types.Primitive
	assert mapping.key_type is types.String
	assert mapping.value_type is types.Primitive
	assert signature.return_type is types.String
	assert alias.base_type is types.Primitive
	assert types.ArrayFixed{}.elem_type is types.Void
	assert types.Map{}.key_type is types.Void
	assert types.FnType{}.return_type is types.Void
}

fn test_callback_normalization_keeps_shared_type_payloads_immutable() {
	callback := types.Type(types.FnType{
		params:      [types.FnParam{ typ: &types.Type(types.Array{ elem_type: &types.builtin_int_type }), is_mut: true }]
		return_type: &types.builtin_string_type
		is_variadic: true
	})
	inputs := [callback, types.Type(types.ArrayFixed{
		elem_type: types.new_type_reference(callback)
		len:       3
		len_expr:  'count'
	}), types.Type(types.Channel{
		elem_type: types.new_type_reference(callback)
		is_mut:    true
	}), types.Type(types.Map{
		key_type:   &types.builtin_string_type
		value_type: types.new_type_reference(callback)
	}), types.Type(types.MultiReturn{
		types: [callback, types.builtin_int_type]
	})]
	for input in inputs {
		before := types.clone_owned_type(input)
		normalized := callback_type_without_variadic_modes(input)
		assert types.semantic_types_equal(input, before)
		assert !types.semantic_types_equal(input, normalized)
		match normalized {
			types.FnType {
				assert !normalized.is_variadic
				assert normalized.params[0].is_mut
			}
			types.ArrayFixed {
				assert normalized.len == 3
				assert normalized.len_expr == 'count'
				assert normalized.elem_type is types.FnType
				assert !normalized.elem_type.is_variadic
			}
			types.Channel {
				assert normalized.is_mut
				assert normalized.elem_type is types.FnType
				assert !normalized.elem_type.is_variadic
			}
			types.Map {
				assert normalized.key_type is types.String
				assert normalized.value_type is types.FnType
				assert !normalized.value_type.is_variadic
			}
			types.MultiReturn {
				first := normalized.types[0]
				assert first is types.FnType
				assert !first.is_variadic
				assert normalized.types[1] is types.Primitive
			}
			else {
				assert false
			}
		}
	}
}

module types

fn test_type_descriptors_keep_two_word_layout() {
	assert sizeof(Type) <= 16
	for size in [sizeof(Unknown), sizeof(ArrayFixed), sizeof(Channel), sizeof(Map), sizeof(FnType),
		sizeof(Alias), sizeof(MultiReturn)] {
		assert size == sizeof(voidptr)
	}
}

fn test_indirect_type_defaults_and_promoted_initializers() {
	assert Unknown{}.reason == ''
	fixed := ArrayFixed{}
	assert fixed.elem_type is Void
	assert fixed.len == 0
	assert fixed.len_expr == ''
	channel := Channel{}
	assert channel.elem_type is Void
	assert !channel.is_mut
	mapping := Map{}
	assert mapping.key_type is Void
	assert mapping.value_type is Void
	signature := FnType{}
	assert signature.params.len == 0
	assert signature.return_type is Void
	assert !signature.is_variadic
	alias := Alias{}
	assert alias.name_id == 0
	assert alias.base_type is Void
	assert MultiReturn{}.types.len == 0
}

fn test_indirect_type_copies_and_owned_clones_preserve_semantics() {
	values := [Type(Unknown{ reason: 'unknown'.clone() }),
		Type(ArrayFixed{ elem_type: &builtin_int_type, len: 3, len_expr: 'size'.clone() }),
		Type(Channel{ elem_type: &builtin_int_type, is_mut: true }),
		Type(Map{ key_type: &builtin_string_type, value_type: &builtin_int_type }), Type(FnType{
			params:      [FnParam{ typ: &builtin_int_type, is_mut: true }]
			return_type: &builtin_string_type
			is_variadic: true
		}), Type(Alias{ name_id: 123, base_type: &builtin_int_type }),
		Type(MultiReturn{ types: [builtin_int_type, builtin_string_type] })]
	for value in values {
		copy := new_type_reference(value)
		owned := clone_owned_type(value)
		assert value == *copy
		assert value == owned
		assert semantic_type_hash(value) == semantic_type_hash(owned)
		match value {
			Unknown {
				assert value == owned as Unknown
			}
			ArrayFixed {
				assert value == owned as ArrayFixed
			}
			Channel {
				assert value == owned as Channel
			}
			Map {
				assert value == owned as Map
			}
			FnType {
				assert value == owned as FnType
			}
			Alias {
				assert value == owned as Alias
			}
			MultiReturn {
				assert value == owned as MultiReturn
			}
			else {
				assert false
			}
		}
	}
}

fn test_promoted_type_payloads_keep_implicit_references() {
	fixed := ArrayFixed{ elem_type: Type(string_), len: 2 }
	channel := Channel{ elem_type: Type(int_), is_mut: true }
	mapping := Map{ key_type: Type(string_), value_type: Type(int_) }
	signature := FnType{ return_type: Type(string_) }
	alias := Alias{ name_id: 123, base_type: Type(int_) }
	assert fixed.elem_type is String
	assert fixed.len == 2
	assert channel.elem_type is Primitive
	assert mapping.key_type is String
	assert mapping.value_type is Primitive
	assert signature.return_type is String
	assert alias.base_type is Primitive
	// A promoted initializer must copy the shared default rather than change it.
	assert ArrayFixed{}.elem_type is Void
	assert Map{}.key_type is Void
	assert FnType{}.return_type is Void
}

fn test_indirect_type_payload_equality_rejects_different_fields() {
	assert Unknown{ reason: 'a' } != Unknown{ reason: 'b' }
	assert ArrayFixed{ len: 2 } != ArrayFixed{ len: 3 }
	assert ArrayFixed{ len_expr: 'a' } != ArrayFixed{ len_expr: 'b' }
	assert Channel{} != Channel{ is_mut: true }
	assert Map{ key_type: &builtin_int_type } != Map{ key_type: &builtin_string_type }
	assert FnType{} != FnType{ is_variadic: true }
	assert FnType{ params: [FnParam{ typ: &builtin_int_type }] } != FnType{ params: [FnParam{ typ: &builtin_string_type }] }
	assert Alias{ name_id: 1 } != Alias{ name_id: 2 }
	assert MultiReturn{ types: [builtin_int_type] } != MultiReturn{ types: [builtin_string_type] }
}

fn test_function_type_names_preserve_nested_mutable_and_variadic_parameters() {
	assert Type(FnType{}).name() == 'fn()'
	mut_int := Type(Pointer{ base_type: &builtin_int_type })
	strings := Type(Array{ elem_type: &builtin_string_type })
	fn_type := Type(FnType{
		params:      [FnParam{ typ: &mut_int, is_mut: true }, FnParam{ typ: &strings }]
		return_type: &builtin_string_type
		is_variadic: true
	})
	assert fn_type.name() == 'fn(mut int, ...string) string'
	outer := Type(FnType{
		params: [FnParam{ typ: &fn_type }, FnParam{ typ: &builtin_int_type, is_mut: true }]
	})
	assert outer.name() == 'fn(fn(mut int, ...string) string, mut int)'
	mutable_variadic := Type(FnType{ params: [FnParam{ typ: &mut_int, is_mut: true }], is_variadic: true })
	assert mutable_variadic.name() == 'fn(mut int)'
}

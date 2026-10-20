module types

import v.flat

fn test_mut_optional_binding_uses_the_declared_payload_view() {
	value := Type(Struct{ name_id: flat.source_type_id('Item') })
	pointer := Type(Pointer{ base_type: new_type_reference(value) })
	payloads := [new_type_reference(value), new_type_reference(pointer)]
	for payload in payloads {
		option := Type(OptionType{ base_type: payload })
		storage := Type(Pointer{ base_type: new_type_reference(option) })
		view := mut_param_binding_type(storage, true, false)
		assert view.name() == '?&Item'
		if payload is Pointer {
			assert view == option
		}
		assert mut_param_binding_type(storage, false, false) == storage
		assert mut_param_binding_type(storage, true, true) == storage
	}
}

fn test_mut_optional_adapters_keep_inline_options_and_nominal_alias_views() {
	value := Type(Struct{ name_id: flat.source_type_id('Item') })
	pointer := Type(Pointer{ base_type: new_type_reference(value) })
	array := Type(Array{ elem_type: new_type_reference(Type(int_)) })
	struct_alias := Type(Alias{ name_id: flat.source_type_id('ItemAlias'), base_type: new_type_reference(value) })
	array_alias := Type(Alias{ name_id: flat.source_type_id('Items'), base_type: new_type_reference(array) })
	pointer_alias := Type(Alias{ name_id: flat.source_type_id('ItemRef'), base_type: new_type_reference(pointer) })
	scalar_alias := Type(Alias{ name_id: flat.source_type_id('Count'), base_type: new_type_reference(Type(int_)) })
	payloads := [new_type_reference(value), new_type_reference(pointer), new_type_reference(array),
		new_type_reference(struct_alias), new_type_reference(array_alias),
		new_type_reference(pointer_alias), new_type_reference(scalar_alias)]
	// A direct pointer payload keeps its ABI. A nominal pointer alias keeps
	// the nonstruct body view while receiving the upstream reference adapter.
	expected_slots := ['&?&Item', '&?&Item', '&?&[]int', '&?&ItemAlias', '&?&Items', '&?&ItemRef',
		'&?&Count']
	expected_views := ['?&Item', '?&Item', '?[]int', '?&ItemAlias', '?Items', '?ItemRef', '?Count']
	for i, payload in payloads {
		option := Type(OptionType{ base_type: payload })
		storage := Type(Pointer{ base_type: new_type_reference(option) })
		semantic := mut_param_semantic_type(storage)
		assert semantic.name() == expected_slots[i], semantic.name()
		assert mut_param_binding_type(storage, true, false).name() == expected_views[i]
		assert mut_param_semantic_type(semantic).name() == expected_slots[i]
		assert mut_param_semantic_type(option) == option
		assert option is OptionType
		assert voidptr(option.base_type) == voidptr(payload)
		assert semantic is Pointer
		adapted_option := *semantic.base_type
		assert adapted_option is OptionType
		assert adapted_option.base_type is Pointer
	}
}

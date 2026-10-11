module types

fn test_mut_optional_binding_preserves_the_declared_payload() {
	value := Type(Struct{ name: 'Item' })
	pointer := Type(Pointer{ base_type: &value })
	payloads := [&value, &pointer]
	for payload in payloads {
		option := Type(OptionType{ base_type: payload })
		storage := Type(Pointer{ base_type: &option })
		assert mut_param_binding_type(storage, true, false) == option
		assert mut_param_binding_type(storage, false, false) == storage
		assert mut_param_binding_type(storage, true, true) == storage
	}
}

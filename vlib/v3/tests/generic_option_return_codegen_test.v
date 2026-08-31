struct OptionPicker {}

fn missing_number() !int {
	return error('missing')
}

fn (picker OptionPicker) or_default[T](fallback T) T {
	_ = picker
	value := missing_number() or { return fallback }
	return value
}

fn test_generic_method_option_type_argument_keeps_concrete_return_abi() {
	fallback := ?int(none)
	value := OptionPicker{}.or_default(fallback)
	assert value == none
}

fn test_assign_generic_method_option_result_to_existing_optional() {
	fallback := ?int(none)
	mut value := ?int(1)
	value = OptionPicker{}.or_default(fallback)
	assert value == none
}

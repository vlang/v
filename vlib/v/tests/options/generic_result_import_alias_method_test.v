import result_method_module as aliased

fn generic_result_value[T](value T) !T {
	return value
}

fn test_method_call_on_unwrapped_generic_result_with_import_alias() {
	assert generic_result_value(aliased.Value{
		text: 'resolved method'
	})!.str() == 'resolved method'
}

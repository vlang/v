type Job = fn ()

fn compact(values []?Job) int {
	return values.len
}

fn combine(values ...?Job) int {
	return compact(values)
}

fn test_variadic_optional_fn_is_an_array_of_options() {
	assert combine() == 0
}

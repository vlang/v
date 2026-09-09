type Transform = fn (string) string

fn take_transform(cb fn (Transform) string) string {
	return cb(to_upper)
}

fn make_transform_callback() fn (fn (string) string) string {
	return fn (cb fn (string) string) string {
		return cb('x')
	}
}

fn to_upper(s string) string {
	return s.to_upper()
}

fn test_same_function_signature_nested_fn() {
	assert take_transform(make_transform_callback()) == 'X'
}

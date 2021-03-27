fn test_error_can_be_converted_to_string() {
	assert 'Option{ error: "an error" }' == error('an error').str()
}

fn test_error_can_be_assigned_to_a_variable() {
	f := error('an error')
	assert 'Option{ error: "an error" }' == f.str()
}

fn test_error_can_be_printed() {
	f := error('an error')
	println(f)
	assert true
}

fn test_error_can_be_interpolated_in_a_string() {
	f := error('an error')
	s := 'hi $f'
	assert s == 'hi Option{ error: "an error" }'
}

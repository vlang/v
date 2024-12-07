struct Struct {
	error ?string
}

fn result_function() !Struct {
	return error('This is an error')
}

fn test_main() {
	some_struct := result_function() or {
		Struct{
			error: err
		}
	}
	// some_struct.error?
	assert some_struct.error or { '${err}' } == 'This is an error'
}

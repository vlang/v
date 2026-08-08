struct Box {}

type Arr = [2]Box

fn empty_ident_to_result(a Arr) !Arr {
	return a
}

fn empty_ident_to_option(a Arr) ?Arr {
	return a
}

fn test_return_ident_fixed_array_of_empty_struct_as_result() {
	a := Arr{}
	b := empty_ident_to_result(a) or { panic(err) }
	assert b.len == 2
}

fn test_return_ident_fixed_array_of_empty_struct_as_option() {
	a := Arr{}
	b := empty_ident_to_option(a) or { panic('unexpected none') }
	assert b.len == 2
}

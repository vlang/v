module main

struct Issue21499Wrapper {
	a int
}

fn issue21499_wrap[T](a &T) Issue21499Wrapper {
	$if T is int {
		return Issue21499Wrapper{a}
	}
	return Issue21499Wrapper{}
}

fn test_generic_ref_arg_struct_init_is_auto_dereferenced() {
	a := 123
	assert issue21499_wrap(a).a == 123
}

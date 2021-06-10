struct Struct {
	f1 fn () [required]
	f2 fn () [attr1; required]
}

fn func() {
}

fn test_struct_fields_storing_required_functions() {
	s := Struct{
		f1: func
		f2: func
	}

	assert s.f1 == func
	assert s.f2 == func
}

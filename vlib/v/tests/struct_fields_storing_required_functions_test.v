struct Struct {
	f fn () [required]
}

fn func() {
}

fn test_struct_fields_storing_required_functions() {
	s := Struct{
		f: func
	}

	assert s.f == func
}

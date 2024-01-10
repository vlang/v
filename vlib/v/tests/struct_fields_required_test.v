struct Fns {
	f1 fn () @[required]
	f2 fn () @[attr1; required]
}

fn func() {
}

fn test_struct_fields_storing_required_functions() {
	s := Fns{
		f1: func
		f2: func
	}

	assert s.f1 == func
	assert s.f2 == func
}

struct Data {
	v1 int @[required]
	v2 int @[required]
}

fn test_required_fields() {
	data := Data{1, 2}
	assert data.v1 == 1
	data2 := Data{
		...data
		v1: 10
	}
	assert data.v2 == data2.v2
}

struct DB {
	name string
}

struct DxB {
	name string
}

struct DeclExprA {
	name string
}

struct AStructWithAVeryLongName {
	name string
}

fn test_struct_names_can_be_used_for_creating_them() {
	a := DB{}
	println(a)
	assert true
	b := DxB{}
	println(b)
	assert true
	c := DeclExprA{}
	println(c)
	assert true
	d := AStructWithAVeryLongName{}
	println(d)
	assert true
}

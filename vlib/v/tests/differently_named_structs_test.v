
struct DB { name string }
struct DxB { name string }
struct DeclExprA { name string }
struct AStructWithAVeryLongName { name string }

fn test_struct_names_can_be_used_for_creating_them() {
	a := DB{}
	assert true
	b := DxB{}
	assert true
	c := DeclExprA{}
	assert true
	d := AStructWithAVeryLongName{}
	assert true
}

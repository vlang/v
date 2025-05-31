type AnonStruct = struct {
	a int
	b int
	c int
}

struct Abc {
	AnonStruct
}

fn test_anon_struct_type_alias_decl_embed() {
	abc := Abc{AnonStruct(struct {10, 20, 30})}
	assert abc.a == 10
	assert abc.b == 20
	assert abc.c == 30

	assert abc.AnonStruct.a == 10
	assert abc.AnonStruct.b == 20
	assert abc.AnonStruct.c == 30
}

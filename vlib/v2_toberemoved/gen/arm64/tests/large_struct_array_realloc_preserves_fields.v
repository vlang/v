module main

enum TypeKindShape {
	void_t
	int_t
	float_t
	ptr_t
	array_t
	struct_t
	func_t
	label_t
	metadata_t
}

struct TypeShape {
	kind        TypeKindShape
	width       int
	elem_type   int
	len         int
	fields      []int
	field_names []string
	params      []int
	ret_type    int
	is_c_struct bool
	is_union    bool
	is_unsigned bool
}

fn main() {
	mut types := []TypeShape{}
	for i := 0; i < 400; i++ {
		types << TypeShape{
			kind:      .array_t
			elem_type: 1
			len:       64 + i
		}
	}
	println(types[0].len)
	println(types[63].len)
	println(types[128].len)
	println(types[399].len)
}

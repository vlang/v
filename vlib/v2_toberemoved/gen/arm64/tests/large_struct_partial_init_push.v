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

struct TypeStoreShape {
mut:
	types []TypeShape
	cache map[string]int
}

fn (mut ts TypeStoreShape) register(t TypeShape) int {
	id := ts.types.len
	ts.types << t
	return id
}

fn (mut ts TypeStoreShape) get_array(elem int, length int) int {
	return ts.register(TypeShape{
		kind:      .array_t
		elem_type: elem
		len:       length
	})
}

fn main() {
	mut ts := TypeStoreShape{}
	ts.cache = map[string]int{}
	id := ts.get_array(1, 128)
	typ := ts.types[id]
	println(typ.len)
	println(typ.elem_type)
	println(int(typ.kind))
}

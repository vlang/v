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

struct ModuleShape {
mut:
	type_store TypeStoreShape
}

fn (mut ts TypeStoreShape) register(t TypeShape) int {
	id := ts.types.len
	ts.types << t
	return id
}

fn (mut ts TypeStoreShape) get_array(elem int, length int) int {
	key := 'a${elem}_${length}'
	if id := ts.cache[key] {
		return id
	}
	id := ts.register(TypeShape{
		kind:      .array_t
		elem_type: elem
		len:       length
	})
	ts.cache[key] = id
	return id
}

fn main() {
	mut ts := TypeStoreShape{
		cache: map[string]int{}
	}
	for i := 0; i < 400; i++ {
		ts.get_array(1, 64 + i)
	}
	src := ModuleShape{
		type_store: ts
	}
	dst := ModuleShape{
		type_store: src.type_store
	}
	println(dst.type_store.types[0].len)
	println(dst.type_store.types[63].len)
	println(dst.type_store.types[128].len)
	println(dst.type_store.types[399].len)
}

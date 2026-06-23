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

struct TargetShape {
	ptr_size      int
	endian_little bool
}

struct ModuleShape {
mut:
	name       string
	target     TargetShape
	ssa_mod    &int = unsafe { nil }
	env        &int = unsafe { nil }
	type_store TypeStoreShape
	values     []int
	instrs     []int
	blocks     []int
	funcs      []int
	globals    []int
}

fn lower(src &ModuleShape) ModuleShape {
	return ModuleShape{
		name:       src.name
		target:     src.target
		ssa_mod:    src.ssa_mod
		env:        src.env
		type_store: src.type_store
		values:     src.values.clone()
		instrs:     src.instrs.clone()
		blocks:     src.blocks.clone()
		funcs:      src.funcs.clone()
		globals:    src.globals.clone()
	}
}

fn fixed_array_size(m &ModuleShape, typ_id int) int {
	typ := m.type_store.types[typ_id]
	if typ.kind == .array_t {
		return typ.len * 4
	}
	return 0
}

fn main() {
	mut src := ModuleShape{}
	src.name = 'main'
	src.target = TargetShape{
		ptr_size:      8
		endian_little: true
	}
	src.type_store.cache = map[string]int{}
	src.type_store.types = [
		TypeShape{
			kind:      .array_t
			elem_type: 1
			len:       128
			fields:    []int{}
			params:    []int{}
		},
	]
	src.values = [1, 2, 3]
	src.instrs = [4, 5]
	src.blocks = [6]
	src.funcs = [7]
	src.globals = [8]
	src.type_store.cache['x'] = 0
	dst := lower(&src)
	println(fixed_array_size(&dst, 0))
}

module main

struct TypeShape {
	kind        int
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

fn fixed_array_size(types []TypeShape, typ_id int) int {
	typ := types[typ_id]
	if typ.kind == 4 {
		return typ.len * 4
	}
	return 0
}

fn main() {
	types := [
		TypeShape{
			kind:      4
			elem_type: 1
			len:       128
			fields:    []int{}
			params:    []int{}
		},
	]
	println(fixed_array_size(types, 0))
}

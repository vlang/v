type Mat4 = [16]f32

struct GameObject {
mut:
	transform Mat4
	children  []&GameObject
}

fn test_array_map_to_fixed_array() {
	mut v1 := &GameObject{}
	eprintln('children: ${v1.children.map(it.transform)}')
	assert '${v1.children.map(it.transform)}' == '[]'
}

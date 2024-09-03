struct Instr {
mut:
	a int
	b int
}

fn test_map_complex_array() {
	mut map1 := map[string][]&Instr{}
	instr := &Instr{
		a: 1
		b: 2
	}
	arr := [instr]
	map1['Hello'] = arr
	map1['Hello'][0].a = 2
	println(map1['Hello'][0].a)
	assert map1['Hello'][0].a == 2
	assert map1['Hello'][0].b == 2
}

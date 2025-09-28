struct MyObject {
mut:
	x int
	s string
}

struct Abc {
mut:
	mymap map[string]MyObject
}

fn test_codegen_for_unsafe_pointers_to_map_values() {
	mut inst := Abc{}
	inst.mymap['abc'] = MyObject{123, 'abc'}
	inst.mymap['def'] = MyObject{456, 'def'}
	dump(inst)

	mut p1 := unsafe { &inst.mymap['abc'] or { nil } }
	p2 := unsafe { &inst.mymap['def'] or { nil } }
	p3 := unsafe { &inst.mymap['zzz_unknown'] or { nil } }
	assert typeof(p1).name == '&MyObject'
	assert typeof(p2).name == '&MyObject'
	assert typeof(p3).name == '&MyObject'
	dump(p1)
	dump(p2)
	dump(p3)
	assert p1 != unsafe { nil }
	assert p2 != unsafe { nil }
	assert p3 == unsafe { nil }
	assert p1.x == 123
	assert p1.s == 'abc'
	assert p2.x == 456
	assert p2.s == 'def'
	// check that `p1` points to the exact same instance that is stored in the map:
	assert inst.mymap['abc'].x == 123
	assert inst.mymap['abc'].s == 'abc'
	p1.x = 999
	p1.s = 'xyz'
	assert inst.mymap['abc'].x == 999
	assert inst.mymap['abc'].s == 'xyz'
}

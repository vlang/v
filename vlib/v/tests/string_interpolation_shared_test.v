struct Abc {
mut:
	x f64
}

struct Qwe {
	s shared Abc
}

fn test_shared_struct_interpolation() {
	shared s := Abc{ x: 6.25 }
	astr := rlock s {
		'$s'
	}
	assert astr.starts_with('Abc{')
	assert astr.contains('x: 6.25')
	assert astr.ends_with('}')
}

fn test_shared_array_interpolation() {
	shared a := [0.25, -6.125, 12.5]
	astr := rlock a {
		'$a'
	}
	assert astr == '[0.25, -6.125, 12.5]'
}

fn test_shared_map_interpolation() {
	shared m := {'xy': 12.125, 'qwe': -6.0625, 'foo': 0.5}
	mstr := rlock m {
		'$m'
	}
	assert mstr == "{'xy': 12.125, 'qwe': -6.0625, 'foo': 0.5}"
}	

fn test_print_shared() {
	shared s := Abc{ x: 6.25 }
	shared a := [0.25, -6.125, 12.5]
	shared m := {'xy': 12.125, 'qwe': -6.0625, 'foo': 0.5}
	rlock a, m, s {
		println(s)
		println(a)
		println(m)
	}
}


struct Foo {
mut:
	a int
}

fn Foo.new_non_ref() Foo {
	return Foo{}
}

fn Foo.new_ref() &Foo {
	return &Foo{}
}

fn get_map_non_ref() map[int]int {
	return {
		0: 0
	}
}

fn get_map_ref() &map[int]int {
	return &{
		0: 0
	}
}

fn get_array() []int {
	return [0]
}

fn test_assign_from_call_expr() {
	shared foo1 := Foo.new_non_ref()
	rlock foo1 {
		assert foo1.a == 0
	}

	shared foo2 := Foo.new_ref()
	rlock foo2 {
		assert foo2.a == 0
	}

	shared map1 := get_map_non_ref()
	rlock map1 {
		assert map1[0] == 0
	}

	shared map2 := get_map_ref()
	rlock map2 {
		assert map2[0] == 0
	}

	shared arr := get_array()
	rlock arr {
		assert arr[0] == 0
	}
}

fn test_re_assign_array() {
	shared arr := [1, 2, 3]
	lock arr {
		arr[0] = 0
		assert arr == [0, 2, 3]
		arr = [0, 0, 0]
		assert arr == [0, 0, 0]
	}
}

fn test_re_assign_struct() {
	shared st := Foo{}
	lock st {
		st.a = 1
		assert st.a == 1
		st = Foo{2}
		assert st.a == 2
	}
}

fn test_re_assign_map() {
	shared m := map[int]int{}
	lock m {
		m[0] = 0
		assert m[0] == 0
		m = {
			0: 1
		}
		assert m[0] == 1
	}
}

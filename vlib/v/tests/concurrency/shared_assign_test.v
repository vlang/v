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

fn get_int_array(arr [3]int) []int {
	return [arr[0], arr[1], arr[2]]
}

fn get_foo_array(arr [3]Foo) []Foo {
	return [arr[0], arr[1], arr[2]]
}

fn test_assign_from_call_expr_with_fixed_array() {
	shared arr_int := get_int_array([1, 2, 3]!)
	lock arr_int {
		assert arr_int == [1, 2, 3]
	}

	shared arr_foo := get_foo_array([Foo.new_non_ref(), Foo.new_non_ref(),
		Foo.new_non_ref()]!)
	lock arr_foo {
		assert arr_foo == [Foo.new_non_ref(), Foo.new_non_ref(),
			Foo.new_non_ref()]
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

fn test_re_assign_array() {
	shared arr := [1, 2, 3]
	lock arr {
		arr[0] = 0
		assert arr == [0, 2, 3]
		arr = [0, 0, 0]
		assert arr == [0, 0, 0]
	}
}

struct Foo {
mut:
	a int
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

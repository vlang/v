interface Number2 {}

fn t(v Number2) {
	match v {
		int, i32, isize, i64 {
			x := isize(v)
			println(x)
			assert x == 42
		}
		else {}
	}
	match v {
		int {
			x := isize(v)
			println(x)
			assert x == 42
		}
		else {}
	}
}

fn test_cast_interface_value_in_match() {
	t(int(42))
}

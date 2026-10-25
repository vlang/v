interface Number2 {}

fn t(v Number2) {
	match v {
		int, i32, int, i64 {
			x := int(v)
			println(x)
			assert x == 42
		}
		else {}
	}

	match v {
		int {
			x := int(v)
			println(x)
			assert x == 42
		}
		else {}
	}
}

fn test_cast_interface_value_in_match() {
	t(int(42))
}

const numbers = {
	'one': fn () int {
		return 1
	}
}

fn test_map_get_anon_fn_value() {
	num1 := numbers['one'] or {
		fn () int {
			return 2
		}
	}
	ret1 := num1()
	println(ret1)
	assert ret1 == 1

	num2 := numbers['two'] or {
		fn () int {
			return 2
		}
	}
	ret2 := num2()
	println(ret2)
	assert ret2 == 2
}

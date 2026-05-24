@[strict_map_index]
module main

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

fn test_map_get_anon_fn_value_if_guard() {
	values := {
		'one': 1
	}
	mut found := false
	if value := values['one'] {
		found = true
		assert value == 1
	}
	assert found
	mut missing_found := false
	if _ := values['two'] {
		missing_found = true
	}
	assert !missing_found
}

fn foo1(mut arr []int) {
	for _, mut j in arr {
		j *= 2
	}
}

fn test_for_in_mut_val_of_array() {
	mut arr := [1, 2, 3]
	foo1(mut arr)
	println(arr)
	assert arr == [2, 4, 6]
}

fn foo2(mut arr [3]int) {
	for _, mut j in arr {
		j *= 2
	}
}

fn test_for_in_mut_val_of_fixed_array() {
	mut arr := [1, 2, 3]!
	foo2(mut arr)
	println(arr)
	assert arr == [2, 4, 6]!
}

fn foo3(mut m map[string][3]int) {
	for i in 0 .. m['hello'].len {
		m['hello'][i] *= 2
	}
}

fn test_fn_mut_val_of_map() {
	mut m := {
		'hello': [1, 2, 3]!
	}
	foo3(mut m)
	println(m)
	assert '${m}' == "{'hello': [2, 4, 6]}"
}

fn foo4(mut m map[string][3]int) {
	for _, mut j in m['hello'] {
		j *= 2
	}
}

fn test_for_in_mut_val_of_map() {
	mut m := {
		'hello': [1, 2, 3]!
	}
	foo4(mut m)
	println(m)
	assert '${m}' == "{'hello': [2, 4, 6]}"
}

fn test_for_in_mut_val_of_map_direct() {
	mut m := {
		'foo': 1
		'bar': 2
	}
	for _, mut j in m {
		j = 3
	}
	println(m)
	assert '${m}' == "{'foo': 3, 'bar': 3}"
}

fn test_for_in_mut_val_of_map_fixed_array() {
	mut m := {
		'foo': [{
			'a': 1
		}]!
		'bar': [{
			'b': 2
		}]!
	}
	for _, mut j in m {
		j = [{
			'c': 3
		}]!
	}
	println(m)
	assert '${m}' == "{'foo': [{'c': 3}], 'bar': [{'c': 3}]}"
}

fn test_for_in_mut_val_of_string() {
	b := 'c'
	mut c := ['a', 'b']
	mut ret := []string{}
	for mut a in c {
		a = a + b
		ret << a
	}
	println(ret)
	assert ret == ['ac', 'bc']
}

fn test_for_in_mut_val_of_float() {
	mut values := [1.0, 2, 3]
	println(values)

	for mut v in values {
		v = 1.0
		v = v + 1.0
	}
	println(values)
	assert values == [2.0, 2, 2]
}

enum Colors {
	red green blue yellow
}

fn test_in_expression() {
	mut a := false
	arr1 := [1, 2]
	arr2 := [0, 2]
	arr3 := [1, 0]

	a = true && 2 in arr1
	assert a == true
	a = false && 2 in arr1
	assert a == false

	a = true && 0 in arr2
	assert a == true
	a = false && 0 in arr3
	assert a == false
	a = true && 0 in arr1
	assert a == false
	a = true && 3 in arr1
	assert a == false
	a = true && 2 !in arr2
	assert a == false
	a = true && 3 !in arr2
	assert a == true

	a = true && 2 !in arr2
	assert a == false
	a = true && 3 !in arr2
	assert a == true

	a = 1 in arr1 && true
	assert a == true
	a = 1 in arr1 && false
	assert a == false
}

/*
not implemented
fn test_in_expression_with_enum() {
	mut a := false
	arr1 := [Colors.green, .blue]
	arr2 := [Colors.red, .blue]
	arr3 := [Colors.green, .red]
	a = true && Colors.blue in arr1
	assert a == true
	a = false && Colors.blue in arr1
	assert a == false
	a = true && Colors.red in arr2
	assert a == true
	a = false && Colors.red in arr3
	assert a == false

	a = true && Colors.red in arr1
	assert a == false
	a = true && Colors.yellow in arr1
	assert a == false

	a = true && !(Colors.blue in arr2)
	assert a == false
	a = true && !(Colors.yellow in arr2)
	assert a == true

	a = Colors.green in arr1 && true
	assert a == true
	a = Colors.green in arr1 && false
	assert a == false
}
*/
fn test_in_expression_with_string() {
	mut a := false
	arr1 := ['ab', 'bc']
	arr2 := ['', 'bc']
	arr3 := ['ab', '']

	a = true && 'bc' in arr1
	assert a == true
	a = false && 'bc' in arr1
	assert a == false

	a = true && '' in arr2
	assert a == true
	a = false && '' in arr3
	assert a == false
	a = true && '' in arr1
	assert a == false
	a = true && 'abc' in arr1
	assert a == false
	a = true && 'bc' !in arr2
	assert a == false
	a = true && 'abc' !in arr2
	assert a == true

	a = true && 'bc' !in arr2
	assert a == false
	a = true && 'abc' !in arr2
	assert a == true

	a = 'ab' in arr1 && true
	assert a == true
	a = 'ab' in arr1 && false
	assert a == false
}

fn test_in_expression_in_map() {
	m := {
		'one': 1
		'two': 2
		'three': 3
	}
	assert 'one' in m
	assert 'four' !in m
}

fn test_in_expression_in_string() {
	s := 'abcd'
	assert 'a' in s
	assert 'ab' in s
	assert 'abcd' in s
	assert 'dbca' !in s
}

fn test_optimized_in_expression() {
	mut a := false
	a = true && 2 in [1, 2]
	assert a == true
	a = false && 2 in [1, 2]
	assert a == false

	a = true && 0 in [0, 2]
	assert a == true
	a = false && 0 in [1, 0]
	assert a == false
	a = true && 0 in [1, 2]
	assert a == false
	a = true && 3 in [1, 2]
	assert a == false
	a = true && 2 !in [0, 2]
	assert a == false
	a = true && 3 !in [0, 2]
	assert a == true

	a = true && 2 !in [0, 2]
	assert a == false
	a = true && 3 !in [0, 2]
	assert a == true

	a = 1 in [1, 2] && true
	assert a == true
	a = 1 in [1, 2] && false
	assert a == false
}

fn test_optimized_in_expression_with_enum() {
	mut a := false
	a = true && Colors.blue in [.green, .blue]
	assert a == true
	a = false && Colors.blue in [.green, .blue]
	assert a == false

	a = true && Colors.red in [.red, .blue]
	assert a == true
	a = false && Colors.red in [.green, .red]
	assert a == false
	a = true && Colors.red in [.green, .blue]
	assert a == false
	a = true && Colors.yellow in [.green, .blue]
	assert a == false
	a = true && Colors.blue !in [.red, .blue]
	assert a == false
	a = true && Colors.yellow !in [.red, .blue]
	assert a == true

	a = true && Colors.blue !in [.red, .blue]
	assert a == false
	a = true && Colors.yellow !in [.red, .blue]
	assert a == true

	a = Colors.green in [.green, .blue] && true
	assert a == true
	a = Colors.green in [.green, .blue] && false
	assert a == false
}

fn test_optimized_in_expression_with_string() {
	mut a := false
	a = true && 'bc' in ['ab', 'bc']
	assert a == true
	a = false && 'bc' in ['ab', 'bc']
	assert a == false

	a = true && '' in ['', 'bc']
	assert a == true
	a = false && '' in ['ab', '']
	assert a == false
	a = true && '' in ['ab', 'bc']
	assert a == false
	a = true && 'abc' in ['ab', 'bc']
	assert a == false
	a = true && 'bc' !in ['', 'bc']
	assert a == false
	a = true && 'abc' !in ['', 'bc']
	assert a == true

	a = true && 'bc' !in ['', 'bc']
	assert a == false
	a = true && 'abc' !in ['', 'bc']
	assert a == true

	a = 'ab' in ['ab', 'bc'] && true
	assert a == true
	a = 'ab' in ['ab', 'bc'] && false
	assert a == false
}

fn test_in_array_init() {
	assert 1 !in []int{}
	assert [1] in [[1], [2]]
}

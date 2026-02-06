fn test_struct_field_init_with_fixed_array_init() {
	mut a := [5]int{init: [1, 2][index] or { 0 }}
	struct ArrayTest {
		aa [5]int
	}

	array_a := ArrayTest{
		aa: a
	}
	println('array_a: ${array_a}')
	assert array_a.aa == [1, 2, 0, 0, 0]!

	array_b := ArrayTest{
		aa: [5]int{init: [1, 2][index] or { 0 }}
	}
	println('array_b: ${array_b}')
	assert array_b.aa == [1, 2, 0, 0, 0]!
}

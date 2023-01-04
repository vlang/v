const numbers = [
	fn () int {
		return 1
	},
	fn () int {
		return 2
	},
]

fn test_array_get_anon_fn_value() {
	num1 := numbers[0]
	ret1 := num1()
	println(ret1)
	assert ret1 == 1

	num2 := numbers[1]
	ret2 := num2()
	println(ret2)
	assert ret2 == 2
}

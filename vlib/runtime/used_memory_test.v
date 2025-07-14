import runtime

fn test_used_memory() {
	used1 := runtime.used_memory()!
	println('used memory : ${used1}')
	assert used1 > 0

	_ := []int{len: 100000}

	used2 := runtime.used_memory()!
	println('used memory : ${used2}')
	assert used2 > used1

	_ := []int{len: 400000}

	used3 := runtime.used_memory()!
	println('used memory : ${used3}')
	assert used3 > used2
}

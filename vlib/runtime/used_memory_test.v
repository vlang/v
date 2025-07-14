import runtime

fn test_used_memory() {
	used1 := runtime.used_memory()!
	println('used memory 1 : ${used1}')

	mut mem1 := unsafe { malloc(4096 * 1024) }
	unsafe { vmemset(mem1, 1, 4096 * 1024) }

	used2 := runtime.used_memory()!
	println('used memory 2 : ${used2}')

	mut mem2 := unsafe { malloc(8192 * 1024) }
	unsafe { vmemset(mem2, 1, 8192 * 1024) }

	used3 := runtime.used_memory()!
	println('used memory 3 : ${used3}')
	assert used1 > 0
	assert used2 > used1
	assert used3 > used2
}

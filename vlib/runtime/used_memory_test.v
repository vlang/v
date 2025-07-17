import runtime

fn test_used_memory() {
	used1 := runtime.used_memory()!
	println('used memory 1 : ${used1}')

	mut mem1 := unsafe { malloc(8 * 1024 * 1024) }
	unsafe { vmemset(mem1, 1, 8 * 1024 * 1024) }
	used2 := runtime.used_memory()!
	println('used memory 2 : ${used2}')

	mut mem2 := unsafe { malloc(64 * 1024 * 1024) }
	unsafe { vmemset(mem2, 1, 64 * 1024 * 1024) }
	used3 := runtime.used_memory()!
	println('used memory 3 : ${used3}')

	assert used1 > 0
	assert used2 >= used1
	assert used3 > used2
	unsafe {
		println(*&u8(mem1 + 1024))
		println(*&u8(mem2 + 1024))
	}
}

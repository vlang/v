fn test_inline_asm() {
	$if !windows {
	$if !tinyc {
	a := 10
	b := 0
	unsafe {
		asm (
			"movl %1, %%eax;"
			"movl %%eax, %0;"
			:"=r"(b)
			:"r"(a)
			:"%eax"
		)
	}
	assert a == 10
	assert b == 10
	//
	e := 0
	unsafe {
		asm(
			"movl $5, %0"
			:"=a"(e)
		)
	}
	assert e == 5
	}
	}
}

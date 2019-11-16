fn test_inline_asm() {
	a := 10
	b := 0
	unsafe {
		asm ("movl %1, %%eax;"
      "movl %%eax, %0;"
     :"=r"(b)
     :"r"(a)
     :"%eax"
     )
	}
	assert a == 10
	assert b == 10
}

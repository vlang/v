fn test_inline_asm() {
	a := 10
	b := 0
	unsafe {
		asm x64 {
			'mov eax, %1'
			'mov %0,eax'
			: "=r" (b)
			: "r" (a)
			: '%eax'}
	}
	assert a == 10
	assert b == 10
	e := 0
	unsafe {
		asm x64 {
			'mov %0, 5'
			: "=a" (e)
		}
	}
	assert e == 5
	
	f := 10
	g := 2
	h := 0
	unsafe {
		asm x64 {
			'mov %[h], %[f]'
			'add %[h], %[g]'
			'add %[h], 5'
			: [h] "=r" (h)
				// output 
			: [f] "r" (f),
			  [g] "r" (g)
				// input 
		}
	}
	assert f == 10
	assert g == 2
	assert h == 17
}

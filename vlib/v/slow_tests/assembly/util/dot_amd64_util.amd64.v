module util

// add sums the supplied 32-bit integers with inline assembly.
pub fn add(a ...i32) i32 {
	mut res := i32(0)
	asm amd64 {
		1:
		addq rax, [in_data + rcx * 4 + 0]
		loop b1
		addq rax, [in_data + rcx * 4 + 0]
		; +a (res)
		; c (a.len - 1) // c is counter (loop) register
		  r (a.data) as in_data
	}
	return res
}

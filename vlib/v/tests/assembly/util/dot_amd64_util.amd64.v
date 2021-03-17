module util

pub fn add(a ...int) int {
	mut res := 0
	asm amd64 {
		loop_start3:
		addq rax, [in_data + rcx * 4 + 0]
		loop loop_start3
		addq rax, [in_data + rcx * 4 + 0]
		; +a (res)
		; c (a.len - 1) // c is counter (loop) register
		  r (a.data) as in_data
	}
	return res
}

@[_naked]
fn naked_fn() {
	asm amd64 {
		push rbp
		mov rbp, rsp
		mov rsp, rbp
		pop rbp
		ret
	}
}

fn test_naked_attr() {
	naked_fn()
}

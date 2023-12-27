@[_naked]
fn naked_fn() {
	asm i386 {
		push ebp
		mov ebp, esp
		mov esp, ebp
		pop ebp
		ret
	}
}

fn test_naked_attr() {
	naked_fn()
}

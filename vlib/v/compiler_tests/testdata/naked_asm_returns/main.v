@[_naked]
fn raw_literal() i32 {
	asm volatile amd64 {
		mov eax, 42
		ret
	}
}

@[_naked]
fn raw_pointer(_value voidptr) voidptr {
	asm volatile amd64 {
		mov rax, rdi
		ret
	}
}

// Six register arguments and two caller-stack arguments must arrive unchanged.
@[_naked]
fn raw_sum8(_a u64, _b u64, _c u64, _d u64, _e u64, _f u64, _g u64, _h u64) u64 {
	asm volatile amd64 {
		mov rax, rdi
		add rax, rsi
		add rax, rdx
		add rax, rcx
		add rax, r8
		add rax, r9
		add rax, [rsp + 8]
		add rax, [rsp + 16]
		ret
	}
}

@[_naked]
fn raw_generic[T](_value &T) &T {
	asm volatile amd64 {
		mov rax, rdi
		ret
	}
}

fn main() {
	if raw_literal() != 42 { exit(1) }
	value := u64(901)
	if raw_pointer(&value) != &value { exit(2) }
	if raw_sum8(1, 2, 4, 8, 16, 32, 64, 128) != 255 { exit(3) }
	if raw_generic(&value) != &value || unsafe { *raw_generic(&value) } != 901 { exit(4) }
	println('naked ABI returns passed')
}

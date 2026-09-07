// vtest build: !windows && !msvc
// vtest vflags: -cc clang -no-retry-compilation

fn test_intel_extended_register_operands_with_clang() {
	increment := i64(23)
	mut result := i64(19)
	asm amd64 intel {
		add result, increment
		; +r (result)
		; r (increment)
		; cc
	}
	assert result == 42
}

fn test_intel_mixed_width_hard_registers_with_clang() {
	mut shifted := i64(21)
	mut extended := i64(0)
	asm amd64 intel {
		mov cl, 1
		shl shifted, cl
		mov al, 42
		movzx extended, al
		; +r (shifted)
		  =r (extended)
		; ; rax
		  rcx
	}
	assert shifted == 42
	assert extended == 42
}

fn intel_generic_add[T](value T, increment T) T {
	mut result := value
	asm amd64 intel {
		add result, increment
		; +r (result)
		; r (increment)
	}
	return result
}

fn test_intel_generic_native_width_operands_with_clang() {
	assert intel_generic_add[i64](19, 23) == 42
}

fn test_intel_segment_register_move_with_clang() {
	mut value := i64(0)
	asm amd64 intel {
		mov value, ds
		mov ds, value
		; +r (value)
	}
	assert value >= 0
}

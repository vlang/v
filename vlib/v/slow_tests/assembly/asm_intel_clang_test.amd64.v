// vtest build: !windows && !msvc
// vtest vflags: -cc clang -no-retry-compilation

enum IntelWide as u64 {
	value = 42
}

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
	mut double_shifted := i64(21)
	shift_source := i64(-1)
	mut extended := i64(0)
	asm amd64 intel {
		mov cl, 1
		shl shifted, cl
		shld double_shifted, shift_source, cl
		mov al, 42
		movzx extended, al
		; +r (shifted)
		  +r (double_shifted)
		  =r (extended)
		; r (shift_source)
		; rax
		  rcx
	}
	assert shifted == 42
	assert double_shifted == 43
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

fn test_intel_wide_enum_operand_with_clang() {
	input := IntelWide.value
	mut result := u64(0)
	asm amd64 intel {
		mov result, input
		; =r (result)
		; r (input)
	}
	assert result == 42
}

fn test_intel_decimal_int_min_literal_with_clang() {
	mut result := i64(0)
	asm amd64 intel {
		mov result, value
		; =r (result)
		; r (-2147483648) as value
	}
	assert result == -2147483648
}

fn test_intel_movq_vector_transfer_with_clang() {
	input := i64(42)
	mut result := i64(0)
	asm amd64 intel {
		movq xmm0, input
		movq result, xmm0
		; =r (result)
		; r (input)
		; xmm0
	}
	assert result == 42
}

fn intel_crc32_narrow_source_with_clang(value i64) i64 {
	mut result := value
	asm amd64 intel {
		crc32 result, al
		; +r (result)
		; ; rax
	}
	return result
}

@[noinline]
fn can_run_intel_crc32_test() bool {
	return false
}

fn test_intel_crc32_narrow_source_compiles_with_clang() {
	if can_run_intel_crc32_test() {
		assert intel_crc32_narrow_source_with_clang(42) != 0
	}
}

fn test_intel_native_width_address_registers_with_clang() {
	value := i64(42)
	base := &value
	mut result := i64(0)
	asm amd64 intel {
		xor rax, rax
		mov result, [base + rax + 0]
		; =r (result)
		; r (base)
		; rax
	}
	assert result == 42
}

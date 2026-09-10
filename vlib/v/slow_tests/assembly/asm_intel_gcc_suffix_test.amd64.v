// vtest build: gcc && !macos && !windows
// vtest vflags: -cc gcc -no-retry-compilation

fn intel_sized_implicit_arithmetic_memory_with_gcc(base &u64) {
	asm amd64 intel {
		divq [base]
		idivq [base]
		mulq [base]
		imulq [base]
		; ; r (base)
		; rax
		  rdx
		  cc
	}
}

fn intel_crc32_byte_memory_source_with_gcc(base &u8, checksum u64) u64 {
	mut result := checksum
	asm amd64 intel {
		crc32b result, [base]
		; +r (result)
		; r (base)
	}
	return result
}

fn intel_crc32_native_memory_source_with_gcc(base &u64, checksum u64) u64 {
	mut result := checksum
	asm amd64 intel {
		crc32q result, [base]
		; +r (result)
		; r (base)
	}
	return result
}

fn intel_gnu_extension_move_memory_sources_with_gcc(byte_base &u8, word_base &i16,
	dword_base &i32) (u64, i64, i64) {
	mut zero_extended := u64(0)
	mut word_extended := i64(0)
	mut dword_extended := i64(0)
	asm amd64 intel {
		movzbq zero_extended, [byte_base]
		movswq word_extended, [word_base]
		movslq dword_extended, [dword_base]
		; =r (zero_extended)
		  =r (word_extended)
		  =r (dword_extended)
		; r (byte_base)
		  r (word_base)
		  r (dword_base)
	}
	return zero_extended, word_extended, dword_extended
}

@[noinline]
fn can_run_intel_gcc_suffix_test() bool {
	return false
}

fn test_intel_gnu_suffixes_compile_with_gcc() {
	if can_run_intel_gcc_suffix_test() {
		byte_value := u8(42)
		native_value := u64(42)
		word_value := i16(-1)
		dword_value := i32(-2)
		intel_sized_implicit_arithmetic_memory_with_gcc(&native_value)
		assert intel_crc32_byte_memory_source_with_gcc(&byte_value, 0) != 0
		assert intel_crc32_native_memory_source_with_gcc(&native_value, 0) != 0
		zero_extended, word_extended, dword_extended := intel_gnu_extension_move_memory_sources_with_gcc(&byte_value, &word_value, &dword_value)
		assert zero_extended == 42
		assert word_extended == -1
		assert dword_extended == -2
	}
}

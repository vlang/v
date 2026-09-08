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

@[noinline]
fn can_run_intel_gcc_suffix_test() bool {
	return false
}

fn test_intel_gnu_suffixes_compile_with_gcc() {
	if can_run_intel_gcc_suffix_test() {
		byte_value := u8(42)
		native_value := u64(42)
		intel_sized_implicit_arithmetic_memory_with_gcc(&native_value)
		assert intel_crc32_byte_memory_source_with_gcc(&byte_value, 0) != 0
		assert intel_crc32_native_memory_source_with_gcc(&native_value, 0) != 0
	}
}

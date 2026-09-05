// vtest build: !windows && !msvc
// vtest vflags: -cc clang -no-retry-compilation

fn test_intel_extended_register_operands_with_clang() {
	increment := 23
	mut result := 19
	asm amd64 intel {
		add result, increment
		; +r (result)
		; r (increment)
		; cc
	}
	assert result == 42
}

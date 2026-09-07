// vtest build: !windows && !msvc

fn test_intel_i386_accepts_32_bit_named_operands() {
	increment := 23
	mut result := 19
	asm i386 intel {
		add result, increment
		; +r (result)
		; r (increment)
		; cc
	}
	assert result == 42
	asm i386 intel {
		mov result, one
		; +r (result)
		; r (1) as one
	}
	assert result == 1
}

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

fn intel_i386_control_register_moves(input int) {
	mut value := input
	asm i386 intel {
		mov value, cr0
		mov cr0, value
		; +r (value)
	}
}

fn intel_i386_crc32_narrow_sources(input int) {
	mut result := input
	asm i386 intel {
		crc32 result, cl
		crc32 result, cx
		; +r (result)
		; ; ecx
	}
}

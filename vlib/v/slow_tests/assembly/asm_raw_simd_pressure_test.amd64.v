// vtest build: !msvc

// raw_simd_pressure deliberately lists xmm6-xmm15 as clobbers. Those registers are
// nonvolatile in the Win64 ABI, so GCC/Clang must preserve them around this block.
fn raw_simd_pressure(seed f32, offset f64) f64 {
	mut sum := f32(0)
	asm amd64 raw {
		"movss %[seed], %%xmm0\n\t"
		"movss %[seed], %%xmm1\n\t"
		"movss %[seed], %%xmm2\n\t"
		"movss %[seed], %%xmm3\n\t"
		"movss %[seed], %%xmm4\n\t"
		"movss %[seed], %%xmm5\n\t"
		"movss %[seed], %%xmm6\n\t"
		"movss %[seed], %%xmm7\n\t"
		"movss %[seed], %%xmm8\n\t"
		"movss %[seed], %%xmm9\n\t"
		"movss %[seed], %%xmm10\n\t"
		"movss %[seed], %%xmm11\n\t"
		"movss %[seed], %%xmm12\n\t"
		"movss %[seed], %%xmm13\n\t"
		"movss %[seed], %%xmm14\n\t"
		"movss %[seed], %%xmm15\n\t"
		"addss %%xmm0, %%xmm1\n\t"
		"addss %%xmm1, %%xmm2\n\t"
		"addss %%xmm2, %%xmm3\n\t"
		"addss %%xmm3, %%xmm4\n\t"
		"addss %%xmm4, %%xmm5\n\t"
		"addss %%xmm5, %%xmm6\n\t"
		"addss %%xmm6, %%xmm7\n\t"
		"addss %%xmm7, %%xmm8\n\t"
		"addss %%xmm8, %%xmm9\n\t"
		"addss %%xmm9, %%xmm10\n\t"
		"addss %%xmm10, %%xmm11\n\t"
		"addss %%xmm11, %%xmm12\n\t"
		"addss %%xmm12, %%xmm13\n\t"
		"addss %%xmm13, %%xmm14\n\t"
		"addss %%xmm14, %%xmm15\n\t"
		"movss %%xmm15, %[sum]"
		; [sum] "=m" (sum)
		; [seed] "m" (seed)
		; xmm0
		  xmm1
		  xmm2
		  xmm3
		  xmm4
		  xmm5
		  xmm6
		  xmm7
		  xmm8
		  xmm9
		  xmm10
		  xmm11
		  xmm12
		  xmm13
		  xmm14
		  xmm15
		  memory
	}
	left := offset + 1.5
	right := offset * 2.0
	return f64(sum) + left + right
}

fn raw_load_fixed_u32(input &[16]u8) u32 {
	mut result := u32(0)
	asm amd64 raw {
		"movl 0(%[input]), %[result]"
		; [result] "=r" (result)
		; [input] "r" (input)
		; memory
	}
	return result
}

fn raw_load_buffer_u32(input []u8) u32 {
	mut result := u32(0)
	asm amd64 raw {
		"movzbl %[input], %[result]"
		; [result] "=r" (result)
		; [input] "m" (input[0])
	}
	return result
}

fn test_raw_simd_pressure_and_operand_binding() {
	assert raw_simd_pressure(1.25, 2.5) == 29.0
	mut input := [16]u8{}
	input[0] = 0x78
	input[1] = 0x56
	input[2] = 0x34
	input[3] = 0x12
	assert raw_load_fixed_u32(&input) == u32(0x12345678)
	assert raw_load_buffer_u32(input[..]) == 0x78
}

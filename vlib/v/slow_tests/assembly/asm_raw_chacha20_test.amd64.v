// vtest build: !msvc

import encoding.binary
import encoding.hex

// raw_chacha20_block_sse2 keeps one ChaCha state row in each XMM register.
// The lane shuffles turn the column round into the diagonal round without scalar work.
fn raw_chacha20_block_sse2(state &[16]u32) [16]u32 {
	mut output := [16]u32{}
	// The raw block writes the fixed array through its first element.
	output_ptr := unsafe { &output[0] }
	asm amd64 raw {
		"movdqu 0(%[state]), %%xmm0\n\t"
		"movdqu 16(%[state]), %%xmm1\n\t"
		"movdqu 32(%[state]), %%xmm2\n\t"
		"movdqu 48(%[state]), %%xmm3\n\t"
		"movdqa %%xmm0, %%xmm8\n\t"
		"movdqa %%xmm1, %%xmm9\n\t"
		"movdqa %%xmm2, %%xmm10\n\t"
		"movdqa %%xmm3, %%xmm11\n\t"
		".rept 10\n\t"

		// Column quarter round.
		"paddd %%xmm1, %%xmm0\n\t"
		"pxor %%xmm0, %%xmm3\n\t"
		"movdqa %%xmm3, %%xmm4\n\t"
		"pslld $16, %%xmm3\n\t"
		"psrld $16, %%xmm4\n\t"
		"por %%xmm4, %%xmm3\n\t"
		"paddd %%xmm3, %%xmm2\n\t"
		"pxor %%xmm2, %%xmm1\n\t"
		"movdqa %%xmm1, %%xmm4\n\t"
		"pslld $12, %%xmm1\n\t"
		"psrld $20, %%xmm4\n\t"
		"por %%xmm4, %%xmm1\n\t"
		"paddd %%xmm1, %%xmm0\n\t"
		"pxor %%xmm0, %%xmm3\n\t"
		"movdqa %%xmm3, %%xmm4\n\t"
		"pslld $8, %%xmm3\n\t"
		"psrld $24, %%xmm4\n\t"
		"por %%xmm4, %%xmm3\n\t"
		"paddd %%xmm3, %%xmm2\n\t"
		"pxor %%xmm2, %%xmm1\n\t"
		"movdqa %%xmm1, %%xmm4\n\t"
		"pslld $7, %%xmm1\n\t"
		"psrld $25, %%xmm4\n\t"
		"por %%xmm4, %%xmm1\n\t"

		// Diagonal quarter round.
		"pshufd $0x39, %%xmm1, %%xmm1\n\t"
		"pshufd $0x4e, %%xmm2, %%xmm2\n\t"
		"pshufd $0x93, %%xmm3, %%xmm3\n\t"
		"paddd %%xmm1, %%xmm0\n\t"
		"pxor %%xmm0, %%xmm3\n\t"
		"movdqa %%xmm3, %%xmm4\n\t"
		"pslld $16, %%xmm3\n\t"
		"psrld $16, %%xmm4\n\t"
		"por %%xmm4, %%xmm3\n\t"
		"paddd %%xmm3, %%xmm2\n\t"
		"pxor %%xmm2, %%xmm1\n\t"
		"movdqa %%xmm1, %%xmm4\n\t"
		"pslld $12, %%xmm1\n\t"
		"psrld $20, %%xmm4\n\t	"
		"por %%xmm4, %%xmm1\n\t"
		"paddd %%xmm1, %%xmm0\n\t"
		"pxor %%xmm0, %%xmm3\n\t"
		"movdqa %%xmm3, %%xmm4\n\t"
		"pslld $8, %%xmm3\n\t"
		"psrld $24, %%xmm4\n\t"
		"por %%xmm4, %%xmm3\n\t"
		"paddd %%xmm3, %%xmm2\n\t"
		"pxor %%xmm2, %%xmm1\n\t"
		"movdqa %%xmm1, %%xmm4\n\t"
		"pslld $7, %%xmm1\n\t"
		"psrld $25, %%xmm4\n\t"
		"por %%xmm4, %%xmm1\n\t"
		"pshufd $0x93, %%xmm1, %%xmm1\n\t"
		"pshufd $0x4e, %%xmm2, %%xmm2\n\t"
		"pshufd $0x39, %%xmm3, %%xmm3\n\t"
		".endr\n\t"
		"paddd %%xmm8, %%xmm0\n\t"
		"paddd %%xmm9, %%xmm1\n\t"
		"paddd %%xmm10, %%xmm2\n\t"
		"paddd %%xmm11, %%xmm3\n\t"
		"movdqu %%xmm0, 0(%[output])\n\t"
		"movdqu %%xmm1, 16(%[output])\n\t"
		"movdqu %%xmm2, 32(%[output])\n\t"
		"movdqu %%xmm3, 48(%[output])"
		;
		; [state] "r" (state)
		  [output] "r" (output_ptr)
		; xmm0
		  xmm1
		  xmm2
		  xmm3
		  xmm4
		  xmm8
		  xmm9
		  xmm10
		  xmm11
		  memory
	}
	return output
}

fn chacha20_row_state() [16]u32 {
	mut state := [16]u32{}
	state[0] = 0x61707865
	state[1] = 0x3320646e
	state[2] = 0x79622d32
	state[3] = 0x6b206574
	state[4] = 0x03020100
	state[5] = 0x07060504
	state[6] = 0x0b0a0908
	state[7] = 0x0f0e0d0c
	state[8] = 0x13121110
	state[9] = 0x17161514
	state[10] = 0x1b1a1918
	state[11] = 0x1f1e1d1c
	state[12] = 1
	state[13] = 0x09000000
	state[14] = 0x4a000000
	state[15] = 0
	return state
}

fn chacha20_state_bytes(state [16]u32) []u8 {
	mut result := []u8{len: 64}
	for i, word in state {
		binary.little_endian_put_u32(mut result[i * 4..i * 4 + 4], word)
	}
	return result
}

fn test_raw_chacha20_simd_block_rfc8439() {
	state := chacha20_row_state()
	actual := chacha20_state_bytes(raw_chacha20_block_sse2(&state))
	expected := hex.decode('10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e')!
	assert actual == expected
}

// vtest build: !msvc

import encoding.hex

// raw_ghash_clmul_product returns the unreduced 256-bit polynomial product.
// GHASH reduction is kept in V below so this fixture checks the CLMUL product exactly.
fn raw_ghash_clmul_product(x &[2]u64, h &[2]u64) [4]u64 {
	mut output := [4]u64{}
	// The raw block writes the fixed array through its first element.
	output_ptr := unsafe { &output[0] }
	asm amd64 raw {
		"movdqu 0(%[x]), %%xmm0\n\t"
		"movdqu 0(%[h]), %%xmm1\n\t"
		"movdqa %%xmm0, %%xmm2\n\t"
		"pclmulqdq $0x00, %%xmm1, %%xmm2\n\t"
		"movdqa %%xmm0, %%xmm3\n\t"
		"pclmulqdq $0x10, %%xmm1, %%xmm3\n\t"
		"movdqa %%xmm1, %%xmm4\n\t"
		"pclmulqdq $0x10, %%xmm0, %%xmm4\n\t"
		"movdqa %%xmm0, %%xmm5\n\t"
		"pclmulqdq $0x11, %%xmm1, %%xmm5\n\t"
		"pxor %%xmm4, %%xmm3\n\t"
		"movdqa %%xmm3, %%xmm6\n\t"
		"pslldq $8, %%xmm6\n\t"
		"pxor %%xmm6, %%xmm2\n\t"
		"psrldq $8, %%xmm3\n\t"
		"pxor %%xmm3, %%xmm5\n\t"
		"movdqu %%xmm2, 0(%[output])\n\t"
		"movdqu %%xmm5, 16(%[output])"
		;
		; [x] "r" (x)
		  [h] "r" (h)
		  [output] "r" (output_ptr)
		; xmm0
		  xmm1
		  xmm2
		  xmm3
		  xmm4
		  xmm5
		  xmm6
		  memory
	}
	return output
}

fn ghash_clmul_product_reference(x [2]u64, h [2]u64) [4]u64 {
	mut product := [4]u64{}
	for x_word in 0 .. 2 {
		for x_bit in 0 .. 64 {
			if ((x[x_word] >> x_bit) & 1) == 0 {
				continue
			}
			x_position := x_word * 64 + x_bit
			for h_word in 0 .. 2 {
				for h_bit in 0 .. 64 {
					if ((h[h_word] >> h_bit) & 1) == 0 {
						continue
					}
					position := x_position + h_word * 64 + h_bit
					product[position / 64] ^= u64(1) << (position & 63)
				}
			}
		}
	}
	return product
}

fn ghash_reduce_product(mut product [4]u64) [2]u64 {
	mut position := 255
	for position >= 128 {
		word := position / 64
		bit := position & 63
		if ((product[word] >> bit) & 1) != 0 {
			product[word] &= ~(u64(1) << bit)
			for offset in [0, 1, 2, 7] {
				target := position - 128 + offset
				product[target / 64] ^= u64(1) << (target & 63)
			}
		}
		position--
	}
	mut result := [2]u64{}
	result[0] = product[0]
	result[1] = product[1]
	return result
}

fn reverse_bits8(value u8) u8 {
	mut reversed := u8(0)
	for bit in 0 .. 8 {
		reversed |= ((value >> bit) & 1) << (7 - bit)
	}
	return reversed
}

fn ghash_block_to_polynomial(block []u8) [2]u64 {
	mut result := [2]u64{}
	for i, value in block {
		position := i * 8
		reversed := reverse_bits8(value)
		if position < 64 {
			result[0] |= u64(reversed) << position
		} else {
			result[1] |= u64(reversed) << (position - 64)
		}
	}
	return result
}

fn polynomial_to_ghash_block(value [2]u64) []u8 {
	mut result := []u8{len: 16}
	for i in 0 .. 16 {
		position := i * 8
		word := if position < 64 { value[0] } else { value[1] }
		result[i] = reverse_bits8(u8(word >> (position & 63)))
	}
	return result
}

fn can_run_pclmul_test() bool {
	mut ecx := u32(0)
	asm amd64 {
		mov eax, 1
		cpuid
		; =c (ecx)
		; ; eax
		  ebx
		  edx
	}
	return (ecx & (u32(1) << 1)) != 0
}

fn test_raw_ghash_clmul_nist_vector() {
	if !can_run_pclmul_test() {
		return
	}
	x_bytes := hex.decode('0388dace60b6a392f328c2b971b2fe78')!
	h_bytes := hex.decode('66e94bd4ef8a2c3b884cfa59ca342b2e')!
	x := ghash_block_to_polynomial(x_bytes)
	h := ghash_block_to_polynomial(h_bytes)
	mut actual_product := raw_ghash_clmul_product(&x, &h)
	expected_product := ghash_clmul_product_reference(x, h)
	assert actual_product == expected_product
	expected := hex.decode('5e2ec746917062882c85b0685353deb7')!
	actual := polynomial_to_ghash_block(ghash_reduce_product(mut actual_product))
	assert actual == expected
}

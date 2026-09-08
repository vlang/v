import encoding.binary
import encoding.hex
import os

// raw_chacha20_block_neon keeps one ChaCha state row in each NEON register.
fn raw_chacha20_block_neon(state &[16]u32) [16]u32 {
	mut output := [16]u32{}
	// The raw block writes the fixed array through its first element.
	output_ptr := unsafe { &output[0] }
	asm arm64 raw {
		"ldr q0, [%[state]]\n\t"
		"ldr q1, [%[state], #16]\n\t"
		"ldr q2, [%[state], #32]\n\t"
		"ldr q3, [%[state], #48]\n\t"
		"mov v8.16b, v0.16b\n\t"
		"mov v9.16b, v1.16b\n\t"
		"mov v10.16b, v2.16b\n\t"
		"mov v11.16b, v3.16b\n\t"
		".rept 10\n\t"

		// Column quarter round.
		"add v0.4s, v0.4s, v1.4s\n\t"
		"eor v3.16b, v3.16b, v0.16b\n\t"
		"mov v4.16b, v3.16b\n\t"
		"shl v3.4s, v3.4s, #16\n\t"
		"ushr v4.4s, v4.4s, #16\n\t"
		"orr v3.16b, v3.16b, v4.16b\n\t"
		"add v2.4s, v2.4s, v3.4s\n\t"
		"eor v1.16b, v1.16b, v2.16b\n\t"
		"mov v4.16b, v1.16b\n\t	"
		"shl v1.4s, v1.4s, #12\n\t"
		"ushr v4.4s, v4.4s, #20\n\t"
		"orr v1.16b, v1.16b, v4.16b\n\t"
		"add v0.4s, v0.4s, v1.4s\n\t"
		"eor v3.16b, v3.16b, v0.16b\n\t"
		"mov v4.16b, v3.16b\n\t"
		"shl v3.4s, v3.4s, #8\n\t"
		"ushr v4.4s, v4.4s, #24\n\t"
		"orr v3.16b, v3.16b, v4.16b\n\t"
		"add v2.4s, v2.4s, v3.4s\n\t"
		"eor v1.16b, v1.16b, v2.16b\n\t"
		"mov v4.16b, v1.16b\n\t"
		"shl v1.4s, v1.4s, #7\n\t"
		"ushr v4.4s, v4.4s, #25\n\t"
		"orr v1.16b, v1.16b, v4.16b\n\t"

		// Diagonal quarter round.
		"ext v1.16b, v1.16b, v1.16b, #4\n\t"
		"ext v2.16b, v2.16b, v2.16b, #8\n\t"
		"ext v3.16b, v3.16b, v3.16b, #12\n\t"
		"add v0.4s, v0.4s, v1.4s\n\t"
		"eor v3.16b, v3.16b, v0.16b\n\t"
		"mov v4.16b, v3.16b\n\t"
		"shl v3.4s, v3.4s, #16\n\t"
		"ushr v4.4s, v4.4s, #16\n\t"
		"orr v3.16b, v3.16b, v4.16b\n\t"
		"add v2.4s, v2.4s, v3.4s\n\t"
		"eor v1.16b, v1.16b, v2.16b\n\t"
		"mov v4.16b, v1.16b\n\t"
		"shl v1.4s, v1.4s, #12\n\t"
		"ushr v4.4s, v4.4s, #20\n\t"
		"orr v1.16b, v1.16b, v4.16b\n\t"
		"add v0.4s, v0.4s, v1.4s\n\t"
		"eor v3.16b, v3.16b, v0.16b\n\t"
		"mov v4.16b, v3.16b\n\t"
		"shl v3.4s, v3.4s, #8\n\t"
		"ushr v4.4s, v4.4s, #24\n\t"
		"orr v3.16b, v3.16b, v4.16b\n\t"
		"add v2.4s, v2.4s, v3.4s\n\t"
		"eor v1.16b, v1.16b, v2.16b\n\t"
		"mov v4.16b, v1.16b\n\t"
		"shl v1.4s, v1.4s, #7\n\t"
		"ushr v4.4s, v4.4s, #25\n\t"
		"orr v1.16b, v1.16b, v4.16b\n\t"
		"ext v1.16b, v1.16b, v1.16b, #12\n\t"
		"ext v2.16b, v2.16b, v2.16b, #8\n\t"
		"ext v3.16b, v3.16b, v3.16b, #4\n\t"
		".endr\n\t"
		"add v0.4s, v0.4s, v8.4s\n\t"
		"add v1.4s, v1.4s, v9.4s\n\t"
		"add v2.4s, v2.4s, v10.4s\n\t"
		"add v3.4s, v3.4s, v11.4s\n\t"
		"str q0, [%[output]]\n\t"
		"str q1, [%[output], #16]\n\t"
		"str q2, [%[output], #32]\n\t"
		"str q3, [%[output], #48]"
		;
		; [state] "r" (state)
		  [output] "r" (output_ptr)
		; v0
		  v1
		  v2
		  v3
		  v4
		  v8
		  v9
		  v10
		  v11
		  memory
	}
	return output
}

fn raw_ghash_pmull_product(x &[2]u64, h &[2]u64) [4]u64 {
	mut output := [4]u64{}
	// The raw block writes the fixed array through its first element.
	output_ptr := unsafe { &output[0] }
	asm arm64 raw {
		"ldr q0, [%[x]]\n\t"
		"ldr q1, [%[h]]\n\t"
		"pmull v2.1q, v0.1d, v1.1d\n\t"
		"pmull2 v3.1q, v0.2d, v1.2d\n\t"
		"pmull v4.1q, v0.1d, v1.2d\n\t"
		"pmull v5.1q, v0.2d, v1.1d\n\t"
		"eor v4.16b, v4.16b, v5.16b\n\t"
		"movi v6.2d, #0\n\t"
		"movi v7.2d, #0\n\t"
		"ins v6.d[1], v4.d[0]\n\t"
		"ins v7.d[0], v4.d[1]\n\t"
		"eor v2.16b, v2.16b, v6.16b\n\t"
		"eor v3.16b, v3.16b, v7.16b\n\t"
		"str q2, [%[output]]\n\t"
		"str q3, [%[output], #16]"
		;
		; [x] "r" (x)
		  [h] "r" (h)
		  [output] "r" (output_ptr)
		; v0
		  v1
		  v2
		  v3
		  v4
		  v5
		  v6
		  v7
		  memory
	}
	return output
}

fn arm64_chacha20_row_state() [16]u32 {
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
	return state
}

fn arm64_chacha20_state_bytes(state [16]u32) []u8 {
	mut result := []u8{len: 64}
	for i, word in state {
		binary.little_endian_put_u32(mut result[i * 4..i * 4 + 4], word)
	}
	return result
}

fn arm64_reverse_bits8(value u8) u8 {
	mut reversed := u8(0)
	for bit in 0 .. 8 {
		reversed |= ((value >> bit) & 1) << (7 - bit)
	}
	return reversed
}

fn arm64_ghash_block_to_polynomial(block []u8) [2]u64 {
	mut result := [2]u64{}
	for i, value in block {
		position := i * 8
		reversed := arm64_reverse_bits8(value)
		if position < 64 {
			result[0] |= u64(reversed) << position
		} else {
			result[1] |= u64(reversed) << (position - 64)
		}
	}
	return result
}

fn arm64_ghash_reduce_product(mut product [4]u64) [2]u64 {
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

fn arm64_polynomial_to_ghash_block(value [2]u64) []u8 {
	mut result := []u8{len: 16}
	for i in 0 .. 16 {
		position := i * 8
		word := if position < 64 { value[0] } else { value[1] }
		result[i] = arm64_reverse_bits8(u8(word >> (position & 63)))
	}
	return result
}

fn can_run_pmull_test() bool {
	$if linux {
		cpuinfo := os.read_file('/proc/cpuinfo') or { return false }
		return cpuinfo.contains('pmull')
	} $else {
		return true
	}
}

fn test_raw_chacha20_neon_vector() {
	state := arm64_chacha20_row_state()
	actual_chacha := arm64_chacha20_state_bytes(raw_chacha20_block_neon(&state))
	expected_chacha := hex.decode('10f1e7e4d13b5915500fdd1fa32071c4c7d1f4c733c068030422aa9ac3d46c4ed2826446079faa0914c2d705d98b02a2b5129cd1de164eb9cbd083e8a2503c4e')!
	assert actual_chacha == expected_chacha
}

fn test_raw_ghash_pmull_vector() {
	if !can_run_pmull_test() {
		return
	}
	x_bytes := hex.decode('0388dace60b6a392f328c2b971b2fe78')!
	h_bytes := hex.decode('66e94bd4ef8a2c3b884cfa59ca342b2e')!
	x := arm64_ghash_block_to_polynomial(x_bytes)
	h := arm64_ghash_block_to_polynomial(h_bytes)
	mut product := raw_ghash_pmull_product(&x, &h)
	mut expected_product := [4]u64{}
	expected_product[0] = 967084790008197592
	expected_product[1] = 8907455666260999920
	expected_product[2] = 3863501629616998091
	expected_product[3] = 3091736768428133317
	assert product == expected_product
	actual_ghash := arm64_polynomial_to_ghash_block(arm64_ghash_reduce_product(mut product))
	expected_ghash := hex.decode('5e2ec746917062882c85b0685353deb7')!
	assert actual_ghash == expected_ghash
}

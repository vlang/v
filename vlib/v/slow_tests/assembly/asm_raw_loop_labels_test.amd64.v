// vtest build: !msvc

import encoding.binary

@[noinline]
fn raw_sum_bytes_percent_equal(data []u8) u64 {
	mut ptr := data.data
	mut len := u64(data.len)
	mut sum := u64(0)
	asm amd64 raw {
		"testq %[len], %[len]\n\t"
		"jz .Ldone%=\n\t"
		".Lloop%=: \n\t"
		"movzbl ( %[ptr] ), %%eax\n\t"
		"addq %%rax, %[sum]\n\t"
		"incq %[ptr]\n\t"
		"decq %[len]\n\t"
		"jnz .Lloop%=\n\t"
		".Ldone%=:"
		; [sum] "+r" (sum)
		  [ptr] "+r" (ptr)
		  [len] "+r" (len)
		;
		; rax
		  memory
		  cc
	}
	return sum
}

@[noinline]
fn raw_sum_bytes_numeric(data []u8) u64 {
	mut ptr := data.data
	mut len := u64(data.len)
	mut sum := u64(0)
	asm amd64 raw {
		"testq %[len], %[len]\n\t"
		"jz 2f\n\t"
		"1:\n\t"
		"movzbl ( %[ptr] ), %%eax\n\t"
		"addq %%rax, %[sum]\n\t"
		"incq %[ptr]\n\t"
		"decq %[len]\n\t"
		"jnz 1b\n\t"
		"2:"
		; [sum] "+r" (sum)
		  [ptr] "+r" (ptr)
		  [len] "+r" (len)
		;
		; rax
		  memory
		  cc
	}
	return sum
}

fn test_raw_loop_labels_and_bound_operands() {
	data := [u8(1), 2, 3, 5, 8, 13, 21, 34]
	expected := u64(87)
	assert raw_sum_bytes_percent_equal(data) == expected
	assert raw_sum_bytes_percent_equal(data) == expected
	assert raw_sum_bytes_numeric(data) == expected
	assert raw_sum_bytes_numeric(data[..4]) == 11
	assert raw_sum_bytes_numeric([]u8{}) == 0
}

// raw_poly1305_blocks_26 processes complete Poly1305 blocks with 26-bit limbs.
// It is deliberately kept in this assembly acceptance test instead of a crypto module.
@[noinline]
fn raw_poly1305_blocks_26(data []u8, block_count u64, hibit u64, r0 u64, r1 u64, r2 u64,
	r3 u64, r4 u64, initial_h0 u64, initial_h1 u64, initial_h2 u64, initial_h3 u64,
	initial_h4 u64) [5]u64 {
	mut ptr := data.data
	mut blocks := block_count
	mut h0 := initial_h0
	mut h1 := initial_h1
	mut h2 := initial_h2
	mut h3 := initial_h3
	mut h4 := initial_h4
	mut d0 := u64(0)
	mut d1 := u64(0)
	mut d2 := u64(0)
	mut d3 := u64(0)
	mut d4 := u64(0)
	s1 := r1 * 5
	s2 := r2 * 5
	s3 := r3 * 5
	s4 := r4 * 5
	asm amd64 raw {
		"testq %[blocks], %[blocks]\n\t"
		"jz 2f\n\t"
		"1:\n\t"
		"movl 0(%[ptr]), %%eax\n\t"
		"movq %%rax, %[d0]\n\t"
		"movl 4(%[ptr]), %%eax\n\t"
		"movq %%rax, %[d1]\n\t"
		"movl 8(%[ptr]), %%eax\n\t"
		"movq %%rax, %[d2]\n\t"
		"movl 12(%[ptr]), %%eax\n\t"
		"movq %%rax, %[d3]\n\t"

		"movq %[d0], %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, %[h0]\n\t"
		"movq %[d0], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d1], %%rdx\n\t"
		"shlq $6, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, %[h1]\n\t"
		"movq %[d1], %%rax\n\t"
		"shrq $20, %%rax\n\t"
		"movq %[d2], %%rdx\n\t"
		"shlq $12, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, %[h2]\n\t"
		"movq %[d2], %%rax\n\t"
		"shrq $14, %%rax\n\t"
		"movq %[d3], %%rdx\n\t"
		"shlq $18, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, %[h3]\n\t"
		"movq %[d3], %%rax\n\t"
		"shrq $8, %%rax\n\t"
		"addq %[hibit], %%rax\n\t"
		"addq %%rax, %[h4]\n\t"

		"movq %[h0], %%rax\n\t"
		"imulq %[r0], %%rax\n\t"
		"movq %[h1], %%rdx\n\t"
		"imulq %[s4], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h2], %%rdx\n\t"
		"imulq %[s3], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h3], %%rdx\n\t"
		"imulq %[s2], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h4], %%rdx\n\t"
		"imulq %[s1], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, %[d0]\n\t"

		"movq %[h0], %%rax\n\t"
		"imulq %[r1], %%rax\n\t"
		"movq %[h1], %%rdx\n\t"
		"imulq %[r0], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h2], %%rdx\n\t"
		"imulq %[s4], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h3], %%rdx\n\t"
		"imulq %[s3], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h4], %%rdx\n\t"
		"imulq %[s2], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, %[d1]\n\t"

		"movq %[h0], %%rax\n\t"
		"imulq %[r2], %%rax\n\t"
		"movq %[h1], %%rdx\n\t"
		"imulq %[r1], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h2], %%rdx\n\t"
		"imulq %[r0], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h3], %%rdx\n\t"
		"imulq %[s4], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h4], %%rdx\n\t"
		"imulq %[s3], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, %[d2]\n\t"

		"movq %[h0], %%rax\n\t"
		"imulq %[r3], %%rax\n\t"
		"movq %[h1], %%rdx\n\t"
		"imulq %[r2], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h2], %%rdx\n\t"
		"imulq %[r1], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h3], %%rdx\n\t"
		"imulq %[r0], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h4], %%rdx\n\t"
		"imulq %[s4], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, %[d3]\n\t"

		"movq %[h0], %%rax\n\t"
		"imulq %[r4], %%rax\n\t"
		"movq %[h1], %%rdx\n\t"
		"imulq %[r3], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h2], %%rdx\n\t"
		"imulq %[r2], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h3], %%rdx\n\t"
		"imulq %[r1], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %[h4], %%rdx\n\t"
		"imulq %[r0], %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, %[d4]\n\t"

		"movq %[d0], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d0], %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, %[h0]\n\t"
		"addq %%rax, %[d1]\n\t"
		"movq %[d1], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d1], %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, %[h1]\n\t"
		"addq %%rax, %[d2]\n\t"
		"movq %[d2], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d2], %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, %[h2]\n\t"
		"addq %%rax, %[d3]\n\t"
		"movq %[d3], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d3], %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, %[h3]\n\t"
		"addq %%rax, %[d4]\n\t"
		"movq %[d4], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq %[d4], %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, %[h4]\n\t"
		"leaq (%%rax, %%rax, 4), %%rax\n\t"
		"addq %%rax, %[h0]\n\t"
		"movq %[h0], %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"andq $0x3ffffff, %[h0]\n\t"
		"addq %%rax, %[h1]\n\t"
		"addq $16, %[ptr]\n\t"
		"decq %[blocks]\n\t"
		"jnz 1b\n\t"
		"2:"
		; [h0] "+m" (h0)
		  [h1] "+m" (h1)
		  [h2] "+m" (h2)
		  [h3] "+m" (h3)
		  [h4] "+m" (h4)
		  [d0] "=m" (d0)
		  [d1] "=m" (d1)
		  [d2] "=m" (d2)
		  [d3] "=m" (d3)
		  [d4] "=m" (d4)
		  [ptr] "+r" (ptr)
		  [blocks] "+r" (blocks)
		; [r0] "m" (r0)
		  [r1] "m" (r1)
		  [r2] "m" (r2)
		  [r3] "m" (r3)
		  [r4] "m" (r4)
		  [s1] "m" (s1)
		  [s2] "m" (s2)
		  [s3] "m" (s3)
		  [s4] "m" (s4)
		  [hibit] "m" (hibit)
		; rax
		  rdx
		  memory
		  cc
	}
	mut h := [5]u64{}
	h[0] = h0
	h[1] = h1
	h[2] = h2
	h[3] = h3
	h[4] = h4
	return h
}

fn pure_poly1305_blocks_26(data []u8, block_count int, hibit u64, r0 u64, r1 u64, r2 u64,
	r3 u64, r4 u64, initial_h [5]u64) [5]u64 {
	mut h := [5]u64{}
	for i in 0 .. 5 {
		h[i] = initial_h[i]
	}
	s1 := r1 * 5
	s2 := r2 * 5
	s3 := r3 * 5
	s4 := r4 * 5
	for block in 0 .. block_count {
		offset := block * 16
		mut t := [4]u64{}
		for word in 0 .. 4 {
			for byte_index in 0 .. 4 {
				t[word] |= u64(data[offset + word * 4 + byte_index]) << (byte_index * 8)
			}
		}
		h[0] += t[0] & 0x3ffffff
		h[1] += ((t[0] >> 26) | (t[1] << 6)) & 0x3ffffff
		h[2] += ((t[1] >> 20) | (t[2] << 12)) & 0x3ffffff
		h[3] += ((t[2] >> 14) | (t[3] << 18)) & 0x3ffffff
		h[4] += (t[3] >> 8) + hibit
		mut d0 := h[0] * r0 + h[1] * s4 + h[2] * s3 + h[3] * s2 + h[4] * s1
		mut d1 := h[0] * r1 + h[1] * r0 + h[2] * s4 + h[3] * s3 + h[4] * s2
		mut d2 := h[0] * r2 + h[1] * r1 + h[2] * r0 + h[3] * s4 + h[4] * s3
		mut d3 := h[0] * r3 + h[1] * r2 + h[2] * r1 + h[3] * r0 + h[4] * s4
		mut d4 := h[0] * r4 + h[1] * r3 + h[2] * r2 + h[3] * r1 + h[4] * r0
		mut c := d0 >> 26
		h[0] = d0 & 0x3ffffff
		d1 += c
		c = d1 >> 26
		h[1] = d1 & 0x3ffffff
		d2 += c
		c = d2 >> 26
		h[2] = d2 & 0x3ffffff
		d3 += c
		c = d3 >> 26
		h[3] = d3 & 0x3ffffff
		d4 += c
		c = d4 >> 26
		h[4] = d4 & 0x3ffffff
		h[0] += c * 5
		c = h[0] >> 26
		h[0] &= 0x3ffffff
		h[1] += c
	}
	return h
}

fn poly1305_tag_from_limbs(h [5]u64, s0 u64, s1 u64) []u8 {
	prime := [u64(0x3fffffb), 0x3ffffff, 0x3ffffff, 0x3ffffff, 0x3ffffff]
	mut reduced := [5]u64{}
	for i in 0 .. 5 {
		reduced[i] = h[i]
	}
	mut borrow := u64(0)
	mut difference := [5]u64{}
	for i in 0 .. 5 {
		subtrahend := prime[i] + borrow
		if reduced[i] >= subtrahend {
			difference[i] = reduced[i] - subtrahend
			borrow = 0
		} else {
			difference[i] = (u64(1) << 26) + reduced[i] - subtrahend
			borrow = 1
		}
	}
	if borrow == 0 {
		reduced = difference
	}
	mut words := [u64(0), 0, 0, 0]
	words[0] = reduced[0] | (reduced[1] << 26)
	words[1] = (reduced[1] >> 6) | (reduced[2] << 20)
	words[2] = (reduced[2] >> 12) | (reduced[3] << 14)
	words[3] = (reduced[3] >> 18) | (reduced[4] << 8)
	mut out := []u8{len: 16}
	mut carry := u64(0)
	for i in 0 .. 4 {
		addend := if i < 2 { s0 >> (i * 32) } else { s1 >> ((i - 2) * 32) }
		value := (words[i] & 0xffffffff) + (addend & 0xffffffff) + carry
		binary.little_endian_put_u32(mut out[i * 4..i * 4 + 4], u32(value))
		carry = value >> 32
	}
	return out
}

fn test_raw_poly1305_blocks_rfc8439() {
	key := [u8(0x85), 0xd6, 0xbe, 0x78, 0x57, 0x55, 0x6d, 0x33, 0x7f, 0x44, 0x52, 0xfe, 0x42, 0xd5,
		0x06, 0xa8, 0x01, 0x03, 0x80, 0x8a, 0xfb, 0x0d, 0xb2, 0xfd, 0x4a, 0xbf, 0xf6, 0xaf, 0x41,
		0x49, 0xf5, 0x1b]
	message := 'Cryptographic Forum Research Group'.bytes()
	r_lo := binary.little_endian_u64(key[0..8]) & u64(0x0ffffffc0fffffff)
	r_hi := binary.little_endian_u64(key[8..16]) & u64(0x0ffffffc0ffffffc)
	r0 := r_lo & 0x3ffffff
	r1 := (r_lo >> 26) & 0x3ffffff
	r2 := ((r_lo >> 52) | (r_hi << 12)) & 0x3ffffff
	r3 := (r_hi >> 14) & 0x3ffffff
	r4 := (r_hi >> 40) & 0x3ffffff
	mut h := raw_poly1305_blocks_26(message[..32], 2, u64(1) << 24, r0, r1, r2, r3, r4, 0, 0, 0, 0, 0)
	mut tail := []u8{len: 16}
	tail[0] = message[32]
	tail[1] = message[33]
	tail[2] = 1
	h = raw_poly1305_blocks_26(tail, 1, 0, r0, r1, r2, r3, r4, h[0], h[1], h[2], h[3], h[4])
	mut pure_h := pure_poly1305_blocks_26(message[..32], 2, u64(1) << 24, r0, r1, r2, r3, r4, [5]u64{})
	pure_h = pure_poly1305_blocks_26(tail, 1, 0, r0, r1, r2, r3, r4, pure_h)
	assert h == pure_h
	s0 := binary.little_endian_u64(key[16..24])
	s1 := binary.little_endian_u64(key[24..32])
	actual := poly1305_tag_from_limbs(h, s0, s1)
	expected := [u8(0xa8), 0x06, 0x1d, 0xc1, 0x30, 0x51, 0x36, 0xc6, 0xc2, 0x2b, 0x8b, 0xaf, 0x0c,
		0x01, 0x27, 0xa9]
	assert actual == expected
}

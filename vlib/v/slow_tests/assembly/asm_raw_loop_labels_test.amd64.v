// vtest build: !msvc && !tinyc

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
	// Keep h0..h4, d0..d4, r0..r4, s1..s4 and hibit in one backing array.
	// One base register also works with ASan's stack layout at -O0.
	mut state := [initial_h0, initial_h1, initial_h2, initial_h3, initial_h4, u64(0), 0, 0, 0,
		0, r0, r1, r2, r3, r4, r1 * 5, r2 * 5, r3 * 5, r4 * 5, hibit]!
	// The assembly uses this pointer only while state is on this stack frame.
	limbs := unsafe { &state[0] }
	asm amd64 raw {
		"testq %[blocks], %[blocks]\n\t"
		"jz 2f\n\t"
		"1:\n\t"
		"movl 0(%[ptr]), %%eax\n\t"
		"movq %%rax, 40(%[limbs])\n\t"
		"movl 4(%[ptr]), %%eax\n\t"
		"movq %%rax, 48(%[limbs])\n\t"
		"movl 8(%[ptr]), %%eax\n\t"
		"movq %%rax, 56(%[limbs])\n\t"
		"movl 12(%[ptr]), %%eax\n\t"
		"movq %%rax, 64(%[limbs])\n\t"

		"movq 40(%[limbs]), %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, 0(%[limbs])\n\t"
		"movq 40(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 48(%[limbs]), %%rdx\n\t"
		"shlq $6, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, 8(%[limbs])\n\t"
		"movq 48(%[limbs]), %%rax\n\t"
		"shrq $20, %%rax\n\t"
		"movq 56(%[limbs]), %%rdx\n\t"
		"shlq $12, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, 16(%[limbs])\n\t"
		"movq 56(%[limbs]), %%rax\n\t"
		"shrq $14, %%rax\n\t"
		"movq 64(%[limbs]), %%rdx\n\t"
		"shlq $18, %%rdx\n\t"
		"orq %%rdx, %%rax\n\t"
		"andq $0x3ffffff, %%rax\n\t"
		"addq %%rax, 24(%[limbs])\n\t"
		"movq 64(%[limbs]), %%rax\n\t"
		"shrq $8, %%rax\n\t"
		"addq 152(%[limbs]), %%rax\n\t"
		"addq %%rax, 32(%[limbs])\n\t"

		"movq 0(%[limbs]), %%rax\n\t"
		"imulq 80(%[limbs]), %%rax\n\t"
		"movq 8(%[limbs]), %%rdx\n\t"
		"imulq 144(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 16(%[limbs]), %%rdx\n\t"
		"imulq 136(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 24(%[limbs]), %%rdx\n\t"
		"imulq 128(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 32(%[limbs]), %%rdx\n\t"
		"imulq 120(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, 40(%[limbs])\n\t"

		"movq 0(%[limbs]), %%rax\n\t"
		"imulq 88(%[limbs]), %%rax\n\t"
		"movq 8(%[limbs]), %%rdx\n\t"
		"imulq 80(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 16(%[limbs]), %%rdx\n\t"
		"imulq 144(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 24(%[limbs]), %%rdx\n\t"
		"imulq 136(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 32(%[limbs]), %%rdx\n\t"
		"imulq 128(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, 48(%[limbs])\n\t"

		"movq 0(%[limbs]), %%rax\n\t"
		"imulq 96(%[limbs]), %%rax\n\t"
		"movq 8(%[limbs]), %%rdx\n\t"
		"imulq 88(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 16(%[limbs]), %%rdx\n\t"
		"imulq 80(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 24(%[limbs]), %%rdx\n\t"
		"imulq 144(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 32(%[limbs]), %%rdx\n\t"
		"imulq 136(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, 56(%[limbs])\n\t"

		"movq 0(%[limbs]), %%rax\n\t"
		"imulq 104(%[limbs]), %%rax\n\t"
		"movq 8(%[limbs]), %%rdx\n\t"
		"imulq 96(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 16(%[limbs]), %%rdx\n\t"
		"imulq 88(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 24(%[limbs]), %%rdx\n\t"
		"imulq 80(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 32(%[limbs]), %%rdx\n\t"
		"imulq 144(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, 64(%[limbs])\n\t"

		"movq 0(%[limbs]), %%rax\n\t"
		"imulq 112(%[limbs]), %%rax\n\t"
		"movq 8(%[limbs]), %%rdx\n\t"
		"imulq 104(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 16(%[limbs]), %%rdx\n\t"
		"imulq 96(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 24(%[limbs]), %%rdx\n\t"
		"imulq 88(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq 32(%[limbs]), %%rdx\n\t"
		"imulq 80(%[limbs]), %%rdx\n\t"
		"addq %%rdx, %%rax\n\t"
		"movq %%rax, 72(%[limbs])\n\t"

		"movq 40(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 40(%[limbs]), %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, 0(%[limbs])\n\t"
		"addq %%rax, 48(%[limbs])\n\t"
		"movq 48(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 48(%[limbs]), %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, 8(%[limbs])\n\t"
		"addq %%rax, 56(%[limbs])\n\t"
		"movq 56(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 56(%[limbs]), %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, 16(%[limbs])\n\t"
		"addq %%rax, 64(%[limbs])\n\t"
		"movq 64(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 64(%[limbs]), %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, 24(%[limbs])\n\t"
		"addq %%rax, 72(%[limbs])\n\t"
		"movq 72(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"movq 72(%[limbs]), %%rdx\n\t"
		"andq $0x3ffffff, %%rdx\n\t"
		"movq %%rdx, 32(%[limbs])\n\t"
		"leaq (%%rax, %%rax, 4), %%rax\n\t"
		"addq %%rax, 0(%[limbs])\n\t"
		"movq 0(%[limbs]), %%rax\n\t"
		"shrq $26, %%rax\n\t"
		"andq $0x3ffffff, 0(%[limbs])\n\t"
		"addq %%rax, 8(%[limbs])\n\t"
		"addq $16, %[ptr]\n\t"
		"decq %[blocks]\n\t"
		"jnz 1b\n\t"
		"2:"
		; [ptr] "+&r" (ptr)
		  [blocks] "+&r" (blocks)
		; [limbs] "r" (limbs)
		; rax
		  rdx
		  memory
		  cc
	}
	return [state[0], state[1], state[2], state[3], state[4]]!
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

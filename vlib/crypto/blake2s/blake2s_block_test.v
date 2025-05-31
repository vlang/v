module blake2s

// from RFC-7693 Appendix B
const expected_m_results = [u32(0x00636261), 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x00000000, 0x00000000]

// from RFC-7693 Appendix B
const expected_v_initial_results = [u32(0x6b08e647), 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f,
	0x9b05688c, 0x1f83d9ab, 0x5be0cd19, 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527c,
	0x9b05688c, 0xe07c2654, 0x5be0cd19]

// from RFC-7693 Appendix B
const expected_v_results = [
	[u32(0x16a3242e), 0xd7b5e238, 0xce8ce24b, 0x927aede1, 0xa7b430d9, 0x93a4a14e, 0xa44e7c31,
		0x41d4759b, 0x95bf33d3, 0x9a99c181, 0x608a3a6b, 0xb666383e, 0x7a8dd50f, 0xbe378ed7,
		0x353d1ee6, 0x3bb44c6b],
	[u32(0x3ae30fe3), 0x0982a96b, 0xe88185b4, 0x3e339b16, 0xf24338cd, 0x0e66d326, 0xe005ed0c,
		0xd591a277, 0x180b1f3a, 0xfcf43914, 0x30db62d6, 0x4847831c, 0x7f00c58e, 0xfb847886,
		0xc544e836, 0x524ab0e2],
	[u32(0x7a3be783), 0x997546c1, 0xd45246df, 0xedb5f821, 0x7f98a742, 0x10e864e2, 0xd4ab70d0,
		0xc63cb1ab, 0x6038da9e, 0x414594b0, 0xf2c218b5, 0x8da0dcb7, 0xd7cd7af5, 0xab4909df,
		0x85031a52, 0xc4edfc98],
	[u32(0x2a8b8cb7), 0x1aca82b2, 0x14045d7f, 0xcc7258ed, 0x383cf67c, 0xe090e7f9, 0x3025d276,
		0x57d04de4, 0x994bacf0, 0xf0982759, 0xf17ee300, 0xd48fc2d5, 0xdc854c10, 0x523898a9,
		0xc03a0f89, 0x47d6cd88],
	[u32(0xc4aa2ddb), 0x111343a3, 0xd54a700a, 0x574a00a9, 0x857d5a48, 0xb1e11989, 0x6f5c52df,
		0xdd2c53a3, 0x678e5f8e, 0x9718d4e9, 0x622cb684, 0x92976076, 0x0e41a517, 0x359dc2be,
		0x87a87ddd, 0x643f9cec],
	[u32(0x3453921c), 0xd7595ee1, 0x592e776d, 0x3ed6a974, 0x4d997cb3, 0xde9212c3, 0x35adf5c9,
		0x9916fd65, 0x96562e89, 0x4ead0792, 0xebfc2712, 0x2385f5b2, 0xf34600fb, 0xd7bc20fb,
		0xeb452a7b, 0xece1aa40],
	[u32(0xbe851b2d), 0xa85f6358, 0x81e6fc3b, 0x0bb28000, 0xfa55a33a, 0x87be1fad, 0x4119370f,
		0x1e2261aa, 0xa1318fd3, 0xf4329816, 0x071783c2, 0x6e536a8d, 0x9a81a601, 0xe7ec80f1,
		0xacc09948, 0xf849a584],
	[u32(0x07e5b85a), 0x069cc164, 0xf9de3141, 0xa56f4680, 0x9e440ad2, 0x9ab659ea, 0x3c84b971,
		0x21dbd9cf, 0x46699f8c, 0x765257ec, 0xaf1d998c, 0x75e4c3b6, 0x523878dc, 0x30715015,
		0x397fee81, 0x4f1fa799],
	[u32(0x435148c4), 0xa5aa2d11, 0x4b354173, 0xd543bc9e, 0xbda2591c, 0xbf1d2569, 0x4fcb3120,
		0x707ada48, 0x565b3fde, 0x32c9c916, 0xeaf4a1ab, 0xb1018f28, 0x8078d978, 0x68ade4b5,
		0x9778fda3, 0x2863b92e],
	[u32(0xd9c994aa), 0xcfec3aa6, 0x700d0ab2, 0x2c38670e, 0xaf6a1f66, 0x1d023ef3, 0x1d9ec27d,
		0x945357a5, 0x3e9ffebd, 0x969fe811, 0xef485e21, 0xa632797a, 0xdeef082e, 0xaf3d80e1,
		0x4e86829b, 0x4deafd3a],
]

// from RFC-7693 Appendix B
const expected_h_results = [u32(0x8c5e8c50), 0xe2147c32, 0xa32ba7e1, 0x2f45eb4e, 0x208b4537,
	0x293ad69e, 0x4c9b994d, 0x82596786]

fn test_mixing_function_g() {
	mut d := new256() or {
		assert false, 'unable to create new 256 bit hash digest: ${err}'
		return
	}

	// set up the message blocks with the value 'abc'
	// the firet block will have the 3 bytes of the text to hash
	// and the rest of the first block and the other 15 blocks
	// will be all zeros.  d.m[1..16] should already be zero.
	d.m[0] = 0x00636261

	// indicate that we have 3 bytes in the message block
	d.t += 3

	// indicate that the message block contains the end of the
	// text being hashed.
	f := true

	// initialize the working vector from the digest and IV values
	mut v := []u32{len: 0, cap: 16}
	v << d.h[..8]
	v << iv[..8]

	// fold in the 64-bit message length
	v[12] ^= u32(d.t & 0x00000000ffffffff)
	v[13] ^= u32(d.t >> 32)

	// and flip the bits in v[14] because this is the end of the
	// text being hashed.
	if f {
		v[14] = ~v[14]
	}

	for i in 0 .. 16 {
		assert v[i] == expected_v_initial_results[i], 'expeccted expected_v_initial_results[${i}] ${expected_v_initial_results[i]:08x} actual v[${i}] ${v[i]:08x}'
	}

	for i in 0 .. 16 {
		assert d.m[i] == expected_m_results[i], 'expeccted expected_m_results[${i}] ${expected_m_results[i]:08x} actual d.m[${i}] ${d.m[i]:08x}'
	}

	for r in 0 .. expected_v_results.len {
		d.mixing_round(mut v, sigma[r])

		for i in 0 .. 16 {
			assert v[i] == expected_v_results[r][i], 'expeccted expected_v_results[${r}][${i}] ${expected_v_results[r][i]:08x} actual v[${i}] ${v[i]:08x}'
		}
	}

	d.h[0] = d.h[0] ^ v[0] ^ v[8]
	d.h[1] = d.h[1] ^ v[1] ^ v[9]
	d.h[2] = d.h[2] ^ v[2] ^ v[10]
	d.h[3] = d.h[3] ^ v[3] ^ v[11]
	d.h[4] = d.h[4] ^ v[4] ^ v[12]
	d.h[5] = d.h[5] ^ v[5] ^ v[13]
	d.h[6] = d.h[6] ^ v[6] ^ v[14]
	d.h[7] = d.h[7] ^ v[7] ^ v[15]

	for i in 0 .. 8 {
		assert d.h[i] == expected_h_results[i], 'expeccted expected_h_results[${i}] ${expected_h_results[i]:08x} actual d.h[${i}] ${d.h[i]:08x}'
	}
}

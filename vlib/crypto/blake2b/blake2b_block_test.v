module blake2b

// from RFC-7693 Appendix A
const expected_m_results = [u64(0x0000000000636261), 0x0000000000000000, 0x0000000000000000,
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
	0x0000000000000000]

// from RFC-7693 Appendix A
const expected_v_initial_results = [u64(0x6a09e667f2bdc948), 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b,
	0xa54ff53a5f1d36f1, 0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b,
	0x5be0cd19137e2179, 0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b,
	0xa54ff53a5f1d36f1, 0x510e527fade682d2, 0x9b05688c2b3e6c1f, 0xe07c265404be4294,
	0x5be0cd19137e2179]

// from RFC-7693 Appendix A
const expected_v_results = [
	[u64(0x86b7c1568029bb79), 0xc12cbcc809ff59f3, 0xc6a5214cc0eaca8e, 0x0c87cd524c14cc5d,
		0x44ee6039bd86a9f7, 0xa447c850aa694a7e, 0xde080f1bb1c0f84b, 0x595cb8a9a1aca66c,
		0xbec3ae837eac4887, 0x6267fc79df9d6ad1, 0xfa87b01273fa6dbe, 0x521a715c63e08d8a,
		0xe02d0975b8d37a83, 0x1c7b754f08b7d193, 0x8f885a76b6e578fe, 0x2318a24e2140fc64],
	[u64(0x53281e83806010f2), 0x3594b403f81b4393, 0x8cd63c7462de0dff, 0x85f693f3da53f974,
		0xbaabdbb2f386d9ae, 0xca5425aec65a10a8, 0xc6a22e2ff0f7aa48, 0xc6a56a51cb89c595,
		0x224e6a3369224f96, 0x500e125e58a92923, 0xe9e4ad0d0e1a0d48, 0x85df9dc143c59a74,
		0x92a3aaaa6d952b7f, 0xc5fdf71090fae853, 0x2a8a40f15a462dd0, 0x572d17effdd37358],
	[u64(0x60ed96aa7ad41725), 0xe46a743c71800b9d, 0x1a04b543a01f156b, 0xa2f8716e775c4877,
		0xda0a61bcde4267ea, 0xb1dd230754d7bdee, 0x25a1422779e06d14, 0xe6823ae4c3ff58a5,
		0xa1677e19f37fd5da, 0x22bdce6976b08c51, 0xf1de8696bec11bf1, 0xa0ebd586a4a1d2c8,
		0xc804ebab11c99fa9, 0x8e0cec959c715793, 0x7c45557fae0d4d89, 0x716343f52fdd265e],
	[u64(0xbb2a77d3a8382351), 0x45eb47971f23b103, 0x98be297f6e45c684, 0xa36077dee3370b89,
		0x8a03c4cb7e97590a, 0x24192e49ebf54ea0, 0x4f82c9401cb32d7a, 0x8ccd013726420dc4,
		0xa9c9a8f17b1fc614, 0x55908187977514a0, 0x5b44273e66b19d27, 0xb6d5c9fca2579327,
		0x086092cfb858437e, 0x5c4be2156dbeecf9, 0x2efede99ed4eff16, 0x3e7b5f234cd1f804],
	[u64(0xc79c15b3d423b099), 0x2da2224e8da97556, 0x77d2b26df1c45c55, 0x8934eb09a3456052,
		0x0f6d9eeed157da2a, 0x6fe66467af88c0a9, 0x4eb0b76284c7aafb, 0x299c8e725d954697,
		0xb2240b59e6d567d3, 0x2643c2370e49ebfd, 0x79e02eef20cdb1ae, 0x64b3eed7bb602f39,
		0xb97d2d439e4df63d, 0xc718e755294c9111, 0x1f0893f2772bb373, 0x1205ea4a7859807d],
	[u64(0xe58f97d6385baee4), 0x7640aa9764da137a, 0xdeb4c7c23efe287e, 0x70f6f41c8783c9f6,
		0x7127cd48c76a7708, 0x9e472af0be3db3f6, 0x0f244c62ddf71788, 0x219828aa83880842,
		0x41cca9073c8c4d0d, 0x5c7912bc10df3b4b, 0xa2c3abbd37510ee2, 0xcb5668cc2a9f7859,
		0x8733794f07ac1500, 0xc67a6be42335aa6f, 0xacb22b28681e4c82, 0xdb2161604cbc9828],
	[u64(0x6e2d286eeadedc81), 0xbcf02c0787e86358, 0x57d56a56dd015edf, 0x55d899d40a5d0d0a,
		0x819415b56220c459, 0xb63c479a6a769f02, 0x258e55e0ec1f362a, 0x3a3b4ec60e19dfdc,
		0x04d769b3fcb048db, 0xb78a9a33e9bff4dd, 0x5777272ae1e930c0, 0x5a387849e578dbf6,
		0x92aac307cf2c0afc, 0x30aaccc4f06dafaa, 0x483893cc094f8863, 0xe03c6cc89c26bf92],
	[u64(0xffc83ece76024d01), 0x1be7bffb8c5cc5f9, 0xa35a18cbac4c65b7, 0xb7c2c7e6d88c285f,
		0x81937da314a50838, 0xe1179523a2541963, 0x3a1fad7106232b8f, 0x1c7ede92ab8b9c46,
		0xa3c2d35e4f685c10, 0xa53d3f73aa619624, 0x30bbcc0285a22f65, 0xbcefbb6a81539e5d,
		0x3841def6f4c9848a, 0x98662c85fba726d4, 0x7762439bd5a851bd, 0xb0b9f0d443d1a889],
	[u64(0x753a70a1e8faeadd), 0x6b0d43ca2c25d629, 0xf8343ba8b94f8c0b, 0xbc7d062b0db5cf35,
		0x58540ee1b1aebc47, 0x63c5b9b80d294cb9, 0x490870ecad27debd, 0xb2a90ddf667287fe,
		0x316cc9ebeefad8fc, 0x4a466bcd021526a4, 0x5da7f7638cec5669, 0xd9c8826727d306fc,
		0x88ed6c4f3bd7a537, 0x19ae688ddf67f026, 0x4d8707aab40f7e6d, 0xfd3f572687fea4f1],
	[u64(0xe630c747ccd59c4f), 0xbc713d41127571ca, 0x46db183025025078, 0x6727e81260610140,
		0x2d04185eac2a8cba, 0x5f311b88904056ec, 0x40bd313009201aab, 0x0099d4f82a2a1eab,
		0x6dd4fbc1de60165d, 0xb3b0b51de3c86270, 0x900aee2f233b08e5, 0xa07199d87ad058d8,
		0x2c6b25593d717852, 0x37e8ca471beaa5f8, 0x2cfc1bac10ef4457, 0x01369ec18746e775],
	[u64(0xe801f73b9768c760), 0x35c6d22320be511d, 0x306f27584f65495e, 0xb51776adf569a77b,
		0xf4f1be86690b3c34, 0x3cc88735d1475e4b, 0x5dac67921ff76949, 0x1cdb9d31ad70cc4e,
		0x35ba354a9c7df448, 0x4929cbe45679d73e, 0x733d1a17248f39db, 0x92d57b736f5f170a,
		0x61b5c0a41d491399, 0xb5c333457e12844a, 0xbd696be010d0d889, 0x02231e1a917fe0bd],
	[u64(0x12ef8a641ec4f6d6), 0xbced5de977c9faf5, 0x733ca476c5148639, 0x97df596b0610f6fc,
		0xf42c16519ad5afa7, 0xaa5ac1888e10467e, 0x217d930aa51787f3, 0x906a6ff19e573942,
		0x75ab709bd3dcbf24, 0xee7ce1f345947aa4, 0xf8960d6c2faf5f5e, 0xe332538a36b6d246,
		0x885bef040ef6aa0b, 0xa4939a417bfb78a3, 0x646cbb7af6dce980, 0xe813a23c60af3b82],
]

// from RFC-7693 Appendix A
const expected_h_results = [u64(0x0d4d1c983fa580ba), 0xe9f6129fb697276a, 0xb7c45a68142f214c,
	0xd1a2ffdb6fbb124b, 0x2d79ab2a39c5877d, 0x95cc3345ded552c2, 0x5a92f1dba88ad318,
	0x239900d4ed8623b9]

fn test_mixing_function_g() {
	mut d := new512() or {
		assert false, 'unable to create new 512 bit hash digest: ${err}'
		return
	}

	// set up the message blocks with the value 'abc'
	// the firet block will have the 3 bytes of the text to hash
	// and the rest of the first block and the other 15 blocks
	// will be all zeros.  d.m[1..16] should already be zero.
	d.m[0] = 0x0000000000636261

	// indicate that we have 3 bytes in the message block
	d.t = d.t.add_64(3)

	// indicate that the message block contains the end of the
	// text being hashed.
	f := true

	// initialize the working vector from the digest and IV values
	mut v := []u64{len: 0, cap: 16}
	v << d.h[..8]
	v << iv[..8]

	// fold in the 128-bit message length
	v[12] ^= d.t.lo
	v[13] ^= d.t.hi

	// and flip the bits in v[14] because this is the end of the
	// text being hashed.
	if f {
		v[14] = ~v[14]
	}

	for i in 0 .. 16 {
		assert v[i] == expected_v_initial_results[i], 'expeccted expected_v_initial_results[${i}] ${expected_v_initial_results[i]:016x} actual v[${i}] ${v[i]:016x}'
	}

	for i in 0 .. 16 {
		assert d.m[i] == expected_m_results[i], 'expeccted expected_m_results[${i}] ${expected_m_results[i]:016x} actual d.m[${i}] ${d.m[i]:016x}'
	}

	for r in 0 .. expected_v_results.len {
		d.mixing_round(mut v, sigma[r % 10])

		for i in 0 .. 16 {
			assert v[i] == expected_v_results[r][i], 'expeccted expected_v_results[${r}][${i}] ${expected_v_results[r][i]:016x} actual v[${i}] ${v[i]:016x}'
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
		assert d.h[i] == expected_h_results[i], 'expeccted expected_h_results[${i}] ${expected_h_results[i]:016x} actual d.h[${i}] ${d.h[i]:016x}'
	}
}

module hkdf

import crypto.sha1
import crypto.sha256
import encoding.hex
import hash

struct HkdfTest {
	new_hash fn () hash.Hash @[required]
	master   string
	salt     string
	prk      string
	info     string
	out      string
}

fn sha1_hash() hash.Hash {
	return sha1.new()
}

fn sha256_hash() hash.Hash {
	return sha256.new()
}

fn decode(s string) []u8 {
	return hex.decode(s) or { panic(err) }
}

fn hkdf_tests() []HkdfTest {
	// Tests from RFC 5869
	return [
		HkdfTest{
			new_hash: sha256_hash
			master:   '0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'
			salt:     '000102030405060708090a0b0c'
			prk:      '077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5'
			info:     'f0f1f2f3f4f5f6f7f8f9'
			out:      '3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865'
		},
		HkdfTest{
			new_hash: sha256_hash
			master:   '000102030405060708090a0b0c0d0e0f' + '101112131415161718191a1b1c1d1e1f' +
				'202122232425262728292a2b2c2d2e2f' + '303132333435363738393a3b3c3d3e3f' +
				'404142434445464748494a4b4c4d4e4f'
			salt:     '606162636465666768696a6b6c6d6e6f' + '707172737475767778797a7b7c7d7e7f' +
				'808182838485868788898a8b8c8d8e8f' + '909192939495969798999a9b9c9d9e9f' +
				'a0a1a2a3a4a5a6a7a8a9aaabacadaeaf'
			prk:      '06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244'
			info:     'b0b1b2b3b4b5b6b7b8b9babbbcbdbebf' + 'c0c1c2c3c4c5c6c7c8c9cacbcccdcecf' +
				'd0d1d2d3d4d5d6d7d8d9dadbdcdddedf' + 'e0e1e2e3e4e5e6e7e8e9eaebecedeeef' +
				'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff'
			out:      'b11e398dc80327a1c8e7f78c596a4934' + '4f012eda2d4efad8a050cc4c19afa97c' +
				'59045a99cac7827271cb41c65e590e09' + 'da3275600c2f09b8367793a9aca3db71' +
				'cc30c58179ec3e87c14c01d5c1f3434f1d87'
		},
		HkdfTest{
			new_hash: sha256_hash
			master:   '0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'
			salt:     ''
			prk:      '19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04'
			info:     ''
			out:      '8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8'
		},
		HkdfTest{
			new_hash: sha256_hash
			master:   '0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'
			salt:     ''
			prk:      '19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04'
			info:     ''
			out:      '8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8'
		},
		HkdfTest{
			new_hash: sha1_hash
			master:   '0b0b0b0b0b0b0b0b0b0b0b'
			salt:     '000102030405060708090a0b0c'
			prk:      '9b6c18c432a7bf8f0e71c8eb88f4b30baa2ba243'
			info:     'f0f1f2f3f4f5f6f7f8f9'
			out:      '085a01ea1b10f36933068b56efa5ad81a4f14b822f5b091568a9cdd4f155fda2c22e422478d305f3f896'
		},
		HkdfTest{
			new_hash: sha1_hash
			master:   '000102030405060708090a0b0c0d0e0f' + '101112131415161718191a1b1c1d1e1f' +
				'202122232425262728292a2b2c2d2e2f' + '303132333435363738393a3b3c3d3e3f' +
				'404142434445464748494a4b4c4d4e4f'
			salt:     '606162636465666768696a6b6c6d6e6f' + '707172737475767778797a7b7c7d7e7f' +
				'808182838485868788898a8b8c8d8e8f' + '909192939495969798999a9b9c9d9e9f' +
				'a0a1a2a3a4a5a6a7a8a9aaabacadaeaf'
			prk:      '8adae09a2a307059478d309b26c4115a224cfaf6'
			info:     'b0b1b2b3b4b5b6b7b8b9babbbcbdbebf' + 'c0c1c2c3c4c5c6c7c8c9cacbcccdcecf' +
				'd0d1d2d3d4d5d6d7d8d9dadbdcdddedf' + 'e0e1e2e3e4e5e6e7e8e9eaebecedeeef' +
				'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff'
			out:      '0bd770a74d1160f7c9f12cd5912a06eb' + 'ff6adcae899d92191fe4305673ba2ffe' +
				'8fa3f1a4e5ad79f3f334b3b202b2173c' + '486ea37ce3d397ed034c7f9dfeb15c5e' +
				'927336d0441f4c4300e2cff0d0900b52d3b4'
		},
		HkdfTest{
			new_hash: sha1_hash
			master:   '0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b'
			salt:     ''
			prk:      'da8c8a73c7fa77288ec6f5e7c297786aa0d32d01'
			info:     ''
			out:      '0ac1af7002b3d761d1e55298da9d0506b9ae52057220a306e07b6b87e8df21d0ea00033de03984d34918'
		},
		HkdfTest{
			new_hash: sha1_hash
			master:   '0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c'
			salt:     ''
			prk:      '2adccada18779e7c2077ad2eb19d3f3e731385dd'
			info:     ''
			out:      '2c91117204d745f3500d636a62f64f0ab3bae548aa53d423b0d1f27ebba6f5e5673a081d70cce7acfc48'
		},
	]
}

fn test_hkdf() {
	for i, tt in hkdf_tests() {
		master := decode(tt.master)
		salt := decode(tt.salt)
		prk_expected := decode(tt.prk)
		info := decode(tt.info)
		out_expected := decode(tt.out)

		prk := extract(tt.new_hash, master, salt)!
		assert prk == prk_expected, 'test ${i}: incorrect PRK: have ${prk}, need ${prk_expected}'

		derived_key := key(tt.new_hash, master, salt, info.bytestr(), out_expected.len)!
		assert derived_key == out_expected, 'test ${i}: incorrect output: have ${derived_key}, need ${out_expected}'

		expanded := expand(tt.new_hash, prk, info.bytestr(), out_expected.len)!
		assert expanded == out_expected, 'test ${i}: incorrect output from expand: have ${expanded}, need ${out_expected}'
	}
}

fn test_hkdf_limit() {
	master := []u8{len: 4, init: u8(index)}
	info := ''
	limit := sha1.new().size() * 255

	// The maximum output bytes should be extractable
	out := key(sha1_hash, master, []u8{}, info, limit)!
	assert out.len == limit

	// Reading one more should return an error
	if _ := key(sha1_hash, master, []u8{}, info, limit + 1) {
		assert false, 'expected key derivation to fail, but it succeeded'
	} else {
		assert err.msg() == 'hkdf: requested key length too large'
	}
}

fn test_direct_hash_constructor() {
	out := key(sha256.new, [u8(0), 1, 2, 3], []u8{}, 'context', sha256.size)!
	assert out.len == sha256.size
	assert out != []u8{len: sha256.size}
}

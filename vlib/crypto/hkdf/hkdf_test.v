module hkdf

import crypto.hmac
import crypto.md5
import crypto.sha1
import crypto.sha256
import crypto.sha512
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

// naive_extract is HKDF-Extract of RFC 5869, section 2.2, written on top of the
// one-shot `hmac.new`, used as a reference.
fn naive_extract(sum fn ([]u8) []u8, block_size int, hash_length int, salt []u8, ikm []u8) []u8 {
	xkey := if salt.len == 0 { []u8{len: hash_length} } else { salt }
	return hmac.new(xkey, ikm, sum, block_size)
}

// naive_expand is HKDF-Expand of RFC 5869, section 2.3, written on top of the
// one-shot `hmac.new`, used as a reference.
fn naive_expand(sum fn ([]u8) []u8, block_size int, prk []u8, info []u8, key_length int) []u8 {
	mut okm := []u8{}
	mut t := []u8{}
	for i := 1; okm.len < key_length; i++ {
		mut msg := t.clone()
		msg << info
		msg << u8(i)
		t = hmac.new(prk, msg, sum, block_size)
		okm << t
	}
	return okm[..key_length]
}

fn check_against_reference[H](name string, h fn () H, sum fn ([]u8) []u8, block_size int, hash_length int) ! {
	ikm := []u8{len: 22, init: u8(index + 1)}
	// salts shorter than, equal to and longer than the block size
	for salt_length in [0, 13, block_size, block_size + 1, 2 * block_size + 3] {
		salt := []u8{len: salt_length, init: u8(index * 7)}
		prk := naive_extract(sum, block_size, hash_length, salt, ikm)
		assert extract(h, ikm, salt)! == prk, '${name}: extract, salt.len=${salt_length}'
		for info in ['', 'some context info'] {
			okm := naive_expand(sum, block_size, prk, info.bytes(), 255 * hash_length)
			for key_length in [0, 1, hash_length - 1, hash_length, hash_length + 1,
				3 * hash_length + 5, 255 * hash_length] {
				assert expand(h, prk, info, key_length)! == okm[..key_length], '${name}: expand, salt.len=${salt_length} info=`${info}` L=${key_length}'
				assert key(h, ikm, salt, info, key_length)! == okm[..key_length], '${name}: key, salt.len=${salt_length} info=`${info}` L=${key_length}'
			}
		}
	}
	// a pseudorandom key longer than the block size is hashed first
	long_prk := []u8{len: 2 * block_size + 3, init: u8(index)}
	assert expand(h, long_prk, 'info', 2 * hash_length)! == naive_expand(sum, block_size,
		long_prk, 'info'.bytes(), 2 * hash_length), '${name}: expand with a long PRK'
}

// MinimalHash has only the methods that hkdf uses, so it does not implement
// `hash.Hash` and always takes the generic path.
struct MinimalHash {
mut:
	d &sha256.Digest
}

fn new_minimal_hash() &MinimalHash {
	return &MinimalHash{
		d: sha256.new()
	}
}

fn (mut m MinimalHash) write(p []u8) !int {
	return m.d.write(p)
}

fn (m &MinimalHash) sum(b []u8) []u8 {
	return m.d.sum(b)
}

fn (m &MinimalHash) size() int {
	return sha256.size
}

fn (m &MinimalHash) block_size() int {
	return sha256.block_size
}

fn test_matches_naive_reference() {
	// concrete digest constructors
	check_against_reference('sha1', sha1.new, sha1.sum, sha1.block_size, sha1.size)!
	check_against_reference('sha224', sha256.new224, sha256.sum224, sha256.block_size,
		sha256.size224)!
	check_against_reference('sha256', sha256.new, sha256.sum256, sha256.block_size, sha256.size)!
	check_against_reference('sha384', sha512.new384, sha512.sum384, sha512.block_size,
		sha512.size384)!
	check_against_reference('sha512', sha512.new, sha512.sum512, sha512.block_size, sha512.size)!
	check_against_reference('sha512_224', sha512.new512_224, sha512.sum512_224, sha512.block_size,
		sha512.size224)!
	check_against_reference('sha512_256', sha512.new512_256, sha512.sum512_256, sha512.block_size,
		sha512.size256)!
	// constructors returning the `hash.Hash` interface
	check_against_reference('hash.Hash sha1', sha1_hash, sha1.sum, sha1.block_size, sha1.size)!
	check_against_reference('hash.Hash sha224', fn () hash.Hash {
		return sha256.new224()
	}, sha256.sum224, sha256.block_size, sha256.size224)!
	check_against_reference('hash.Hash sha256', sha256_hash, sha256.sum256, sha256.block_size,
		sha256.size)!
	check_against_reference('hash.Hash sha384', fn () hash.Hash {
		return sha512.new384()
	}, sha512.sum384, sha512.block_size, sha512.size384)!
	check_against_reference('hash.Hash sha512', fn () hash.Hash {
		return sha512.new()
	}, sha512.sum512, sha512.block_size, sha512.size)!
	check_against_reference('hash.Hash sha512_224', fn () hash.Hash {
		return sha512.new512_224()
	}, sha512.sum512_224, sha512.block_size, sha512.size224)!
	check_against_reference('hash.Hash sha512_256', fn () hash.Hash {
		return sha512.new512_256()
	}, sha512.sum512_256, sha512.block_size, sha512.size256)!
	// the generic path: a `hash.Hash` without `copy_from`, and a type that is not a `hash.Hash`
	check_against_reference('md5', md5.new, md5.sum, md5.block_size, md5.size)!
	check_against_reference('MinimalHash', new_minimal_hash, sha256.sum256, sha256.block_size,
		sha256.size)!
}

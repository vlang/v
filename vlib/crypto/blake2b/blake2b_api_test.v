// Coverage for the blake2b convenience surface that blake2b_test.v does not
// reach: the 160/256/384-bit constructors and one-shot hashes, every keyed
// variant, new_digest parameter handling, and Digest.str.
//
// The 512-bit unkeyed rows for "abc" and "" are the published BLAKE2b values.
// Every other expectation was measured with this compiler and then confirmed
// against CPython 3.13's hashlib.blake2b, an independent implementation of the
// BLAKE2 specification, so the table is a cross-implementation check rather
// than a recording of this compiler's output.
module main

import crypto.blake2b
import encoding.hex

// UnkeyedVector is one (digest size, input, digest) triple. `input` is hex.
struct UnkeyedVector {
	size   int
	input  string
	output string
}

// KeyedVector adds the hex encoded MAC key.
struct KeyedVector {
	size   int
	input  string
	key    string
	output string
}

// BoundaryVector is a digest over `len` bytes of the pattern from `pattern`.
struct BoundaryVector {
	len    int
	output string
}

const fox = '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'

const key64 = '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f' +
	'202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'

const unkeyed_vectors = [
	UnkeyedVector{
		size:   blake2b.size512
		input:  ''
		output: '786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce'
	},
	UnkeyedVector{
		size:   blake2b.size512
		input:  '616263'
		output: 'ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923'
	},
	UnkeyedVector{
		size:   blake2b.size384
		input:  '616263'
		output: '6f56a82c8e7ef526dfe182eb5212f7db9df1317e57815dbda46083fc30f54ee6c66ba83be64b302d7cba6ce15bb556f4'
	},
	UnkeyedVector{
		size:   blake2b.size256
		input:  '616263'
		output: 'bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319'
	},
	UnkeyedVector{
		size:   blake2b.size160
		input:  '616263'
		output: '384264f676f39536840523f284921cdc68b6846b'
	},
	UnkeyedVector{
		size:   blake2b.size512
		input:  '00'
		output: '2fa3f686df876995167e7c2e5d74c4c7b6e48f8068fe0e44208344d480f7904c36963e44115fe3eb2a3ac8694c28bcb4f5a0f3276f2e79487d8219057a506e4b'
	},
	UnkeyedVector{
		size:   blake2b.size512
		input:  '0001020304050607'
		output: 'e998e0dc03ec30eb99bb6bfaaf6618acc620320d7220b3af2b23d112d8e9cb1262f3c0d60d183b1ee7f096d12dae42c958418600214d04f5ed6f5e718be35566'
	},
	UnkeyedVector{
		size:   blake2b.size512
		input:  fox
		output: 'a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918'
	},
	UnkeyedVector{
		size:   blake2b.size384
		input:  fox
		output: 'b7c81b228b6bd912930e8f0b5387989691c1cee1e65aade4da3b86a3c9f678fc8018f6ed9e2906720c8d2a3aeda9c03d'
	},
	UnkeyedVector{
		size:   blake2b.size256
		input:  fox
		output: '01718cec35cd3d796dd00020e0bfecb473ad23457d063b75eff29c0ffa2e58a9'
	},
	UnkeyedVector{
		size:   blake2b.size160
		input:  fox
		output: '3c523ed102ab45a37d54f5610d5a983162fde84f'
	},
]

const keyed_vectors = [
	KeyedVector{
		size:   blake2b.size512
		input:  '616263'
		key:    '6b'
		output: 'aa65cf292e7df1f7439b350072d55485083ccf55b149a400c8c0548233f46447d9f95242a31bf783081c997a6c26e086bc8c0f363dd0c03e8f8edfae0c4aa5ca'
	},
	KeyedVector{
		size:   blake2b.size384
		input:  '616263'
		key:    '6b'
		output: '87e3bc09578eec3129bcfe0a49d828f804fc411ccff622fb1c10a3cad995f1687e3a6acdac659d86ba432c5525cd0ab1'
	},
	KeyedVector{
		size:   blake2b.size256
		input:  '616263'
		key:    '6b'
		output: '12f0b4a482a321476483eac3387d86e810573152916fb35bf7a6b9951f221db3'
	},
	KeyedVector{
		size:   blake2b.size160
		input:  '616263'
		key:    '6b'
		output: '80c13d2f8ead0851ff032b67bac5384ee17f35c7'
	},
	KeyedVector{
		size:   blake2b.size512
		input:  '616263'
		key:    key64
		output: '06bbc3dedf13a31139498655251b7588ccd3bb5aaa071b2d44d8e0a04095579ed590fbfdcf941f4370ce5ce623624e7a76d33e7a8109dcda9b57d72f8f8efa51'
	},
	KeyedVector{
		size:   blake2b.size384
		input:  '616263'
		key:    key64
		output: '93043d2104d6cc8ad34b52d905288a06811559eb8e9f8892d79e2b181f91deb536923f536e6da57296e36d9cdb0aa74d'
	},
	KeyedVector{
		size:   blake2b.size256
		input:  '616263'
		key:    key64
		output: 'dff38c978666dff5631db35ca15535520d134f5c8060ea569c6a178ad393719f'
	},
	KeyedVector{
		size:   blake2b.size160
		input:  '616263'
		key:    key64
		output: 'f3464811aec9776024bd78c73dbaad63a62c509b'
	},
	KeyedVector{
		size:   blake2b.size512
		input:  ''
		key:    '6b'
		output: 'a393a0e4093eea8bfd03ebe262849654a10fbf67afc7f4f533efc0f992b33cbc574f32066446c2447ef23d5e86fabfd213b9eed79173ee8900909f2da52269cc'
	},
	KeyedVector{
		size:   blake2b.size160
		input:  ''
		key:    key64
		output: '15efac5a414effae1c5bc667974437c08cb07465'
	},
	KeyedVector{
		size:   blake2b.size512
		input:  fox
		key:    '6b6579'
		output: '66f642208454bf2e066dac9eab68fae0146bb544c1d46e1f427008f068a45d872cd0c1fc23e7ba82a95d084aadf5e4af9edaf761fb6ced9e485a28c59a3f714c'
	},
	KeyedVector{
		size:   blake2b.size384
		input:  fox
		key:    '6b6579'
		output: '7dd5bc8af8eaf5135df5014bda601faf0c744f9286718cc9edc9f74c20a07c3e8ac1b05ebdb21880ff420320d064b3ba'
	},
	KeyedVector{
		size:   blake2b.size256
		input:  fox
		key:    '6b6579'
		output: '27fbd5f2cdea2c98fa372a1a3b572a2f51c06bc627e306de84663f48c8b0eb13'
	},
	KeyedVector{
		size:   blake2b.size160
		input:  fox
		key:    '6b6579'
		output: '6dc7bc109586c90d88d501dc74207680dee0b56f'
	},
]

// block_size is 128, so these lengths straddle every write path boundary.
const boundary_vectors = [
	BoundaryVector{
		len:    1
		output: '2fa3f686df876995167e7c2e5d74c4c7b6e48f8068fe0e44208344d480f7904c36963e44115fe3eb2a3ac8694c28bcb4f5a0f3276f2e79487d8219057a506e4b'
	},
	BoundaryVector{
		len:    63
		output: 'd10bf9a15b1c9fc8d41f89bb140bf0be08d2f3666176d13baac4d381358ad074c9d4748c300520eb026daeaea7c5b158892fde4e8ec17dc998dcd507df26eb63'
	},
	BoundaryVector{
		len:    64
		output: '2fc6e69fa26a89a5ed269092cb9b2a449a4409a7a44011eecad13d7c4b0456602d402fa5844f1a7a758136ce3d5d8d0e8b86921ffff4f692dd95bdc8e5ff0052'
	},
	BoundaryVector{
		len:    65
		output: 'fcbe8be7dcb49a32dbdf239459e26308b84dff1ea480df8d104eeff34b46fae98627b450c2267d48c0946a697c5b59531452ac0484f1c84e3a33d0c339bb2e28'
	},
	BoundaryVector{
		len:    127
		output: 'b6292669ccd38d5f01caae96ba272c76a879a45743afa0725d83b9ebb26665b731f1848c52f11972b6644f554c064fa90780dbbbf3a89d4fc31f67df3e5857ef'
	},
	BoundaryVector{
		len:    128
		output: '2319e3789c47e2daa5fe807f61bec2a1a6537fa03f19ff32e87eecbfd64b7e0e8ccff439ac333b040f19b0c4ddd11a61e24ac1fe0f10a039806c5dcc0da3d115'
	},
	BoundaryVector{
		len:    129
		output: 'f59711d44a031d5f97a9413c065d1e614c417ede998590325f49bad2fd444d3e4418be19aec4e11449ac1a57207898bc57d76a1bcf3566292c20c683a5c4648f'
	},
	BoundaryVector{
		len:    200
		output: 'fb3c1f0f56a56f8e316fdf5d853c8c872c39635d083634c3904fc3ac07d1b578e85ff0e480e92d44ade33b62e893ee32343e79ddf6ef292e89b582d312502314'
	},
]

// pattern returns `n` bytes whose value at index i is i mod 256.
fn pattern(n int) []u8 {
	mut b := []u8{len: n}
	for i in 0 .. n {
		b[i] = u8(i)
	}
	return b
}

fn decode_hex(s string) []u8 {
	return hex.decode(s) or { panic('bad hex "${s}": ${err}') }
}

fn sum_with_size(data []u8, size int) []u8 {
	return match size {
		blake2b.size160 { blake2b.sum160(data) }
		blake2b.size256 { blake2b.sum256(data) }
		blake2b.size384 { blake2b.sum384(data) }
		blake2b.size512 { blake2b.sum512(data) }
		else { panic('unexpected digest size ${size}') }
	}
}

fn pmac_with_size(data []u8, key []u8, size int) []u8 {
	return match size {
		blake2b.size160 { blake2b.pmac160(data, key) }
		blake2b.size256 { blake2b.pmac256(data, key) }
		blake2b.size384 { blake2b.pmac384(data, key) }
		blake2b.size512 { blake2b.pmac512(data, key) }
		else { panic('unexpected digest size ${size}') }
	}
}

fn keyed_digest_with_size(key []u8, size int) !&blake2b.Digest {
	return match size {
		blake2b.size160 { blake2b.new_pmac160(key)! }
		blake2b.size256 { blake2b.new_pmac256(key)! }
		blake2b.size384 { blake2b.new_pmac384(key)! }
		blake2b.size512 { blake2b.new_pmac512(key)! }
		else { panic('unexpected digest size ${size}') }
	}
}

fn plain_digest_with_size(size int) !&blake2b.Digest {
	return match size {
		blake2b.size160 { blake2b.new160()! }
		blake2b.size256 { blake2b.new256()! }
		blake2b.size384 { blake2b.new384()! }
		blake2b.size512 { blake2b.new512()! }
		else { panic('unexpected digest size ${size}') }
	}
}

fn test_unkeyed_vectors() {
	for v in unkeyed_vectors {
		got := sum_with_size(decode_hex(v.input), v.size)
		assert got.len == v.size, 'input ${v.input}: got ${got.len} bytes, want ${v.size}'
		assert got == decode_hex(v.output), 'input ${v.input} size ${v.size}'
	}
}

fn test_keyed_vectors() {
	for v in keyed_vectors {
		got := pmac_with_size(decode_hex(v.input), decode_hex(v.key), v.size)
		assert got.len == v.size, 'input ${v.input}: got ${got.len} bytes, want ${v.size}'
		assert got == decode_hex(v.output), 'input ${v.input} key ${v.key} size ${v.size}'
	}
}

fn test_block_boundary_lengths() {
	for v in boundary_vectors {
		got := blake2b.sum512(pattern(v.len))
		assert got.len == blake2b.size512
		assert got == decode_hex(v.output), 'length ${v.len}'
	}
}

fn test_multi_block_input() {
	// 100000 bytes exercises the streaming loop well past the last block flush.
	big := pattern(100_000)
	got := blake2b.sum512(big)
	assert got == decode_hex('903faa04cbaa8c969a72dee2216e0ab460476493df672f8fb486ddfef43ffe3eafaa6db2be060d10269ecd84f592fe61af485a1bdb46913106f5921244b5d34d')
}

fn test_named_digest_constructors_match_sum() {
	for v in unkeyed_vectors {
		data := decode_hex(v.input)
		mut d := plain_digest_with_size(v.size)!
		d.write(data)!
		assert d.checksum() == sum_with_size(data, v.size), 'input ${v.input} size ${v.size}'
	}
}

fn test_named_keyed_digest_constructors_match_pmac() {
	for v in keyed_vectors {
		data := decode_hex(v.input)
		key := decode_hex(v.key)
		mut d := keyed_digest_with_size(key, v.size)!
		d.write(data)!
		assert d.checksum() == pmac_with_size(data, key, v.size), 'input ${v.input} size ${v.size}'
	}
}

fn test_new_digest_matches_named_constructors() {
	abc := 'abc'.bytes()
	for size in [blake2b.size160, blake2b.size256, blake2b.size384, blake2b.size512] {
		mut named := plain_digest_with_size(size)!
		named.write(abc)!

		mut generic := blake2b.new_digest(u8(size), []u8{})!
		generic.write(abc)!

		want := named.checksum()
		got := generic.checksum()
		assert got == want, 'digest size ${size}'
		assert got == sum_with_size(abc, size), 'digest size ${size}'
	}
}

fn test_new_digest_matches_keyed_constructors() {
	abc := 'abc'.bytes()
	key := decode_hex(key64)
	for size in [blake2b.size160, blake2b.size256, blake2b.size384, blake2b.size512] {
		mut generic := blake2b.new_digest(u8(size), key)!
		generic.write(abc)!
		assert generic.checksum() == pmac_with_size(abc, key, size), 'digest size ${size}'
	}
}

fn test_new_digest_accepts_every_size_1_to_64() {
	abc := 'abc'.bytes()
	for size in 1 .. blake2b.size512 + 1 {
		mut d := blake2b.new_digest(u8(size), []u8{})!
		d.write(abc)!
		assert d.checksum().len == size, 'digest size ${size}'
	}
	mut one := blake2b.new_digest(1, []u8{})!
	one.write(abc)!
	assert one.checksum() == [u8(0x6b)]

	mut two := blake2b.new_digest(2, []u8{})!
	two.write(abc)!
	assert two.checksum() == [u8(0xae), 0x1e]
}

fn test_new_digest_rejects_out_of_range_hash_size() {
	blake2b.new_digest(0, []u8{}) or {
		assert err.msg() == 'Hash size 0 must be between 1 and 64'
		return
	}
	assert false, 'hash size 0 was accepted'

	blake2b.new_digest(65, []u8{}) or {
		assert err.msg() == 'Hash size 65 must be between 1 and 64'
		return
	}
	assert false, 'hash size 65 was accepted'
}

fn test_new_digest_rejects_oversized_key() {
	blake2b.new_digest(blake2b.size512, []u8{len: 65}) or {
		assert err.msg() == 'Key size 65 must be between 0 and 64'
		return
	}
	assert false, 'a 65 byte key was accepted'
}

fn test_empty_key_is_the_same_as_no_key() {
	for v in unkeyed_vectors {
		keyed := blake2b.pmac512(decode_hex(v.input), []u8{})
		assert keyed == sum_with_size(decode_hex(v.input), blake2b.size512), 'input ${v.input}'

		mut d := blake2b.new_pmac512([]u8{})!
		d.write(decode_hex(v.input))!
		assert d.checksum() == sum_with_size(decode_hex(v.input), blake2b.size512), 'input ${v.input}'
	}
}

fn test_streaming_writes_match_one_shot() {
	abc := 'abc'.bytes()
	mut repeated := blake2b.new512()!
	for _ in 0 .. 3 {
		repeated.write(abc)!
	}
	assert repeated.checksum() == blake2b.sum512('abcabcabc'.bytes())

	mut byte_at_a_time := blake2b.new512()!
	for i in 0 .. abc.len {
		byte_at_a_time.write(abc[i..i + 1])!
	}
	assert byte_at_a_time.checksum() == blake2b.sum512(abc)

	// Across a block boundary, in irregular chunks.
	big := pattern(200)
	mut chunked := blake2b.new512()!
	for start := 0; start < big.len; start += 37 {
		end := if start + 37 < big.len { start + 37 } else { big.len }
		chunked.write(big[start..end])!
	}
	assert chunked.checksum() == blake2b.sum512(big)

	// A zero length write is a no-op, not an extra empty block.
	mut nothing := blake2b.new512()!
	nothing.write([]u8{})!
	assert nothing.checksum() == blake2b.sum512([]u8{})
}

fn test_keyed_block_boundary() {
	// A full length key fills one block on its own, so this also covers the
	// path where the input buffer is already full when more data arrives.
	key := decode_hex(key64)
	mut d := blake2b.new_pmac512(key)!
	d.write(pattern(128))!
	got := d.checksum()
	assert got == decode_hex('72065ee4dd91c2d8509fa1fc28a37c7fc9fa7d5b3f8ad3d0d7a25626b57b1b44788d4caf806290425f9890a3a2a35a905ab4b37acfd0da6e4517b2525c9651e4')
	assert got == blake2b.pmac512(pattern(128), key)
}

fn test_keyed_digest_pads_the_key_to_one_block() {
	mut d := blake2b.new_pmac512([u8(1), 2, 3])!
	s := d.str()
	// A short key occupies the start of a full 128 byte block, zero padded.
	assert s.contains('input_buffer.len: 128')
	assert s.contains('input_buffer: [1, 2, 3, 0, 0, 0,')
	assert s.contains('hash_size: 64')

	mut full := blake2b.new_pmac512(decode_hex(key64))!
	assert full.str().contains('input_buffer.len: 128')
}

fn test_digest_str() {
	fresh := blake2b.new512()!
	s := fresh.str()
	assert s.starts_with('&blake2b.Digest{')
	assert s.ends_with('}')
	assert s.contains('hash_size: 64')
	assert s.contains('input_buffer: []')
	assert s.contains('input_buffer.len: 0')
	assert s.contains('t: 0')
	// h[0] is IV[0] with the parameter block XORed in: 0x01010000 ^ digest_length.
	assert s.contains('h: [0x6a09e667f2bdc948,')

	mut written := blake2b.new512()!
	written.write('abc'.bytes())!
	w := written.str()
	assert w.contains('input_buffer: [97, 98, 99]')
	assert w.contains('input_buffer.len: 3')
	// t only advances when a block is flushed, so 3 buffered bytes still read 0.
	assert w.contains('t: 0')
}

fn test_truncated_digests_are_not_prefixes() {
	// The digest length is part of the BLAKE2b parameter block, so a shorter
	// digest is a different hash, not a truncation of the longer one.
	abc := 'abc'.bytes()
	full := blake2b.sum512(abc)
	assert blake2b.sum384(abc) != full[..blake2b.size384]
	assert blake2b.sum256(abc) != full[..blake2b.size256]
	assert blake2b.sum160(abc) != full[..blake2b.size160]
}

// NOTE: checksum finalizes the digest in place, so a second call hashes the
// zero padded remainder of the previous block and returns something else.
// That is what the implementation does today and no caller in the tree relies
// on it, so this pins the current contract rather than the desirable one.
fn test_checksum_is_single_use() {
	mut d := blake2b.new512()!
	d.write('abc'.bytes())!
	first := d.checksum()
	assert first == blake2b.sum512('abc'.bytes())
	assert d.checksum() != first
}

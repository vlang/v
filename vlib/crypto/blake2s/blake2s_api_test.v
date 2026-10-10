// Coverage for the blake2s convenience surface that blake2s_test.v does not
// reach: the 128/160/224-bit constructors and one-shot hashes, every keyed
// variant, new_digest parameter handling, and Digest.str.
//
// The 256-bit unkeyed row for "abc" is the vector published in RFC 7693
// Appendix A. Every other expectation was measured with this compiler and then
// confirmed against CPython 3.13's hashlib.blake2s, an independent RFC 7693
// implementation, so the table is a cross-implementation check rather than a
// recording of this compiler's output.
module main

import crypto.blake2s
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

const unkeyed_vectors = [
	UnkeyedVector{
		size:   blake2s.size256
		input:  ''
		output: '69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9'
	},
	// RFC 7693 Appendix A.
	UnkeyedVector{
		size:   blake2s.size256
		input:  '616263'
		output: '508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982'
	},
	UnkeyedVector{
		size:   blake2s.size224
		input:  '616263'
		output: '0b033fc226df7abde29f67a05d3dc62cf271ef3dfea4d387407fbd55'
	},
	UnkeyedVector{
		size:   blake2s.size160
		input:  '616263'
		output: '5ae3b99be29b01834c3b508521ede60438f8de17'
	},
	UnkeyedVector{
		size:   blake2s.size128
		input:  '616263'
		output: 'aa4938119b1dc7b87cbad0ffd200d0ae'
	},
	UnkeyedVector{
		size:   blake2s.size256
		input:  '00'
		output: 'e34d74dbaf4ff4c6abd871cc220451d2ea2648846c7757fbaac82fe51ad64bea'
	},
	UnkeyedVector{
		size:   blake2s.size256
		input:  '0001020304050607'
		output: 'c7e887b546623635e93e0495598f1726821996c2377705b93a1f636f872bfa2d'
	},
	UnkeyedVector{
		size:   blake2s.size256
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		output: '606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812'
	},
	UnkeyedVector{
		size:   blake2s.size224
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		output: 'e4e5cb6c7cae41982b397bf7b7d2d9d1949823ae78435326e8db4912'
	},
	UnkeyedVector{
		size:   blake2s.size160
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		output: '5a604fec9713c369e84b0ed68daed7d7504ef240'
	},
	UnkeyedVector{
		size:   blake2s.size128
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		output: '96fd07258925748a0d2fb1c8a1167a73'
	},
]

const keyed_vectors = [
	KeyedVector{
		size:   blake2s.size256
		input:  '616263'
		key:    '6b'
		output: 'b34bf0a0fd9106b51f4067e3b0e35e4dd53de2073d06d55e2db96786a2bbdc79'
	},
	KeyedVector{
		size:   blake2s.size224
		input:  '616263'
		key:    '6b'
		output: '57987259db58a3d9085d9a71dc4c3f8a50711b282900d1f1ce279bad'
	},
	KeyedVector{
		size:   blake2s.size160
		input:  '616263'
		key:    '6b'
		output: '8a598b6c353f80828a44668da0f6d0a082f9bf5e'
	},
	KeyedVector{
		size:   blake2s.size128
		input:  '616263'
		key:    '6b'
		output: 'f335237426ff5582d0c607ef240684b5'
	},
	KeyedVector{
		size:   blake2s.size256
		input:  '616263'
		key:    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		output: 'a281f725754969a702f6fe36fc591b7def866e4b70173ece402fc01c064d6b65'
	},
	KeyedVector{
		size:   blake2s.size224
		input:  '616263'
		key:    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		output: '68d9475f85fbe4cc53eba9b10c318cdd3b063150f3d7418ec0ffd5be'
	},
	KeyedVector{
		size:   blake2s.size160
		input:  '616263'
		key:    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		output: '3a2bef77b62bbf673ccf403ad0f8d2110e3147b9'
	},
	KeyedVector{
		size:   blake2s.size128
		input:  '616263'
		key:    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		output: '61ba5f165c194692e09d12520cc4c74a'
	},
	KeyedVector{
		size:   blake2s.size256
		input:  ''
		key:    '6b'
		output: 'e4db13614567e4ed83bed1b46d4d51717081c2eaf18d4b71fd85d487b9572929'
	},
	KeyedVector{
		size:   blake2s.size128
		input:  ''
		key:    '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'
		output: '9536f9b267655743dee97b8a670f9f53'
	},
	KeyedVector{
		size:   blake2s.size256
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		key:    '6b6579'
		output: 'eec94d00b8c9d214636adfad587bc9c75f271d7a64d9639ef2e959f94da468e6'
	},
	KeyedVector{
		size:   blake2s.size224
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		key:    '6b6579'
		output: 'e15c7bd304eeb5c610257c7a2804c8dd55be18f64ac2093bcb0da12c'
	},
	KeyedVector{
		size:   blake2s.size160
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		key:    '6b6579'
		output: 'b232657c51a5783677064dbe373fc04a0e608c16'
	},
	KeyedVector{
		size:   blake2s.size128
		input:  '54686520717569636b2062726f776e20666f78206a756d7073206f76657220746865206c617a7920646f67'
		key:    '6b6579'
		output: 'f7e1470b0b9afb58dcbafccb1b8b8530'
	},
]

// block_size is 64, so these lengths straddle every write path boundary.
const boundary_vectors = [
	BoundaryVector{
		len:    1
		output: 'e34d74dbaf4ff4c6abd871cc220451d2ea2648846c7757fbaac82fe51ad64bea'
	},
	BoundaryVector{
		len:    63
		output: 'e57cb79487dd57902432b250733813bd96a84efce59f650fac26e6696aefafc3'
	},
	BoundaryVector{
		len:    64
		output: '56f34e8b96557e90c1f24b52d0c89d51086acf1b00f634cf1dde9233b8eaaa3e'
	},
	BoundaryVector{
		len:    65
		output: '1b53ee94aaf34e4b159d48de352c7f0661d0a40edff95a0b1639b4090e974472'
	},
	BoundaryVector{
		len:    127
		output: 'f18417b39d617ab1c18fdf91ebd0fc6d5516bb34cf39364037bce81fa04cecb1'
	},
	BoundaryVector{
		len:    128
		output: '1fa877de67259d19863a2a34bcc6962a2b25fcbf5cbecd7ede8f1fa36688a796'
	},
	BoundaryVector{
		len:    129
		output: '5bd169e67c82c2c2e98ef7008bdf261f2ddf30b1c00f9e7f275bb3e8a28dc9a2'
	},
	BoundaryVector{
		len:    200
		output: '6d244e1a06ce4ef578dd0f63aff0936706735119ca9c8d22d86c801414ab9741'
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
		blake2s.size128 { blake2s.sum128(data) }
		blake2s.size160 { blake2s.sum160(data) }
		blake2s.size224 { blake2s.sum224(data) }
		blake2s.size256 { blake2s.sum256(data) }
		else { panic('unexpected digest size ${size}') }
	}
}

fn pmac_with_size(data []u8, key []u8, size int) []u8 {
	return match size {
		blake2s.size128 { blake2s.pmac128(data, key) }
		blake2s.size160 { blake2s.pmac160(data, key) }
		blake2s.size224 { blake2s.pmac224(data, key) }
		blake2s.size256 { blake2s.pmac256(data, key) }
		else { panic('unexpected digest size ${size}') }
	}
}

fn digest_with_size(key []u8, size int) !&blake2s.Digest {
	return match size {
		blake2s.size128 { blake2s.new_pmac128(key)! }
		blake2s.size160 { blake2s.new_pmac160(key)! }
		blake2s.size224 { blake2s.new_pmac224(key)! }
		blake2s.size256 { blake2s.new_pmac256(key)! }
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
		got := blake2s.sum256(pattern(v.len))
		assert got.len == blake2s.size256
		assert got == decode_hex(v.output), 'length ${v.len}'
	}
}

fn test_multi_block_input() {
	// 100000 bytes exercises the streaming loop well past the last block flush.
	big := pattern(100_000)
	got := blake2s.sum256(big)
	assert got == decode_hex('306171e0afc389321a65792391173662539d7a859bf19ab0f7c4b500edd42b48')
}

fn test_named_digest_constructors_match_sum() {
	for v in unkeyed_vectors {
		data := decode_hex(v.input)
		mut d := match v.size {
			blake2s.size128 { blake2s.new128()! }
			blake2s.size160 { blake2s.new160()! }
			blake2s.size224 { blake2s.new224()! }
			blake2s.size256 { blake2s.new256()! }
			else { panic('unexpected digest size ${v.size}') }
		}
		d.write(data)!
		assert d.checksum() == sum_with_size(data, v.size), 'input ${v.input} size ${v.size}'
	}
}

fn test_named_keyed_digest_constructors_match_pmac() {
	for v in keyed_vectors {
		data := decode_hex(v.input)
		key := decode_hex(v.key)
		mut d := digest_with_size(key, v.size)!
		d.write(data)!
		assert d.checksum() == pmac_with_size(data, key, v.size), 'input ${v.input} size ${v.size}'
	}
}

fn test_new_digest_matches_named_constructors() {
	abc := 'abc'.bytes()
	for size in [blake2s.size128, blake2s.size160, blake2s.size224, blake2s.size256] {
		mut named := match size {
			blake2s.size128 { blake2s.new128()! }
			blake2s.size160 { blake2s.new160()! }
			blake2s.size224 { blake2s.new224()! }
			else { blake2s.new256()! }
		}
		named.write(abc)!

		mut generic := blake2s.new_digest(u8(size), []u8{})!
		generic.write(abc)!

		want := named.checksum()
		got := generic.checksum()
		assert got == want, 'digest size ${size}'
		assert got == sum_with_size(abc, size), 'digest size ${size}'
	}
}

fn test_new_digest_matches_keyed_constructors() {
	abc := 'abc'.bytes()
	key := decode_hex('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f')
	for size in [blake2s.size128, blake2s.size160, blake2s.size224, blake2s.size256] {
		mut generic := blake2s.new_digest(u8(size), key)!
		generic.write(abc)!
		assert generic.checksum() == pmac_with_size(abc, key, size), 'digest size ${size}'
	}
}

fn test_new_digest_accepts_every_size_1_to_32() {
	abc := 'abc'.bytes()
	for size in 1 .. blake2s.size256 + 1 {
		mut d := blake2s.new_digest(u8(size), []u8{})!
		d.write(abc)!
		assert d.checksum().len == size, 'digest size ${size}'
	}
	mut one := blake2s.new_digest(1, []u8{})!
	one.write(abc)!
	assert one.checksum() == [u8(0x0d)]
}

fn test_new_digest_rejects_out_of_range_hash_size() {
	blake2s.new_digest(0, []u8{}) or {
		assert err.msg() == 'Hash size 0 must be between 1 and 32'
		return
	}
	assert false, 'hash size 0 was accepted'

	blake2s.new_digest(33, []u8{}) or {
		assert err.msg() == 'Hash size 33 must be between 1 and 32'
		return
	}
	assert false, 'hash size 33 was accepted'
}

fn test_new_digest_rejects_oversized_key() {
	blake2s.new_digest(blake2s.size256, []u8{len: 33}) or {
		assert err.msg() == 'Key size 33 must be between 0 and 32'
		return
	}
	assert false, 'a 33 byte key was accepted'
}

fn test_empty_key_is_the_same_as_no_key() {
	for v in unkeyed_vectors {
		keyed := blake2s.pmac256(decode_hex(v.input), []u8{})
		assert keyed == sum_with_size(decode_hex(v.input), blake2s.size256), 'input ${v.input}'

		mut d := blake2s.new_pmac256([]u8{})!
		d.write(decode_hex(v.input))!
		assert d.checksum() == sum_with_size(decode_hex(v.input), blake2s.size256), 'input ${v.input}'
	}
}

fn test_streaming_writes_match_one_shot() {
	abc := 'abc'.bytes()
	mut repeated := blake2s.new256()!
	for _ in 0 .. 3 {
		repeated.write(abc)!
	}
	assert repeated.checksum() == blake2s.sum256('abcabcabc'.bytes())

	mut byte_at_a_time := blake2s.new256()!
	for i in 0 .. abc.len {
		byte_at_a_time.write(abc[i..i + 1])!
	}
	assert byte_at_a_time.checksum() == blake2s.sum256(abc)

	// Across a block boundary, in irregular chunks.
	big := pattern(200)
	mut chunked := blake2s.new256()!
	for start := 0; start < big.len; start += 37 {
		end := if start + 37 < big.len { start + 37 } else { big.len }
		chunked.write(big[start..end])!
	}
	assert chunked.checksum() == blake2s.sum256(big)

	// A zero length write is a no-op, not an extra empty block.
	mut nothing := blake2s.new256()!
	nothing.write([]u8{})!
	assert nothing.checksum() == blake2s.sum256([]u8{})
}

fn test_keyed_digest_pads_the_key_to_one_block() {
	mut d := blake2s.new_pmac256([u8(1), 2, 3])!
	s := d.str()
	// A short key occupies the start of a full 64 byte block, zero padded.
	assert s.contains('input_buffer.len: 64')
	assert s.contains('input_buffer: [1, 2, 3, 0, 0, 0,')
	assert s.contains('hash_size: 32')

	// A full length key still lands in exactly one block.
	mut full := blake2s.new_pmac256(decode_hex('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f'))!
	assert full.str().contains('input_buffer.len: 64')
}

fn test_digest_str() {
	fresh := blake2s.new256()!
	s := fresh.str()
	assert s.starts_with('&blake2s.Digest{')
	assert s.ends_with('}')
	assert s.contains('hash_size: 32')
	assert s.contains('input_buffer: []')
	assert s.contains('input_buffer.len: 0')
	assert s.contains('t: 0')
	// h[0] is IV[0] with the parameter block XORed in: 0x01010000 ^ digest_length.
	assert s.contains('h: [0x000000006b08e647,')

	mut written := blake2s.new256()!
	written.write('abc'.bytes())!
	w := written.str()
	assert w.contains('input_buffer: [97, 98, 99]')
	assert w.contains('input_buffer.len: 3')
	// t only advances when a block is flushed, so 3 buffered bytes still read 0.
	assert w.contains('t: 0')
}

fn test_truncated_digests_are_not_prefixes() {
	// The digest length is part of the BLAKE2s parameter block, so a shorter
	// digest is a different hash, not a truncation of the longer one.
	abc := 'abc'.bytes()
	full := blake2s.sum256(abc)
	assert blake2s.sum224(abc) != full[..blake2s.size224]
	assert blake2s.sum160(abc) != full[..blake2s.size160]
	assert blake2s.sum128(abc) != full[..blake2s.size128]
}

// NOTE: checksum finalizes the digest in place, so a second call hashes the
// zero padded remainder of the previous block and returns something else.
// That is what the implementation does today and no caller in the tree relies
// on it, so this pins the current contract rather than the desirable one.
fn test_checksum_is_single_use() {
	mut d := blake2s.new256()!
	d.write('abc'.bytes())!
	first := d.checksum()
	assert first == blake2s.sum256('abc'.bytes())
	assert d.checksum() != first
}

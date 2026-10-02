// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha256
import hash

// verify sha256.Digest implements hash.Hash
fn test_digest_implements_hash() {
	get_digest := fn () hash.Hash {
		return sha256.new()
	}
	mut digest := get_digest()
	assert digest.size() == sha256.size
	digest.free()
}

fn test_crypto_sha256() {
	assert sha256.sum('This is a sha256 checksum.'.bytes()).hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_writer() {
	mut digest := sha256.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	mut sum := digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
	sum = digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_writer_reset() {
	mut digest := sha256.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	_ = digest.sum([])
	digest.reset()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha256 checksum.'.bytes()) or { assert false }
	sum := digest.sum([])
	assert sum.hex() == 'dc7163299659529eae29683eb1ffec50d6c8fc7275ecb10c145fde0e125b8727'
}

fn test_crypto_sha256_224() {
	data := 'hello world\n'.bytes()
	mut digest := sha256.new224()
	expected := '95041dd60ab08c0bf5636d50be85fe9790300f39eb84602858a9b430'

	// with sum224 function
	sum224 := sha256.sum224(data)
	assert sum224.hex() == expected

	// with sum
	_ := digest.write(data)!
	sum := digest.sum([])
	assert sum.hex() == expected

	// with checksum
	digest.reset()
	_ := digest.write(data)!
	chksum := digest.sum([])
	assert chksum.hex() == expected
}

fn test_checksum_into_matches_sum() {
	for n in 0 .. 200 {
		data := []u8{len: n, init: u8(index * 7 + 1)}
		mut d := sha256.new()
		d.write(data)!
		// a longer buffer keeps its bytes after the checksum
		mut out := []u8{len: sha256.size + 3, init: 0xaa}
		d.checksum_into(mut out)
		assert out[..sha256.size] == sha256.sum256(data), 'n=${n}'
		assert out[sha256.size..] == [u8(0xaa), 0xaa, 0xaa]

		mut d224 := sha256.new224()
		d224.write(data)!
		mut out224 := []u8{len: sha256.size224}
		d224.checksum_into(mut out224)
		assert out224 == sha256.sum224(data), 'n=${n}'
	}
}

fn test_copy_from() {
	data := 'The quick brown fox jumps over the lazy dog, again and again and again.'.bytes()
	for split in [0, 1, 55, 56, 63, 64, 65, data.len] {
		for is224 in [false, true] {
			mut src := if is224 { sha256.new224() } else { sha256.new() }
			src.write(data[..split])!
			// `dst` starts as the other variant, with unrelated data in it
			mut dst := if is224 { sha256.new() } else { sha256.new224() }
			dst.write('unrelated'.bytes())!
			dst.copy_from(src)
			dst.write(data[split..])!
			mut out := []u8{len: dst.size()}
			dst.checksum_into(mut out)
			expected := if is224 { sha256.sum224(data) } else { sha256.sum256(data) }
			assert out == expected, 'split=${split} is224=${is224}'
			// `src` is not changed by copy_from
			assert src.sum([]) == if is224 {
				sha256.sum224(data[..split])
			} else {
				sha256.sum256(data[..split])
			}
		}
	}
}

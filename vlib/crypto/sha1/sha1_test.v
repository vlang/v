// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.sha1
import hash

// verify sha1.Digest implements hash.Hash
fn test_digest_implements_hash() {
	get_digest := fn () hash.Hash {
		return sha1.new()
	}
	mut digest := get_digest()
	assert digest.size() == sha1.size
	digest.free()
}

fn test_crypto_sha1() {
	assert sha1.sum('This is a sha1 checksum.'.bytes()).hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}

fn test_crypto_sha1_writer() {
	mut digest := sha1.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha1 checksum.'.bytes()) or { assert false }
	mut sum := digest.sum([])
	assert sum.hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
	sum = digest.sum([])
	assert sum.hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}

fn test_crypto_sha1_writer_reset() {
	mut digest := sha1.new()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha1 checksum.'.bytes()) or { assert false }
	_ = digest.sum([])
	digest.reset()
	digest.write('This is a'.bytes()) or { assert false }
	digest.write(' sha1 checksum.'.bytes()) or { assert false }
	sum := digest.sum([])
	assert sum.hex() == 'e100d74442faa5dcd59463b808983c810a8eb5a1'
}

fn test_checksum_into_matches_sum() {
	for n in 0 .. 200 {
		data := []u8{len: n, init: u8(index * 7 + 1)}
		mut d := sha1.new()
		d.write(data)!
		// a longer buffer keeps its bytes after the checksum
		mut out := []u8{len: sha1.size + 3, init: 0xaa}
		d.checksum_into(mut out)
		assert out[..sha1.size] == sha1.sum(data), 'n=${n}'
		assert out[sha1.size..] == [u8(0xaa), 0xaa, 0xaa]
	}
}

fn test_copy_from() {
	data := 'The quick brown fox jumps over the lazy dog, again and again and again.'.bytes()
	for split in [0, 1, 55, 56, 63, 64, 65, data.len] {
		mut src := sha1.new()
		src.write(data[..split])!
		// `dst` starts with unrelated data in it
		mut dst := sha1.new()
		dst.write('unrelated'.bytes())!
		dst.copy_from(src)
		dst.write(data[split..])!
		mut out := []u8{len: sha1.size}
		dst.checksum_into(mut out)
		assert out == sha1.sum(data), 'split=${split}'
		// `src` is not changed by copy_from
		assert src.sum([]) == sha1.sum(data[..split])
	}
}

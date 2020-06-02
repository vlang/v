// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import crypto.rand

fn get_random_bytes(no_bytes int) []byte {
	r := rand.read(no_bytes) or {
		assert false
		return []
	}
	assert r.len == no_bytes
	return r
}

fn test_crypto_rand_read() {
	no_bytes := 100
	r1 := get_random_bytes(no_bytes)
	r2 := get_random_bytes(no_bytes)
	mut equals := 0
	for i in 0 .. r1.len {
		if r1[i] == r2[i] {
			equals++
		}
	}
	assert (100.0 * f32(equals) / f32(no_bytes)) < 20.0
}

fn test_crypto_rand_int_u64() {
	max := u64(160)
	mut unique := []u64{}
	for _ in 0 .. 80 {
		r := rand.int_u64(max) or {
			assert false
			return
		}
		if r >= max {
			assert false
		}
		if r !in unique {
			unique << r
		}
	}
	assert unique.len >= 10
}

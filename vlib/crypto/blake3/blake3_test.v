// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

import encoding.hex
import json
import os

struct TestObject {
	comment        string
	key            string
	context_string string
	cases          []TestVectors
}

struct TestVectors {
	input_len  u64
	hash       string
	keyed_hash string
	derive_key string
}

const object_string = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata',
	'test_vectors.json'))) or { panic(err) }

const test_object = json.decode(TestObject, object_string) or { panic(err) }

const data_segment = []u8{len: 251, cap: 251, init: u8(index)}

fn test_run_test_vectors() {
	mut data := []u8{}
	for i in 0 .. 408 {
		data << data_segment
	}

	for case in test_object.cases {
		extended_length := u64(case.hash.len)

		// test Blake3 hash

		mut hash_d := Digest.new_hash() or {
			assert false, 'Digest.new_hash error: ${err}'
			return
		}

		hash_d.write(data[..case.input_len]) or {
			assert false, 'hash_d.write error: ${err}'
			return
		}

		hash_bytes := hex.decode(case.hash) or {
			assert false, 'invalid hex string: ${err}'
			return
		}

		// test various sizes and see if we can ask for different
		// sizes without recomputing the hash

		for i in [u64(16), 20, 24, 28, 32, 48, 64, 72, 96] {
			if hash_bytes.len > i {
				assert hash_d.checksum(i) == hash_bytes[..i], 'hash failed output length ${i}'
			}
		}

		assert hash_d.checksum(u64(hash_bytes.len)) == hash_bytes, 'hash failed output length ${extended_length}'

		// test Blake3 keyed hash

		mut keyed_hash_d := Digest.new_keyed_hash(test_object.key.bytes()) or {
			assert false, 'Digest.new_keyed_hash error: ${err}'
			return
		}

		keyed_hash_d.write(data[..case.input_len]) or {
			assert false, 'keyed_hash_d.write error: ${err}'
			return
		}

		keyed_hash_bytes := hex.decode(case.keyed_hash) or {
			assert false, 'invalid hex string: ${err}'
			return
		}

		// test various sizes and see if we can ask for different
		// sizes without recomputing the hash

		for i in [u64(16), 20, 24, 28, 32, 48, 64, 72, 96] {
			if keyed_hash_bytes.len > i {
				assert keyed_hash_d.checksum(i) == keyed_hash_bytes[..i], 'keyed hash failed output length ${i}'
			}
		}

		assert keyed_hash_d.checksum(u64(keyed_hash_bytes.len)) == keyed_hash_bytes, 'keyed hash failed output length ${extended_length}'
		// test Blake3 derive key hash

		mut derive_key_hash_d := Digest.new_derive_key_hash(test_object.context_string.bytes()) or {
			assert false, 'Digest.new_derive_key_hash error: ${err}'
			return
		}

		derive_key_hash_d.write(data[..case.input_len]) or {
			assert false, 'derive_key_hash_d.write error: ${err}'
			return
		}

		derive_key_hash_bytes := hex.decode(case.derive_key) or {
			assert false, 'invalid hex string: ${err}'
			return
		}

		// test various sizes and see if we can ask for different
		// sizes without recomputing the hash

		for i in [u64(16), 20, 24, 28, 32, 48, 64, 72, 96] {
			if derive_key_hash_bytes.len > i {
				assert derive_key_hash_d.checksum(i) == derive_key_hash_bytes[..i], 'derive key hash failed output length ${i}'
			}
		}

		assert derive_key_hash_d.checksum(u64(derive_key_hash_bytes.len)) == derive_key_hash_bytes, 'derive key hash failed output length ${extended_length}'
	}
}

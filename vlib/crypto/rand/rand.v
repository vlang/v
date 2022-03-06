// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

struct ReadError {
	Error
}

pub fn (err ReadError) msg() string {
	return 'crypto.rand.read() error reading random bytes'
}

// bytes returns an array of `bytes_needed` random bytes.
// Note: this call can block your program for a long period of time,
// if your system does not have access to enough entropy.
// See also rand.bytes(), if you do not need really random bytes,
// but instead pseudo random ones, from a pseudo random generator
// that can be seeded, and that is usually faster.
pub fn bytes(bytes_needed int) ?[]byte {
	return read(bytes_needed)
}

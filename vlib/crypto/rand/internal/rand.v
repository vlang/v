// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

struct ReadError {
	Error
}

// msg returns the error message.
pub fn (err ReadError) msg() string {
	return 'crypto.rand.read() error reading random bytes'
}

// bytes returns an array filled from the operating system's random source.
pub fn bytes(bytes_needed int) ![]u8 {
	if bytes_needed < 0 {
		return error('can not read < 0 random bytes')
	}
	mut buffer := []u8{len: bytes_needed}
	read(mut buffer)!
	return buffer
}

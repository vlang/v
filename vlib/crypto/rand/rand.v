// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import crypto.rand.internal

// bytes returns an array of `bytes_needed` random bytes.
// Note: this call can block your program for a long period of time,
// if your system does not have access to enough entropy.
// See also rand.bytes(), if you do not need really random bytes,
// but instead pseudo random ones, from a pseudo random generator
// that can be seeded, and that is usually faster.
pub fn bytes(bytes_needed int) ![]u8 {
	return internal.bytes(bytes_needed)
}

// read fills `buffer` with random bytes from the OS.
// On Android, reads require the getrandom syscall (Linux 3.17 or a kernel backport).
// They may block until the kernel's entropy pool is initialized. If the syscall
// is unavailable or denied, an error is returned; no weaker random source is used.
pub fn read(mut buffer []u8) ! {
	internal.read(mut buffer)!
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

// read fills `buffer` with random bytes from the OS.
pub fn read(mut buffer []u8) ! {
	return error('crypto.rand.read is not implemented on this platform')
}

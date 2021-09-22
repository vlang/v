// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

// read returns an array of `bytes_needed` random bytes read from the OS.
pub fn read(bytes_needed int) ?[]byte {
	return error('rand.read is not implemented on this platform')
}

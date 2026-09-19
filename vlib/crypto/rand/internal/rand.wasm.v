// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

// read reports that OS entropy is not implemented by the WASM backend.
pub fn read(mut buffer []u8) ! {
	_ = buffer
	return error('crypto.rand.read is not implemented on the WASM backend')
}

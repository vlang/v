// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module internal

// read fills `buffer` with random bytes from the Web Crypto API.
pub fn read(mut buffer []u8) ! {
	#const crypto_source = typeof globalThis.crypto !== 'undefined' ? globalThis.crypto : (typeof require === 'function' ? require('crypto').webcrypto : undefined)
	#if (!crypto_source || typeof crypto_source.getRandomValues !== 'function') return error(new string('crypto.rand.read is not implemented on this platform'))
	#try {
	#const random_values = new Uint8Array(buffer.val.len.valueOf())
	#for (let offset = 0; offset < random_values.length; offset += 65536) crypto_source.getRandomValues(random_values.subarray(offset, offset + 65536))
	#for (let i = 0; i < random_values.length; i++) buffer.val.arr.arr[i] = new u8(random_values[i])
	#} catch (e) { return error(new string('' + e)); }
}

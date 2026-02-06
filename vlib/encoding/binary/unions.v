// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module binary

// Unions used across the little endian/big endian conversion routines.

union U16 {
mut:
	b [2]u8
	u u16
}

union U32 {
mut:
	b [4]u8
	u u32
}

union U64 {
mut:
	b [8]u8
	u u64
}

union F32 {
mut:
	b [4]u8
	u f32
}

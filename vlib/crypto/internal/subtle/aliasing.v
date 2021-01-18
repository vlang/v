// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package subtle implements functions that are often useful in cryptographic
// code but require careful thought to use correctly.
module subtle

// NOTE: require unsafe in future
// any_overlap reports whether x and y share memory at any (not necessarily
// corresponding) index. The memory beyond the slice length is ignored.
pub fn any_overlap(x []byte, y []byte) bool {
	// NOTE: Remember to come back to this (joe-c)
	return x.len > 0 && y.len > 0 &&  // &x.data[0] <= &y.data[y.len-1] &&
	// &y.data[0] <= &x.data[x.len-1]
	unsafe {&x[0] <= &y[y.len - 1] && &y[0] <= &x[x.len - 1]}
}

// inexact_overlap reports whether x and y share memory at any non-corresponding
// index. The memory beyond the slice length is ignored. Note that x and y can
// have different lengths and still not have any inexact overlap.
//
// inexact_overlap can be used to implement the requirements of the crypto/cipher
// AEAD, Block, BlockMode and Stream interfaces.
pub fn inexact_overlap(x []byte, y []byte) bool {
	if x.len == 0 || y.len == 0 || unsafe {&x[0] == &y[0]} {
		return false
	}
	return any_overlap(x, y)
}

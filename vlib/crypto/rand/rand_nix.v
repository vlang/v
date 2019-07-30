// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

import math

#include <sys/syscall.h>

import const(
	SYS_getrandom
)

// const (
// 	SYS_getrandom = 278 // AArch65
// 	SYS_getrandom = 384 // ARM
// 	SYS_getrandom = 355 // x86
// 	SYS_getrandom = 318 // x86_64
// )

const (
	ReadBatchSize = 256
)

pub fn read(bytes_needed int) ?[]byte {	
	mut buffer := malloc(bytes_needed)
	mut bytes_read := 0
	// getrandom syscall wont block if requesting <= 256 bytes
	if bytes_needed > ReadBatchSize {
		no_batches := int(math.floor(f64(bytes_needed/ReadBatchSize)))
		for i:=0; i<no_batches; i++ {
			if _getrandom(ReadBatchSize, buffer+bytes_read) == -1 {
				return ReadError
			}
			bytes_read += ReadBatchSize
		}
	}
	if _getrandom(bytes_needed-bytes_read, buffer+bytes_read) == -1 {
		return ReadError
	}
	
	return c_array_to_bytes_tmp(bytes_needed, buffer)
}

fn _getrandom(bytes_needed int, buffer byteptr) int {
	if bytes_needed > ReadBatchSize {
		panic('_getrandom() dont request more thane $ReadBatchSize bytes at once.')
	}
	return C.syscall(SYS_getrandom, buffer, bytes_needed, 0)
}

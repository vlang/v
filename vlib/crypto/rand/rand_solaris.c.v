// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module rand

import math

#include <sys/random.h>

fn C.getrandom(p byteptr, n size_t, flags u32) int

const (
	read_batch_size = 256
)

pub fn read(bytes_needed int) ?[]byte {	
	mut buffer := &byte(0)
	unsafe {
	   buffer = malloc(bytes_needed)
	}
	mut bytes_read := 0
	// getrandom syscall wont block if requesting <= 256 bytes
	if bytes_needed > read_batch_size {
		no_batches := int(math.floor(f64(bytes_needed/read_batch_size)))
		for i:=0; i<no_batches; i++ {
			if v_getrandom(read_batch_size, buffer+bytes_read) == -1 {
				return read_error
			}
			bytes_read += read_batch_size
		}
	}
	if v_getrandom(bytes_needed-bytes_read, buffer+bytes_read) == -1 {
		return read_error
	}
	
	return c_array_to_bytes_tmp(bytes_needed, buffer)
}

fn v_getrandom(bytes_needed int, buffer voidptr) int {
	if bytes_needed > read_batch_size {
		panic('getrandom() dont request more than $read_batch_size bytes at once.')
	}
	return C.getrandom(buffer, bytes_needed, 0)
}

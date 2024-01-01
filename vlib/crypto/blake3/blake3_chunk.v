// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

import encoding.binary

struct Chunk {
mut:
	chunk_number   u64
	chaining_value []u32
	block_words    []u32
	block_len      u32
	flags          u32
}

fn (c Chunk) str() string {
	return 'Chunk {\n    chunk_number: ${c.chunk_number}\n    chaining_value: ${c.chaining_value[0]:08x} ${c.chaining_value[1]:08x} ${c.chaining_value[2]:08x} ${c.chaining_value[3]:08x}   ${c.chaining_value[4]:08x} ${c.chaining_value[5]:08x} ${c.chaining_value[6]:08x} ${c.chaining_value[7]:08x}\n       block_words: ${c.block_words[0]:08x} ${c.block_words[1]:08x} ${c.block_words[2]:08x} ${c.block_words[3]:08x}   ${c.block_words[4]:08x} ${c.block_words[5]:08x} ${c.block_words[6]:08x} ${c.block_words[7]:08x}\n                    ${c.block_words[8]:08x} ${c.block_words[9]:08x} ${c.block_words[10]:08x} ${c.block_words[11]:08x}   ${c.block_words[12]:08x} ${c.block_words[13]:08x} ${c.block_words[14]:08x} ${c.block_words[15]:08x}\n         block_len: ${c.block_len}\n             flags: ${c.flags:08x}'
}

// process_input handles up to 1024 bytes of input
//
// A chunk consists of 0 to 1024 bytes of input data.  This
// method is only called when we have 1024 bytes of data
// or when we are processing the last chunk and there is
// less than 1024 bytes.
//
// The only time that it is legal to have 0 bytes input is
// when we are processing chunk 0.  If we are processing
// any other chunk, we panic because this is a private
// method and if we are passing in 0 bytes on some chunk
// other than 0, there is an internal algorithm bug.
//
// If this method is passed more than 1024 bytes of input data,
// we panic because this also is an internal algorithm bug.
//
// After this method returns, all the input data is processed
// into the chunk and the 16 32-bit words of the compression
// state is returned.  These 16 words can either form the
// chaining value for the chunk or the block_words used in
// generating the output hash from the root node.
//
// If this chunk is also the root node, the caller needs to
// set root to true.
//
// As a potential speed up, we could try spawning this function
// in a concurrent task and see if it is worth the overhead.
fn (mut c Chunk) process_input(input []u8, key_words []u32, counter u64, flags u32, root bool) []u32 {
	mut remaining_input := unsafe { input[..] }

	if remaining_input.len == 0 && counter != 0 {
		panic('trying to process 0 bytes in chunk ${counter}')
	}

	if remaining_input.len > chunk_size {
		panic('trying to process ${remaining_input.len} bytes in chunk ${counter}')
	}

	c.chunk_number = counter
	c.chaining_value = key_words.clone()
	c.block_words = []u32{len: 16, cap: 16, init: 0}

	for i in 0 .. 16 {
		c.block_len = u32(block_size)
		c.flags = flags | if i == 0 { u32(Flags.chunk_start) } else { u32(0) }

		if remaining_input.len <= block_size {
			c.block_len = u32(remaining_input.len)

			for remaining_input.len < block_size {
				remaining_input << u8(0)
			}

			c.flags |= u32(Flags.chunk_end) | if root { u32(Flags.root) } else { u32(0) }
		}

		for j in 0 .. 16 {
			c.block_words[j] = binary.little_endian_u32_at(remaining_input, j * 4)
		}

		remaining_input = unsafe { remaining_input[block_size..] }

		words := f(c.chaining_value, c.block_words, c.chunk_number, c.block_len, c.flags)

		if c.flags & u32(Flags.chunk_end) == 0 {
			c.chaining_value = words[..8]
		} else {
			return words
		}
	}

	panic('processing more than 16 ${block_size} byte blocks in chunk ${c.chunk_number}')
}

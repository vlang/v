// Copyright (c) 2023 Kim Shrier. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Package blake3 implements the Blake3 cryptographic hash
// as described in:
// https://github.com/BLAKE3-team/BLAKE3-specs/blob/master/blake3.pdf
// Version 20211102173700

module blake3

import encoding.binary

// size256 is the size, in bytes, of a Blake3 256 checksum.
pub const size256 = 32

// key_length is the length, in bytes, of a Blake3 key
pub const key_length = 32

// block_size is the block size, in bytes, of the Blake3 hash functions.
pub const block_size = 64

// chunk_size is the chunk size, in bytes, of the Blake3 hash functions.
// A chunk consists of 16 blocks.
pub const chunk_size = 1024

// G rotation constants
const r1 = 16
const r2 = 12
const r3 = 8
const r4 = 7

// negative G rotation constants so we can rotate right.
const nr1 = -1 * r1
const nr2 = -1 * r2
const nr3 = -1 * r3
const nr4 = -1 * r4

// initialization vector
const iv = [
	u32(0x6a09e667),
	u32(0xbb67ae85),
	u32(0x3c6ef372),
	u32(0xa54ff53a),
	u32(0x510e527f),
	u32(0x9b05688c),
	u32(0x1f83d9ab),
	u32(0x5be0cd19),
]

// message word schedule permutations
const sigma = [
	[u8(0), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
	[u8(2), 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8],
	[u8(3), 4, 10, 12, 13, 2, 7, 14, 6, 5, 9, 0, 11, 15, 8, 1],
	[u8(10), 7, 12, 9, 14, 3, 13, 15, 4, 0, 11, 2, 5, 8, 1, 6],
	[u8(12), 13, 9, 11, 15, 10, 14, 8, 7, 2, 5, 3, 0, 1, 6, 4],
	[u8(9), 14, 11, 5, 8, 12, 15, 1, 13, 3, 0, 10, 2, 6, 4, 7],
	[u8(11), 15, 5, 0, 1, 9, 8, 6, 14, 10, 2, 12, 3, 4, 7, 13],
]

// internal flags
enum Flags as u32 {
	chunk_start         = 1 << 0
	chunk_end           = 1 << 1
	parent              = 1 << 2
	root                = 1 << 3
	keyed_hash          = 1 << 4
	derive_key_context  = 1 << 5
	derive_key_material = 1 << 6
}

struct KeyWordsLengthError {
	Error
	length u32
}

fn (err KeyWordsLengthError) msg() string {
	return 'key_words length, ${err.length} bytes, must be 8 32-bit words'
}

struct DigestFlagsError {
	Error
	flags u32
}

fn (err DigestFlagsError) msg() string {
	lines := [
		'Digest flags ${err.flags:08x} must either be 0 or have only one bit set.',
		'legal bits are:',
		'    keyed_hash          ${u32(Flags.keyed_hash):08x}',
		'    derive_key_context  ${u32(Flags.derive_key_context):08x}',
		'    derive_key_material ${u32(Flags.derive_key_material):08x}',
	]

	return lines.join('\n')
}

// empty tree node
struct Empty {}

// parent node containing the propagated chaining value
struct Node {
	chaining_value []u32
}

fn (n Node) str() string {
	return 'Node chaining_value: ${n.chaining_value[0]:08x} ${n.chaining_value[1]:08x} ${n.chaining_value[2]:08x} ${n.chaining_value[3]:08x}   ${n.chaining_value[4]:08x} ${n.chaining_value[5]:08x} ${n.chaining_value[6]:08x} ${n.chaining_value[7]:08x}'
}

type TreeNode = Empty | Node

// data needed to generate arbitrary amounts of hash output
struct HashState {
mut:
	words          []u32
	chaining_value []u32
	block_words    []u32
	block_len      u32
	flags          u32
}

// Digest holds the state needed to compute a Blake3 hash
struct Digest {
	key_words []u32 // these form the initial chaining value of a chunk
	flags     u32   // only the keyed_hash, derive_key_context, or derive_key_material bits
mut:
	chunk_counter u64        // number of the next chunk to be created
	input         []u8       // unconsumed input
	binary_edge   []TreeNode // the right-hand edge of the binary tree
}

// Digest.new_hash initializes a Digest structure for a Blake3 hash
pub fn Digest.new_hash() !Digest {
	return Digest.new(iv, 0)
}

// Digest.new_keyed_hash initializes a Digest structure for a Blake3 keyed hash
pub fn Digest.new_keyed_hash(key []u8) !Digest {
	// treat the key bytes as little endian u32 values
	mut key_words := []u32{len: 8, cap: 8}
	for i in 0 .. 8 {
		key_words[i] = binary.little_endian_u32_at(key, i * 4)
	}

	return Digest.new(key_words, u32(Flags.keyed_hash))
}

// Digest.new_derive_key_hash initializes a Digest structure for deriving a Blake3 key
pub fn Digest.new_derive_key_hash(context []u8) !Digest {
	mut context_digest := Digest.new(iv, u32(Flags.derive_key_context))!

	context_digest.write(context)!
	context_key := context_digest.checksum_internal(key_length)

	// treat the context key bytes as little endian u32 values
	mut key_words := []u32{len: 8, cap: 8}
	for i in 0 .. 8 {
		key_words[i] = binary.little_endian_u32_at(context_key, i * 4)
	}

	return Digest.new(key_words, u32(Flags.derive_key_material))
}

fn Digest.new(key_words []u32, flags u32) !Digest {
	if key_words.len != 8 {
		return KeyWordsLengthError{
			length: u32(key_words.len)
		}
	}

	// flags must be 0 for performing a blake3 hash.  Other bits modify
	// the type of hash performed.  Only 1 of the keyed_hash,
	// derive_key_context, or derive_key_material bits can be set.
	// Having other bits or multiple bits set is invalid at the
	// Digest level.
	match flags {
		u32(0) {}
		u32(Flags.keyed_hash) {}
		u32(Flags.derive_key_context) {}
		u32(Flags.derive_key_material) {}
		else {
			return DigestFlagsError{
				flags: flags
			}
		}
	}

	return Digest{
		key_words:     key_words
		flags:         flags
		chunk_counter: 0
		input:         []u8{}
		binary_edge:   []TreeNode{}
	}
}

// write adds bytes to the hash
pub fn (mut d Digest) write(data []u8) ! {
	// if no data is being added to the hash, just return.
	if data.len == 0 {
		return
	}

	d.input << data

	// if we have more than 1024 bytes in the input,
	// process it in chunks.

	for d.input.len > chunk_size {
		mut chunk := Chunk{}
		words := chunk.process_input(d.input[..chunk_size], d.key_words, d.chunk_counter,
			d.flags, false)

		d.add_node(Node{ chaining_value: words[..8] }, 0)

		d.chunk_counter += 1
		d.input = d.input[chunk_size..]
	}
}

// checksum finalizes the hash and returns the generated bytes.
//
// This is the point in the hashing operation that we need to know
// how many bytes of hash to generate.  Normally this is 32 but can
// be any size up to 2**64.
pub fn (mut d Digest) checksum(size u64) []u8 {
	return d.checksum_internal(size)
}

fn (mut d Digest) checksum_internal(size u64) []u8 {
	// process the last of the input
	mut chunk := Chunk{}

	root := d.chunk_counter == 0
	mut words := chunk.process_input(d.input, d.key_words, d.chunk_counter, d.flags, root)

	// starting with the current (last) chunk, work your way up to the
	// top of the tree.

	mut state := HashState{
		words:          words
		chaining_value: chunk.chaining_value
		block_words:    chunk.block_words
		block_len:      chunk.block_len
		flags:          chunk.flags
	}

	mut right_node := Node{
		chaining_value: words[..8].clone()
	}

	for i, left_node in d.binary_edge {
		match left_node {
			Empty {
				// nothing to do here, just skip it
			}
			Node {
				mut block_words := left_node.chaining_value.clone()
				block_words << right_node.chaining_value

				mut flags := d.flags | u32(Flags.parent)
				flags |= if i == d.binary_edge.len - 1 { u32(Flags.root) } else { u32(0) }

				words = f(d.key_words, block_words, u64(0), block_size, flags)

				state.words = words
				state.chaining_value = d.key_words
				state.block_words = block_words
				state.block_len = block_size
				state.flags = flags

				right_node = Node{
					chaining_value: words[..8].clone()
				}
			}
		}
	}

	return root_output_bytes(state, size)
}

fn root_output_bytes(state HashState, size u64) []u8 {
	mut output := []u8{cap: int(size)}
	mut bytes_needed := size

	mut block_number := u64(0)
	mut words := state.words.clone()

	for bytes_needed > 0 {
		for word in words {
			mut hash_bytes := []u8{len: 4, cap: 4}
			binary.little_endian_put_u32(mut hash_bytes, word)

			for hash_byte in hash_bytes {
				output << hash_byte
				bytes_needed -= 1

				if bytes_needed == 0 {
					return output
				}
			}
		}

		block_number += 1
		words = f(state.chaining_value, state.block_words, block_number, state.block_len,
			state.flags)
	}

	return output
}

fn (mut d Digest) add_node(node Node, level u8) {
	// if we are above the highst level,
	// just add the node at the top
	if d.binary_edge.len == level {
		d.binary_edge << node
		return
	}

	edge_node := d.binary_edge[level]

	match edge_node {
		Empty {
			d.binary_edge[level] = node
		}
		Node {
			mut block_words := edge_node.chaining_value.clone()
			block_words << node.chaining_value

			words := f(d.key_words, block_words, u64(0), block_size, d.flags | u32(Flags.parent))
			parent_node := Node{
				chaining_value: words[..8]
			}

			d.binary_edge[level] = Empty{}

			d.add_node(parent_node, level + 1)
		}
	}

	return
}

// sum256 returns the Blake3 256 bit hash of the data.
pub fn sum256(data []u8) []u8 {
	mut d := Digest.new_hash() or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal(size256)
}

// sum_keyed256 returns the Blake3 256 bit keyed hash of the data.
pub fn sum_keyed256(data []u8, key []u8) []u8 {
	mut d := Digest.new_keyed_hash(key) or { panic(err) }
	d.write(data) or { panic(err) }
	return d.checksum_internal(size256)
}

// sum_derived_key256 returns the Blake3 256 bit derived key hash of the key material
pub fn sum_derive_key256(context []u8, key_material []u8) []u8 {
	mut d := Digest.new_derive_key_hash(context) or { panic(err) }
	d.write(key_material) or { panic(err) }
	return d.checksum_internal(size256)
}

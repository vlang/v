@[has_globals]
module v2

// Huffman coding for HPACK (RFC 7541 Appendix B).
import sync

const huffman_trie_null = -1

const huffman_eos_symbol = 256

struct DecodeTrieNode {
mut:
	left   int
	right  int
	symbol int
}

__global huffman_decode_trie = []DecodeTrieNode{}
__global huffman_trie_once = sync.new_once()

struct HuffmanEntry {
	code       u32
	bit_length u8
}

const huffman_table = [
	HuffmanEntry{0x1ff8, 13},
	HuffmanEntry{0x7fffd8, 23},
	HuffmanEntry{0xfffffe2, 28},
	HuffmanEntry{0xfffffe3, 28},
	HuffmanEntry{0xfffffe4, 28},
	HuffmanEntry{0xfffffe5, 28},
	HuffmanEntry{0xfffffe6, 28},
	HuffmanEntry{0xfffffe7, 28},
	HuffmanEntry{0xfffffe8, 28},
	HuffmanEntry{0xffffea, 24},
	HuffmanEntry{0x3ffffffc, 30},
	HuffmanEntry{0xfffffe9, 28},
	HuffmanEntry{0xfffffea, 28},
	HuffmanEntry{0x3ffffffd, 30},
	HuffmanEntry{0xfffffeb, 28},
	HuffmanEntry{0xfffffec, 28},
	HuffmanEntry{0xfffffed, 28},
	HuffmanEntry{0xfffffee, 28},
	HuffmanEntry{0xfffffef, 28},
	HuffmanEntry{0xffffff0, 28},
	HuffmanEntry{0xffffff1, 28},
	HuffmanEntry{0xffffff2, 28},
	HuffmanEntry{0x3ffffffe, 30},
	HuffmanEntry{0xffffff3, 28},
	HuffmanEntry{0xffffff4, 28},
	HuffmanEntry{0xffffff5, 28},
	HuffmanEntry{0xffffff6, 28},
	HuffmanEntry{0xffffff7, 28},
	HuffmanEntry{0xffffff8, 28},
	HuffmanEntry{0xffffff9, 28},
	HuffmanEntry{0xffffffa, 28},
	HuffmanEntry{0xffffffb, 28},
	HuffmanEntry{0x14, 6},
	HuffmanEntry{0x3f8, 10},
	HuffmanEntry{0x3f9, 10},
	HuffmanEntry{0xffa, 12},
	HuffmanEntry{0x1ff9, 13},
	HuffmanEntry{0x15, 6},
	HuffmanEntry{0xf8, 8},
	HuffmanEntry{0x7fa, 11},
	HuffmanEntry{0x3fa, 10},
	HuffmanEntry{0x3fb, 10},
	HuffmanEntry{0xf9, 8},
	HuffmanEntry{0x7fb, 11},
	HuffmanEntry{0xfa, 8},
	HuffmanEntry{0x16, 6},
	HuffmanEntry{0x17, 6},
	HuffmanEntry{0x18, 6},
	HuffmanEntry{0x0, 5},
	HuffmanEntry{0x1, 5},
	HuffmanEntry{0x2, 5},
	HuffmanEntry{0x19, 6},
	HuffmanEntry{0x1a, 6},
	HuffmanEntry{0x1b, 6},
	HuffmanEntry{0x1c, 6},
	HuffmanEntry{0x1d, 6},
	HuffmanEntry{0x1e, 6},
	HuffmanEntry{0x1f, 6},
	HuffmanEntry{0x5c, 7},
	HuffmanEntry{0xfb, 8},
	HuffmanEntry{0x7ffc, 15},
	HuffmanEntry{0x20, 6},
	HuffmanEntry{0xffb, 12},
	HuffmanEntry{0x3fc, 10},
	HuffmanEntry{0x1ffa, 13},
	HuffmanEntry{0x21, 6},
	HuffmanEntry{0x5d, 7},
	HuffmanEntry{0x5e, 7},
	HuffmanEntry{0x5f, 7},
	HuffmanEntry{0x60, 7},
	HuffmanEntry{0x61, 7},
	HuffmanEntry{0x62, 7},
	HuffmanEntry{0x63, 7},
	HuffmanEntry{0x64, 7},
	HuffmanEntry{0x65, 7},
	HuffmanEntry{0x66, 7},
	HuffmanEntry{0x67, 7},
	HuffmanEntry{0x68, 7},
	HuffmanEntry{0x69, 7},
	HuffmanEntry{0x6a, 7},
	HuffmanEntry{0x6b, 7},
	HuffmanEntry{0x6c, 7},
	HuffmanEntry{0x6d, 7},
	HuffmanEntry{0x6e, 7},
	HuffmanEntry{0x6f, 7},
	HuffmanEntry{0x70, 7},
	HuffmanEntry{0x71, 7},
	HuffmanEntry{0x72, 7},
	HuffmanEntry{0xfc, 8},
	HuffmanEntry{0x73, 7},
	HuffmanEntry{0xfd, 8},
	HuffmanEntry{0x1ffb, 13},
	HuffmanEntry{0x7fff0, 19},
	HuffmanEntry{0x1ffc, 13},
	HuffmanEntry{0x3ffc, 14},
	HuffmanEntry{0x22, 6},
	HuffmanEntry{0x7ffd, 15},
	HuffmanEntry{0x3, 5},
	HuffmanEntry{0x23, 6},
	HuffmanEntry{0x4, 5},
	HuffmanEntry{0x24, 6},
	HuffmanEntry{0x5, 5},
	HuffmanEntry{0x25, 6},
	HuffmanEntry{0x26, 6},
	HuffmanEntry{0x27, 6},
	HuffmanEntry{0x6, 5},
	HuffmanEntry{0x74, 7},
	HuffmanEntry{0x75, 7},
	HuffmanEntry{0x28, 6},
	HuffmanEntry{0x29, 6},
	HuffmanEntry{0x2a, 6},
	HuffmanEntry{0x7, 5},
	HuffmanEntry{0x2b, 6},
	HuffmanEntry{0x76, 7},
	HuffmanEntry{0x2c, 6},
	HuffmanEntry{0x8, 5},
	HuffmanEntry{0x9, 5},
	HuffmanEntry{0x2d, 6},
	HuffmanEntry{0x77, 7},
	HuffmanEntry{0x78, 7},
	HuffmanEntry{0x79, 7},
	HuffmanEntry{0x7a, 7},
	HuffmanEntry{0x7b, 7},
	HuffmanEntry{0x7ffe, 15},
	HuffmanEntry{0x7fc, 11},
	HuffmanEntry{0x3ffd, 14},
	HuffmanEntry{0x1ffd, 13},
	HuffmanEntry{0xffffffc, 28},
	HuffmanEntry{0xfffe6, 20},
	HuffmanEntry{0x3fffd2, 22},
	HuffmanEntry{0xfffe7, 20},
	HuffmanEntry{0xfffe8, 20},
	HuffmanEntry{0x3fffd3, 22},
	HuffmanEntry{0x3fffd4, 22},
	HuffmanEntry{0x3fffd5, 22},
	HuffmanEntry{0x7fffd9, 23},
	HuffmanEntry{0x3fffd6, 22},
	HuffmanEntry{0x7fffda, 23},
	HuffmanEntry{0x7fffdb, 23},
	HuffmanEntry{0x7fffdc, 23},
	HuffmanEntry{0x7fffdd, 23},
	HuffmanEntry{0x7fffde, 23},
	HuffmanEntry{0xffffeb, 24},
	HuffmanEntry{0x7fffdf, 23},
	HuffmanEntry{0xffffec, 24},
	HuffmanEntry{0xffffed, 24},
	HuffmanEntry{0x3fffd7, 22},
	HuffmanEntry{0x7fffe0, 23},
	HuffmanEntry{0xffffee, 24},
	HuffmanEntry{0x7fffe1, 23},
	HuffmanEntry{0x7fffe2, 23},
	HuffmanEntry{0x7fffe3, 23},
	HuffmanEntry{0x7fffe4, 23},
	HuffmanEntry{0x1fffdc, 21},
	HuffmanEntry{0x3fffd8, 22},
	HuffmanEntry{0x7fffe5, 23},
	HuffmanEntry{0x3fffd9, 22},
	HuffmanEntry{0x7fffe6, 23},
	HuffmanEntry{0x7fffe7, 23},
	HuffmanEntry{0xffffef, 24},
	HuffmanEntry{0x3fffda, 22},
	HuffmanEntry{0x1fffdd, 21},
	HuffmanEntry{0xfffe9, 20},
	HuffmanEntry{0x3fffdb, 22},
	HuffmanEntry{0x3fffdc, 22},
	HuffmanEntry{0x7fffe8, 23},
	HuffmanEntry{0x7fffe9, 23},
	HuffmanEntry{0x1fffde, 21},
	HuffmanEntry{0x7fffea, 23},
	HuffmanEntry{0x3fffdd, 22},
	HuffmanEntry{0x3fffde, 22},
	HuffmanEntry{0xfffff0, 24},
	HuffmanEntry{0x1fffdf, 21},
	HuffmanEntry{0x3fffdf, 22},
	HuffmanEntry{0x7fffeb, 23},
	HuffmanEntry{0x7fffec, 23},
	HuffmanEntry{0x1fffe0, 21},
	HuffmanEntry{0x1fffe1, 21},
	HuffmanEntry{0x3fffe0, 22},
	HuffmanEntry{0x1fffe2, 21},
	HuffmanEntry{0x7fffed, 23},
	HuffmanEntry{0x3fffe1, 22},
	HuffmanEntry{0x7fffee, 23},
	HuffmanEntry{0x7fffef, 23},
	HuffmanEntry{0xfffea, 20},
	HuffmanEntry{0x3fffe2, 22},
	HuffmanEntry{0x3fffe3, 22},
	HuffmanEntry{0x3fffe4, 22},
	HuffmanEntry{0x7ffff0, 23},
	HuffmanEntry{0x3fffe5, 22},
	HuffmanEntry{0x3fffe6, 22},
	HuffmanEntry{0x7ffff1, 23},
	HuffmanEntry{0x3ffffe0, 26},
	HuffmanEntry{0x3ffffe1, 26},
	HuffmanEntry{0xfffeb, 20},
	HuffmanEntry{0x7fff1, 19},
	HuffmanEntry{0x3fffe7, 22},
	HuffmanEntry{0x7ffff2, 23},
	HuffmanEntry{0x3fffe8, 22},
	HuffmanEntry{0x1ffffec, 25},
	HuffmanEntry{0x3ffffe2, 26},
	HuffmanEntry{0x3ffffe3, 26},
	HuffmanEntry{0x3ffffe4, 26},
	HuffmanEntry{0x7ffffde, 27},
	HuffmanEntry{0x7ffffdf, 27},
	HuffmanEntry{0x3ffffe5, 26},
	HuffmanEntry{0xfffff1, 24},
	HuffmanEntry{0x1ffffed, 25},
	HuffmanEntry{0x7fff2, 19},
	HuffmanEntry{0x1fffe3, 21},
	HuffmanEntry{0x3ffffe6, 26},
	HuffmanEntry{0x7ffffe0, 27},
	HuffmanEntry{0x7ffffe1, 27},
	HuffmanEntry{0x3ffffe7, 26},
	HuffmanEntry{0x7ffffe2, 27},
	HuffmanEntry{0xfffff2, 24},
	HuffmanEntry{0x1fffe4, 21},
	HuffmanEntry{0x1fffe5, 21},
	HuffmanEntry{0x3ffffe8, 26},
	HuffmanEntry{0x3ffffe9, 26},
	HuffmanEntry{0xffffffd, 28},
	HuffmanEntry{0x7ffffe3, 27},
	HuffmanEntry{0x7ffffe4, 27},
	HuffmanEntry{0x7ffffe5, 27},
	HuffmanEntry{0xfffec, 20},
	HuffmanEntry{0xfffff3, 24},
	HuffmanEntry{0xfffed, 20},
	HuffmanEntry{0x1fffe6, 21},
	HuffmanEntry{0x3fffe9, 22},
	HuffmanEntry{0x1fffe7, 21},
	HuffmanEntry{0x1fffe8, 21},
	HuffmanEntry{0x7ffff3, 23},
	HuffmanEntry{0x3fffea, 22},
	HuffmanEntry{0x3fffeb, 22},
	HuffmanEntry{0x1ffffee, 25},
	HuffmanEntry{0x1ffffef, 25},
	HuffmanEntry{0xfffff4, 24},
	HuffmanEntry{0xfffff5, 24},
	HuffmanEntry{0x3ffffea, 26},
	HuffmanEntry{0x7ffff4, 23},
	HuffmanEntry{0x3ffffeb, 26},
	HuffmanEntry{0x7ffffe6, 27},
	HuffmanEntry{0x3ffffec, 26},
	HuffmanEntry{0x3ffffed, 26},
	HuffmanEntry{0x7ffffe7, 27},
	HuffmanEntry{0x7ffffe8, 27},
	HuffmanEntry{0x7ffffe9, 27},
	HuffmanEntry{0x7ffffea, 27},
	HuffmanEntry{0x7ffffeb, 27},
	HuffmanEntry{0xffffffe, 28},
	HuffmanEntry{0x7ffffec, 27},
	HuffmanEntry{0x7ffffed, 27},
	HuffmanEntry{0x7ffffee, 27},
	HuffmanEntry{0x7ffffef, 27},
	HuffmanEntry{0x7fffff0, 27},
	HuffmanEntry{0x3ffffee, 26},
	HuffmanEntry{0x3fffffff, 30},
]!

// huffman_encoded_length calculates the encoded length in bits for the given data.
pub fn huffman_encoded_length(data []u8) int {
	mut bits := 0
	for b in data {
		bits += int(huffman_table[b].bit_length)
	}
	return bits
}

// encode_huffman encodes data using Huffman coding.
pub fn encode_huffman(data []u8) []u8 {
	if data.len == 0 {
		return []u8{}
	}

	total_bits := huffman_encoded_length(data)
	total_bytes := (total_bits + 7) / 8

	mut result := []u8{len: total_bytes}
	mut current_byte := u8(0)
	mut bits_in_byte := 0
	mut byte_index := 0

	for b in data {
		entry := huffman_table[b]
		mut code := entry.code
		mut bits_left := int(entry.bit_length)

		for bits_left > 0 {
			bits_to_write := if bits_left < (8 - bits_in_byte) {
				bits_left
			} else {
				8 - bits_in_byte
			}

			shift := bits_left - bits_to_write
			mask := (u32(1) << bits_to_write) - 1
			bits := u8((code >> shift) & mask)

			current_byte |= bits << (8 - bits_in_byte - bits_to_write)
			bits_in_byte += bits_to_write
			bits_left -= bits_to_write

			if bits_in_byte == 8 {
				result[byte_index] = current_byte
				byte_index++
				current_byte = 0
				bits_in_byte = 0
			}
		}
	}

	if bits_in_byte > 0 {
		current_byte |= u8((1 << (8 - bits_in_byte)) - 1)
		result[byte_index] = current_byte
	}

	return result
}

fn build_huffman_decode_trie() {
	huffman_decode_trie = []DecodeTrieNode{cap: 513}
	huffman_decode_trie << DecodeTrieNode{
		left:   huffman_trie_null
		right:  huffman_trie_null
		symbol: huffman_trie_null
	}

	for sym in 0 .. 257 {
		entry := huffman_table[sym]
		code := entry.code
		nbits := int(entry.bit_length)

		mut node_idx := 0
		for bit_i := nbits - 1; bit_i >= 0; bit_i-- {
			bit := int((code >> u32(bit_i)) & 1)
			if bit == 0 {
				if huffman_decode_trie[node_idx].left == huffman_trie_null {
					huffman_decode_trie << DecodeTrieNode{
						left:   huffman_trie_null
						right:  huffman_trie_null
						symbol: huffman_trie_null
					}
					huffman_decode_trie[node_idx].left = huffman_decode_trie.len - 1
				}
				node_idx = huffman_decode_trie[node_idx].left
			} else {
				if huffman_decode_trie[node_idx].right == huffman_trie_null {
					huffman_decode_trie << DecodeTrieNode{
						left:   huffman_trie_null
						right:  huffman_trie_null
						symbol: huffman_trie_null
					}
					huffman_decode_trie[node_idx].right = huffman_decode_trie.len - 1
				}
				node_idx = huffman_decode_trie[node_idx].right
			}
		}
		huffman_decode_trie[node_idx].symbol = sym
	}
}

fn validate_huffman_padding(bits_remaining int, current_node_idx int) ! {
	if bits_remaining > 7 {
		return error('invalid Huffman padding: ${bits_remaining} bits remaining')
	}
	if bits_remaining > 0 {
		mut check_idx := 0
		for _ in 0 .. bits_remaining {
			right_child := huffman_decode_trie[check_idx].right
			if right_child == huffman_trie_null {
				return error('invalid Huffman padding')
			}
			if huffman_decode_trie[right_child].symbol != huffman_trie_null {
				return error('invalid Huffman padding')
			}
			check_idx = right_child
		}
		if check_idx != current_node_idx {
			return error('invalid Huffman padding')
		}
	}
}

// decode_huffman decodes Huffman encoded data using a binary trie (RFC 7541 §5.2).
pub fn decode_huffman(data []u8) ![]u8 {
	if data.len == 0 {
		return []u8{}
	}

	huffman_trie_once.do(build_huffman_decode_trie)

	mut result := []u8{cap: data.len * 2}
	mut node_idx := 0
	mut bits_since_root := 0

	for b in data {
		for bit_pos := 7; bit_pos >= 0; bit_pos-- {
			bit := int((b >> u8(bit_pos)) & 1)
			bits_since_root++

			next := if bit == 0 {
				huffman_decode_trie[node_idx].left
			} else {
				huffman_decode_trie[node_idx].right
			}

			if next == huffman_trie_null {
				return error('invalid Huffman code after ${bits_since_root} bits')
			}
			node_idx = next

			sym := huffman_decode_trie[node_idx].symbol
			if sym == huffman_trie_null {
				continue
			}
			if sym == huffman_eos_symbol {
				return error('invalid Huffman sequence: EOS symbol in data')
			}
			result << u8(sym)
			node_idx = 0
			bits_since_root = 0
		}
	}

	validate_huffman_padding(bits_since_root, node_idx)!

	return result
}

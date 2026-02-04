// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Huffman coding for HPACK (RFC 7541 Appendix B)
// This implements the static Huffman table from RFC 7541

// HuffmanEntry represents a Huffman code entry
struct HuffmanEntry {
	code       u32 // The Huffman code
	bit_length u8  // Number of bits in the code
}

// Static Huffman table from RFC 7541 Appendix B
const huffman_table = [
	HuffmanEntry{0x1ff8, 13}, // 0
	HuffmanEntry{0x7fffd8, 23}, // 1
	HuffmanEntry{0xfffffe2, 28}, // 2
	HuffmanEntry{0xfffffe3, 28}, // 3
	HuffmanEntry{0xfffffe4, 28}, // 4
	HuffmanEntry{0xfffffe5, 28}, // 5
	HuffmanEntry{0xfffffe6, 28}, // 6
	HuffmanEntry{0xfffffe7, 28}, // 7
	HuffmanEntry{0xfffffe8, 28}, // 8
	HuffmanEntry{0xffffea, 24}, // 9
	HuffmanEntry{0x3ffffffc, 30}, // 10
	HuffmanEntry{0xfffffe9, 28}, // 11
	HuffmanEntry{0xfffffea, 28}, // 12
	HuffmanEntry{0x3ffffffd, 30}, // 13
	HuffmanEntry{0xfffffeb, 28}, // 14
	HuffmanEntry{0xfffffec, 28}, // 15
	HuffmanEntry{0xfffffed, 28}, // 16
	HuffmanEntry{0xfffffee, 28}, // 17
	HuffmanEntry{0xfffffef, 28}, // 18
	HuffmanEntry{0xffffff0, 28}, // 19
	HuffmanEntry{0xffffff1, 28}, // 20
	HuffmanEntry{0xffffff2, 28}, // 21
	HuffmanEntry{0x3ffffffe, 30}, // 22
	HuffmanEntry{0xffffff3, 28}, // 23
	HuffmanEntry{0xffffff4, 28}, // 24
	HuffmanEntry{0xffffff5, 28}, // 25
	HuffmanEntry{0xffffff6, 28}, // 26
	HuffmanEntry{0xffffff7, 28}, // 27
	HuffmanEntry{0xffffff8, 28}, // 28
	HuffmanEntry{0xffffff9, 28}, // 29
	HuffmanEntry{0xffffffa, 28}, // 30
	HuffmanEntry{0xffffffb, 28}, // 31
	HuffmanEntry{0x14, 6}, // 32 ' '
	HuffmanEntry{0x3f8, 10}, // 33 '!'
	HuffmanEntry{0x3f9, 10}, // 34 '"'
	HuffmanEntry{0xffa, 12}, // 35 '#'
	HuffmanEntry{0x1ff9, 13}, // 36 '$'
	HuffmanEntry{0x15, 6}, // 37 '%'
	HuffmanEntry{0xf8, 8}, // 38 '&'
	HuffmanEntry{0x7fa, 11}, // 39 '''
	HuffmanEntry{0x3fa, 10}, // 40 '('
	HuffmanEntry{0x3fb, 10}, // 41 ')'
	HuffmanEntry{0xf9, 8}, // 42 '*'
	HuffmanEntry{0x7fb, 11}, // 43 '+'
	HuffmanEntry{0xfa, 8}, // 44 ','
	HuffmanEntry{0x16, 6}, // 45 '-'
	HuffmanEntry{0x17, 6}, // 46 '.'
	HuffmanEntry{0x18, 6}, // 47 '/'
	HuffmanEntry{0x0, 5}, // 48 '0'
	HuffmanEntry{0x1, 5}, // 49 '1'
	HuffmanEntry{0x2, 5}, // 50 '2'
	HuffmanEntry{0x19, 6}, // 51 '3'
	HuffmanEntry{0x1a, 6}, // 52 '4'
	HuffmanEntry{0x1b, 6}, // 53 '5'
	HuffmanEntry{0x1c, 6}, // 54 '6'
	HuffmanEntry{0x1d, 6}, // 55 '7'
	HuffmanEntry{0x1e, 6}, // 56 '8'
	HuffmanEntry{0x1f, 6}, // 57 '9'
	HuffmanEntry{0x5c, 7}, // 58 ':'
	HuffmanEntry{0xfb, 8}, // 59 ';'
	HuffmanEntry{0x7ffc, 15}, // 60 '<'
	HuffmanEntry{0x20, 6}, // 61 '='
	HuffmanEntry{0xffb, 12}, // 62 '>'
	HuffmanEntry{0x3fc, 10}, // 63 '?'
	HuffmanEntry{0x1ffa, 13}, // 64 '@'
	HuffmanEntry{0x21, 6}, // 65 'A'
	HuffmanEntry{0x5d, 7}, // 66 'B'
	HuffmanEntry{0x5e, 7}, // 67 'C'
	HuffmanEntry{0x5f, 7}, // 68 'D'
	HuffmanEntry{0x60, 7}, // 69 'E'
	HuffmanEntry{0x61, 7}, // 70 'F'
	HuffmanEntry{0x62, 7}, // 71 'G'
	HuffmanEntry{0x63, 7}, // 72 'H'
	HuffmanEntry{0x64, 7}, // 73 'I'
	HuffmanEntry{0x65, 7}, // 74 'J'
	HuffmanEntry{0x66, 7}, // 75 'K'
	HuffmanEntry{0x67, 7}, // 76 'L'
	HuffmanEntry{0x68, 7}, // 77 'M'
	HuffmanEntry{0x69, 7}, // 78 'N'
	HuffmanEntry{0x6a, 7}, // 79 'O'
	HuffmanEntry{0x6b, 7}, // 80 'P'
	HuffmanEntry{0x6c, 7}, // 81 'Q'
	HuffmanEntry{0x6d, 7}, // 82 'R'
	HuffmanEntry{0x6e, 7}, // 83 'S'
	HuffmanEntry{0x6f, 7}, // 84 'T'
	HuffmanEntry{0x70, 7}, // 85 'U'
	HuffmanEntry{0x71, 7}, // 86 'V'
	HuffmanEntry{0x72, 7}, // 87 'W'
	HuffmanEntry{0xfc, 8}, // 88 'X'
	HuffmanEntry{0x73, 7}, // 89 'Y'
	HuffmanEntry{0xfd, 8}, // 90 'Z'
	HuffmanEntry{0x1ffb, 13}, // 91 '['
	HuffmanEntry{0x7fff0, 19}, // 92 '\'
	HuffmanEntry{0x1ffc, 13}, // 93 ']'
	HuffmanEntry{0x3ffc, 14}, // 94 '^'
	HuffmanEntry{0x22, 6}, // 95 '_'
	HuffmanEntry{0x7ffd, 15}, // 96 '`'
	HuffmanEntry{0x3, 5}, // 97 'a'
	HuffmanEntry{0x23, 6}, // 98 'b'
	HuffmanEntry{0x4, 5}, // 99 'c'
	HuffmanEntry{0x24, 6}, // 100 'd'
	HuffmanEntry{0x5, 5}, // 101 'e'
	HuffmanEntry{0x25, 6}, // 102 'f'
	HuffmanEntry{0x26, 6}, // 103 'g'
	HuffmanEntry{0x27, 6}, // 104 'h'
	HuffmanEntry{0x6, 5}, // 105 'i'
	HuffmanEntry{0x74, 7}, // 106 'j'
	HuffmanEntry{0x75, 7}, // 107 'k'
	HuffmanEntry{0x28, 6}, // 108 'l'
	HuffmanEntry{0x29, 6}, // 109 'm'
	HuffmanEntry{0x2a, 6}, // 110 'n'
	HuffmanEntry{0x7, 5}, // 111 'o'
	HuffmanEntry{0x2b, 6}, // 112 'p'
	HuffmanEntry{0x76, 7}, // 113 'q'
	HuffmanEntry{0x2c, 6}, // 114 'r'
	HuffmanEntry{0x8, 5}, // 115 's'
	HuffmanEntry{0x9, 5}, // 116 't'
	HuffmanEntry{0x2d, 6}, // 117 'u'
	HuffmanEntry{0x77, 7}, // 118 'v'
	HuffmanEntry{0x78, 7}, // 119 'w'
	HuffmanEntry{0x79, 7}, // 120 'x'
	HuffmanEntry{0x7a, 7}, // 121 'y'
	HuffmanEntry{0x7b, 7}, // 122 'z'
	HuffmanEntry{0x7ffe, 15}, // 123 '{'
	HuffmanEntry{0x7fc, 11}, // 124 '|'
	HuffmanEntry{0x3ffd, 14}, // 125 '}'
	HuffmanEntry{0x1ffd, 13}, // 126 '~'
	HuffmanEntry{0xffffffc, 28}, // 127
	HuffmanEntry{0xfffe6, 20}, // 128
	HuffmanEntry{0x3fffd2, 22}, // 129
	HuffmanEntry{0xfffe7, 20}, // 130
	HuffmanEntry{0xfffe8, 20}, // 131
	HuffmanEntry{0x3fffd3, 22}, // 132
	HuffmanEntry{0x3fffd4, 22}, // 133
	HuffmanEntry{0x3fffd5, 22}, // 134
	HuffmanEntry{0x7fffd9, 23}, // 135
	HuffmanEntry{0x3fffd6, 22}, // 136
	HuffmanEntry{0x7fffda, 23}, // 137
	HuffmanEntry{0x7fffdb, 23}, // 138
	HuffmanEntry{0x7fffdc, 23}, // 139
	HuffmanEntry{0x7fffdd, 23}, // 140
	HuffmanEntry{0x7fffde, 23}, // 141
	HuffmanEntry{0xffffeb, 24}, // 142
	HuffmanEntry{0x7fffdf, 23}, // 143
	HuffmanEntry{0xffffec, 24}, // 144
	HuffmanEntry{0xffffed, 24}, // 145
	HuffmanEntry{0x3fffd7, 22}, // 146
	HuffmanEntry{0x7fffe0, 23}, // 147
	HuffmanEntry{0xffffee, 24}, // 148
	HuffmanEntry{0x7fffe1, 23}, // 149
	HuffmanEntry{0x7fffe2, 23}, // 150
	HuffmanEntry{0x7fffe3, 23}, // 151
	HuffmanEntry{0x7fffe4, 23}, // 152
	HuffmanEntry{0x1fffdc, 21}, // 153
	HuffmanEntry{0x3fffd8, 22}, // 154
	HuffmanEntry{0x7fffe5, 23}, // 155
	HuffmanEntry{0x3fffd9, 22}, // 156
	HuffmanEntry{0x7fffe6, 23}, // 157
	HuffmanEntry{0x7fffe7, 23}, // 158
	HuffmanEntry{0xffffef, 24}, // 159
	HuffmanEntry{0x3fffda, 22}, // 160
	HuffmanEntry{0x1fffdd, 21}, // 161
	HuffmanEntry{0xfffe9, 20}, // 162
	HuffmanEntry{0x3fffdb, 22}, // 163
	HuffmanEntry{0x3fffdc, 22}, // 164
	HuffmanEntry{0x7fffe8, 23}, // 165
	HuffmanEntry{0x7fffe9, 23}, // 166
	HuffmanEntry{0x1fffde, 21}, // 167
	HuffmanEntry{0x7fffea, 23}, // 168
	HuffmanEntry{0x3fffdd, 22}, // 169
	HuffmanEntry{0x3fffde, 22}, // 170
	HuffmanEntry{0xfffff0, 24}, // 171
	HuffmanEntry{0x1fffdf, 21}, // 172
	HuffmanEntry{0x3fffdf, 22}, // 173
	HuffmanEntry{0x7fffeb, 23}, // 174
	HuffmanEntry{0x7fffec, 23}, // 175
	HuffmanEntry{0x1fffe0, 21}, // 176
	HuffmanEntry{0x1fffe1, 21}, // 177
	HuffmanEntry{0x3fffe0, 22}, // 178
	HuffmanEntry{0x1fffe2, 21}, // 179
	HuffmanEntry{0x7fffed, 23}, // 180
	HuffmanEntry{0x3fffe1, 22}, // 181
	HuffmanEntry{0x7fffee, 23}, // 182
	HuffmanEntry{0x7fffef, 23}, // 183
	HuffmanEntry{0xfffea, 20}, // 184
	HuffmanEntry{0x3fffe2, 22}, // 185
	HuffmanEntry{0x3fffe3, 22}, // 186
	HuffmanEntry{0x3fffe4, 22}, // 187
	HuffmanEntry{0x7ffff0, 23}, // 188
	HuffmanEntry{0x3fffe5, 22}, // 189
	HuffmanEntry{0x3fffe6, 22}, // 190
	HuffmanEntry{0x7ffff1, 23}, // 191
	HuffmanEntry{0x3ffffe0, 26}, // 192
	HuffmanEntry{0x3ffffe1, 26}, // 193
	HuffmanEntry{0xfffeb, 20}, // 194
	HuffmanEntry{0x7fff1, 19}, // 195
	HuffmanEntry{0x3fffe7, 22}, // 196
	HuffmanEntry{0x7ffff2, 23}, // 197
	HuffmanEntry{0x3fffe8, 22}, // 198
	HuffmanEntry{0x1ffffec, 25}, // 199
	HuffmanEntry{0x3ffffe2, 26}, // 200
	HuffmanEntry{0x3ffffe3, 26}, // 201
	HuffmanEntry{0x3ffffe4, 26}, // 202
	HuffmanEntry{0x7ffffde, 27}, // 203
	HuffmanEntry{0x7ffffdf, 27}, // 204
	HuffmanEntry{0x3ffffe5, 26}, // 205
	HuffmanEntry{0xfffff1, 24}, // 206
	HuffmanEntry{0x1ffffed, 25}, // 207
	HuffmanEntry{0x7fff2, 19}, // 208
	HuffmanEntry{0x1fffe3, 21}, // 209
	HuffmanEntry{0x3ffffe6, 26}, // 210
	HuffmanEntry{0x7ffffe0, 27}, // 211
	HuffmanEntry{0x7ffffe1, 27}, // 212
	HuffmanEntry{0x3ffffe7, 26}, // 213
	HuffmanEntry{0x7ffffe2, 27}, // 214
	HuffmanEntry{0xfffff2, 24}, // 215
	HuffmanEntry{0x1fffe4, 21}, // 216
	HuffmanEntry{0x1fffe5, 21}, // 217
	HuffmanEntry{0x3ffffe8, 26}, // 218
	HuffmanEntry{0x3ffffe9, 26}, // 219
	HuffmanEntry{0xffffffd, 28}, // 220
	HuffmanEntry{0x7ffffe3, 27}, // 221
	HuffmanEntry{0x7ffffe4, 27}, // 222
	HuffmanEntry{0x7ffffe5, 27}, // 223
	HuffmanEntry{0xfffec, 20}, // 224
	HuffmanEntry{0xfffff3, 24}, // 225
	HuffmanEntry{0xfffed, 20}, // 226
	HuffmanEntry{0x1fffe6, 21}, // 227
	HuffmanEntry{0x3fffe9, 22}, // 228
	HuffmanEntry{0x1fffe7, 21}, // 229
	HuffmanEntry{0x1fffe8, 21}, // 230
	HuffmanEntry{0x7ffff3, 23}, // 231
	HuffmanEntry{0x3fffea, 22}, // 232
	HuffmanEntry{0x3fffeb, 22}, // 233
	HuffmanEntry{0x1ffffee, 25}, // 234
	HuffmanEntry{0x1ffffef, 25}, // 235
	HuffmanEntry{0xfffff4, 24}, // 236
	HuffmanEntry{0xfffff5, 24}, // 237
	HuffmanEntry{0x3ffffea, 26}, // 238
	HuffmanEntry{0x7ffff4, 23}, // 239
	HuffmanEntry{0x3ffffeb, 26}, // 240
	HuffmanEntry{0x7ffffe6, 27}, // 241
	HuffmanEntry{0x3ffffec, 26}, // 242
	HuffmanEntry{0x3ffffed, 26}, // 243
	HuffmanEntry{0x7ffffe7, 27}, // 244
	HuffmanEntry{0x7ffffe8, 27}, // 245
	HuffmanEntry{0x7ffffe9, 27}, // 246
	HuffmanEntry{0x7ffffea, 27}, // 247
	HuffmanEntry{0x7ffffeb, 27}, // 248
	HuffmanEntry{0xffffffe, 28}, // 249
	HuffmanEntry{0x7ffffec, 27}, // 250
	HuffmanEntry{0x7ffffed, 27}, // 251
	HuffmanEntry{0x7ffffee, 27}, // 252
	HuffmanEntry{0x7ffffef, 27}, // 253
	HuffmanEntry{0x7fffff0, 27}, // 254
	HuffmanEntry{0x3ffffee, 26}, // 255
	HuffmanEntry{0x3fffffff, 30}, // EOS (256)
]!

// huffman_encoded_length calculates the encoded length in bits for the given data
pub fn huffman_encoded_length(data []u8) int {
	mut bits := 0
	for b in data {
		bits += int(huffman_table[b].bit_length)
	}
	return bits
}

// encode_huffman encodes data using Huffman coding
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

// decode_huffman decodes Huffman encoded data
pub fn decode_huffman(data []u8) ![]u8 {
	if data.len == 0 {
		return []u8{}
	}

	mut result := []u8{cap: data.len * 2}
	mut bits := u32(0)
	mut n_bits := 0

	for b in data {
		bits = (bits << 8) | u32(b)
		n_bits += 8

		for n_bits >= 5 {
			mut found := false
			for i := 0; i < 256; i++ {
				entry := huffman_table[i]
				if n_bits >= int(entry.bit_length) {
					shift := n_bits - int(entry.bit_length)
					mask := (u32(1) << entry.bit_length) - 1
					if ((bits >> shift) & mask) == entry.code {
						result << u8(i)
						bits &= (u32(1) << shift) - 1
						n_bits = shift
						found = true
						break
					}
				}
			}
			if !found {
				if n_bits >= 8 {
					return error('invalid Huffman code')
				}
				break
			}
		}
	}

	if n_bits > 0 {
		padding_mask := (u32(1) << n_bits) - 1
		if (bits & padding_mask) != padding_mask {
			return error('invalid Huffman padding')
		}
	}

	return result
}

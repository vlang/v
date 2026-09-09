// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import hash.huffman

// h2_huffman_eos is the index of the EOS (end-of-string) symbol in the
// HPACK Huffman table (RFC 7541 Appendix B).
const h2_huffman_eos = 256

// h2_max_code_bits is the longest HPACK Huffman code length (RFC 7541
// Appendix B). 30 bits is too wide for a flat decode table, so decoding goes
// bit-at-a-time against h2_huffman_decode_map.
const h2_max_code_bits = 30

// h2_huffman_table holds the canonical HPACK Huffman codes, rebuilt at startup
// from the per-symbol bit lengths via the shared hash.huffman builder. The
// codes are MSB-first (the order they appear on the wire).
const h2_huffman_table = build_h2_huffman_table()

// h2_huffman_decode_map maps a (bit_length, code) pair, packed as
// `(bit_length << 32) | code`, to its symbol. It is built once at startup
// from the canonical code table and is read-only afterwards.
const h2_huffman_decode_map = h2_huffman_table.decode_map() or { panic('hpack: ${err}') }

fn build_h2_huffman_table() huffman.Table {
	return huffman.build(
		lengths:   h2_huffman_code_lens[..].map(int(it))
		max_bits:  h2_max_code_bits
		bit_order: .msb_first
	) or { panic('hpack: ${err}') }
}

// h2_huffman_encode returns the HPACK Huffman encoding of `input`
// (RFC 7541 Section 5.2). The final byte is padded with the most significant
// bits of the EOS code, i.e. all ones.
fn h2_huffman_encode(input []u8) []u8 {
	mut out := []u8{cap: input.len}
	mut acc := u64(0)
	mut nbits := 0
	for b in input {
		code := u64(h2_huffman_table.codes[b])
		clen := h2_huffman_table.lengths[b]
		acc = (acc << clen) | code
		nbits += clen
		for nbits >= 8 {
			nbits -= 8
			out << u8((acc >> nbits) & 0xff)
		}
		// Keep only the still-buffered low bits, so acc stays bounded.
		acc = if nbits == 0 { u64(0) } else { acc & ((u64(1) << nbits) - 1) }
	}
	if nbits > 0 {
		pad := 8 - nbits
		out << u8(((acc << pad) | ((u64(1) << pad) - 1)) & 0xff)
	}
	return out
}

// h2_huffman_decode reverses h2_huffman_encode. It returns an error for the
// invalid cases called out by RFC 7541 Section 5.2: an explicit EOS symbol,
// padding longer than 7 bits, or padding that is not all ones.
fn h2_huffman_decode(input []u8) ![]u8 {
	mut out := []u8{cap: input.len + input.len / 2}
	mut cur := u64(0)
	mut cur_len := 0
	for b in input {
		for bit := 7; bit >= 0; bit-- {
			cur = (cur << 1) | u64((b >> u8(bit)) & 1)
			cur_len++
			if cur_len > h2_max_code_bits {
				return error('hpack: invalid huffman code (no symbol within ${h2_max_code_bits} bits)')
			}
			if sym := h2_huffman_decode_map[(u64(cur_len) << 32) | cur] {
				if sym == h2_huffman_eos {
					return error('hpack: EOS symbol encountered in huffman string')
				}
				out << u8(sym)
				cur = 0
				cur_len = 0
			}
		}
	}
	if cur_len >= 8 {
		return error('hpack: invalid huffman padding (more than 7 bits left)')
	}
	if cur_len > 0 {
		mask := (u64(1) << cur_len) - 1
		if cur & mask != mask {
			return error('hpack: invalid huffman padding (not all ones)')
		}
	}
	return out
}

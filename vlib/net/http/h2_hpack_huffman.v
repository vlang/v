// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// h2_huffman_eos is the index of the EOS (end-of-string) symbol in the
// HPACK Huffman table (RFC 7541 Appendix B).
const h2_huffman_eos = 256

// h2_huffman_decode_map maps a (bit_length, code) pair, packed as
// `(bit_length << 32) | code`, to its symbol. It is built once at startup
// from the canonical code table and is read-only afterwards.
const h2_huffman_decode_map = build_h2_huffman_decode_map()

fn build_h2_huffman_decode_map() map[u64]int {
	mut m := map[u64]int{}
	for sym in 0 .. h2_huffman_codes.len {
		l := u64(h2_huffman_code_lens[sym])
		code := u64(h2_huffman_codes[sym])
		m[(l << 32) | code] = sym
	}
	return m
}

// h2_huffman_encode returns the HPACK Huffman encoding of `input`
// (RFC 7541 Section 5.2). The final byte is padded with the most significant
// bits of the EOS code, i.e. all ones.
fn h2_huffman_encode(input []u8) []u8 {
	mut out := []u8{cap: input.len}
	mut acc := u64(0)
	mut nbits := 0
	for b in input {
		code := u64(h2_huffman_codes[b])
		clen := int(h2_huffman_code_lens[b])
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
			if cur_len > 30 {
				return error('hpack: invalid huffman code (no symbol within 30 bits)')
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

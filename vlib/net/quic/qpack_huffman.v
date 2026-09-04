// Verbatim copy of net.http's h2_hpack_huffman.v (module/identifier renames
// only) -- see qpack_huffman_table.v's module doc comment for why this is a
// copy rather than a fresh implementation: RFC 9204 §4.1.2 mandates QPACK
// reuse RFC 7541 Appendix B's Huffman table "without modification", so
// reusing the already-tested codec built on top of it is the lower-risk
// choice over a second hand-written implementation of the same algorithm.
module quic

import hash.huffman

// qpack_huffman_eos is the index of the EOS (end-of-string) symbol in the
// HPACK Huffman table (RFC 7541 Appendix B).
const qpack_huffman_eos = 256

// qpack_max_code_bits is the longest HPACK Huffman code length (RFC 7541
// Appendix B). 30 bits is too wide for a flat decode table, so decoding goes
// bit-at-a-time against qpack_huffman_decode_map.
const qpack_max_code_bits = 30

// qpack_huffman_table holds the canonical HPACK Huffman codes, rebuilt at
// startup from the per-symbol bit lengths via the shared hash.huffman
// builder. The codes are MSB-first (the order they appear on the wire).
const qpack_huffman_table = build_qpack_huffman_table()

// qpack_huffman_decode_map maps a (bit_length, code) pair, packed as
// `(bit_length << 32) | code`, to its symbol. It is built once at startup
// from the canonical code table and is read-only afterwards.
const qpack_huffman_decode_map = qpack_huffman_table.decode_map() or { panic('qpack: ${err}') }

fn build_qpack_huffman_table() huffman.Table {
	return huffman.build(
		lengths: qpack_huffman_code_lens[..].map(int(it))
		max_bits: qpack_max_code_bits
		bit_order: .msb_first
	) or { panic('qpack: ${err}') }
}

// qpack_huffman_encode returns the HPACK Huffman encoding of `input` (RFC
// 9204 §4.1.2, reusing RFC 7541 §5.2 unmodified). The final byte is padded
// with the most significant bits of the EOS code, i.e. all ones.
fn qpack_huffman_encode(input []u8) []u8 {
	mut out := []u8{cap: input.len}
	mut acc := u64(0)
	mut nbits := 0
	for b in input {
		code := u64(qpack_huffman_table.codes[b])
		clen := qpack_huffman_table.lengths[b]
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

// qpack_huffman_decode reverses qpack_huffman_encode. It returns an error
// for the invalid cases called out by RFC 7541 Section 5.2: an explicit EOS
// symbol, padding longer than 7 bits, or padding that is not all ones.
fn qpack_huffman_decode(input []u8) ![]u8 {
	mut out := []u8{cap: input.len + input.len / 2}
	mut cur := u64(0)
	mut cur_len := 0
	for b in input {
		for bit := 7; bit >= 0; bit-- {
			cur = (cur << 1) | u64((b >> u8(bit)) & 1)
			cur_len++
			if cur_len > qpack_max_code_bits {
				return error('qpack: invalid huffman code (no symbol within ${qpack_max_code_bits} bits)')
			}
			if sym := qpack_huffman_decode_map[(u64(cur_len) << 32) | cur] {
				if sym == qpack_huffman_eos {
					return error('qpack: EOS symbol encountered in huffman string')
				}
				out << u8(sym)
				cur = 0
				cur_len = 0
			}
		}
	}
	if cur_len >= 8 {
		return error('qpack: invalid huffman padding (more than 7 bits left)')
	}
	if cur_len > 0 {
		mask := (u64(1) << cur_len) - 1
		if cur & mask != mask {
			return error('qpack: invalid huffman padding (not all ones)')
		}
	}
	return out
}

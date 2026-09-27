// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Module huffman builds canonical Huffman codes from a per-symbol array of bit
// lengths. This is the assignment step shared by RFC 1951 (DEFLATE, used by
// compress.deflate / zlib / gzip) and RFC 7541 Appendix B (HPACK, used by the
// HTTP/2 code in net.http): given only the lengths, both standards rebuild the
// exact same codes via the `bl_count` / `next_code` algorithm (RFC 1951 §3.2.2).
//
// What the two callers do NOT share — and therefore what this module is
// parameterized by — is the maximum code length (DEFLATE caps at 15 bits and
// decodes via a flat 2^max_bits table; HPACK goes up to 30 bits, where a flat
// table is infeasible) and the bit order (DEFLATE is LSB-first and bit-reverses
// every code, HPACK is MSB-first). The bit I/O loops and codec-specific
// semantics (EOS/padding, end-of-block, extra bits, distance alphabets) stay in
// the callers.
module huffman

// BitOrder selects how a code's bits are laid out in the returned `u32`.
pub enum BitOrder {
	// msb_first keeps the canonical code as-is: the first transmitted bit is
	// the most-significant bit of the code (RFC 7541 / HPACK).
	msb_first
	// lsb_first reverses each code within its length, so the first transmitted
	// bit is the least-significant bit (RFC 1951 / DEFLATE). This is the form a
	// flat LSB-first decode table is indexed by.
	lsb_first
}

// Config parameterizes build(). All fields are required and have no defaults:
// the two callers have intentionally different requirements (15 vs 30 bits,
// LSB vs MSB), so an implicit default would silently fit only one of them.
@[params]
pub struct Config {
pub:
	lengths   []int    @[required] // per-symbol code length in bits; 0 marks an unused symbol
	max_bits  int      @[required] // maximum allowed code length; must be >= every nonzero length
	bit_order BitOrder @[required] // .msb_first (HPACK) or .lsb_first (DEFLATE)
}

// Table is the result of build(): the canonical code for every symbol, plus the
// metadata a caller needs to drive its own bit I/O and to build a decode
// structure (flat_table() for small max_bits, decode_map() otherwise).
@[noinit]
pub struct Table {
pub:
	codes     []u32 // per-symbol code, right-aligned in a u32, in `bit_order`
	lengths   []int // per-symbol bit length (a copy of the input)
	max_bits  int
	bit_order BitOrder
}

// max_flat_bits is the largest max_bits for which flat_table() will
// allocate a table (2^max_bits entries). DEFLATE's 15 fits; HPACK's 30 does
// not and must use decode_map() / a bit-at-a-time decoder instead.
pub const max_flat_bits = 18

// flat_invalid_entry marks a flat_table() slot that no code maps to.
pub const flat_invalid_entry = u32(0xffff_ffff)

// flat_length_bits is how many low bits of a flat_table() entry hold the
// code length; the symbol is stored in the remaining high bits. 5 bits hold
// lengths up to 31, covering every max_bits <= max_flat_bits.
pub const flat_length_bits = 5

// next_codes validates `lengths` against `max_bits` and returns the canonical
// starting code for each length (RFC 1951 §3.2.2): next_code[l] is the code the
// first symbol of length l receives, and callers post-increment it per symbol.
// It also reports whether the code is `complete` (uses the whole code space,
// i.e. the Kraft inequality holds with equality), which lets flat_table() skip
// pre-filling the table. It is the single source of truth for the code
// assignment shared by build() and flat_table(). Errors on max_bits < 1 or
// > 32, a negative length or one exceeding max_bits, or an over-subscribed code
// (Kraft inequality); an incomplete (under-subscribed) code is allowed, as both
// DEFLATE and HPACK use.
fn next_codes(lengths []int, max_bits int) !([]u32, bool) {
	if max_bits < 1 {
		return error('huffman: max_bits must be >= 1, got ${max_bits}')
	}
	if max_bits > 32 {
		return error('huffman: max_bits ${max_bits} exceeds 32 (u32 code storage)')
	}
	mut bl_count := []int{len: max_bits + 1}
	defer {
		unsafe { bl_count.free() }
	}
	for sym, l in lengths {
		if l < 0 {
			return error('huffman: negative length ${l} for symbol ${sym}')
		}
		if l > max_bits {
			return error('huffman: length ${l} for symbol ${sym} exceeds max_bits ${max_bits}')
		}
		if l > 0 {
			bl_count[l]++
		}
	}
	// Kraft inequality: sum over used symbols of 2^(max_bits - len) must not
	// exceed 2^max_bits, i.e. the code must not be over-subscribed. left == 0 at
	// the end means the code is complete (covers every index of a flat table).
	mut left := u64(1) << max_bits
	for bits in 1 .. max_bits + 1 {
		used := u64(bl_count[bits]) << (max_bits - bits)
		if used > left {
			return error('huffman: over-subscribed code (lengths exceed the code space)')
		}
		left -= used
	}
	mut next_code := []u32{len: max_bits + 1}
	mut c := u32(0)
	for bits in 1 .. max_bits + 1 {
		c = (c + u32(bl_count[bits - 1])) << 1
		next_code[bits] = c
	}
	return next_code, left == 0
}

// build assigns a canonical Huffman code to every symbol from its bit length.
// Symbols with length 0 are unused and get code 0. It returns the codes plus
// the metadata for a decode structure; use it when you need the per-symbol
// codes (e.g. HPACK encoding) and/or decode_map(). Callers that only want a
// flat decode table should use flat_table(), which avoids materializing the
// codes array. See next_codes() for the validation rules.
pub fn build(cfg Config) !Table {
	mut next_code, _ := next_codes(cfg.lengths, cfg.max_bits)!
	defer {
		unsafe { next_code.free() }
	}
	mut codes := []u32{len: cfg.lengths.len}
	for sym, l in cfg.lengths {
		if l == 0 {
			continue
		}
		code := next_code[l]
		next_code[l]++
		codes[sym] = if cfg.bit_order == .lsb_first { bit_reverse(code, l) } else { code }
	}
	return Table{
		codes:     codes
		lengths:   cfg.lengths.clone()
		max_bits:  cfg.max_bits
		bit_order: cfg.bit_order
	}
}

// flat_table builds a 2^max_bits lookup table for fast decode of short codes
// (the DEFLATE strategy), directly from the lengths. Each entry is
// `(symbol << flat_length_bits) | length`, or flat_invalid_entry for indices no
// code reaches. The table is indexed by the next max_bits bits read from the
// stream in `bit_order`. It returns an error if max_bits > max_flat_bits, since
// the table would be prohibitively large (use build() + decode_map() instead).
//
// Unlike build(), this allocates no per-symbol codes array and does not copy
// the lengths: it assigns each code inline while filling the table, so a hot
// caller rebuilding a tree per block (compress.deflate) pays no extra
// allocation over hand-rolling the loop.
pub fn flat_table(cfg Config) ![]u32 {
	if cfg.max_bits > max_flat_bits {
		return error('huffman: max_bits ${cfg.max_bits} > max_flat_bits ${max_flat_bits}; use build() + decode_map()')
	}
	mut next_code, complete := next_codes(cfg.lengths, cfg.max_bits)!
	defer {
		unsafe { next_code.free() }
	}
	table_size := 1 << cfg.max_bits
	// A complete code writes every table slot, so the invalid pre-fill is dead
	// work; allocate zeroed (vcalloc) and skip it. An incomplete code leaves
	// gaps that must read back as flat_invalid_entry, so it pays the fill.
	mut table := if complete {
		[]u32{len: table_size}
	} else {
		[]u32{len: table_size, init: flat_invalid_entry}
	}
	for sym, l in cfg.lengths {
		if l == 0 {
			continue
		}
		raw := next_code[l]
		next_code[l]++
		entry := (u32(sym) << flat_length_bits) | u32(l)
		// The (max_bits - l) bits beyond the code are don't-cares, so a code of
		// length l fills 2^(max_bits - l) table slots. Where those slots sit
		// depends on bit_order: LSB-first codes occupy every index whose low l
		// bits match the code (stride by 2^l); MSB-first codes occupy a
		// contiguous block whose high l bits match the code.
		if cfg.bit_order == .lsb_first {
			step := 1 << l
			mut idx := int(bit_reverse(raw, l))
			for idx < table_size {
				table[idx] = entry
				idx += step
			}
		} else {
			block := 1 << (cfg.max_bits - l)
			base := int(raw) * block
			for k in 0 .. block {
				table[base + k] = entry
			}
		}
	}
	return table
}

// decode_map builds a map from a packed (length, code) key to its symbol, for
// codecs whose max_bits is too large for a flat table (HPACK's 30 bits). The
// key is `(u64(length) << 32) | code`; a decoder accumulates bits MSB-first and
// looks up after each bit. Only defined for .msb_first tables, where the
// accumulated value matches the stored code; it returns an error otherwise.
pub fn (t Table) decode_map() !map[u64]int {
	if t.bit_order != .msb_first {
		return error('huffman: decode_map requires .msb_first bit order')
	}
	mut m := map[u64]int{}
	for sym, l in t.lengths {
		if l == 0 {
			continue
		}
		m[(u64(l) << 32) | u64(t.codes[sym])] = sym
	}
	return m
}

// bit_reverse reverses the low `n` bits of `v` (used to convert a canonical
// MSB-first code into the LSB-first form a DEFLATE bit reader consumes).
fn bit_reverse(v u32, n int) u32 {
	mut r := u32(0)
	mut val := v
	for _ in 0 .. n {
		r = (r << 1) | (val & 1)
		val >>= 1
	}
	return r
}

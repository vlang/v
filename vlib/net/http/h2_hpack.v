// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// This file implements HPACK header compression (RFC 7541), used by HTTP/2.
// It is self-contained: it only depends on the static and Huffman tables in
// the sibling h2_hpack_*.v files, and does not touch the rest of net.http.

// h2_hpack_default_table_size is the default maximum size of the HPACK
// dynamic table, in bytes (RFC 7541 Section 4.2, and the default value of
// SETTINGS_HEADER_TABLE_SIZE).
pub const h2_hpack_default_table_size = 4096

// H2HeaderField is a single HTTP/2 header field (a name/value pair).
// Field names are expected to be lowercase, as required by RFC 7540.
pub struct H2HeaderField {
pub:
	name  string
	value string
}

// H2DynEntry is one entry in the HPACK dynamic table.
struct H2DynEntry {
	name  string
	value string
	size  int // name.len + value.len + 32 (RFC 7541 Section 4.1)
}

// H2DynTable is the HPACK dynamic table: a size-bounded FIFO of header fields.
// entries[0] is always the most recently added entry, which corresponds to the
// lowest dynamic index in the HPACK index space.
struct H2DynTable {
mut:
	entries  []H2DynEntry
	cur_size int
	max_size int = h2_hpack_default_table_size
}

// add inserts a new entry at the front of the table, evicting the oldest
// entries until it fits. Per RFC 7541 Section 4.4, an entry larger than the
// whole table empties it and is not added.
fn (mut t H2DynTable) add(name string, value string) {
	sz := name.len + value.len + 32
	for t.entries.len > 0 && t.cur_size + sz > t.max_size {
		t.cur_size -= t.entries.last().size
		t.entries.delete_last()
	}
	if sz > t.max_size {
		return
	}
	t.entries.insert(0, H2DynEntry{
		name:  name
		value: value
		size:  sz
	})
	t.cur_size += sz
}

// set_max_size updates the maximum table size, evicting oldest entries as
// needed to respect the new limit.
fn (mut t H2DynTable) set_max_size(n int) {
	t.max_size = n
	for t.entries.len > 0 && t.cur_size > t.max_size {
		t.cur_size -= t.entries.last().size
		t.entries.delete_last()
	}
}

// get returns the dynamic-table entry at 1-based dynamic index `i`
// (i.e. i==1 is the newest entry), or none if out of range.
fn (t &H2DynTable) get(i int) ?H2HeaderField {
	if i < 1 || i > t.entries.len {
		return none
	}
	e := t.entries[i - 1]
	return H2HeaderField{
		name:  e.name
		value: e.value
	}
}

// H2HpackReader is a cursor over an HPACK header block being decoded.
struct H2HpackReader {
	buf []u8
mut:
	pos int
}

// read_int reads an HPACK variable-length integer with a prefix of
// `prefix_bits` bits at the current position, advancing past it
// (RFC 7541 Section 5.1).
fn (mut r H2HpackReader) read_int(prefix_bits int) !u64 {
	if r.pos >= r.buf.len {
		return error('hpack: integer truncated')
	}
	max_prefix := u64((1 << prefix_bits) - 1)
	mut value := u64(r.buf[r.pos]) & max_prefix
	r.pos++
	if value < max_prefix {
		return value
	}
	mut m := 0
	for {
		if r.pos >= r.buf.len {
			return error('hpack: integer continuation truncated')
		}
		b := r.buf[r.pos]
		r.pos++
		value += u64(b & 0x7f) << m
		m += 7
		// A 64-bit value never needs more than 9 continuation bytes; reject
		// pathological inputs early (RFC 7541 Section 5.1 security note).
		if m > 63 {
			return error('hpack: integer overflow')
		}
		if b & 0x80 == 0 {
			break
		}
	}
	return value
}

// read_string reads an HPACK string literal at the current position,
// advancing past it and decoding Huffman coding when the H bit is set.
fn (mut r H2HpackReader) read_string() !string {
	if r.pos >= r.buf.len {
		return error('hpack: string truncated')
	}
	huffman := (r.buf[r.pos] & 0x80) != 0
	length := r.read_int(7)!
	// Compare in u64 space, before narrowing to int, so an oversized length
	// from a malicious peer cannot truncate past this bounds check.
	if length > u64(r.buf.len - r.pos) {
		return error('hpack: string length exceeds buffer')
	}
	n := int(length)
	raw := r.buf[r.pos..r.pos + n]
	r.pos += n
	if huffman {
		return h2_huffman_decode(raw)!.bytestr()
	}
	return raw.bytestr()
}

// h2_hpack_write_int appends an HPACK variable-length integer to `out`, using
// a prefix of `prefix_bits` bits whose high bits are set from `high_bits`.
fn h2_hpack_write_int(mut out []u8, value u64, prefix_bits int, high_bits u8) {
	max_prefix := u64((1 << prefix_bits) - 1)
	if value < max_prefix {
		out << high_bits | u8(value)
		return
	}
	out << high_bits | u8(max_prefix)
	mut v := value - max_prefix
	for v >= 0x80 {
		out << u8((v & 0x7f) | 0x80)
		v >>= 7
	}
	out << u8(v)
}

// h2_encode_string appends an HPACK string literal to `out`, choosing Huffman
// coding when it is shorter than the raw bytes (RFC 7541 Section 5.2).
fn h2_encode_string(mut out []u8, s string) {
	raw := s.bytes()
	huff := h2_huffman_encode(raw)
	if huff.len < raw.len {
		h2_hpack_write_int(mut out, u64(huff.len), 7, 0x80) // H = 1
		out << huff
	} else {
		h2_hpack_write_int(mut out, u64(raw.len), 7, 0x00) // H = 0
		out << raw
	}
}

// h2_is_sensitive reports whether a header should never be added to the HPACK
// dynamic table (encoded as "never indexed"), to avoid CRIME-style attacks.
fn h2_is_sensitive(name string) bool {
	return name == 'cookie' || name == 'authorization' || name == 'proxy-authorization'
}

// h2_hpack_find_static searches the static table for `name`/`value`.
// It returns the 1-based HPACK index and whether the value also matched.
// The index is 0 when the name is not present at all.
fn h2_hpack_find_static(name string, value string) (int, bool) {
	mut name_idx := 0
	for i, f in h2_hpack_static_table {
		if f.name == name {
			if f.value == value {
				return i + 1, true
			}
			if name_idx == 0 {
				name_idx = i + 1
			}
		}
	}
	return name_idx, false
}

// H2HpackEncoder encodes header field lists into HPACK header blocks.
// This encoder never adds entries to the dynamic table: it uses indexed
// representations for static-table hits and literal-without-indexing (or
// never-indexed, for sensitive headers) otherwise. That keeps encoder and
// decoder state trivially in sync while remaining fully interoperable.
pub struct H2HpackEncoder {
pub mut:
	dyn_table              H2DynTable
	pending_max_table_size int = -1 // -1 = no pending update; ≥0 = emit size update on next encode
}

// encode returns the HPACK header block for `fields`.
pub fn (mut e H2HpackEncoder) encode(fields []H2HeaderField) []u8 {
	mut out := []u8{}
	if e.pending_max_table_size >= 0 {
		// RFC 7541 §6.3: emit Dynamic Table Size Update at the start of the
		// first header block after a peer-requested table size change.
		h2_hpack_write_int(mut out, u64(e.pending_max_table_size), 5, 0x20)
		e.pending_max_table_size = -1
	}
	for f in fields {
		e.encode_field(mut out, f)
	}
	return out
}

fn (mut e H2HpackEncoder) encode_field(mut out []u8, f H2HeaderField) {
	sensitive := h2_is_sensitive(f.name)
	idx, exact := h2_hpack_find_static(f.name, f.value)
	if exact && !sensitive {
		// Indexed Header Field (RFC 7541 Section 6.1).
		h2_hpack_write_int(mut out, u64(idx), 7, 0x80)
		return
	}
	// Literal Header Field without Indexing (6.2.2) or Never Indexed (6.2.3).
	pattern := if sensitive { u8(0x10) } else { u8(0x00) }
	if idx != 0 {
		h2_hpack_write_int(mut out, u64(idx), 4, pattern)
	} else {
		h2_hpack_write_int(mut out, 0, 4, pattern)
		h2_encode_string(mut out, f.name)
	}
	h2_encode_string(mut out, f.value)
}

// H2HpackDecoder decodes HPACK header blocks into header field lists,
// maintaining the dynamic table across calls on the same connection.
pub struct H2HpackDecoder {
pub mut:
	dyn_table        H2DynTable
	max_dynamic_size int = h2_hpack_default_table_size // upper bound we advertised to the peer
}

// lookup resolves a 1-based HPACK index against the static and dynamic tables.
fn (d &H2HpackDecoder) lookup(idx u64) !H2HeaderField {
	if idx == 0 {
		return error('hpack: index 0 is not valid')
	}
	if idx <= u64(h2_hpack_static_len) {
		return h2_hpack_static_table[idx - 1]
	}
	// Validate the dynamic index in u64 space before narrowing to int, so an
	// out-of-range index from a malicious peer cannot truncate onto a valid
	// dynamic entry.
	dyn_idx := idx - u64(h2_hpack_static_len)
	if dyn_idx > u64(d.dyn_table.entries.len) {
		return error('hpack: index ${idx} out of range')
	}
	return d.dyn_table.get(int(dyn_idx)) or { error('hpack: index ${idx} out of range') }
}

fn (d &H2HpackDecoder) read_literal(mut r H2HpackReader, prefix_bits int) !H2HeaderField {
	name_index := r.read_int(prefix_bits)!
	name := if name_index == 0 {
		r.read_string()!
	} else {
		d.lookup(name_index)!.name
	}
	value := r.read_string()!
	return H2HeaderField{
		name:  name
		value: value
	}
}

// decode parses one HPACK header block and returns its header fields,
// updating the dynamic table as instructed by the block.
pub fn (mut d H2HpackDecoder) decode(block []u8) ![]H2HeaderField {
	mut out := []H2HeaderField{}
	mut r := H2HpackReader{
		buf: block
	}
	mut seen_field := false
	for r.pos < block.len {
		b := block[r.pos]
		if b & 0x80 != 0 {
			// Indexed Header Field (RFC 7541 Section 6.1).
			idx := r.read_int(7)!
			out << d.lookup(idx)!
			seen_field = true
		} else if b & 0x40 != 0 {
			// Literal Header Field with Incremental Indexing (6.2.1).
			f := d.read_literal(mut r, 6)!
			d.dyn_table.add(f.name, f.value)
			out << f
			seen_field = true
		} else if b & 0x20 != 0 {
			// Dynamic Table Size Update (6.3). Must precede any header field.
			if seen_field {
				return error('hpack: dynamic table size update after a header field')
			}
			new_size := r.read_int(5)!
			if int(new_size) > d.max_dynamic_size {
				return error('hpack: dynamic table size update ${new_size} exceeds limit ${d.max_dynamic_size}')
			}
			d.dyn_table.set_max_size(int(new_size))
		} else {
			// Literal without Indexing (6.2.2) or Never Indexed (6.2.3);
			// neither updates the dynamic table.
			f := d.read_literal(mut r, 4)!
			out << f
			seen_field = true
		}
	}
	return out
}

// set_max_dynamic_size updates the maximum dynamic-table size the decoder will
// accept (i.e. the SETTINGS_HEADER_TABLE_SIZE value advertised to the peer),
// shrinking the table immediately if needed.
pub fn (mut d H2HpackDecoder) set_max_dynamic_size(n int) {
	d.max_dynamic_size = n
	if d.dyn_table.max_size > n {
		d.dyn_table.set_max_size(n)
	}
}

module quic

// This file covers RFC 9204 §4.5: the Encoded Field Section Prefix (Required
// Insert Count wraparound math + Base sign/delta math) and the 6 field line
// representation types. Decoding resolves static/dynamic table references
// directly to name/value strings (mirrors `h2_hpack.v`'s `decode()`, which
// also resolves straight to `H2HeaderField`s rather than returning an
// intermediate index-only representation for a second pass).

// QpackFieldLine is one decoded (or to-be-encoded) HTTP field line.
// `never_index` is the 'N' bit (RFC 9204 §4.5.4, §7.1.3): when set on a
// decoded literal, this codebase (a client/server endpoint, never an
// intermediary) has no further re-encoding obligation regarding it -- see
// this module's matrix entry for why §7.1.3's re-encoding constraint is N/A
// here. It is still carried through so a caller CHOOSING to set it on an
// outgoing literal (e.g. for a sensitive header, see `qpack_is_sensitive`
// in qpack_encoder.v) round-trips correctly.
pub struct QpackFieldLine {
pub:
	name        string
	value       string
	never_index bool
}

// qpack_max_entries is MaxEntries from RFC 9204 §4.5.1.1: the largest
// number of entries the dynamic table could hold at the decoder's
// configured maximum capacity (NOT the table's current, possibly smaller,
// capacity -- the RFC is explicit that this uses "the maximum capacity of
// the dynamic table as specified by the decoder", a value fixed by SETTINGS
// for the life of the connection, independent of subsequent Set Dynamic
// Table Capacity instructions).
pub fn qpack_max_entries(max_table_capacity u64) u64 {
	return max_table_capacity / 32
}

// encode_ric transforms a Required Insert Count into its wire encoding
// (RFC 9204 §4.5.1.1), transcribed directly from the RFC's own pseudocode.
// Errors if `max_table_capacity` is too small to ever hold an entry (< 32
// bytes, RFC 9204's own per-entry overhead) -- mirrors `decode_ric`'s own
// `full_range == 0` guard just below, which this function used to lack,
// dividing by zero instead (self-found via Luna review).
pub fn encode_ric(req_insert_count u64, max_table_capacity u64) !u64 {
	if req_insert_count == 0 {
		return 0
	}
	full_range := 2 * qpack_max_entries(max_table_capacity)
	if full_range == 0 {
		return error('qpack: cannot encode a nonzero Required Insert Count with max table capacity ${max_table_capacity} too small to hold any entry')
	}
	return (req_insert_count % full_range) + 1
}

// decode_ric reconstructs the Required Insert Count from its wire encoding
// (RFC 9204 §4.5.1.1), given the decoder's own current total insert count
// and configured max table capacity. Transcribed directly from the RFC's
// own decoder pseudocode, including its error branches: a decoder MUST
// treat an EncodedInsertCount value no conformant encoder could have
// produced as QPACK_DECOMPRESSION_FAILED.
pub fn decode_ric(encoded_insert_count u64, total_inserts u64, max_table_capacity u64) !u64 {
	if encoded_insert_count == 0 {
		return 0
	}
	max_entries := qpack_max_entries(max_table_capacity)
	full_range := 2 * max_entries
	if full_range == 0 || encoded_insert_count > full_range {
		return error('qpack: encoded Required Insert Count out of range')
	}
	max_value := total_inserts + max_entries
	max_wrapped := (max_value / full_range) * full_range
	mut req_insert_count := max_wrapped + encoded_insert_count - 1
	if req_insert_count > max_value {
		if req_insert_count <= full_range {
			return error('qpack: encoded Required Insert Count out of range')
		}
		req_insert_count -= full_range
	}
	if req_insert_count == 0 {
		return error('qpack: encoded Required Insert Count decodes to zero')
	}
	return req_insert_count
}

// decode_base resolves the Base from its Sign bit and Delta Base value
// (RFC 9204 §4.5.1.2). Rejects the case the RFC calls out explicitly: Sign
// bit 1 with Delta Base >= Required Insert Count would make Base negative,
// which MUST be treated as invalid (and, not incidentally, is exactly what
// would underflow the unsigned subtraction below).
pub fn decode_base(sign bool, delta_base u64, req_insert_count u64) !u64 {
	if !sign {
		return req_insert_count + delta_base
	}
	if req_insert_count <= delta_base {
		return error('qpack: invalid Base (Sign=1 with Delta Base >= Required Insert Count)')
	}
	return req_insert_count - delta_base - 1
}

// encode_base computes the Sign bit and Delta Base value for a given Base
// and Required Insert Count (RFC 9204 §4.5.1.2, the encoder's side of the
// same relationship `decode_base` reverses).
pub fn encode_base(base u64, req_insert_count u64) (bool, u64) {
	if base >= req_insert_count {
		return false, base - req_insert_count
	}
	return true, req_insert_count - base - 1
}

// QpackFieldSectionPrefix is the decoded Required Insert Count + Base pair
// that precedes every encoded field section (RFC 9204 §4.5.1, Figure 12).
pub struct QpackFieldSectionPrefix {
pub:
	required_insert_count u64
	base                  u64
}

// decode_field_section_prefix decodes the two-integer prefix at the start
// of `buf` (RFC 9204 §4.5.1). Unlike this module's frame-level decoders,
// this returns a hard error (not `none`) on truncation: by the time this
// runs, `buf` is already a complete, length-delimited HEADERS frame
// payload (h3_frame.v's `H3FrameDecoder` only ever hands over a frame once
// its declared Length is fully buffered) -- so a short prefix here means
// the encoding itself is malformed, not merely "not arrived yet".
pub fn decode_field_section_prefix(buf []u8, total_inserts u64, max_table_capacity u64) !(QpackFieldSectionPrefix, int) {
	enc_ric, ric_len := decode_prefixed_int(buf, 8)!
	req_insert_count := decode_ric(enc_ric, total_inserts, max_table_capacity)!
	rest := buf[ric_len..]
	if rest.len == 0 {
		return error('qpack: encoded field section prefix truncated (Delta Base missing)')
	}
	sign := rest[0] & 0x80 != 0
	delta_base, delta_len := decode_prefixed_int(rest, 7)!
	base := decode_base(sign, delta_base, req_insert_count)!
	prefix := QpackFieldSectionPrefix{
		required_insert_count: req_insert_count
		base: base
	}
	return prefix, ric_len + delta_len
}

// encode_field_section_prefix encodes the Required Insert Count + Base
// prefix for an encoded field section (RFC 9204 §4.5.1). Errors if
// `req_insert_count` is nonzero and `max_table_capacity` is too small to
// represent it (see `encode_ric`).
pub fn encode_field_section_prefix(req_insert_count u64, base u64, max_table_capacity u64) ![]u8 {
	mut out := []u8{}
	enc_ric := encode_ric(req_insert_count, max_table_capacity)!
	encode_prefixed_int(mut out, enc_ric, 8, 0)
	sign, delta_base := encode_base(base, req_insert_count)
	high := if sign { u8(0x80) } else { u8(0) }
	encode_prefixed_int(mut out, delta_base, 7, high)
	return out
}

// QpackDecodedFieldLine is one decoded field line plus, if it referenced
// the dynamic table, the absolute index of that reference -- the caller
// (the decoder driver) needs this to verify it does not exceed the
// declared Required Insert Count (RFC 9204 §2.2.3) and to track it for
// eventual Section Acknowledgment.
pub struct QpackDecodedFieldLine {
pub:
	line             QpackFieldLine
	referenced_index ?u64
	consumed         int
}

// decode_qpack_field_line decodes ONE field line representation starting
// at `buf[0]` (RFC 9204 §4.5.2-§4.5.6), resolving any static or dynamic
// table reference. `base` must already be known (from
// `decode_field_section_prefix`). Same "already-complete buffer" contract
// as `decode_field_section_prefix` -- errors, does not return `none`, on
// truncation.
pub fn decode_qpack_field_line(buf []u8, base u64, dynamic_table &QpackDynamicTable) !QpackDecodedFieldLine {
	if buf.len == 0 {
		return error('qpack: field line representation truncated')
	}
	first := buf[0]
	if first & 0x80 != 0 {
		// Indexed Field Line (§4.5.2): 1 T Index(6+)
		is_static := first & 0x40 != 0
		index, len := decode_prefixed_int(buf, 6)!
		if is_static {
			e := qpack_static_lookup(int(index))!
			return QpackDecodedFieldLine{
				line: QpackFieldLine{
					name: e.name
					value: e.value
				}
				referenced_index: none
				consumed: len
			}
		}
		abs := dynamic_table.resolve_relative_from_base(index, base)!
		e := dynamic_table.get(abs)!
		return QpackDecodedFieldLine{
			line: QpackFieldLine{
				name: e.name
				value: e.value
			}
			referenced_index: u64(abs)
			consumed: len
		}
	}
	if first & 0x40 != 0 {
		// Literal Field Line with Name Reference (§4.5.4): 01 N T NameIndex(4+)
		never_index := first & 0x20 != 0
		is_static := first & 0x10 != 0
		name_index, idx_len := decode_prefixed_int(buf, 4)!
		value, _, val_len := decode_prefixed_string(buf[idx_len..], 7)!
		if is_static {
			e := qpack_static_lookup(int(name_index))!
			return QpackDecodedFieldLine{
				line: QpackFieldLine{
					name: e.name
					value: value
					never_index: never_index
				}
				referenced_index: none
				consumed: idx_len + val_len
			}
		}
		abs := dynamic_table.resolve_relative_from_base(name_index, base)!
		e := dynamic_table.get(abs)!
		return QpackDecodedFieldLine{
			line: QpackFieldLine{
				name: e.name
				value: value
				never_index: never_index
			}
			referenced_index: u64(abs)
			consumed: idx_len + val_len
		}
	}
	if first & 0x20 != 0 {
		// Literal Field Line with Literal Name (§4.5.6): 001 N H NameLen(3+)
		never_index := first & 0x10 != 0
		name, _, name_len := decode_prefixed_string(buf, 3)!
		value, _, val_len := decode_prefixed_string(buf[name_len..], 7)!
		return QpackDecodedFieldLine{
			line: QpackFieldLine{
				name: name
				value: value
				never_index: never_index
			}
			referenced_index: none
			consumed: name_len + val_len
		}
	}
	if first & 0x10 != 0 {
		// Indexed Field Line with Post-Base Index (§4.5.3): 0001 Index(4+)
		index, len := decode_prefixed_int(buf, 4)!
		abs := dynamic_table.resolve_post_base(index, base)!
		e := dynamic_table.get(abs)!
		return QpackDecodedFieldLine{
			line: QpackFieldLine{
				name: e.name
				value: e.value
			}
			referenced_index: u64(abs)
			consumed: len
		}
	}
	// Literal Field Line with Post-Base Name Reference (§4.5.5): 0000 N NameIdx(3+)
	never_index := first & 0x08 != 0
	name_index, idx_len := decode_prefixed_int(buf, 3)!
	value, _, val_len := decode_prefixed_string(buf[idx_len..], 7)!
	abs := dynamic_table.resolve_post_base(name_index, base)!
	e := dynamic_table.get(abs)!
	return QpackDecodedFieldLine{
		line: QpackFieldLine{
			name: e.name
			value: value
			never_index: never_index
		}
		referenced_index: u64(abs)
		consumed: idx_len + val_len
	}
}

// encode_indexed_static encodes an Indexed Field Line referencing the
// static table (RFC 9204 §4.5.2, Figure 13: `1 1` + 6-bit prefix index).
pub fn encode_indexed_static(index u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, index, 6, 0xC0)
	return out
}

// encode_indexed_dynamic_relative encodes an Indexed Field Line
// referencing the dynamic table by Base-relative index (RFC 9204 §4.5.2,
// Figure 13: `1 0` + 6-bit prefix index).
pub fn encode_indexed_dynamic_relative(rel_index u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, rel_index, 6, 0x80)
	return out
}

// encode_indexed_dynamic_post_base encodes an Indexed Field Line with
// Post-Base Index (RFC 9204 §4.5.3, Figure 14: `0001` + 4-bit prefix
// index).
pub fn encode_indexed_dynamic_post_base(post_index u64) []u8 {
	mut out := []u8{}
	encode_prefixed_int(mut out, post_index, 4, 0x10)
	return out
}

// encode_literal_with_name_ref encodes a Literal Field Line with Name
// Reference (RFC 9204 §4.5.4, Figure 15: `01` + N + T + 4-bit prefix name
// index + value string literal). `name_index` must already be resolved to
// the correct context (static index, or Base-relative dynamic index).
pub fn encode_literal_with_name_ref(is_static bool, never_index bool, name_index u64, value string) []u8 {
	mut out := []u8{}
	mut high := u8(0x40)
	if never_index {
		high |= 0x20
	}
	if is_static {
		high |= 0x10
	}
	encode_prefixed_int(mut out, name_index, 4, high)
	encode_prefixed_string(mut out, value, 7, 0)
	return out
}

// encode_literal_with_post_base_name_ref encodes a Literal Field Line with
// Post-Base Name Reference (RFC 9204 §4.5.5, Figure 16: `0000` + N + 3-bit
// prefix post-Base name index + value string literal).
pub fn encode_literal_with_post_base_name_ref(never_index bool, post_index u64, value string) []u8 {
	mut out := []u8{}
	mut high := u8(0x00)
	if never_index {
		high |= 0x08
	}
	encode_prefixed_int(mut out, post_index, 3, high)
	encode_prefixed_string(mut out, value, 7, 0)
	return out
}

// encode_literal_with_literal_name encodes a Literal Field Line with
// Literal Name (RFC 9204 §4.5.6, Figure 17: `001` + N + H + 3-bit prefix
// name length/string + value string literal).
pub fn encode_literal_with_literal_name(never_index bool, name string, value string) []u8 {
	mut out := []u8{}
	mut high := u8(0x20)
	if never_index {
		high |= 0x10
	}
	encode_prefixed_string(mut out, name, 3, high)
	encode_prefixed_string(mut out, value, 7, 0)
	return out
}

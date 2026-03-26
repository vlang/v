module v2

// HPACK primitives: header fields, dynamic table, and integer/string codecs (RFC 7541).

// HeaderField represents a name-value pair.
pub struct HeaderField {
pub mut:
	name      string
	value     string
	sensitive bool // if true, encode as never-indexed (RFC 7541 §6.2.3)
}

// size returns the size of the header field in bytes (RFC 7541 Section 4.1).
pub fn (h HeaderField) size() int {
	return 32 + h.name.len + h.value.len
}

// DynamicTable represents the HPACK dynamic table.
// Uses LIFO ordering per RFC 7541 §2.3.3: newest entry at index 0.
pub struct DynamicTable {
mut:
	entries  []HeaderField
	size     int
	max_size int = 4096
}

// add adds an entry to the dynamic table, evicting oldest entries as needed (RFC 7541 §4.4).
pub fn (mut dt DynamicTable) add(field HeaderField) {
	entry_size := field.size()

	if entry_size > dt.max_size {
		dt.entries = []HeaderField{}
		dt.size = 0
		return
	}

	for dt.size + entry_size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}

	// insert(0) is O(n), but max_size keeps the array small (~128 entries max)
	dt.entries.insert(0, field)
	dt.size += entry_size
}

// get retrieves an entry from the dynamic table (1-indexed).
pub fn (dt DynamicTable) get(index int) ?HeaderField {
	if index < 1 || index > dt.entries.len {
		return none
	}
	return dt.entries[index - 1]
}

// set_max_size updates the maximum size of the dynamic table.
pub fn (mut dt DynamicTable) set_max_size(size int) {
	dt.max_size = size

	for dt.size > dt.max_size && dt.entries.len > 0 {
		removed := dt.entries.pop()
		dt.size -= removed.size()
	}
}

fn get_indexed(dynamic_table &DynamicTable, index int) ?HeaderField {
	if index == 0 {
		return none
	}

	if index < static_table.len {
		return static_table[index]
	}

	dynamic_index := index - static_table.len + 1
	return dynamic_table.get(dynamic_index)
}

fn encode_hpack_integer(value int, prefix_bits int) []u8 {
	mut result := []u8{cap: 5}
	max_prefix := (1 << prefix_bits) - 1

	if value < max_prefix {
		result << u8(value)
	} else {
		result << u8(max_prefix)
		mut remaining := value - max_prefix

		for remaining >= 128 {
			result << u8((remaining % 128) + 128)
			remaining = remaining / 128
		}
		result << u8(remaining)
	}

	return result
}

fn decode_integer(data []u8, prefix_bits int) !(int, int) {
	if data.len == 0 {
		return error('empty data')
	}

	max_prefix := (1 << prefix_bits) - 1
	mask := u8(max_prefix)

	value := int(data[0] & mask)

	if value < max_prefix {
		return value, 1
	}

	mut result := value
	mut m := 0
	mut idx := 1

	for idx < data.len {
		if m > 25 {
			return error('integer overflow')
		}
		b := data[idx]
		result += int(b & 0x7f) << m
		m += 7
		idx++

		if (b & 0x80) == 0 {
			return result, idx
		}
	}

	return error('incomplete integer')
}

fn encode_string(s string, huffman bool) []u8 {
	if huffman {
		huffman_encoded := encode_huffman(s.bytes())
		encoded_len := encode_hpack_integer(huffman_encoded.len, 7)
		mut result := []u8{cap: encoded_len.len + huffman_encoded.len}
		result << (encoded_len[0] | 0x80)
		if encoded_len.len > 1 {
			result << encoded_len[1..]
		}
		result << huffman_encoded
		return result
	} else {
		encoded := encode_hpack_integer(s.len, 7)
		mut result := []u8{cap: encoded.len + s.len}
		result << encoded
		result << s.bytes()
		return result
	}
}

fn decode_string(data []u8) !(string, int) {
	if data.len == 0 {
		return error('empty data')
	}

	huffman := (data[0] & 0x80) != 0
	length, bytes_read := decode_integer(data, 7)!

	if data.len < bytes_read + length {
		return error('incomplete string')
	}

	str_data := data[bytes_read..bytes_read + length]

	if huffman {
		decoded := decode_huffman(str_data)!
		return decoded.bytestr(), bytes_read + length
	}

	return str_data.bytestr(), bytes_read + length
}

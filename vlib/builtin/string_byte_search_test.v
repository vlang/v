module builtin

fn test_byte_search_respects_unaligned_string_boundaries() {
	bytes := []u8{len: 320, init: u8(index)}
	for start in [0, 1, 3, 7, 15, 31] {
		for size in [0, 1, 2, 7, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257] {
			view := unsafe { tos(&bytes[start], size) }
			for byte in 0 .. 256 {
				mut first := -1
				mut last := -1
				for i in 0 .. size {
					if bytes[start + i] == u8(byte) {
						if first < 0 {
							first = i
						}
						last = i
					}
				}
				needle_bytes := [u8(byte)]
				needle := unsafe { tos(&needle_bytes[0], 1) }
				assert view.index_u8(u8(byte)) == first
				assert view.contains_u8(u8(byte)) == (first >= 0)
				assert view.index_(needle) == first
				assert view.contains(needle) == (first >= 0)
				assert view.index_last_(needle) == last
				assert view.last_index_u8(u8(byte)) == last
			}
		}
	}
}

fn byte_search_reference_index(s string, needle string) int {
	if needle.len == 0 {
		return 0
	}
	for start := 0; start + needle.len <= s.len; start++ {
		mut matches := true
		for i in 0 .. needle.len {
			if s[start + i] != needle[i] {
				matches = false
				break
			}
		}
		if matches {
			return start
		}
	}
	return -1
}

fn test_short_needle_search_handles_repeated_prefixes_and_nuls() {
	for text in ['', 'a', 'aaaaaaaaaaaaaaaaaaaaaaaaab', 'prefix.typeof(value).suffix', '\x00a\x00b\x00a\x00b\x00c',
		'ababaabababaabababaabababac'] {
		for needle in ['', 'a', 'b', 'c', '\x00', '\x00a', 'a\x00b', 'ab', 'aba', 'ababac', 'aaab',
			'aaaaaaaaaaaaaaaaab', 'typeof(', '.suffix', 'missing'] {
			for start in 0 .. text.len + 1 {
				for end in start .. text.len + 1 {
					view := unsafe { tos(text.str + start, end - start) }
					expected := byte_search_reference_index(view, needle)
					assert view.index_(needle) == expected
					assert view.contains(needle) == (expected >= 0)
					assert view.starts_with(needle) == (expected == 0)
					mut suffix := false
					if needle.len <= view.len {
						suffix = byte_search_reference_index(view[view.len - needle.len..], needle) == 0
					}
					assert view.ends_with(needle) == suffix
					mut last := -1
					for offset in 0 .. view.len + 1 {
						if offset + needle.len <= view.len
							&& byte_search_reference_index(view[offset..], needle) == 0 {
							last = offset
						}
					}
					assert view.index_last_(needle) == last
					for offset in [-1, 0, view.len / 2, view.len, view.len + 1] {
						from := if offset < 0 { 0 } else { offset }
						mut after := -1
						if from < view.len {
							found := byte_search_reference_index(view[from..], needle)
							if found >= 0 { after = from + found }
						}
						assert view.index_after_(needle, offset) == after
						assert (view.index_after(needle, offset) or { -1 }) == after
					}
				}
			}
		}
	}
}

fn test_short_needle_search_respects_borrowed_match_boundaries() {
	for size in 2 .. 17 {
		mut needle_bytes := []u8{len: size, init: u8(index % 3)}
		needle := needle_bytes.bytestr()
		for start in 0 .. 20 {
			mut bytes := []u8{len: start + size + 20, init: 0xff}
			copy(mut bytes[start..], needle_bytes)
			for end in start .. bytes.len + 1 {
				view := unsafe { tos(bytes.data, end) }
				assert view.index_(needle) == byte_search_reference_index(view, needle)
			}
		}
	}
}

fn test_byte_search_handles_adjacent_borrows_and_repeated_bytes() {
	for needle in [u8(0), 1, 0x7f, 0x80, 0xff] {
		mut bytes := []u8{len: 96, init: needle ^ 1}
		for offset in 0 .. bytes.len {
			bytes[offset] = needle
			for start in 0 .. offset + 1 {
				for end in offset + 1 .. bytes.len + 1 {
					view := unsafe { tos(&bytes[start], end - start) }
					assert view.index_u8(needle) == offset - start
					assert view.last_index_u8(needle) == offset - start
				}
			}
			bytes[offset] = needle ^ 1
		}
	}
}

fn test_byte_contains_checks_every_short_match_position() {
	for size in 1 .. 34 {
		for start in [0, 1, 3, 7, 15, 31] {
			for byte in [u8(0), 1, 0x7f, 0x80, 0xfe, 0xff] {
				mut bytes := []u8{len: start + size + 32, init: byte ^ 1}
				view := unsafe { tos(&bytes[start], size) }
				assert !view.contains_u8(byte)
				for position in 0 .. size {
					bytes[start + position] = byte
					assert view.contains_u8(byte)
					bytes[start + position] = byte ^ 1
				}
				bytes[start + size] = byte
				assert !view.contains_u8(byte)
				if start > 0 {
					bytes[start - 1] = byte
					assert !view.contains_u8(byte)
				}
			}
		}
	}
}

fn test_pair_search_respects_every_short_window_and_borrowed_boundary() {
	for first in [u8(0), 1, 0x7f, 0x80, 0xfe, 0xff] {
		for second in [u8(0), 1, 0x7f, 0x80, 0xfe, 0xff] {
			needle_bytes := [first, second]
			needle := unsafe { tos(&needle_bytes[0], 2) }
			for start in 0 .. 8 {
				for size in 0 .. 35 {
					mut bytes := []u8{len: start + size + 2, init: first ^ 1}
					view := unsafe { tos(&bytes[start], size) }
					assert view.index_(needle) == byte_search_reference_index(view, needle)
					for at in 0 .. size - 1 {
						bytes[start + at] = first
						bytes[start + at + 1] = second
						expected := byte_search_reference_index(view, needle)
						assert view.index_(needle) == expected
						assert view.contains(needle) == (expected >= 0)
						bytes[start + at] = first ^ 1
						bytes[start + at + 1] = first ^ 1
					}
					// A pair straddling the end must not become an in-span match.
					if size > 0 { bytes[start + size - 1] = first }
					bytes[start + size] = second
					assert view.index_(needle) == byte_search_reference_index(view, needle)
				}
			}
		}
	}
}

fn test_pair_search_rejects_adjacent_mask_borrows() {
	for first in 0 .. 256 {
		for second in 0 .. 256 {
			needle_bytes := [u8(first), u8(second)]
			needle := unsafe { tos(&needle_bytes[0], 2) }
			mut bytes := []u8{len: 32, init: if index & 1 == 0 { u8(first) } else { u8(second) ^ 1 }}
			view := unsafe { tos(&bytes[0], bytes.len) }
			assert view.index_(needle) == byte_search_reference_index(view, needle)
			bytes[30] = u8(first)
			bytes[31] = u8(second)
			assert view.index_(needle) == byte_search_reference_index(view, needle)
		}
	}
}

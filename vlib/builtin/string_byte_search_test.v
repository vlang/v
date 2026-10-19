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
				}
			}
		}
	}
}

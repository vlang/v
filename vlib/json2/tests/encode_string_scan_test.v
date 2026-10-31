module json2_test

import json2

fn escaped_single_byte(b u8, escape_unicode bool) string {
	return match b {
		`"` { r'\"' }
		`\\` { r'\\' }
		`\b` { r'\b' }
		`\f` { r'\f' }
		`\n` { r'\n' }
		`\r` { r'\r' }
		`\t` { r'\t' }
		else {
			if b < 0x20 {
				'\\u00${b:02x}'
			} else if escape_unicode && b >= 0x80 {
				// A single non-ASCII byte surrounded by ASCII is invalid UTF-8.
				r'\ufffd'
			} else {
				[b].bytestr()
			}
		}
	}
}

fn test_every_byte_at_scan_boundaries_and_short_tails() {
	for escape_unicode in [false, true] {
		for offset in 0 .. 17 {
			prefix := 'a'.repeat(offset)
			for tail in [0, 1, 2, 7, 8, 9, 15, 16, 17, 31] {
				suffix := 'z'.repeat(tail)
				for value in 0 .. 256 {
					byte := u8(value)
					text := prefix + [byte].bytestr() + suffix
					expected := '"' + prefix + escaped_single_byte(byte, escape_unicode) + suffix + '"'
					assert json2.encode(text, escape_unicode: escape_unicode) == expected, 'byte=${value}, offset=${offset}, tail=${tail}, escape_unicode=${escape_unicode}'
				}
			}
		}
	}
}

fn test_unicode_and_adjacent_escapes_across_word_boundaries() {
	for offset in 0 .. 17 {
		prefix := 'a'.repeat(offset)
		text := prefix + 'مرحبا 😀 é\n"\\\t' + 'z'.repeat(17)
		expected := '"' + prefix + r'\u0645\u0631\u062d\u0628\u0627 \uD83D\ude00 \u00e9\n\"\\\t' + 'z'.repeat(17) + '"'
		assert json2.encode(text, escape_unicode: true) == expected
		assert json2.decode[string](json2.encode(text))! == text
	}
}

fn test_unaligned_strings_and_exact_allocation_ends() {
	for offset in 0 .. 8 {
		for length in 0 .. 97 {
			// A trailing NUL is the only readable byte beyond the string's length.
			mut storage := []u8{len: offset + length + 1, init: `x`}
			storage[storage.len - 1] = 0
			text := unsafe { (&storage[offset]).vstring_with_len(length) }
			for escape_unicode in [false, true] {
				assert json2.encode(text, escape_unicode: escape_unicode) == '"' + 'x'.repeat(length) + '"'
			}
			unsafe { storage.free() }
		}
	}
}

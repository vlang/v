module ssa

import strconv

// decode_native_c_string converts the scanner's escaped C literal payload to bytes.
fn decode_native_c_string(text string) string {
	if !text.contains('\\') {
		return text
	}
	mut bytes := []u8{cap: text.len}
	mut i := 0
	for i < text.len {
		if text[i] != `\\` || i + 1 == text.len {
			bytes << text[i]
			i++
			continue
		}
		next := text[i + 1]
		if next == `\n` || (next == `\r` && i + 2 < text.len && text[i + 2] == `\n`) {
			i += if next == `\n` { 2 } else { 3 }
			for i < text.len && text[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `$` || (next == `0`
			&& (i + 3 >= text.len || text[i + 2] !in `0` .. `8` || text[i + 3] !in `0` .. `8`)) {
			bytes << if next == `$` { u8(`$`) } else { u8(0) }
			i += 2
			continue
		}
		quote := if next in [`'`, `"`] { next } else { u8(0) }
		decoded := strconv.unquote_char(text[i..], quote) or {
			// Keep unknown escapes as the parser does for ordinary V strings.
			bytes << u8(`\\`)
			bytes << next
			i += 2
			continue
		}
		if decoded.multibyte {
			bytes << decoded.value.str().bytes()
		} else {
			bytes << u8(decoded.value)
		}
		i = text.len - decoded.tail.len
	}
	return bytes.bytestr()
}

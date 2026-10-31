module json2

fn (mut encoder Encoder) encode_key(key string) {
	if encoder.json5 && encoder.unquoted_keys && json5_is_identifier(key) {
		for byte in key {
			encoder.output << byte
		}
	} else {
		encoder.encode_string(key)
	}
}

fn json5_is_identifier(key string) bool {
	if key.len == 0 {
		return false
	}
	for i, ch in key.runes_iterator() {
		if i == 0 {
			if !json5_is_ident_start(int(ch)) {
				return false
			}
		} else if !json5_is_ident_part(int(ch)) {
			return false
		}
	}
	return true
}

@[manualfree]
fn (mut encoder Encoder) encode_json5_string(value string) {
	quote := if encoder.single_quotes { u8(`'`) } else { u8(`"`) }
	encoder.output << quote
	runes := value.runes()
	defer { unsafe { runes.free() } }
	mut utf8_buffer := [5]u8{}
	for ch in runes {
		if int(ch) == int(quote) || int(ch) == r_backslash {
			encoder.output << `\\`
			encoder.output << u8(ch)
			continue
		}
		match ch {
			`\n`, `\r`, `\t`, `\b`, `\f` {
				encoder.output << `\\`
				encoder.output << match ch {
					`\n` { u8(`n`) }
					`\r` { u8(`r`) }
					`\t` { u8(`t`) }
					`\b` { u8(`b`) }
					else { u8(`f`) }
				}
			}
			else {
				if ch < 0x20 || ch == 0x2028 || ch == 0x2029
					|| (encoder.escape_unicode && ch >= 0x80) {
					if ch > 0xffff {
						point := u32(ch) - 0x10000
						encoder.encode_unicode_escape(0xD800 + (point >> 10), false)
						encoder.encode_unicode_escape(0xDC00 + (point & 0x3ff), false)
					} else {
						encoder.encode_unicode_escape(u32(ch), false)
					}
				} else {
					// This view borrows the local UTF-8 buffer and is copied immediately.
					encoded := unsafe { utf32_to_str_no_malloc(u32(ch), mut &utf8_buffer[0]) }
					unsafe { encoder.output.push_many(encoded.str, encoded.len) }
				}
			}
		}
	}
	encoder.output << quote
}

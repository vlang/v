module json2

fn (mut encoder Encoder) encode_key(key string) {
	if encoder.json5 && encoder.unquoted_keys && json5_is_identifier(key) {
		encoder.output << key.bytes()
	} else {
		encoder.encode_string(key)
	}
}

fn json5_is_identifier(key string) bool {
	if key.len == 0 {
		return false
	}
	for i, ch in key.runes() {
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

fn (mut encoder Encoder) encode_json5_string(value string) {
	encoder.output << `'`
	for ch in value.runes() {
		match ch {
			`\\`, `'` {
				encoder.output << `\\`
				encoder.output << u8(ch)
			}
			`\n` { encoder.output << '\\n'.bytes() }
			`\r` { encoder.output << '\\r'.bytes() }
			`\t` { encoder.output << '\\t'.bytes() }
			`\b` { encoder.output << '\\b'.bytes() }
			`\f` { encoder.output << '\\f'.bytes() }
			else {
				if ch < 0x20 || ch == 0x2028 || ch == 0x2029
					|| (encoder.escape_unicode && ch >= 0x80) {
					if ch > 0xffff {
						point := u32(ch) - 0x10000
						encoder.output << '\\u${0xD800 + (point >> 10):04x}\\u${0xDC00 + (point & 0x3ff):04x}'.bytes()
					} else {
						encoder.output << '\\u${u32(ch):04x}'.bytes()
					}
				} else {
					encoder.output << ch.str().bytes()
				}
			}
		}
	}
	encoder.output << `'`
}

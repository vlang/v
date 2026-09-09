module ui

fn key_event_with_char(c rune) C.KEY_EVENT_RECORD {
	mut e := C.KEY_EVENT_RECORD{}
	unsafe {
		e.uChar.UnicodeChar = c
	}
	return e
}

fn test_key_code_for_char_maps_ascii_characters() {
	assert key_code_for_char(key_event_with_char(`a`)) == .a
	assert key_code_for_char(key_event_with_char(rune(0x1B))) == .escape
}

fn test_key_code_for_char_ignores_non_ascii_characters() {
	// characters with a 0x1B low byte, like `创` (U+521B) and `唛` (U+551B),
	// previously produced fake .escape key events
	assert key_code_for_char(key_event_with_char(rune(0x521B))) == .null
	assert key_code_for_char(key_event_with_char(rune(0x551B))) == .null
	// `é` (U+00E9) previously mapped to an invalid KeyCode value
	assert key_code_for_char(key_event_with_char(rune(0xE9))) == .null
}

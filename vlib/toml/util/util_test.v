module util

fn test_is_key_char() {
	for c in [`a`, `z`, `A`, `Z`, `q`, `0`, `9`, `_`, `-`, `.`, `+`, ` `, `/`, `!`] {
		// only letters are key characters; `_` and `-` are recognised later, when tokenizing
		assert is_key_char(u8(c)) == ((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`))
	}
}

fn test_is_ascii_control_character() {
	for b in u8(0) .. 0x20 {
		assert is_ascii_control_character(b)
	}
	assert is_ascii_control_character(0x7f)
	for b in [u8(0x20), 0x21, 0x41, 0x7e, 0x80, 0xff] {
		assert !is_ascii_control_character(b)
	}
}

// TOML allows tab but rejects every other ASCII control character.
fn test_is_illegal_ascii_control_character() {
	assert !is_illegal_ascii_control_character(0x09)
	for b in u8(0) .. 0x09 {
		assert is_illegal_ascii_control_character(b)
	}
	for b in u8(0x0a) .. 0x20 {
		assert is_illegal_ascii_control_character(b)
	}
	assert is_illegal_ascii_control_character(0x7f)
	for b in [u8(0x20), 0x41, 0x7e, 0x80, 0xff] {
		assert !is_illegal_ascii_control_character(b)
	}
}

// illegal is a strict subset of control, and the only difference is tab
fn test_illegal_is_control_without_tab() {
	for i in 0 .. 256 {
		b := u8(i)
		if is_ascii_control_character(b) {
			assert is_illegal_ascii_control_character(b) == (b != 0x09)
		} else {
			assert !is_illegal_ascii_control_character(b)
		}
	}
}

module quic

fn test_is_h3_reserved_codepoint_matches_known_grease_values() {
	// RFC 9114 §6.2.3/§7.2.4.1/§7.2.8/§8.1 give the same first few terms
	// explicitly: 0x21, 0x40, ... through 0x3ffffffffffffffe.
	assert is_h3_reserved_codepoint(0x21)
	assert is_h3_reserved_codepoint(0x40)
	assert is_h3_reserved_codepoint(0x5f)
	assert is_h3_reserved_codepoint(0x7e)
	assert is_h3_reserved_codepoint(0x3ffffffffffffffe)
}

fn test_is_h3_reserved_codepoint_rejects_adjacent_non_grease_values() {
	// Boundary values immediately next to grease values must NOT match --
	// this is exactly the off-by-one a modular-arithmetic slip would produce.
	assert !is_h3_reserved_codepoint(0x20)
	assert !is_h3_reserved_codepoint(0x22)
	assert !is_h3_reserved_codepoint(0x3f)
	assert !is_h3_reserved_codepoint(0x41)
	assert !is_h3_reserved_codepoint(0x5e)
	assert !is_h3_reserved_codepoint(0x60)
}

fn test_is_h3_reserved_codepoint_rejects_defined_values() {
	// None of the values this phase actually assigns meaning to may ever
	// collide with the grease sequence -- spot-check the ones in scope.
	assert !is_h3_reserved_codepoint(0x00) // DATA / control stream type

	assert !is_h3_reserved_codepoint(0x01) // HEADERS / push stream type

	assert !is_h3_reserved_codepoint(0x03) // CANCEL_PUSH

	assert !is_h3_reserved_codepoint(0x04) // SETTINGS

	assert !is_h3_reserved_codepoint(0x06) // MAX_FIELD_SECTION_SIZE setting

	assert !is_h3_reserved_codepoint(0x07) // GOAWAY

	assert !is_h3_reserved_codepoint(0x0d) // MAX_PUSH_ID

	assert !is_h3_reserved_codepoint(0x0100) // H3_NO_ERROR

	assert !is_h3_reserved_codepoint(0x0110) // H3_VERSION_FALLBACK
}

fn test_is_h3_reserved_codepoint_rejects_values_below_first_grease_term() {
	assert !is_h3_reserved_codepoint(0)
	assert !is_h3_reserved_codepoint(1)
	assert !is_h3_reserved_codepoint(0x1f)
}

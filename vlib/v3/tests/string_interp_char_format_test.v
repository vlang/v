fn test_character_interpolation_with_static_width() {
	assert '${u8(`f`):1c}' == 'f'
	assert '${u8(`f`):3c}' == '  f'
	assert '${u8(`f`):-3c}' == 'f  '
}

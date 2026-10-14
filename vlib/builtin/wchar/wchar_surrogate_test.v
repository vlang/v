import builtin.wchar

// A C.wchar_t is an UTF-16 code unit on windows, and a whole code point elsewhere.
fn test_to_string_keeps_a_character_above_the_basic_multilingual_plane() {
	$if windows {
		units := [u16(`a`), 0xd83d, 0xde00, u16(`b`), 0]
		assert unsafe { wchar.to_string(units.data) } == 'a😀b'
		assert unsafe { wchar.to_string2(units.data, 4) } == 'a😀b'
		// a pair at the end of the input
		assert unsafe { wchar.to_string2(units.data, 3) }.bytes().hex() == '61f09f9880'
		// The low surrogate is outside of the given length: the high one is unpaired,
		// and is kept in its 3 byte form, like string_from_wide2 does.
		assert unsafe { wchar.to_string2(units.data, 2) }.bytes().hex() == '61eda0bd'
		reversed := [u16(0xde00), 0xd83d, 0]
		assert unsafe { wchar.to_string(reversed.data) }.bytes().hex() == 'edb880eda0bd'
	} $else {
		p := wchar.from_string('a😀b')
		assert unsafe { wchar.length_in_characters(p) } == 3
		assert unsafe { wchar.to_string(p) } == 'a😀b'
		assert unsafe { wchar.to_string2(p, 2) } == 'a😀'
	}
}

fn test_to_string_keeps_text_of_the_basic_multilingual_plane() {
	for text in ['', 'abc 123', 'Проба', 'é € 中文 한국어'] {
		p := wchar.from_string(text)
		assert unsafe { wchar.to_string(p) } == text
		assert unsafe { wchar.to_string2(p, text.runes().len) } == text
	}
}

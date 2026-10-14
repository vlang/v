module readline

// test_analyse_treats_rune_255_as_character checks that U+00FF (ÿ) is
// inserted as a character instead of ending the line: 255 is a valid rune,
// not an EOF indicator (read errors never reach analyse; read_char returns
// them as errors first).
fn test_analyse_treats_rune_255_as_character() {
	mut r := Readline{}
	assert r.analyse(255) == .insert_character
	assert r.analyse(int(`ÿ`)) == .insert_character
	// the real control characters still end the line
	assert r.analyse(0) == .eof
	assert r.analyse(0x3) == .eof
	assert r.analyse(0x4) == .eof
}

// test_delete_word_left_on_single_character_line checks that Ctrl+W on a
// one-character line deletes it instead of panicking on index -1: after the
// end-of-line clamp the cursor is 0, so there is no previous character.
fn test_delete_word_left_on_single_character_line() {
	mut r := Readline{
		current: [rune(`a`)]
		cursor:  1
	}
	r.delete_word_left()
	assert r.current == []rune{}
	assert r.cursor == 0
}

fn test_delete_word_left_handles_empty_spaces_and_unicode() {
	for text in ['', ' ', 'a ', 'ÿ', '日本語'] {
		mut r := Readline{ current: text.runes(), cursor: text.runes().len }
		r.delete_word_left()
		assert r.current == []rune{}
		assert r.cursor == 0
	}
	mut r := Readline{ current: 'left right'.runes(), cursor: 5 }
	r.delete_word_left()
	assert r.current.string() == 'right'
	assert r.cursor == 0
}

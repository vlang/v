module readline

fn C.clearerr(stream &C.FILE)

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

// test_read_line_utf8_leaves_raw_mode_when_reading_fails checks that a read
// error does not leave the terminal in raw mode. stdin is replaced by
// /dev/null, so the first read fails; `is_raw` stands for raw mode having been
// enabled on a terminal, which `read_line_utf8` must undo before returning.
fn test_read_line_utf8_leaves_raw_mode_when_reading_fails() {
	saved := C.dup(0)
	devnull := C.open(c'/dev/null', C.O_RDONLY)
	assert saved >= 0 && devnull >= 0
	// If stdin were not replaced, the test would read the real terminal.
	assert C.dup2(devnull, 0) == 0
	defer {
		C.dup2(saved, 0)
		C.close(saved)
		C.close(devnull)
		C.clearerr(C.stdin)
	}
	mut r := Readline{
		is_raw: true
	}
	r.read_line_utf8('') or {
		assert !r.is_raw, 'read_line_utf8 returned `${err.msg()}` with raw mode still on'
		return
	}
	assert false, 'reading from /dev/null must fail'
}

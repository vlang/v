import ncurses

fn test_ncurses_constants_and_helpers() {
	$if windows {
		assert !ncurses.is_supported()
	} $else {
		assert ncurses.is_supported()
		assert ncurses.ok == 0
		assert ncurses.err == -1
		assert ncurses.cursor_normal == 1
		assert ncurses.key_code_yes != 0
		assert ncurses.key_f(1) != 0
		assert ncurses.color_pair(1) != 0
		assert ncurses.a_bold != 0
	}
}

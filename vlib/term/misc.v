module term

// h_divider will return a horizontal divider line with a dynamic width,
// that depends on the current terminal settings
pub fn h_divider(divider string) string {
	mut cols := 76
	term_cols, _ := get_terminal_size()

	if term_cols > 0 {
		cols = term_cols
	}

	result := divider.repeat(1 + (cols / divider.len))
	return result[0..cols]
}

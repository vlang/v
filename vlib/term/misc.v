module term

// h_divider will return a horizontal divider line with a dynamic width,
// that depends on the current terminal settings
pub fn h_divider(divider string) string {
	mut cols := 76
	term_cols, term_rows := get_terminal_size()
  eprintln('h_divider term_cols: $term_cols')
  eprintln('h_divider term_rows: $term_rows')

	if term_cols > 0 {
		cols = term_cols
	}

	result := divider.repeat(1 + (cols / divider.len))
	return result[0..cols]
}

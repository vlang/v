module term

#const $tty = require('tty');
// get_terminal_size returns a number of columns and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	$if js_node {
		cols := 0
		rows := 0
		#let sizes = $tty.WriteStream(1).getWindowSize();
		#cols.val = sizes[0];
		#rows.val = sizes[1];

		return cols, rows
	} $else {
		return default_columns_size, default_rows_size
	}
}

// clear clears current terminal screen.
pub fn clear() bool {
	print('\x1b[2J')
	print('\x1b[H')
	return true
}

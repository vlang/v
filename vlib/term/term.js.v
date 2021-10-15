module term

#const $tty = require('tty');
// get_terminal_size returns a number of colums and rows of terminal window.
pub fn get_terminal_size() (int, int) {
	$if js_node {
		colums := 0
		rows := 0
		#let sizes = $tty.WriteStream(1).getWindowSize();
		#colums.val = sizes[0];
		#rows.val = sizes[1];

		return colums, rows
	} $else {
		return default_columns_size, default_rows_size
	}
}

// clear clears current terminal screen.
pub fn clear() {
	print('\x1b[2J')
	print('\x1b[H')
}

module progress

import term

// terminal_size returns the columns and rows of the console, or (0, 0).
// NOTE: term.get_terminal_size() inspects the stdout handle, so this is only
// exact when the bars draw to stdout or both streams are the same console.
// Escape sequences also need virtual-terminal processing, which V's runtime
// turns on for stdout and stderr at startup only when stdout is a terminal;
// is_interactive() checks the console's real state, so otherwise the bars fall
// back to plain output. (Not tested on Windows.)
fn terminal_size(fd int) (int, int) {
	_ = fd
	w, h := term.get_terminal_size()
	return w, h
}

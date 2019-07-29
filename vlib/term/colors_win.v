// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module term

import os

// Calling this functions enables color terminal output on windows
// Maybe implement a way to auto run an init function when a module
// is imported on a certain os. for example to run this?
pub fn enable_term_color_win() {
	$if windows {
		mode_wanted := os.ENABLE_PROCESSED_OUTPUT | os.ENABLE_VIRTUAL_TERMINAL_PROCESSING
		mut mode_current := 0
		h_output := C.GetStdHandle(os.STD_OUTPUT_HANDLE)
		if h_output == os.INVALID_HANDLE_VALUE {
			panic('term.enable_term_color_win(): error getting output handle.')
		}
		if !C.GetConsoleMode(h_output, &mode_current) {
			panic('term.enable_term_color_win(): error getting console mode.')
		}
		if mode_wanted == mode_current {
			return
		}
		if !C.SetConsoleMode(h_output, mode_wanted) {
			panic('term.enable_term_color_win(): error setting console mode.')
		}
	}
	$else {
		println('term.enable_term_color_win() should only be called on windows.')
	}
}

pub fn format(msg, open, close string) string {
	enable_term_color_win()
	return _format(msg, open, close)
}

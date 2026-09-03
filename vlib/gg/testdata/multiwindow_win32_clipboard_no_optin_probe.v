module main

import gg

fn main() {
	mut app := gg.App{}
	window := gg.WindowId{}
	mut rejected := 0
	_ = app.request_clipboard_text(window) or {
		if err.msg() != 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App' {
			panic(err.msg())
		}
		rejected++
		gg.ClipboardRequestId{}
	}
	_ = app.set_clipboard_text(window, 'disabled') or {
		if err.msg() != 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App' {
			panic(err.msg())
		}
		rejected++
		gg.ClipboardRequestId{}
	}
	if rejected != 2 {
		panic('clipboard APIs were admitted without opt-in')
	}
	println('CCOMPILER=' + @CCOMPILER)
}

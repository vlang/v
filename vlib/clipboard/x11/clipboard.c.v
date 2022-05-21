// Currently there is only X11 Selections support and no way to handle Wayland
// but since Wayland isn't extremely adopted, we are covering almost all Linux distros.
module x11

import os


[heap]
pub struct Clipboard {
mut:
	text      string // text data sent or received
	got_text  bool   // used to confirm that we have got the text
	is_owner  bool   // to save selection owner state
}

// new_clipboard returns a new `Clipboard` instance allocated on the heap.
// The `Clipboard` resources can be released with `free()`
pub fn new_clipboard() &Clipboard {
	return &Clipboard{}
}

pub fn (cb &Clipboard) check_availability() bool {
	return os.execute("type xsel").exit_code == 0
}

pub fn (mut cb Clipboard) free() {
}

// clear empties the current selection in the system clipboard.
pub fn (mut cb Clipboard) clear() {
	os.system("xsel -cb")
	cb.is_owner = false
	cb.text = ''
}

pub fn (cb &Clipboard) has_ownership() bool {
	return cb.is_owner
}

fn (cb &Clipboard) take_ownership() {
}

// set_text stores `text` in the system clipboard.
pub fn (mut cb Clipboard) set_text(text string) bool {
	output := os.execute("printf " + text + " | xsel -b")
	cb.is_owner = output.exit_code == 0
	return cb.is_owner
}

// get_text retrieves the current selection in the system clipboard.
pub fn (mut cb Clipboard) get_text() string {
	output := os.execute("xsel -b")
	cb.text = output.output
	return cb.text
}

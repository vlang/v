module dummy

pub struct Clipboard {
mut:
	text     string // text data sent or received
	got_text bool   // used to confirm that we have got the text
	is_owner bool   // to save selection owner state
}

// new_clipboard returns a new `Clipboard` instance allocated on the heap.
// The `Clipboard` resources can be released with `free()`
pub fn new_clipboard() &Clipboard {
	return &Clipboard{}
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	return &Clipboard{}
}

pub fn (mut cb Clipboard) set_text(text string) bool {
	cb.text = text
	cb.is_owner = true
	cb.got_text = true
	return true
}

pub fn (mut cb Clipboard) get_text() string {
	return cb.text
}

pub fn (mut cb Clipboard) clear() {
	cb.text = ''
	cb.is_owner = false
}

pub fn (mut cb Clipboard) free() {
}

pub fn (cb &Clipboard) has_ownership() bool {
	return cb.is_owner
}

pub fn (cb &Clipboard) check_availability() bool {
	// This is a dummy clipboard implementation,
	// which can be always used, although it does not do much...
	return true
}

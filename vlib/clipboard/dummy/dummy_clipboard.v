module dummy

pub struct Clipboard {
mut:
	text     string // text data sent or received
	got_text bool // used to confirm that we have got the text
	is_owner bool // to save selection owner state
}

pub fn new_clipboard() &Clipboard {
	return &Clipboard{}
}

pub fn new_primary() &Clipboard {
	return &Clipboard{}
}

fn (mut cb Clipboard) set_text(text string) bool {
	cb.text = text
	cb.is_owner = true
	cb.got_text = true
	return true
}

fn (mut cb Clipboard) get_text() string {
	return cb.text
}

fn (mut cb Clipboard) clear() {
	cb.text = ''
	cb.is_owner = false
}

fn (mut cb Clipboard) free() {
}

fn (cb &Clipboard) has_ownership() bool {
	return cb.is_owner
}

fn (cb &Clipboard) check_availability() bool {
	// This is a dummy clipboard implementation,
	// which can be always used, although it does not do much...
	return true
}

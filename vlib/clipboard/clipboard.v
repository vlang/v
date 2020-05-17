module clipboard

// create a new clipboard
pub fn new() &Clipboard {
	return new_clipboard()
}

// copy some text into the clipboard
pub fn (mut cb Clipboard) copy(text string) bool {
	return cb.set_text(text)
}

// get the text from the clipboard
pub fn (mut cb Clipboard) paste() string {
	return cb.get_text()
}

// clear the clipboard
pub fn (mut cb Clipboard) clear_all() {
	cb.clear()
}

// destroy the clipboard
pub fn (mut cb Clipboard) destroy() {
	cb.free()
}

// check if we own the clipboard
pub fn (cb Clipboard) check_ownership() bool {
	return cb.has_ownership()
}

// check if clipboard can be used
pub fn (cb &Clipboard) is_available() bool {
	return cb.check_availability()
}

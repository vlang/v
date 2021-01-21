module clipboard

// new returns a new `Clipboard` instance allocated on the heap.
// The `Clipboard` resources can be released with `free()`
pub fn new() &Clipboard {
	return new_clipboard()
}

// copy copies `text` into the clipboard.
pub fn (mut cb Clipboard) copy(text string) bool {
	return cb.set_text(text)
}

// paste returns current entry as a `string` from the clipboard.
pub fn (mut cb Clipboard) paste() string {
	return cb.get_text()
}

// clear_all clears the clipboard.
pub fn (mut cb Clipboard) clear_all() {
	cb.clear()
}

// destroy destroys the clipboard and free it's resources.
pub fn (mut cb Clipboard) destroy() {
	cb.free()
}

// check_ownership returns `true` if the `Clipboard` has the content ownership.
pub fn (cb Clipboard) check_ownership() bool {
	return cb.has_ownership()
}

// is_available returns `true` if the clipboard is available for use.
pub fn (cb &Clipboard) is_available() bool {
	return cb.check_availability()
}

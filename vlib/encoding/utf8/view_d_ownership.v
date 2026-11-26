module utf8

// View borrows a validated UTF-8 byte array. The bytes may contain zeroes
// and need no trailing zero. Call `str` when an owned V string is needed.
// View is available in ownership mode.
pub struct View[^a] {
	bytes &^a []u8
}

// view returns a borrowed view when `bytes` contains valid UTF-8.
// Keep the source bytes unchanged while the view is in use.
pub fn view[^a](bytes &^a []u8) ?View[^a] {
	if bytes.len > 0 && !validate(&(*bytes)[0], bytes.len) {
		return none
	}
	return View[^a]{
		bytes: bytes
	}
}

// as_bytes returns the original UTF-8 bytes.
pub fn (view &View[^a]) as_bytes[^a]() &^a []u8 {
	return view.bytes
}

// str copies the view into a string with a trailing zero.
pub fn (view &View[^a]) str[^a]() string {
	return view.bytes.bytestr()
}

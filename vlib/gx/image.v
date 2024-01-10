module gx

pub struct Image {
mut:
	obj voidptr
pub:
	id     int
	width  int
	height int
}

// is_empty returns true if the Image `i` is empty.
pub fn (i Image) is_empty() bool {
	return i.obj == unsafe { nil }
}

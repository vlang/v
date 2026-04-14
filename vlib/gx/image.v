@[deprecated(msg: 'module gx is deprecated, use `import gg` instead', after: '2026-01-24')]
module gx

@[deprecated(msg: 'use gg.Image', after: '2026-01-24')]
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

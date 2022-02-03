module colors

pub struct Image {
mut:
	obj voidptr
pub:
	id     int
	width  int
	height int
}

pub fn (i Image) is_empty() bool {
	return isnil(i.obj)
}

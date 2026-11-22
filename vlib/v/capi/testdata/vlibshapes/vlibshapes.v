module vlibshapes

pub struct Box {
pub:
	n int
}

// value returns a value through a value receiver.
pub fn (b Box) value() int {
	return b.n
}

@[deprecated: 'use value()']
pub fn (b Box) old_value() int {
	return b.n
}

pub fn (a Box) + (b Box) int {
	return a.n + b.n
}

pub const size = 42

pub fn sizes(dst []i32) int {
	return dst.len
}

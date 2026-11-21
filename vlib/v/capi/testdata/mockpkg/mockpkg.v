module mockpkg

// add returns the sum of a and b.
pub fn add(a int, b int) int {
	return a + b
}

// Client is an opaque connection handle.
pub struct Client {
pub:
	name string
}

pub fn (c &Client) read(buf []u8) !int {
	return buf.len
}

pub enum Mode {
	fast
	slow
}

pub const version = '1.0'

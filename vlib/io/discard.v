module io

// Discard is a Writer that discards all data written to it. Mirrors Go's
// io.Discard.
pub struct Discard {}

// discard is a shared Writer that discards all data written to it.
pub const discard = Discard{}

// write implements Writer for Discard.
pub fn (mut d Discard) write(buf []u8) !int {
	return buf.len
}

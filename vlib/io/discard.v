module io

// Discard is a stateless Writer that discards all data written to it.
pub struct Discard {}

// discard is a reusable Discard value. Copy it to a mutable local before writing:
// `mut sink := io.discard`.
pub const discard = Discard{}

// write accepts all bytes in buf without storing or modifying them.
// It returns buf.len, including zero for an empty buffer, and never errors.
pub fn (mut d Discard) write(buf []u8) !int {
	return buf.len
}

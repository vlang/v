module io

// Writer is the interface that wraps the `write` method, which
// writes `buf.len` bytes to the underlying data stream.
pub interface Writer {
mut:
	write(buf []u8) !int
}

// RandomWriter is the interface that wraps the `write_to` method,
// which writes `buf.len` bytes to the underlying data stream at a random `pos`.
pub interface RandomWriter {
	write_to(pos u64, buf []u8) !int
}

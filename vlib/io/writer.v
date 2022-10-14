module io

// Writer represents a stream of data that can be wrote to
pub interface Writer {
mut:
	write(buf []u8) !int
}

// RandomWriter represents a stream of data that can be wrote to
// at a random pos
pub interface RandomWriter {
	write_to(pos u64, buf []u8) !int
}

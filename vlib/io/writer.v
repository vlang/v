module io

// Writer represents a stream of data that can be wrote to
pub interface Writer {
	write(buf []byte) ?int
}

// RandomWriter represents a stream of data that can be wrote to
// at a random pos
pub interface RandomWriter {
	write_to(pos int, buf []byte) ?int
}

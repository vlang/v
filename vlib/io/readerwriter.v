module io

// ReaderWriter represents a stream that can be read from and wrote to
pub interface ReaderWriter {
	// from Reader
	read_into(mut buf []byte) ?int

	// from Writer
	write(buf []byte) ?int
}

struct ReaderWriterImpl {
	r Reader
	w Writer
}

pub fn (mut r ReaderWriterImpl) read_into(mut buf []byte) ? {
	r.r.read_into(mut buf)?
}

pub fn (mut r ReaderWriterImpl) write(buf []byte) ? {
	r.w.write(mut buf)?
}

// make_readerwriter takes a rstream and a wstream and makes
// an rwstream with them
pub fn make_readerwriter(r Reader, w Writer) ReaderWriterImpl {
	return {r, w}
}
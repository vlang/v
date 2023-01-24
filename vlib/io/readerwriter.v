module io

// ReaderWriter represents a stream that can be read and written.
pub interface ReaderWriter {
	Reader
	Writer
}

// ReaderWriterImpl is a ReaderWriter that can be made from
// a separate reader and writer (see fn make_readerwriter).
struct ReaderWriterImpl {
mut:
	r Reader
	w Writer
}

// read reads up to `buf.len` bytes into `buf`. It returns
// the number of bytes read or any error encountered.
pub fn (mut r ReaderWriterImpl) read(mut buf []u8) !int {
	return r.r.read(mut buf)
}

// write writes `buf.len` bytes from `buf` to the underlying
// data stream. It returns the number of bytes written or any error
// encountered.
pub fn (mut r ReaderWriterImpl) write(buf []u8) !int {
	return r.w.write(buf)
}

// make_readerwriter takes a rstream and a wstream and makes
// an rwstream with them.
pub fn make_readerwriter(r Reader, w Writer) ReaderWriterImpl {
	return ReaderWriterImpl{
		r: r
		w: w
	}
}

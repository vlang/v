module io

// ReaderWriter represents a stream that can be read from and wrote to
pub interface ReaderWriter {
	// from Reader
	read(mut buf []byte) ?int
		// from Writer
	write(buf []byte) ?int
}

// ReaderWriterImpl is a ReaderWriter that can be made from
// a seperate reader and writer (see fn make_readerwriter)
struct ReaderWriterImpl {
	r Reader
	w Writer
}

pub fn (mut r ReaderWriterImpl) read(mut buf []byte) ?int {
	return r.r.read(mut buf)
}

pub fn (mut r ReaderWriterImpl) write(buf []byte) ?int {
	return r.w.write(buf)
}

// make_readerwriter takes a rstream and a wstream and makes
// an rwstream with them
pub fn make_readerwriter(r Reader, w Writer) ReaderWriterImpl {
	return {
		r: r
		w: w
	}
}

struct Zzz_CoerceInterfaceTableGeneration {
}

fn (_ Zzz_CoerceInterfaceTableGeneration) write(buf []byte) ?int {
	return none
}

fn (_ Zzz_CoerceInterfaceTableGeneration) read(mut buf []byte) ?int {
	return none
}

fn zzz_reader_writer_coerce_compiler() {
	x := Zzz_CoerceInterfaceTableGeneration{}
	_ := make_readerwriter(x, x)
}

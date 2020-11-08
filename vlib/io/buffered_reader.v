module io

// BufferedReader provides a buffered interface for a reader
struct BufferedReader {
mut:
	reader  Reader
	buf     []byte
	// current offset in the buffer
	offset  int
	len int
}

// BufferedReaderConfig are options that can be given to a reader
pub struct BufferedReaderConfig {
	reader   Reader
	buf_cap  int = 128 * 1024 // large for fast reading of big(ish) files
}

// new_buffered_reader creates new BufferedReader
pub fn new_buffered_reader(o BufferedReaderConfig) &BufferedReader {
	assert o.buf_cap >= 2

	// create
	r := &BufferedReader{
		reader: o.reader
		buf: []byte{len: o.buf_cap, cap: o.buf_cap}
		offset: 0
	}
	return r
}

// read fufills the Reader interface
pub fn (mut r BufferedReader) read(mut buf []byte) ?int {
	// read data out of the buffer if we dont have any
	if r.offset >= r.len-1 {
		r.fill_buffer()?
	}

	read := copy(buf, r.buf[r.offset..r.len])
	r.offset += read

	return read
}

// fill buffer attempts to refill the internal buffer
fn (mut r BufferedReader) fill_buffer() ? {
	// TODO we should keep track of when we get an end of stream
	// from the upstream reader so that we dont have to keep
	// trying to call this
	r.offset = 0
	r.len = r.reader.read(mut r.buf) or {
		0
	}
}

// read_line reads a line from the buffered reader
pub fn (mut r BufferedReader) read_line() ?string {
	mut line := []byte{}
	for {
		if r.offset >= (r.len-1) {
			// go fetch some new data
			r.fill_buffer()?
		}

		if r.len == 0 {
			// if we have no data then return nothing
			return none
		}

		// try and find a newline character
		mut i := r.offset
		for ; i < r.len; i++ {
			c := r.buf[i]
			if c == `\n` {
				// great, we hit something
				// do some checking for whether we hit \r\n or just \n

				if i != 0 && r.buf[i-1] == `\r` {
					x := i-1
					line << r.buf[r.offset..x]
				} else {
					line << r.buf[r.offset..i]
				}

				r.offset = i + 1

				return line.bytestr()
			}
		}

		line << r.buf[r.offset..i]

		r.offset = i
	}
}

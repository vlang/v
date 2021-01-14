module io

// BufferedReader provides a buffered interface for a reader
struct BufferedReader {
mut:
	reader        Reader
	buf           []byte
	// current offset in the buffer
	offset        int
	len           int
	// Whether we reached the end of the upstream reader
	end_of_stream bool
}

// BufferedReaderConfig are options that can be given to a reader
pub struct BufferedReaderConfig {
	reader Reader
	cap    int = 128 * 1024 // large for fast reading of big(ish) files
}

// new_buffered_reader creates new BufferedReader
pub fn new_buffered_reader(o BufferedReaderConfig) &BufferedReader {
	assert o.cap >= 2
	// create
	r := &BufferedReader{
		reader: o.reader
		buf: []byte{len: o.cap, cap: o.cap}
		offset: 0
	}
	return r
}

// read fufills the Reader interface
pub fn (mut r BufferedReader) read(mut buf []byte) ?int {
	// read data out of the buffer if we dont have any
	if r.needs_fill() {
		if !r.fill_buffer() {
			// end of stream
			return none
		}
	}
	read := copy(buf, r.buf[r.offset..r.len])
	r.offset += read
	return read
}

// fill_buffer attempts to refill the internal buffer
// and returns whether it got any data
fn (mut r BufferedReader) fill_buffer() bool {
	if r.end_of_stream {
		// we know we have already reached the end of stream
		// so return early
		return true
	}
	r.offset = 0
	r.len = 0
	r.len = r.reader.read(mut r.buf) or {
		// end of stream was reached
		r.end_of_stream = true
		return false
	}
	// we got some data
	return true
}

// needs_fill returns whether the buffer needs refilling
fn (r BufferedReader) needs_fill() bool {
	return r.offset >= r.len - 1
}

// end_of_stream returns whether the end of the stream was reached
pub fn (r BufferedReader) end_of_stream() bool {
	return r.end_of_stream
}

// read_line attempts to read a line from the buffered reader
// it will read until it finds a new line character (\n) or
// the end of stream
pub fn (mut r BufferedReader) read_line() ?string {
	if r.end_of_stream {
		return none
	}
	mut line := []byte{}
	for {
		if r.needs_fill() {
			// go fetch some new data
			if !r.fill_buffer() {
				// We are at the end of the stream
				if line.len == 0 {
					// we had nothing so return nothing
					return none
				}
				return line.bytestr()
			}
		}
		// try and find a newline character
		mut i := r.offset
		for ; i < r.len; i++ {
			c := r.buf[i]
			if c == `\n` {
				// great, we hit something
				// do some checking for whether we hit \r\n or just \n
				if i != 0 && r.buf[i - 1] == `\r` {
					x := i - 1
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
	return none
}

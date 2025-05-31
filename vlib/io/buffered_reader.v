module io

// BufferedReader provides a buffered interface for a reader.
pub struct BufferedReader {
mut:
	reader Reader
	buf    []u8
	offset int // current offset in the buffer
	len    int
	fails  int // how many times fill_buffer has read 0 bytes in a row
	mfails int // maximum fails, after which we can assume that the stream has ended
pub mut:
	end_of_stream bool // whether we reached the end of the upstream reader
	total_read    int  // total number of bytes read
}

// BufferedReaderConfig are options that can be given to a buffered reader.
pub struct BufferedReaderConfig {
pub:
	reader  Reader
	cap     int = 128 * 1024 // large for fast reading of big(ish) files
	retries int = 2          // how many times to retry before assuming the stream ended
}

// BufferedReadLineConfig are options that can be given to the read_line() function.
@[params]
pub struct BufferedReadLineConfig {
pub:
	delim u8 = `\n` // line delimiter
}

// new_buffered_reader creates a new BufferedReader.
pub fn new_buffered_reader(o BufferedReaderConfig) &BufferedReader {
	if o.cap <= 0 {
		panic('new_buffered_reader should be called with a positive `cap`')
	}
	// create
	r := &BufferedReader{
		reader: o.reader
		buf:    []u8{len: o.cap, cap: o.cap}
		offset: 0
		mfails: o.retries
	}
	return r
}

// read fufills the Reader interface.
pub fn (mut r BufferedReader) read(mut buf []u8) !int {
	if r.end_of_stream {
		return Eof{}
	}
	// read data out of the buffer if we dont have any
	if r.needs_fill() {
		if !r.fill_buffer() {
			// end of stream
			return Eof{}
		}
	}
	read := copy(mut buf, r.buf[r.offset..r.len])
	if read == 0 {
		return NotExpected{
			cause: 'invalid copy of buffer'
			code:  -1
		}
	}
	r.offset += read
	r.total_read += read
	return read
}

// free deallocates the memory for a buffered reader's internal buffer.
pub fn (mut r BufferedReader) free() {
	unsafe {
		r.buf.free()
	}
}

// fill_buffer attempts to refill the internal buffer
// and returns whether it got any data.
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
	if r.len == 0 {
		r.fails++
	} else {
		r.fails = 0
	}
	if r.fails >= r.mfails {
		// When reading 0 bytes several times in a row, assume the stream has ended.
		// This prevents infinite loops ¯\_(ツ)_/¯ ...
		r.end_of_stream = true
		return false
	}
	// we got some data
	return true
}

// needs_fill returns whether the buffer needs refilling.
fn (r BufferedReader) needs_fill() bool {
	return r.offset >= r.len
}

// end_of_stream returns whether the end of the stream was reached.
pub fn (r BufferedReader) end_of_stream() bool {
	return r.end_of_stream
}

// read_line attempts to read a line from the buffered reader.
// It will read until it finds the specified line delimiter
// such as (\n, the default or \0) or the end of stream.
pub fn (mut r BufferedReader) read_line(config BufferedReadLineConfig) !string {
	if r.end_of_stream {
		return Eof{}
	}
	mut line := []u8{}
	for {
		if r.needs_fill() {
			// go fetch some new data
			if !r.fill_buffer() {
				// We are at the end of the stream
				if line.len == 0 {
					// we had nothing so return nothing
					return Eof{}
				}
				return line.bytestr()
			}
		}
		// try and find a newline character
		mut i := r.offset
		for ; i < r.len; i++ {
			r.total_read++
			c := r.buf[i]
			if c == config.delim {
				// great, we hit something
				// do some checking for whether we hit \r\n or just \n
				if i != 0 && config.delim == `\n` && r.buf[i - 1] == `\r` {
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
	return Eof{}
}

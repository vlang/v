module io

import strings

// StringReader is able to read data from an Reader interface to a dynamically
// growing buffer using a string builder. Unlike the BufferedReader, StringReader will
// keep the entire contents of the buffer in memory, allowing the incoming data to be reused
// and read in an efficient matter
pub struct StringReader {
mut:
	reader Reader
	offset int // current offset in the buffer
pub mut:
	end_of_stream bool // whether we reached the end of the upstream reader
	builder       strings.Builder
}

// new creates a new StringReader and sets the string builder size to `initial_size`
pub fn StringReader.new(reader Reader, initial_size int) StringReader {
	return StringReader{
		reader: reader
		builder: strings.new_builder(initial_size)
	}
}

// from_string creates a new StringReader and fills the builder with `source`
pub fn StringReader.from_string(reader Reader, source string) StringReader {
	mut r := StringReader{
		reader: reader
		builder: strings.new_builder(source.len)
	}
	r.builder.write_string(source)
	return r
}

// needs_fill returns whether the buffer needs refilling
pub fn (r StringReader) needs_fill() bool {
	return r.offset >= r.builder.len
}

// needs_fill_until returns whether the buffer needs refilling in order to read
// `n` bytes
pub fn (r StringReader) needs_fill_until(n int) bool {
	return r.offset + n >= r.builder.len
}

// fill_bufer tries to read data into the buffer until either a 0 length read or if read_to_end_of_stream
// is true then the end of the stream. It returns the number of bytes read
pub fn (mut r StringReader) fill_buffer(read_till_end_of_stream bool) !int {
	if r.end_of_stream {
		return Eof{}
	}

	start := r.builder.len
	mut end := start

	// make sure there is enough room in the string builder
	unsafe { r.builder.grow_len(read_all_len) }
	defer {
		// shrink the length of the buffer to the total of bytes read
		r.builder.go_back(r.builder.len - end)
	}

	for {
		read := r.reader.read(mut r.builder[start..]) or {
			r.end_of_stream = true
			break
		}
		end += read

		if !read_till_end_of_stream && read == 0 {
			break
		} else if r.builder.len == end {
			unsafe { r.builder.grow_len(read_all_grow_len) }
		}
	}

	if end == start {
		return Eof{}
	}

	return end - start
}

// fill_buffer_until tries read `n` amount of bytes from the reader into the buffer
// and returns the actual number of bytes read
pub fn (mut r StringReader) fill_buffer_until(n int) !int {
	if r.end_of_stream {
		return Eof{}
	}

	start := r.builder.len
	// make sure there is enough room in the string builder
	if n > read_all_len {
		unsafe { r.builder.grow_len(read_all_len) }
	} else {
		unsafe { r.builder.grow_len(n) }
	}

	mut end := start
	for {
		read := r.reader.read(mut r.builder[start..]) or {
			r.end_of_stream = true
			break
		}
		end += read

		if read == 0 || end - start == n {
			break
		} else if r.builder.len == end {
			if n - end > read_all_grow_len {
				unsafe { r.builder.grow_len(read_all_grow_len) }
			} else {
				unsafe { r.builder.grow_len(n - end) }
			}
		}
	}

	if end == start {
		return Eof{}
	}
	return end - start
}

// read_all_bytes reads all bytes from a reader until either a 0 length read or if read_to_end_of_stream
// is true then the end of the stream. It returns a copy of the read data
pub fn (mut r StringReader) read_all_bytes(read_till_end_of_stream bool) ![]u8 {
	start := r.offset
	// ignore Eof error from fill buffer
	r.fill_buffer(read_till_end_of_stream) or {}
	r.offset = r.builder.len
	// check if there was still data in the buffer, but the reader has reached its end of stream
	if start == r.offset {
		return Eof{}
	}

	return r.get_part(start, r.offset - start)!
}

// read_all reads all bytes from a reader until either a 0 length read or if read_to_end_of_stream
// is true then the end of the stream. It produces a string from the read data
pub fn (mut r StringReader) read_all(read_till_end_of_stream bool) !string {
	buf := r.read_all_bytes(read_till_end_of_stream)!
	return unsafe { tos(buf.data, buf.len) }
}

// read_bytes tries to read n amount of bytes from the reader
pub fn (mut r StringReader) read_bytes(n int) ![]u8 {
	start := r.offset

	if r.needs_fill_until(n) {
		actual_read := r.fill_buffer_until(n - (r.builder.len - r.offset))!
		r.offset += actual_read
	} else {
		r.offset += n
	}

	return r.get_part(start, r.offset - start)!
}

// read_bytes tries to read `n` amount of bytes from the reader and produces a string
// from the read data
pub fn (mut r StringReader) read_string(n int) !string {
	buf := r.read_bytes(n)!
	return unsafe { tos(buf.data, buf.len) }
}

// read implements the Reader interface
pub fn (mut r StringReader) read(mut buf []u8) !int {
	start := r.offset

	read := r.fill_buffer_until(buf.len - start)!
	r.offset += read

	copy(mut buf, r.builder[start..read])
	return r.builder.len - start
}

// read_line attempts to read a line from the reader.
// It will read until it finds the specified line delimiter
// such as (\n, the default or \0) or the end of stream.
@[direct_array_access]
pub fn (mut r StringReader) read_line(config BufferedReadLineConfig) !string {
	if r.end_of_stream && r.needs_fill() {
		return Eof{}
	}

	start := r.offset
	for {
		if r.needs_fill() {
			r.fill_buffer(false) or {
				// we are at the end of the stream
				if r.offset == start {
					return Eof{}
				}
				return r.get_string_part(start, r.offset - start)!
			}
		}
		// try to find a newline character
		mut i := r.offset
		for ; i < r.builder.len; i++ {
			c := r.builder[i]
			if c == config.delim {
				// great, we hit something
				// do some checking for whether we hit \r\n or just \n
				mut x := i
				if i != 0 && config.delim == `\n` && r.builder[i - 1] == `\r` {
					x--
				}
				r.offset = i + 1
				return r.get_string_part(start, x - start)!
			}
		}
		r.offset = i
	}

	return Eof{}
}

// write implements the Writer interface
pub fn (mut r StringReader) write(buf []u8) !int {
	return r.builder.write(buf)!
}

// get_data returns a copy of the buffer
@[inline]
pub fn (r StringReader) get_data() []u8 {
	unsafe {
		mut x := malloc_noscan(r.builder.len)
		vmemcpy(x, &u8(r.builder.data), r.builder.len)
		return x.vbytes(r.builder.len)
	}
}

// get get_part returns a copy of a part of the buffer from `start` till `start` + `n`
pub fn (r StringReader) get_part(start int, n int) ![]u8 {
	if start + n > r.builder.len {
		return Eof{}
	}

	unsafe {
		mut x := malloc_noscan(n)
		vmemcpy(x, &u8(r.builder.data) + start, n)
		return x.vbytes(n)
	}
}

// get_string produces a string from all the bytes in the buffer
@[inline]
pub fn (r StringReader) get_string() string {
	return r.builder.spart(0, r.builder.len)
}

// get_string_part produces a string from `start` till `start` + `n` of the buffer
pub fn (r StringReader) get_string_part(start int, n int) !string {
	if start + n > r.builder.len {
		return Eof{}
	}

	return r.builder.spart(start, n)
}

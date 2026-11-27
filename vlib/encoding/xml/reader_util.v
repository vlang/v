module xml

import io
import os

fn next_char(mut reader io.Reader, mut buf []u8) !u8 {
	if reader.read(mut buf)! == 0 {
		return error('Unexpected End Of File.')
	}
	return buf[0]
}

struct XMLInput {
	source  string
	options XMLStringOptions
mut:
	stream   &io.Reader = unsafe { nil }
	position int
}

@[inline]
fn next_input_byte(mut reader XMLInput, mut buf []u8) !u8 {
	if reader.position < reader.source.len {
		byte := reader.source[reader.position]
		reader.position++
		return byte
	}
	if reader.read(mut buf)! == 0 {
		return error('Unexpected End Of File.')
	}
	return buf[0]
}

fn (mut reader XMLInput) read(mut buf []u8) !int {
	if reader.stream != unsafe { nil } {
		return reader.stream.read(mut buf)
	}
	if reader.position >= reader.source.len {
		return io.Eof{}
	}
	remaining := reader.source.len - reader.position
	count := if buf.len < remaining { buf.len } else { remaining }
	unsafe {
		vmemcpy(buf.data, reader.source.str + reader.position, count)
	}
	reader.position += count
	return count
}

fn (mut reader XMLInput) advance_until(delimiter u8) (string, bool) {
	remaining := reader.source.substr_unsafe(reader.position, reader.source.len)
	index := remaining.index_u8(delimiter)
	if index < 0 {
		reader.position = reader.source.len
		return remaining, false
	}
	reader.position += index + 1
	return remaining.substr_unsafe(0, index), true
}

// eof_error returns `err` as it is, unless it only reports the end of the input. A reader does that
// with an `Eof`, which has no message, so it is replaced by an error with `msg`, which says what
// was being parsed when the input ended.
fn eof_error(err IError, msg string) IError {
	if err is os.Eof || err is io.Eof || err.msg() == 'Unexpected End Of File.' {
		return error(msg)
	}
	return err
}

struct FullBufferReader {
	contents []u8
mut:
	position int
}

@[direct_array_access]
fn (mut fbr FullBufferReader) read(mut buf []u8) !int {
	if fbr.position >= fbr.contents.len {
		return io.Eof{}
	}
	remaining := fbr.contents.len - fbr.position
	n := if buf.len < remaining { buf.len } else { remaining }
	unsafe {
		vmemcpy(&u8(buf.data), &u8(fbr.contents.data) + fbr.position, n)
	}
	fbr.position += n
	return n
}

module xml

import io
import os

fn next_char(mut reader io.Reader, mut buf []u8) !u8 {
	if reader.read(mut buf)! == 0 {
		return error('Unexpected End Of File.')
	}
	return buf[0]
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

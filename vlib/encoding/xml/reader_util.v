module xml

import io

fn next_char(mut reader io.Reader, mut buf []u8) !u8 {
	if reader.read(mut buf)! == 0 {
		return error('Unexpected End Of File.')
	}
	return buf[0]
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

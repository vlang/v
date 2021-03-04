module main

import strings

const (
	content_length = 'Content-Length: '
)

fn C.fgetc(stream byteptr) int

struct Stdio {}

pub fn (_ Stdio) send(output string) {
	print('Content-Length: $output.len\r\n\r\n$output')
}

pub fn (_ Stdio) receive() ?string {
	first_line := get_raw_input()
	if first_line.len < 1 || !first_line.starts_with(content_length) {
		return error('content length is missing')
	}
	mut conlen := first_line[content_length.len..].int()
	mut buf := strings.new_builder(conlen)
	for conlen >= 0 {
		c := C.fgetc(C.stdin)
		$if !windows {
			if c == 10 {
				continue
			}
		}
		buf.write_b(byte(c))
		conlen--
	}
	payload := buf.str()
	unsafe { buf.free() }
	return payload[1..]
}

fn get_raw_input() string {
	eof := C.EOF
	mut buf := strings.new_builder(200)
	for {
		c := C.fgetc(C.stdin)
		chr := byte(c)
		if buf.len > 2 && (c == eof || chr in [`\r`, `\n`]) {
			break
		}
		buf.write_b(chr)
	}
	return buf.str()
}

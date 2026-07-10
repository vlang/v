module document

import strings

// KdlParseError is a parse error with source location.
pub struct KdlParseError {
	Error
pub:
	line   int
	column int
	offset int
	msg    string
}

pub fn (e KdlParseError) msg() string {
	mut sb := strings.new_builder(128)
	sb.write_string('kdl: ')
	if e.line > 0 || e.column > 0 {
		sb.write_string('${e.line + 1}:${e.column + 1}: ')
	}
	sb.write_string(e.msg)
	return sb.str()
}

module errors

import v.token

pub enum Reporter {
	scanner
	parser
	checker
	builder
	gen
}

[minify]
pub struct Error {
pub:
	message   string
	details   string
	file_path string
	pos       token.Pos
	backtrace string
	reporter  Reporter
}

pub struct Warning {
pub:
	message   string
	details   string
	file_path string
	pos       token.Pos
	reporter  Reporter
}

pub struct Notice {
pub:
	message   string
	details   string
	file_path string
	pos       token.Pos
	reporter  Reporter
}

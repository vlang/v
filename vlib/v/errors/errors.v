module errors

import v.token

pub enum Reporter {
	scanner
	parser
	checker
	gen
}

pub struct Error {
pub:
	message   string
	details   string
	file_path string
	pos       token.Position
	backtrace string
	reporter  Reporter
}

pub struct Warning {
pub:
	message   string
	details   string
	file_path string
	pos       token.Position
	reporter  Reporter
}

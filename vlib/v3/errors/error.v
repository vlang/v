module errors

import v3.token

pub enum Reporter {
	scanner
	parser
	checker
	gen
}

pub struct CompilerMessage {
pub:
	message   string
	details   string
	file_path string
	pos       token.Pos
	reporter  Reporter
}

pub struct Error {
	CompilerMessage
}

pub struct Warning {
	CompilerMessage
}

module errors

import v.token

pub enum Reporter {
	scanner
	parser
	checker
	builder
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

pub struct Notice {
	CompilerMessage
}

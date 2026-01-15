module errors

import v.token

pub enum Reporter {
	scanner
	parser
	checker
	builder
	gen
}

// CallStackItem represents a single location in the call stack
pub struct CallStackItem {
pub:
	file_path string
	pos       token.Pos
}

pub struct CompilerMessage {
pub:
	message    string
	details    string
	file_path  string
	pos        token.Pos
	reporter   Reporter
	call_stack []CallStackItem // call stack for compile-time errors
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

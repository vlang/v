module errors

import v.token

pub enum Reporter {
	unknown
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
pub mut:
	reporter Reporter
pub:
	message    string
	details    string
	file_path  string
	pos        token.Pos
	call_stack []CallStackItem // call stack for compile-time errors
}

pub struct ErrorMessage {
	CompilerMessage
}

pub struct WarningMessage {
	CompilerMessage
}

pub struct NoticeMessage {
	CompilerMessage
}

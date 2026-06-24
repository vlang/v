module errors

import v3.token

// Reporter lists reporter values used by errors.
pub enum Reporter {
	scanner
	parser
	checker
	gen
}

// CompilerMessage represents compiler message data used by errors.
pub struct CompilerMessage {
pub:
	message   string
	details   string
	file_path string
	pos       token.Pos
	reporter  Reporter
}

// Error represents error data used by errors.
pub struct Error {
	CompilerMessage
}

// Warning represents warning data used by errors.
pub struct Warning {
	CompilerMessage
}

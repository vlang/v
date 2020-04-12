module ast

import v.token

pub enum Reporter {
	scanner
	parser
	checker
	gen
}

pub enum IssueType {
	warn
	error
}

pub struct Issue {
	typ       IssueType
	message   string
	file_path string
	pos       token.Position
	reporter  Reporter
	backtrace string
}

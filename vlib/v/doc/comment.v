module doc

import v.token

const (
	example_pattern = '\x01 Example: '
)

pub struct DocComment {
pub mut:
	text     string // Raw text content of the comment, excluding the comment token chars ('//, /*, */')
	is_multi bool   // Is a block / multi-line comment
	pos      token.Pos
}

// is_example returns true if the contents of this comment is an inline doc example.
// The current convention is '// Example: <content>'
pub fn (dc DocComment) is_example() bool {
	return dc.text.trim_space().starts_with(doc.example_pattern)
}

// example returns the content of the inline example body
pub fn (dc DocComment) example() string {
	return dc.text.all_after(doc.example_pattern)
}

// is_multi_line_example returns true if an example line has no inline code
pub fn (dc DocComment) is_multi_line_example() bool {
	return dc.text.trim_space() == '\x01 Example:'
}

// has_triple_backtick returns true if the comment starts or ends a markdown code block
pub fn (dc DocComment) has_triple_backtick() bool {
	return dc.text.starts_with('\x01 ```')
}

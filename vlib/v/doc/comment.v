module doc

const (
	example_pattern = '\x01 Example: '
)

pub struct DocComment {
pub mut:
	text     string // Raw text content of the comment, excluding the comment token chars ('//, /*, */')
	is_multi bool   // Is a block / multi-line comment
	pos      DocPos = DocPos{-1, -1, 0}
}

// is_example returns true if the contents of this comment is a doc example.
// The current convention is '// Example: <content>'
pub fn (dc DocComment) is_example() bool {
	return dc.text.starts_with(example_pattern)
}

// example returns the content of the example body
pub fn (dc DocComment) example() string {
	return dc.text.all_after(example_pattern)
}

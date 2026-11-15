module html

import os

// parse parses and returns the DOM from the given text.
// Note: this function converts tags to lowercase.
// E.g. `<MyTag>content</MyTag>` is parsed as `<mytag>content</mytag>`.
// If a parser resource limit is exceeded, it returns an empty DOM with an error available from `parse_error()`.
pub fn parse(text string) DocumentObjectModel {
	return parse_checked(text) or {
		return DocumentObjectModel{
			root:            &Tag{}
			constructed:     true
			parse_error_msg: err.msg()
		}
	}
}

// parse_checked parses the given text and returns an error if nesting exceeds 256 elements or aggregate rebuilt
// `Tag.content` bytes would exceed min(64 MiB, 256 times the input size).
pub fn parse_checked(text string) !DocumentObjectModel {
	mut parser := Parser{}
	parser.parse_html_checked(text)!
	return parser.get_dom()
}

// parse_file parses and returns the DOM from the contents of a file.
// Note: this function converts tags to lowercase.
// E.g. `<MyTag>content</MyTag>` is parsed as `<mytag>content</mytag>`.
// If a parser resource limit is exceeded or the file cannot be read, it returns an empty DOM with an error
// available from `parse_error()`.
pub fn parse_file(filename string) DocumentObjectModel {
	return parse_file_checked(filename) or {
		return DocumentObjectModel{
			root:            &Tag{}
			constructed:     true
			parse_error_msg: err.msg()
		}
	}
}

// parse_file_checked parses a file and returns an error if it cannot be read or a parser resource limit is exceeded.
pub fn parse_file_checked(filename string) !DocumentObjectModel {
	content := os.read_file(filename)!
	return parse_checked(content)
}

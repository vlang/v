module html

import os

// parse parses and returns the DOM from the given text.
pub fn parse(text string) DocumentObjectModel {
	mut parser := Parser{}
	parser.parse_html(text)
	return parser.get_dom()
}

// parse_file parses and returns the DOM from the contents of a file.
pub fn parse_file(filename string) DocumentObjectModel {
	content := os.read_file(filename) or { return DocumentObjectModel{
		root: &Tag{}
	} }
	return parse(content)
}

module html

import os

// parse parses and returns the DOM from the given text.
// Note: this function converts tags to lowercase.
// E.g. <MyTag>content</MyTag> is parsed as <mytag>content</mytag>.
pub fn parse(text string) DocumentObjectModel {
	mut parser := Parser{}
	parser.parse_html(text)
	return parser.get_dom()
}

// parse_file parses and returns the DOM from the contents of a file.
// Note: this function converts tags to lowercase.
// E.g. <MyTag>content</MyTag> is parsed as <mytag>content</mytag>.
pub fn parse_file(filename string) DocumentObjectModel {
	content := os.read_file(filename) or { return DocumentObjectModel{
		root: &Tag{}
	} }
	return parse(content)
}

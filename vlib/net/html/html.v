module html

import os

pub fn parse(text string) []&Tag {
	mut parser := Parser{}
	parser.parse_html(text)
	return parser.tags
}

pub fn parse_file(filename string) []&Tag {
	content := os.read_file(filename) or { return []&Tag{} }
	return parse(content)
}

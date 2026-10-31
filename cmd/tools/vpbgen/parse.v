module main

import os

// parse_text parses .proto `text` and returns the file, or the diagnostics.
pub fn parse_text(path string, text string) !File {
	mut p := new_parser(path, text)!
	mut file := p.parse()!
	if p.errors.len > 0 {
		return error(p.errors.join_lines())
	}
	return file
}

// parse_file parses the .proto file at `path`.
pub fn parse_file(path string) !File {
	text := os.read_file(path)!
	return parse_text(path, text)
}

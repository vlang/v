// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

// import os
import x.toml.ast
import x.toml.input
import x.toml.scanner
import x.toml.parser

// Config is used to configure the toml parser.
// Only one of the fields `text` or `file_path`, is allowed to be set at time of configuration.
pub struct Config {
pub:
	text           string // TOML text
	file_path      string // '/path/to/file.toml'
	parse_comments bool
}

// parse_file parses the TOML file in `path`.
// on successful parsing parse_file returns an `&ast.Root` node.
pub fn parse_file(path string) &ast.Root {
	in_config := input.Config{
		file_path: path
	}
	scanner_config := scanner.Config{
		input: in_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return p.parse()
}

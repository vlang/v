// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module toml

import os
import x.toml.ast
import x.toml.input
import x.toml.scanner
import x.toml.parser

pub type Any = []Any | bool | f64 | i64 | map[string]Any | string // TODO add more builtin types - or use json2.Any + Date etc. ??

// Config is used to configure the toml parser.
// Only one of the fields `text` or `file_path`, is allowed to be set at time of configuration.
pub struct Config {
pub:
	text           string // TOML text
	file_path      string // '/path/to/file.toml'
	parse_comments bool
}

// Doc is a representation of a TOML document.
// A document can be constructed from a `string` buffer or from a file path
pub struct Doc {
pub:
	ast &ast.Root
}

// parse_file parses the TOML file in `path`.
pub fn parse_file(path string) Doc {
	input_config := input.Config{
		file_path: path
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// parse_text parses the TOML document provided in `text`.
pub fn parse_text(text string) Doc {
	input_config := input.Config{
		text: text
	}
	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// parse is a convenience function that parses the TOML document provided in `input`.
// parse automatically try to determine if type of `input` is a file or text.
// For explicit parsing of input see `parse_file` or `parse_text`.
pub fn parse(toml string) Doc {
	mut input_config := input.Config{}
	if os.is_file(toml) {
		input_config = input.Config{
			file_path: toml
		}
	} else {
		input_config = input.Config{
			text: toml
		}
	}

	scanner_config := scanner.Config{
		input: input_config
	}
	parser_config := parser.Config{
		scanner: scanner.new_scanner(scanner_config)
	}
	mut p := parser.new_parser(parser_config)
	return Doc{
		ast: p.parse()
	}
}

// value queries a value from the TOML document.
pub fn (d Doc) value(key string) Any {
	values := d.ast.table as map[string]ast.Value
	value := values[key]

	// `match` isn't very good for these types of constructs...
	if value is ast.Quoted {
		return Any((value as ast.Quoted).text)
	} else if value is ast.Number {
		str := (value as ast.Number).text
		if str.contains('.') {
			return Any(str.f64())
		}
		return Any(str.i64())
	} else if value is ast.Bool {
		str := (value as ast.Bool).text
		if str == 'true' {
			return Any(true)
		}
		return Any(false)
	}
	/*
	TODO else if value is map[string]ast.Value {
		return value(???)
	}*/
	// TODO add more types
	panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t convert "$value"')
	return Any('')
}

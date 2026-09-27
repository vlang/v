// Copyright (c) 2019-2024 Delyan Angelov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vflags

import os
import strings

// join_env_vflags_and_os_args returns all the arguments (the ones from the env variable VFLAGS too), passed on the command line.
pub fn join_env_vflags_and_os_args() []string {
	vosargs := os.getenv('VOSARGS')
	if vosargs != '' {
		return tokenize_to_args(vosargs)
	}
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		mut args := []string{}
		args << os.args[0]
		args << tokenize_to_args(vflags)
		args << os.args#[1..]
		return args
	}
	return os.args
}

// tokenize_to_args converts the input `s`, into an array of arguments.
// The arguments are separated by one or more spaces in the input `s`.
// The separating spaces are ignored.
// It supports quoted arguments, where "several little words" for example,
// will become a *single argument* in the output.
// The quotes can be single or double ones.
// Adjacent quoted and unquoted parts form one argument. Only unquoted whitespace
// separates arguments; an entirely empty quoted argument is retained.
pub fn tokenize_to_args(s string) []string {
	mut tokens := []string{}
	mut ctoken := strings.new_builder(20)
	mut in_quotes := false
	mut has_quoted_part := false
	mut quote_char := ` `
	for i in 0 .. s.len {
		c := s[i]
		if !in_quotes && c in [`"`, `'`] {
			in_quotes = true
			quote_char = c
		} else if in_quotes && c == quote_char {
			if i > 0 && s[i - 1] == `\\` {
				// support escaping a quote with a \
				ctoken.go_back(1)
				ctoken.write_u8(c)
			} else {
				in_quotes = false
				// A closing quote ends a part, not the argument: "build dir"/app
				// is one path. Remember empty quoted parts until a separator or EOF.
				has_quoted_part = true
			}
		} else if c.is_space() && !in_quotes {
			// space outside quotes means end of a token
			if ctoken.len > 0 || has_quoted_part {
				tokens << ctoken.str()
				has_quoted_part = false
			}
		} else {
			// s[i] is a UTF-8 byte, not a decoded Unicode code point.
			ctoken.write_u8(c)
		}
	}
	// add the potential remaining token too
	if ctoken.len > 0 || has_quoted_part {
		tokens << ctoken.str()
	}
	return tokens
}

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

import strings

// args_after returns all os.args, located *after* a specified `cut_word`.
// When `cut_word` is NOT found, os.args is returned unmodified.
pub fn args_after(cut_word string) []string {
	if args.len == 0 {
		return []string{}
	}
	mut cargs := []string{}
	if cut_word !in args {
		cargs = args.clone()
	} else {
		mut found := false
		cargs << args[0]
		for a in args[1..] {
			if a == cut_word {
				found = true
				continue
			}
			if !found {
				continue
			}
			cargs << a
		}
	}
	return cargs
}

// args_before returns all os.args, located *before* a specified `cut_word`.
// When `cut_word` is NOT found, os.args is returned unmodified.
pub fn args_before(cut_word string) []string {
	if args.len == 0 {
		return []string{}
	}
	mut cargs := []string{}
	if cut_word !in args {
		cargs = args.clone()
	} else {
		cargs << args[0]
		for a in args[1..] {
			if a == cut_word {
				break
			}
			cargs << a
		}
	}
	return cargs
}

// split_args parses a directive or tool response into literal argv elements.
// Quotes and quoting backslash escapes group text; no shell expansion is performed.
pub fn split_args(input string) ![]string {
	mut parsed_args := []string{}
	mut current := strings.new_builder(input.len)
	mut quote := u8(0)
	mut has_arg := false
	mut i := 0
	for i < input.len {
		ch := input[i]
		if quote == 0 && ch in [` `, `\t`, `\r`, `\n`] {
			if has_arg {
				parsed_args << current.str()
				current = strings.new_builder(input.len - i)
				has_arg = false
			}
			i++
			continue
		}
		if ch in [`'`, `"`] {
			if quote == 0 {
				quote = ch
				has_arg = true
				i++
				continue
			}
			if quote == ch {
				quote = 0
				i++
				continue
			}
		}
		if ch == `\\` && quote != `'` {
			if i + 1 < input.len {
				next := input[i + 1]
				escapable := if quote == `"` {
					next in [`"`, `\\`]
				} else {
					next in [` `, `\t`, `\r`, `\n`, `'`, `"`, `\\`]
				}
				if escapable {
					current.write_u8(next)
					has_arg = true
					i += 2
					continue
				}
			}
			current.write_u8(ch)
			has_arg = true
			i++
			continue
		}
		current.write_u8(ch)
		has_arg = true
		i++
	}
	if quote != 0 {
		return error('unterminated quote in argument list')
	}
	if has_arg {
		parsed_args << current.str()
	}
	return parsed_args
}

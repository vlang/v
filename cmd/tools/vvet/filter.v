module main

import v.token

type FilteredLines = map[ErrorType]map[int]bool

fn (mut fl FilteredLines) comments(is_multi bool, pos token.Pos) {
	if !is_multi {
		return
	}
	for ln in pos.line_nr + 1 .. pos.last_line + 1 {
		fl[.space_indent][ln] = true
	}
}

fn (mut fl FilteredLines) assigns(pos token.Pos) {
	if pos.line_nr == pos.last_line {
		return
	}
	for ln in pos.line_nr + 1 .. pos.last_line {
		fl[.trailing_space][ln] = true
		fl[.space_indent][ln] = true
	}
	fl[.trailing_space][pos.line_nr] = true
	fl[.space_indent][pos.last_line] = true
}

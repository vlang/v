module main

type FilteredLines = map[ErrorType]map[int]bool

fn (mut fl FilteredLines) comments(is_multi bool, start_line int, end_line int) {
	if !is_multi {
		return
	}
	for ln in start_line + 1 .. end_line + 1 {
		fl[.space_indent][ln] = true
	}
}

fn (mut fl FilteredLines) assigns(start_line int, end_line int) {
	if start_line == end_line {
		return
	}
	for ln in start_line + 1 .. end_line {
		fl[.trailing_space][ln] = true
		fl[.space_indent][ln] = true
	}
	fl[.trailing_space][start_line] = true
	fl[.space_indent][end_line] = true
}

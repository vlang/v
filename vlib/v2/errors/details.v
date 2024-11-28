module errors

import os
import v2.token

// loads the source file and generates the error message including the source
// line and column. offending token is highlighted in the snipped of source code.
// since this is used on errors, loading the source file isnt an issue.
// TODO: probably needs a better name
pub fn details(file &token.File, pos token.Position, row_padding int) string {
	src := os.read_file(file.name) or {
		// TODO: error util
		panic('error reading ${file.name}')
	}
	line_start := if pos.line - row_padding - 1 > 0 {
		file.line_start(pos.line - row_padding)
	} else {
		0
	}
	mut line_end := pos.offset + 1
	for i := 0; line_end < src.len; {
		if src[line_end] == `\n` {
			i++
			if i == row_padding + 1 {
				break
			}
		}
		line_end++
	}
	lines_src := src[line_start..line_end].split('\n')
	line_no_start, _ := file.find_line_and_column(line_start)
	mut lines_src_formatted := []string{}
	for i in 0 .. lines_src.len {
		line_no := line_no_start + i
		line_src := lines_src[i]
		line_spaces := line_src.replace('\t', '    ')
		lines_src_formatted << '${line_no:5d} | ' + line_spaces
		if line_no == pos.line {
			space_diff := line_spaces.len - line_src.len
			lines_src_formatted << '        ' + ' '.repeat(space_diff + pos.column - 1) + '^'
		}
	}
	return lines_src_formatted.join('\n')
}

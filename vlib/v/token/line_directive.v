module token

// LineDirective is a `#line N "file"` directive of a source file. Like in C, the
// source line after the directive is line N of the logical file `file`, and the
// lines after it count up from there, until the next directive.
pub struct LineDirective {
pub:
	// line is the first physical line of the file that the directive applies to.
	line int
	// logical_line is the line number reported for `line`.
	logical_line int
	// file is the logical file name; it is empty for the physical file itself.
	file string
}

// parse_line_directive parses the arguments of a `#line` directive, `N` or `N "file"`,
// and returns the logical line number and the file name ('' when it is not given).
// A trailing `//` comment is allowed.
pub fn parse_line_directive(args string) !(int, string) {
	text := args.trim_space()
	mut i := 0
	for i < text.len && text[i] != ` ` && text[i] != `\t` {
		i++
	}
	number := text[..i]
	if number.len == 0 || number.starts_with('//') {
		return error('expected a line number, like `#line 42 "file.v"`')
	}
	mut line := i64(0)
	for c in number {
		if !c.is_digit() {
			return error('`${number}` is not a valid line number')
		}
		line = line * 10 + i64(c - `0`)
		if line > max_i32 {
			return error('line number `${number}` is too large')
		}
	}
	if line == 0 {
		return error('line numbers start at 1, not 0')
	}
	rest := text[i..].trim_space()
	if rest.len == 0 || rest.starts_with('//') {
		return int(line), ''
	}
	quote := rest[0]
	if quote != `"` && quote != `'` {
		return error('the file name must be a quoted string, like `#line 42 "file.v"`')
	}
	mut file := []u8{cap: rest.len}
	mut j := 1
	for j < rest.len && rest[j] != quote {
		if rest[j] == `\\` && j + 1 < rest.len && rest[j + 1] in [quote, `\\`] {
			j++
		}
		file << rest[j]
		j++
	}
	if j >= rest.len {
		return error('unterminated file name string')
	}
	if file.len == 0 {
		return error('the file name cannot be empty')
	}
	trailing := rest[j + 1..].trim_space()
	if trailing.len > 0 && !trailing.starts_with('//') {
		return error('unexpected `${trailing}` after the file name')
	}
	return int(line), file.bytestr()
}

// add_line_directive records a `#line` directive found at the byte `offset` of the
// file: the lines after it report `logical_line` and up, in `file`, or in the current
// logical file when `file` is empty. Recording the same directive again, as a
// scanner that backtracks does, replaces its entry.
pub fn (mut f File) add_line_directive(offset int, logical_line int, file string) {
	line := f.find_line(offset) + 1
	// The scanner reports directives in source order, so this is the end of the list,
	// unless the same part of the file is scanned again.
	mut lo, mut hi := 0, f.line_directives.len
	if hi > 0 && f.line_directives[hi - 1].line < line {
		lo = hi
	}
	for lo < hi {
		mid := (lo + hi) / 2
		if f.line_directives[mid].line < line {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	logical_file := if file.len > 0 {
		file
	} else if lo > 0 {
		f.line_directives[lo - 1].file
	} else {
		''
	}
	directive := LineDirective{
		line:         line
		logical_line: logical_line
		file:         logical_file
	}
	if lo == f.line_directives.len {
		f.line_directives << directive
	} else if f.line_directives[lo].line == line {
		f.line_directives[lo] = directive
	} else {
		f.line_directives.insert(lo, directive)
	}
}

// has_line_directives reports whether the file uses `#line` directives.
@[inline]
pub fn (f &File) has_line_directives() bool {
	return f.line_directives.len > 0
}

// line_directives returns the `#line` directives of the file, ordered by line.
pub fn (f &File) line_directives() []LineDirective {
	return f.line_directives
}

// copy_line_directives copies the `#line` directives of `src`, like when a parser
// worker clones a file index. The file names are cloned, since they can outlive the
// memory of the worker.
pub fn (mut f File) copy_line_directives(src &File) {
	if src.line_directives.len == 0 {
		return
	}
	f.line_directives = []LineDirective{cap: src.line_directives.len}
	for directive in src.line_directives {
		f.line_directives << LineDirective{
			...directive
			file: directive.file.clone()
		}
	}
}

// logical_line maps the physical line `line` of the file to the file name and line
// that it reports, following the `#line` directives before it.
pub fn (f &File) logical_line(line int) (string, int) {
	mut lo, mut hi := 0, f.line_directives.len
	for lo < hi {
		mid := (lo + hi) / 2
		if f.line_directives[mid].line <= line {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo == 0 {
		return f.name, line
	}
	directive := f.line_directives[lo - 1]
	name := if directive.file.len > 0 { directive.file } else { f.name }
	// The lines after `#line 2147483647` keep the largest line number.
	logical_line := i64(directive.logical_line) + line - directive.line
	return name, if logical_line > max_i32 { max_i32 } else { int(logical_line) }
}

// logical_position_at resolves a file-local byte offset to the position that it
// reports: the file and line follow the `#line` directives of the file, the column
// stays the one in the physical line. Without directives it is position_at.
pub fn (f &File) logical_position_at(offset int) Position {
	line, column := f.find_line_and_column(offset)
	if f.line_directives.len == 0 {
		return Position{
			filename: f.name
			offset:   offset
			line:     line
			column:   column
		}
	}
	filename, logical_line := f.logical_line(line)
	return Position{
		filename: filename
		offset:   offset
		line:     logical_line
		column:   column
	}
}

// logical_position resolves `pos` like logical_position_at.
pub fn (f &File) logical_position(pos Pos) Position {
	return f.logical_position_at(pos.offset)
}

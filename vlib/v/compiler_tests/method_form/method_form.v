module method_form

import strings

// The method form of a program of the tests of generic constraints: each of its
// generic functions becomes a method, with the same type parameters, of `Host`,
// a struct without type parameters, and each use of one goes through `host`, a
// constant `Host`. The type parameters of a method's own are to behave as those
// of a function, with their constraints too: a test checks both forms of its
// program, and the method form has to report what the program reports, where it
// reports it.

// host_receiver is what the declaration of a generic function gets before its
// name, `fn (host_ Host) longest[T Named](...)`.
const host_receiver = '(host_ Host) '

// host_value is what a use of a generic function gets before its name,
// `host.longest(a, b)`.
const host_value = 'host.'

// host_decls end the method form: they add no line before the program's own.
const host_decls = '\nstruct Host {}\n\nconst host = Host{}\n'

// Insert is text that the method form adds to a line of the program: `width`
// bytes before its column `col`, both counted from 1.
pub struct Insert {
pub:
	col   int
	width int
}

// MethodForm is the method form of a program, and what it adds to each line.
pub struct MethodForm {
pub:
	source  string
	inserts map[int][]Insert // by line, counted from 1
}

// of returns the method form of `source`, or none when `source` declares no
// generic function.
pub fn of(source string) ?MethodForm {
	names := generic_fn_names(source)
	if names.len == 0 {
		return none
	}
	mut inserts := map[int][]Insert{}
	mut b := strings.new_builder(source.len + 256)
	for i, line in source.split('\n') {
		if i > 0 {
			b.write_u8(`\n`)
		}
		mut line_inserts := []Insert{}
		b.write_string(rewrite_line(line, names, mut line_inserts))
		if line_inserts.len > 0 {
			inserts[i + 1] = line_inserts
		}
	}
	b.write_string(host_decls)
	return MethodForm{
		source:  b.str()
		inserts: inserts
	}
}

// generic_fn_names returns the names of the generic functions that `source`
// declares at its top level: `fn longest[T Named](...)`, not methods.
fn generic_fn_names(source string) []string {
	mut names := []string{}
	for line in source.split('\n') {
		name := generic_fn_decl_name(line) or { continue }
		if name !in names {
			names << name
		}
	}
	return names
}

// generic_fn_decl_name returns the name of the generic function that `line`
// declares.
fn generic_fn_decl_name(line string) ?string {
	rest := if line.starts_with('pub fn ') {
		line['pub fn '.len..]
	} else if line.starts_with('fn ') {
		line['fn '.len..]
	} else {
		return none
	}
	mut end := 0
	for end < rest.len && is_name_byte(rest[end]) {
		end++
	}
	if end == 0 || end >= rest.len || rest[end] != `[` {
		return none
	}
	return rest[..end]
}

// rewrite_line returns `line` in the method form, and notes in `inserts` what
// it added. Comments are left as they are, and so is the text of a string, but
// not the code of its interpolations.
fn rewrite_line(line string, names []string, mut inserts []Insert) string {
	mut b := strings.new_builder(line.len + 16)
	mut start := 0
	if name := generic_fn_decl_name(line) {
		start = line.index(name + '[') or { 0 }
		b.write_string(line[..start])
		b.write_string(host_receiver)
		inserts << Insert{
			col:   start + 1
			width: host_receiver.len
		}
		b.write_string(name)
		start += name.len
	}
	// The name a method declaration declares is no use of a function.
	declared := method_decl_name_start(line)
	// The quote of the string the scan is in, or 0, and those of the strings
	// whose interpolations it is in.
	mut quote := u8(0)
	mut quotes := []u8{}
	mut i := start
	for i < line.len {
		c := line[i]
		if quote != 0 {
			if c == `\\` && i + 1 < line.len {
				b.write_u8(c)
				b.write_u8(line[i + 1])
				i += 2
				continue
			}
			if c == quote {
				quote = 0
			} else if c == `$` && i + 1 < line.len && line[i + 1] == `{` {
				// The code of an interpolation, until its `}`.
				quotes << quote
				quote = 0
				b.write_string('\${')
				i += 2
				continue
			}
			b.write_u8(c)
			i++
			continue
		}
		if c == `/` && i + 1 < line.len && line[i + 1] == `/` {
			b.write_string(line[i..])
			break
		}
		if c == `'` || c == `"` {
			quote = c
			b.write_u8(c)
			i++
			continue
		}
		if c == `}` && quotes.len > 0 {
			quote = quotes.pop()
			b.write_u8(c)
			i++
			continue
		}
		if is_name_byte(c) && (i == 0 || (!is_name_byte(line[i - 1]) && line[i - 1] != `.`)) {
			mut end := i
			for end < line.len && is_name_byte(line[end]) {
				end++
			}
			word := line[i..end]
			if i != declared && word in names && end < line.len && line[end] in [`(`, `[`] {
				b.write_string(host_value)
				inserts << Insert{
					col:   i + 1
					width: host_value.len
				}
			}
			b.write_string(word)
			i = end
			continue
		}
		b.write_u8(c)
		i++
	}
	return b.str()
}

// method_decl_name_start returns where the name that the method declaration
// `line` declares starts, `longest` of `fn (u User) longest() string {`, or -1.
fn method_decl_name_start(line string) int {
	open := if line.starts_with('pub fn (') {
		'pub fn '.len
	} else if line.starts_with('fn (') {
		'fn '.len
	} else {
		return -1
	}
	close := line.index_after(')', open) or { return -1 }
	mut start := close + 1
	for start < line.len && line[start] == ` ` {
		start++
	}
	return start
}

fn is_name_byte(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

// col returns where the column `col` of the line `line` of the program is in
// its method form.
pub fn (m MethodForm) col(line int, col int) int {
	mut moved := col
	for insert in m.inserts[line] or { []Insert{} } {
		if insert.col <= col {
			moved += insert.width
		}
	}
	return moved
}

// differences tells how the errors of the method form, `method_errors`, differ
// from those of the program, `errors`, or returns '' when the method form
// reports what the program reports, where the method form moves it. Both are
// lists of `line:col: message`, in any order. A message may name the function
// through `Host`; an error at the name of a use may be at `host` instead.
pub fn (m MethodForm) differences(errors []string, method_errors []string) string {
	if errors.len != method_errors.len {
		return 'the program reports ${errors.len} errors, its method form ${method_errors.len}: ${errors}, ${method_errors}'
	}
	mut wanted := []ErrorPlace{cap: errors.len}
	for err in errors {
		wanted << split_error(err) or { return 'an error without a place: `${err}`' }
	}
	mut found := []ErrorPlace{cap: method_errors.len}
	for err in method_errors {
		place := split_error(err) or {
			return 'an error without a place in the method form: `${err}`'
		}
		found << ErrorPlace{
			...place
			msg: normalized_message(place.msg)
		}
	}
	// By line and message, then by column: the method form moves no column of a
	// line past another.
	wanted.sort_with_compare(compare_places)
	found.sort_with_compare(compare_places)
	for i, want in wanted {
		got := found[i]
		if want.line != got.line || want.msg != got.msg || !m.cols_match(want.line, want.col, got.col) {
			return 'the program reports `${want.text}`, its method form `${got.text}` (at column ${m.col(want.line, want.col)} of the method form)'
		}
	}
	return ''
}

// ErrorPlace is an error of a check, `text`, read as `line:col: msg`.
struct ErrorPlace {
	text string
	line int
	col  int
	msg  string
}

fn compare_places(a &ErrorPlace, b &ErrorPlace) int {
	if a.line != b.line {
		return a.line - b.line
	}
	if a.msg != b.msg {
		return if a.msg < b.msg { -1 } else { 1 }
	}
	return a.col - b.col
}

// cols_match reports whether an error at the column `col` of the line `line` of
// the program and one at `method_col` of the method form are at the same place.
fn (m MethodForm) cols_match(line int, col int, method_col int) bool {
	if method_col == m.col(line, col) {
		return true
	}
	// At the name of a use: the method form may point at `host`.
	for insert in m.inserts[line] or { []Insert{} } {
		if insert.col == col && method_col == m.col(line, col) - insert.width {
			return true
		}
	}
	return false
}

// split_error reads `line:col: message`.
fn split_error(err string) ?ErrorPlace {
	parts := err.split_nth(':', 3)
	if parts.len < 3 {
		return none
	}
	return ErrorPlace{
		text: err
		line: parts[0].int()
		col:  parts[1].int()
		msg:  parts[2].trim_space()
	}
}

// normalized_message is `msg` of the method form as the program would say it:
// without the `Host` that the method form puts before a function's name.
fn normalized_message(msg string) string {
	return msg.replace('`Host.', '`').replace('Host.', '').replace(host_value, '')
}

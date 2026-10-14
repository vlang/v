module driver

// The driver looks into C text in several places: for the directives of a header,
// for the identifiers of a line, for the parts of the name of a macro, for what a
// macro left in preprocessed C. They all go by the rules of this file for what a
// comment, a literal, a number and an identifier are.

// v3_c_skip_comment returns the position after the comment that starts at `start`
// in `text`, or `start` when none starts there. A comment that does not end ends
// with the text.
fn v3_c_skip_comment(text string, start int) int {
	if start + 1 >= text.len || text[start] != `/` {
		return start
	}
	if text[start + 1] == `*` {
		end := text.index_after_('*/', start + 2)
		return if end < 0 { text.len } else { end + 2 }
	}
	if text[start + 1] == `/` {
		end := text.index_after_('\n', start + 2)
		return if end < 0 { text.len } else { end }
	}
	return start
}

// v3_c_skip_literal returns the position after the string or character literal
// that starts at `start` in `text`. A literal ends with its line at the latest.
fn v3_c_skip_literal(text string, start int) int {
	quote := text[start]
	mut i := start + 1
	for i < text.len && text[i] != quote && text[i] != `\n` {
		i += if text[i] == `\\` { 2 } else { 1 }
	}
	if i > text.len {
		return text.len
	}
	return if i < text.len && text[i] == quote { i + 1 } else { i }
}

// v3_c_skip_number returns the position after the number that starts at `start`
// in `text`. A number takes letters and dots, and a `'` between two of its digits.
fn v3_c_skip_number(text string, start int) int {
	mut i := start
	for i < text.len && (text[i].is_alnum() || text[i] in [`_`, `.`]
		|| (text[i] == `'` && i + 1 < text.len && text[i + 1].is_alnum())) {
		i++
	}
	return i
}

// v3_c_identifier_end returns the position after the identifier that starts at
// `start` in `text`, or `start` when none starts there.
fn v3_c_identifier_end(text string, start int) int {
	if start >= text.len || !(text[start] == `_` || text[start].is_letter()) {
		return start
	}
	mut i := start + 1
	for i < text.len && (text[i] == `_` || text[i].is_alnum()) {
		i++
	}
	return i
}

// v3_c_identifiers returns the identifiers of the C `text`, in their order: what is
// in a comment or in a literal, or part of a number, is none.
fn v3_c_identifiers(text string) []string {
	mut names := []string{}
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` {
			after := v3_c_skip_comment(text, i)
			i = if after > i { after } else { i + 1 }
		} else if c in [`"`, `'`] {
			i = v3_c_skip_literal(text, i)
		} else if c.is_digit() {
			i = v3_c_skip_number(text, i)
		} else if c == `_` || c.is_letter() {
			end := v3_c_identifier_end(text, i)
			names << text[i..end]
			i = end
		} else {
			i++
		}
	}
	return names
}

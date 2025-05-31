// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast

pub enum CommentsLevel {
	keep
	indent
}

// CommentsOptions defines the way comments are going to be written
// - has_nl: adds an newline at the end of a list of comments
// - same_line: line comments will be on the same line as the last statement
// - level:  either .keep (don't indent), or .indent (increment indentation)
// - prev_line: the line number of the previous token to save linebreaks
@[minify; params]
pub struct CommentsOptions {
	has_nl    bool = true
	same_line bool
	level     CommentsLevel
	prev_line int = -1
}

pub fn (mut f Fmt) comment(node ast.Comment, options CommentsOptions) {
	if node.text.starts_with('\x01 vfmt on') {
		f.vfmt_on(node.pos.line_nr)
	}
	defer {
		// ensure that the `vfmt off` comment itself was sent to the output,
		// by deferring the check for that state transition:
		if node.text.starts_with('\x01 vfmt off') {
			f.vfmt_off(node.pos.line_nr)
		}
	}
	// Shebang in .vsh files
	if node.text.starts_with('#!') {
		f.writeln(node.text)
		return
	}
	if options.level == .indent {
		f.indent++
	}
	if !node.text.contains('\n') {
		is_separate_line := !options.same_line || node.text.starts_with('\x01')
		mut s := node.text.trim_left('\x01').trim_right(' ')
		mut out_s := '//'
		if s != '' {
			if is_char_alphanumeric(s[0]) {
				out_s += ' '
			}
			out_s += s
		}
		if !is_separate_line && f.indent > 0 {
			f.remove_new_line() // delete the generated \n
			f.write(' ')
		}
		f.write(out_s)
	} else {
		lines := node.text.split_into_lines()
		f.write('/*')
		for i, line in lines {
			f.empty_line = false
			if i == lines.len - 1 {
				f.write(line)
				if node.text[node.text.len - 1] == `\n` {
					f.writeln('')
				}
				f.write('*/')
			} else {
				f.writeln(line.trim_right(' '))
			}
		}
	}
	if options.level == .indent {
		f.indent--
	}
}

pub fn (mut f Fmt) comments(comments []ast.Comment, options CommentsOptions) {
	mut prev_line := options.prev_line
	for i, c in comments {
		if options.prev_line > -1
			&& ((c.pos.line_nr > prev_line && f.out.len > 1 && f.out.last_n(1) != '\n')
			|| (c.pos.line_nr > prev_line + 1 && f.out.len > 2 && f.out.last_n(2) != '\n\n')) {
			f.writeln('')
		}
		if i == 0 && f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
			f.write(' ')
		}
		f.comment(c, options)
		if i < comments.len - 1 || options.has_nl {
			f.writeln('')
		}
		prev_line = c.pos.last_line
	}
}

pub fn (mut f Fmt) comments_before_field(comments []ast.Comment) {
	// They behave the same as comments after the last field. This alias is just for clarity.
	f.comments_after_last_field(comments)
}

pub fn (mut f Fmt) comments_after_last_field(comments []ast.Comment) {
	for comment in comments {
		f.indent++
		f.empty_line = true
		f.comment(comment, same_line: true)
		f.writeln('')
		f.indent--
	}
}

pub fn (mut f Fmt) import_comments(comments []ast.Comment, options CommentsOptions) {
	if comments.len == 0 {
		return
	}
	if options.same_line {
		f.remove_new_line(imports_buffer: true)
	}
	for c in comments {
		ctext := c.text.trim_left('\x01')
		if ctext == '' {
			continue
		}
		mut out_s := if options.same_line { ' ' } else { '' } + '//'
		if is_char_alphanumeric(ctext[0]) {
			out_s += ' '
		}
		out_s += ctext
		f.out_imports.writeln(out_s)
	}
}

fn is_char_alphanumeric(c u8) bool {
	return c.is_letter() || c.is_digit()
}

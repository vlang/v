// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
// - inline: line comments will be on the same line as the last statement
// - level:  either .keep (don't indent), or .indent (increment indentation)
// - iembed: a /* ... */ block comment used inside expressions; // comments the whole line
// - prev_line: the line number of the previous token to save linebreaks
[minify; params]
pub struct CommentsOptions {
	has_nl    bool = true
	inline    bool
	level     CommentsLevel
	iembed    bool
	prev_line int = -1
}

pub fn (mut f Fmt) comment(node ast.Comment, options CommentsOptions) {
	if node.text.starts_with('\x01 vfmt on') {
		f.vfmt_on(node.pos.line_nr)
	}
	defer {
		// ensure that the `vfmt off` comment itself was sent to the output,
		// by defering the check for that state transition:
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
	if options.iembed {
		x := node.text.trim_left('\x01').trim_space()
		if x.contains('\n') {
			f.writeln('/*')
			f.writeln(x)
			f.write('*/')
		} else {
			f.write('/* ${x} */')
		}
	} else if !node.text.contains('\n') {
		is_separate_line := !options.inline || node.text.starts_with('\x01')
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
		lines := node.text.trim_space().split_into_lines()
		start_break := is_char_alphanumeric(node.text[0]) || node.text[0].is_space()
		end_break := is_char_alphanumeric(node.text.trim('\t').bytes().last())
			|| node.text.bytes().last().is_space()
		f.write('/*')
		if start_break {
			f.writeln('')
		}
		for line in lines {
			f.writeln(line.trim_right(' '))
			f.empty_line = false
		}
		if end_break {
			f.empty_line = true
		} else {
			f.remove_new_line()
		}
		f.write('*/')
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
		if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
			f.write(' ')
		}
		f.comment(c, options)
		if !options.iembed && (i < comments.len - 1 || options.has_nl) {
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
		f.comment(comment, inline: true)
		f.writeln('')
		f.indent--
	}
}

pub fn (mut f Fmt) import_comments(comments []ast.Comment, options CommentsOptions) {
	if comments.len == 0 {
		return
	}
	if options.inline {
		f.remove_new_line(imports_buffer: true)
	}
	for c in comments {
		ctext := c.text.trim_left('\x01')
		if ctext == '' {
			continue
		}
		mut out_s := if options.inline { ' ' } else { '' } + '//'
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

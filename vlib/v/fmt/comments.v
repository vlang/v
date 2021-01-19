// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast

enum CommentsLevel {
	keep
	indent
}

// CommentsOptions defines the way comments are going to be written
// - has_nl: adds an newline at the end of the list of comments
// - inline: single-line comments will be on the same line as the last statement
// - iembed: a /* ... */ embedded comment; used in expressions; // comments the whole line
// - level: either .keep (don't indent), or .indent (increment indentation)
struct CommentsOptions {
	has_nl bool = true
	inline bool
	level  CommentsLevel
	iembed bool
}

pub fn (mut f Fmt) comment(node ast.Comment, options CommentsOptions) {
	if node.text.starts_with('#!') {
		f.writeln(node.text)
		return
	}
	if options.level == .indent {
		f.indent++
	}
	if options.iembed {
		x := node.text.trim_left('\x01')
		if x.contains('\n') {
			f.writeln('/*')
			f.writeln(x.trim_space())
			f.write('*/')
		} else {
			f.write('/* ${x.trim(' ')} */')
		}
	} else if !node.text.contains('\n') {
		is_separate_line := !options.inline || node.text.starts_with('\x01')
		mut s := node.text.trim_left('\x01')
		mut out_s := '//'
		if s != '' {
			match s[0] {
				`a`...`z`, `A`...`Z`, `0`...`9` { out_s += ' ' }
				else {}
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
		f.writeln('/*')
		for line in lines {
			f.writeln(line)
			f.empty_line = false
		}
		f.empty_line = true
		f.write('*/')
	}
	if options.level == .indent {
		f.indent--
	}
}

pub fn (mut f Fmt) comments(comments []ast.Comment, options CommentsOptions) {
	for i, c in comments {
		if !f.out.last_n(1)[0].is_space() {
			f.write(' ')
		}
		f.comment(c, options)
		if !options.iembed && (i < comments.len - 1 || options.has_nl) {
			f.writeln('')
		}
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

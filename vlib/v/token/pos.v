// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Pos {
pub:
	len     int // length of the literal in the source
	line_nr int // the line number in the source where the token occured
	pos     int // the position of the token in scanner text
	col     int // the column in the source where the token occured
pub mut:
	last_line int // the line number where the ast object ends (used by vfmt)
}

[unsafe]
pub fn (mut p Pos) free() {
}

pub fn (p Pos) line_str() string {
	return '\{l: ${p.line_nr + 1:5}, c: ${p.col:3}, p: ${p.pos:5}, ll: ${p.last_line + 1:5}}'
}

pub fn (pos Pos) extend(end Pos) Pos {
	return Pos{
		...pos
		len: end.pos - pos.pos + end.len
		last_line: end.last_line
	}
}

pub fn (pos Pos) extend_with_last_line(end Pos, last_line int) Pos {
	return Pos{
		len: end.pos - pos.pos + end.len
		line_nr: pos.line_nr
		pos: pos.pos
		col: pos.col
		last_line: last_line - 1
	}
}

pub fn (mut pos Pos) update_last_line(last_line int) {
	pos.last_line = last_line - 1
}

[inline]
pub fn (tok &Token) pos() Pos {
	return Pos{
		len: tok.len
		line_nr: tok.line_nr - 1
		pos: tok.pos
		last_line: tok.line_nr - 1
		col: tok.col - 1
	}
}

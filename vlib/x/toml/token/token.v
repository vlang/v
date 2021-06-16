// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Token {
pub:
	kind    Kind   // the token number/enum; for quick comparisons
	lit     string // literal representation of the token
	col     int    // the column in the source where the token occured
	line_nr int    // the line number in the source where the token occured
	pos     int    // the position of the token in scanner text
	len     int    // length of the literal
}

pub enum Kind {
	unknown
	eof
	bare // user
	number // 123
	quoted // 'foo', "foo", """foo""" or '''foo'''
	plus // +
	minus // -
	comma // ,
	colon // :
	hash // # comment
	assign // =
	lcbr // {
	rcbr // }
	lsbr // [
	rsbr // ]
	nl // \n linefeed / newline character
	cr // \r carrige return
	tab // \t character
	whitespace // ` `
	dot // .
	_end_
}

[inline]
pub fn (tok &Token) position() Position {
	return Position{
		len: tok.len
		line_nr: tok.line_nr - 1
		pos: tok.pos
		col: tok.col - 1
	}
}

// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

// Token holds information about the current scan of bytes.
pub struct Token {
pub:
	kind    Kind   // the token number/enum; for quick comparisons
	lit     string // literal representation of the token
	col     int    // the column in the source where the token occured
	line_nr int    // the line number in the source where the token occured
	pos     int    // the position of the token in scanner text
	len     int    // length of the literal
}

// Kind represents a logical type of entity found in any given TOML document.
pub enum Kind {
	unknown
	eof
	bare // user
	boolean // true or false
	number // 123
	quoted // 'foo', "foo", """foo""" or '''foo'''
	plus // +
	minus // -
	underscore // _
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
	period // .
	_end_
}

[inline]
pub fn (tok &Token) pos() Pos {
	return Pos{
		len: tok.len
		line_nr: tok.line_nr - 1
		pos: tok.pos
		col: tok.col - 1
	}
}

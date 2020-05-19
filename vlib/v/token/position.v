// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Position {
pub:
	len     int // length of the literal in the source
	line_nr int // the line number in the source where the token occured
	pos     int // the position of the token in scanner text
}

pub fn (pos Position) str() string {
	return 'Position{ line_nr: $pos.line_nr, pos: $pos.pos, len: $pos.len }'
}

pub fn (pos Position) extend(end Position) Position {
	return {
		pos |
		len: end.pos - pos.pos + end.len
	}
}

[inline]
pub fn (tok &Token) position() Position {
	return Position{
		len: tok.len
		line_nr: tok.line_nr - 1
		pos: tok.pos
	}
}

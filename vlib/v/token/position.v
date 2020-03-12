// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Position {
pub:
	line_nr int // the line number in the source where the token occured
	pos     int // the position of the token in scanner text
}

[inline]
pub fn (tok &Token) position() Position {
	return Position{
		line_nr: tok.line_nr - 1
		pos: tok.pos
	}
}

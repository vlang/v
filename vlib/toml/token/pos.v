// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

// Position represents a position in a TOML document.
pub struct Pos {
pub:
	len     int // length of the literal in the source
	line_nr int // the line number in the source where the token occured
	pos     int // the position of the token in scanner text
	col     int // the column in the source where the token occured
}

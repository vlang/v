// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

[inline]
pub fn is_key_char(c byte) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) // || c == `_`  || c == `-` <- these are identified when tokenizing
}

[if trace_toml ?]
pub fn printdbg(id string, message string) {
	eprintln(id + ' ' + message)
}

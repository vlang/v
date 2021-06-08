// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

[heap]
pub struct Scope {
mut:
	parent               &Scope = 0
	children             []&Scope
}

[unsafe]
pub fn (s &Scope) free() {
	unsafe {
		for child in s.children {
			child.free()
		}
		s.children.free()
	}
}

pub fn (s &Scope) is_root() bool {
	return isnil(s.parent)
}

/*
pub fn new_scope(parent &Scope) &Scope {
	return &Scope{
		parent: parent
	}
}
*/

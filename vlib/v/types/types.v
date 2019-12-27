// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub struct Type {
pub:
	name string
	idx  int
}

pub const (
	void_type = Type{'void', 0}
	int_type = Type{'int', 1}
	string_type = Type{'string', 2}
)


pub fn check(got, expected &Type) bool {
	if got.idx != expected.idx {
		return false
	}
	return true
}

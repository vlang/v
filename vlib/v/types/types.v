// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub enum Kind {
	struct_
	builtin
	enum_
}

pub struct Type {
pub:
	name   string
	idx    int
	// kind Kind
	fields []Field
}

pub struct Field {
pub:
	name     string
	type_idx int
}

pub const (
	void_type = Type{
		name: 'void'
		idx: 0
	}
	int_type = Type{
		name: 'int'
		idx: 1
	}
	string_type = Type{
		name: 'string'
		idx: 2
	}
	f64_type = Type{
		name: 'f64'
		idx: 3
	}
	bool_type = Type{
		name: 'bool'
		idx: 4
	}
	voidptr_type = Type{
		name: 'voidptr'
		idx: 5
	}
)

pub fn check(got, expected &Type) bool {
	if got.idx != expected.idx {
		return false
	}
	return true
}

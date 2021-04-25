// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token

pub enum AttrKind {
	plain // [name]
	string // ['name']
	number // [123]
	comptime_define // [if name]
}

// e.g. `[unsafe]`
pub struct Attr {
pub:
	name    string // [name]
	has_arg bool
	arg     string // [name: arg]
	kind    AttrKind
	pos     token.Position
}

// str returns the string representation without square brackets
pub fn (a Attr) str() string {
	mut s := ''
	mut arg := if a.has_arg {
		s += '$a.name: '
		a.arg
	} else {
		a.name
	}
	s += match a.kind {
		.plain, .number { arg }
		.string { "'$arg'" }
		.comptime_define { 'if $arg' }
	}
	return s
}

pub fn (attrs []Attr) contains(str string) bool {
	return attrs.any(it.name == str)
}

pub fn (attrs []Attr) find_comptime_define() ?string {
	for a in attrs {
		if a.kind == .comptime_define {
			return a.name
		}
	}
	return none
}

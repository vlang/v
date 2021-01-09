// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

import v.token

// e.g. `[unsafe]`
pub struct Attr {
pub:
	name          string // [name]
	is_string     bool   // ['name']
	is_ctdefine   bool   // [if name]
	arg           string // [name: arg]
	is_string_arg bool   // [name: 'arg']
	pos           token.Position
}

// no square brackets
pub fn (attr Attr) str() string {
	mut s := ''
	if attr.is_ctdefine {
		s += 'if '
	}
	if attr.is_string {
		s += "'$attr.name'"
	} else {
		s += attr.name
		if attr.arg.len > 0 {
			s += ': '
			if attr.is_string_arg {
				mut a := attr.arg.replace('\\', '\\\\')
				// FIXME: other escapes e.g. \r\n
				a = a.replace("'", "\\'")
				s += "'$a'"
			} else {
				s += attr.arg
			}
		}
	}
	return s
}

pub fn (attrs []Attr) contains(str string) bool {
	for a in attrs {
		if a.name == str {
			return true
		}
	}
	return false
}

// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module table

// e.g. `[unsafe_fn]`
pub struct Attr {
pub:
	name        string
	is_string   bool // `['xxx']`
	is_ctdefine bool // `[if flag]`
}

pub fn (attrs []Attr) contains(str string) bool {
	for a in attrs {
		if a.name == str {
			return true
		}
	}
	return false
}


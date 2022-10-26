// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import toml.input

// Root represents the root structure of any parsed TOML text snippet or file.
[heap]
pub struct Root {
pub:
	input input.Config // User input configuration
pub mut:
	comments []Comment
	table    Value
	// errors           []errors.Error    // all the checker errors in the file
}

pub fn (r Root) str() string {
	mut s := typeof(r).name + '{\n'
	s += '  input:  ${r.input}\n'
	s += '  table:  ${r.table}\n'
	s += '}'
	return s
}

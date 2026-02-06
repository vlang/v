// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module types

pub struct Module {
	name    string
	path    string
	imports []Module
	scope   &Scope = new_scope(unsafe { nil })
}

pub fn new_module(name string, path string) &Module {
	return &Module{
		name:  name
		path:  path
		scope: new_scope(universe)
	}
}

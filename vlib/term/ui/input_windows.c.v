// Copyright (c) 2020 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ui

const (
	not_implemented_yet = "term.input: error: Windows support isn't implemented yet"
)

pub fn init(cfg Config) &Context {
	panic(not_implemented_yet)
	return &Context{}
}

pub fn (mut ctx Context) run() {
	panic(not_implemented_yet)
}

pub fn (mut ctx Context) save_title() {
	panic(not_implemented_yet)
}

pub fn (mut ctx Context) load_title() {
	panic(not_implemented_yet)
}

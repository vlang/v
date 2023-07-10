// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

struct ImportCallPatch {
	mod  string
	name string
mut:
	pos int
}

struct FunctionCallPatch {
	name string
mut:
	pos int
}

type CallPatch = FunctionCallPatch | ImportCallPatch

struct FunctionGlobalPatch {
	idx GlobalIndex
mut:
	pos int
}

type FunctionPatch = CallPatch | FunctionGlobalPatch

struct FunctionLocal {
	typ  ValType
	name ?string
}

pub struct Function {
	tidx int
	idx  int
mut:
	patches []FunctionPatch // sorted
	label   int
	export  bool
	mod     &Module = unsafe { nil }
	code    []u8
	locals  []FunctionLocal
pub:
	name string
pub mut:
	export_name ?string
}

// export_name sets the export name of the function to `name`
pub fn (mut func Function) export_name(name string) {
	func.export_name = name
}

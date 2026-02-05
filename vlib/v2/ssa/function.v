// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

pub enum CallConv {
	c_decl
	fast_call
	wasm_std
}

pub enum Linkage {
	external
	private
	internal
}

pub struct Function {
pub:
	id   int
	name string
	typ  TypeID
pub mut:
	blocks []BlockID
	params []ValueID

	linkage   Linkage
	call_conv CallConv
}

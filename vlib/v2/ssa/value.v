// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

pub type ValueID = int

pub enum ValueKind {
	unknown
	constant
	argument
	global
	instruction
	basic_block
	string_literal   // V string struct literal (by value)
	c_string_literal // C string literal (raw char pointer)
	func_ref         // Function pointer reference (for map hash/eq/clone/free functions)
}

pub struct Value {
pub:
	id  ValueID
	typ TypeID
	// Index into the specific arena (instrs, blocks, globals)
	index int
pub mut:
	kind ValueKind
	name string
	uses []ValueID
}

pub struct ConstantData {
pub:
	int_val   i64
	float_val f64
	str_val   string
}

pub struct GlobalVar {
pub:
	name          string
	typ           TypeID
	linkage       Linkage
	alignment     int
	is_constant   bool
	initial_value i64  // For constants/enums, the initial integer value
	initial_data  []u8 // For constant arrays: serialized element data
}

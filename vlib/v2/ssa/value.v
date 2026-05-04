// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

pub type ValueID = int

pub fn (id ValueID) str() string {
	return int(id).str()
}

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

// str returns the symbolic name for an SSA value kind.
pub fn (k ValueKind) str() string {
	return match k {
		.unknown { 'unknown' }
		.constant { 'constant' }
		.argument { 'argument' }
		.global { 'global' }
		.instruction { 'instruction' }
		.basic_block { 'basic_block' }
		.string_literal { 'string_literal' }
		.c_string_literal { 'c_string_literal' }
		.func_ref { 'func_ref' }
	}
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
pub mut:
	name          string
	typ           TypeID
	linkage       Linkage
	alignment     int
	is_constant   bool
	initial_value i64  // For constants/enums, the initial integer value
	initial_data  []u8 // For constant arrays: serialized element data
}

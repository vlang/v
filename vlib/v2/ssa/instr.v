// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.token

pub enum OpCode {
	// Terminators
	ret
	br
	jmp
	switch_
	unreachable

	// Binary (integer)
	add
	sub
	mul
	sdiv
	udiv
	srem
	urem

	// Binary (float)
	fadd
	fsub
	fmul
	fdiv
	frem

	// Bitwise
	shl
	lshr
	ashr
	and_
	or_
	xor

	// Memory
	alloca
	heap_alloc // Heap allocate memory for a type (malloc+zero): returns ptr
	load
	store
	get_element_ptr
	fence
	cmpxchg
	atomicrmw

	// Conversion
	trunc
	zext
	sext
	fptoui
	fptosi
	uitofp
	sitofp
	bitcast

	// Comparisons (signed)
	lt
	gt
	le
	ge
	eq
	ne
	// Comparisons (unsigned)
	ult
	ugt
	ule
	uge

	// Other
	phi
	call
	call_indirect // Indirect call through function pointer
	call_sret     // Call with struct return (x8 indirect return on ARM64)
	select
	assign             // copy for phi elimination
	inline_string_init // Create string struct by value: (string){str, len, is_lit}

	// Aggregate (struct/tuple) operations
	extractvalue // Extract element from struct/tuple: extractvalue %tuple, index
	insertvalue  // Insert element into struct/tuple: insertvalue %tuple, %val, index
	struct_init  // Create struct: operands are field values in order
}

pub enum AtomicOrdering {
	not_atomic
	unordered
	monotonic
	acquire
	release
	acq_rel
	seq_cst
}

pub struct Instruction {
pub mut:
	op OpCode
	// Operands are IDs of other Values
	operands []ValueID
pub:
	block BlockID
	typ   TypeID // Result type

	pos        token.Pos
	atomic_ord AtomicOrdering
	inline     InlineHint // Inline hint for call instructions
}

pub enum InlineHint {
	none   // No hint, let optimizer decide
	always // Always inline (e.g., V's [inline] attribute)
	never  // Never inline (e.g., V's [noinline] attribute)
	hint   // Suggest inlining (optimizer may ignore)
}

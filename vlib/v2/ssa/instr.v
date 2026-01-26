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

	// Binary
	add
	sub
	mul
	sdiv
	udiv
	srem
	urem

	// Bitwise
	shl
	lshr
	ashr
	and_
	or_
	xor

	// Memory
	alloca
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

	// Comparisons
	lt
	gt
	le
	ge
	eq
	ne

	// Other
	phi
	call
	select
	assign // copy for phi elimination
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
}

// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ssa

pub type BlockID = int

pub struct BasicBlock {
pub:
	id     BlockID
	val_id ValueID
	name   string
	parent int // Function ID
pub mut:
	instrs []ValueID

	// Control Flow Graph
	preds []BlockID
	succs []BlockID

	// Dominators
	idom     BlockID
	dom_tree []BlockID
}

// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

pub struct TargetData {
pub:
	ptr_size      int
	endian_little bool
}

@[heap]
pub struct Module {
pub mut:
	name       string
	target     TargetData
	type_store TypeStore

	// Arenas
	values  []Value
	instrs  []Instruction
	blocks  []BasicBlock
	funcs   []Function
	globals []GlobalVar
}

pub fn Module.new(name string) &Module {
	mut m := &Module{
		name:       name
		type_store: TypeStore.new()
	}
	// Reserve ID 0 to represent "null" or "invalid", avoiding collisions
	// with map lookups returning 0.
	m.values << Value{
		kind: .unknown
		id:   0
	}
	return m
}

pub fn (mut m Module) new_function(name string, ret TypeID, params []TypeID) int {
	id := m.funcs.len
	m.funcs << Function{
		id:   id
		name: name
		typ:  ret
	}
	return id
}

pub fn (mut m Module) add_block(func_id int, name string) BlockID {
	id := m.blocks.len
	// FIX: Sanitize block names for C labels (replace . with _)
	safe_name := name.replace('.', '_')
	unique_name := '${safe_name}_${id}'

	// Store 'id' (index in blocks arena) in the Value
	val_id := m.add_value_node(.basic_block, 0, unique_name, id)

	m.blocks << BasicBlock{
		id:     id
		val_id: val_id
		name:   unique_name
		parent: func_id
	}
	m.funcs[func_id].blocks << id
	return id
}

// Updated to accept 'index'
pub fn (mut m Module) add_value_node(kind ValueKind, typ TypeID, name string, index int) ValueID {
	id := m.values.len
	m.values << Value{
		id:    id
		kind:  kind
		typ:   typ
		name:  name
		index: index
	}
	return id
}

pub fn (m Module) get_block_from_val(val_id int) int {
	return m.values[val_id].index
}

pub fn (mut m Module) add_instr(op OpCode, block BlockID, typ TypeID, operands []ValueID) ValueID {
	// 1. Save Instruction Index
	instr_idx := m.instrs.len

	instr := Instruction{
		op:       op
		block:    block
		typ:      typ
		operands: operands
	}
	m.instrs << instr

	// 2. Pass instr_idx to Value
	val_id := m.add_value_node(.instruction, typ, 'v${m.values.len}', instr_idx)

	// 3. Link Block
	m.blocks[block].instrs << val_id

	// 4. Update Def-Use
	for op_id in operands {
		if op_id < m.values.len {
			m.values[op_id].uses << val_id
		}
	}

	return val_id
}

pub fn (mut m Module) add_global(name string, typ TypeID, is_const bool) int {
	return m.add_global_with_value(name, typ, is_const, 0)
}

pub fn (mut m Module) add_global_with_value(name string, typ TypeID, is_const bool, initial_value i64) int {
	id := m.globals.len
	g := GlobalVar{
		name:          name
		typ:           typ
		is_constant:   is_const
		initial_value: initial_value
	}
	m.globals << g

	// FIX: The Value representing a global is a POINTER to the data
	ptr_typ := m.type_store.get_ptr(typ)
	return m.add_value_node(.global, ptr_typ, name, id)
}

pub fn (mut m Module) add_instr_front(op OpCode, block BlockID, typ TypeID, operands []ValueID) ValueID {
	instr_idx := m.instrs.len
	instr := Instruction{
		op:       op
		block:    block
		typ:      typ
		operands: operands
	}
	m.instrs << instr
	val_id := m.add_value_node(.instruction, typ, 'v${m.values.len}', instr_idx)

	// Prepend to block instructions
	m.blocks[block].instrs.prepend(val_id)

	for op_id in operands {
		if op_id < m.values.len {
			m.values[op_id].uses << val_id
		}
	}
	return val_id
}

pub fn (mut m Module) replace_uses(old_val int, new_val int) {
	// Copy uses, because we modify instr operands which might change things
	uses := m.values[old_val].uses.clone()
	for use_id in uses {
		use_val := m.values[use_id]
		if use_val.kind == .instruction {
			mut replaced := false
			for i in 0 .. m.instrs[use_val.index].operands.len {
				if m.instrs[use_val.index].operands[i] == old_val {
					m.instrs[use_val.index].operands[i] = new_val
					replaced = true
				}
			}
			// Only add to uses list once per user, even if used multiple times
			if replaced && use_id !in m.values[new_val].uses {
				m.values[new_val].uses << use_id
			}
		}
	}
	m.values[old_val].uses = []
}

fn dfs(mut m Module, blk int, mut visited map[int]bool, mut rpo []int) {
	visited[blk] = true
	for s in m.blocks[blk].succs {
		if !visited[s] {
			dfs(mut m, s, mut visited, mut rpo)
		}
	}
	rpo << blk
}

fn (mut m Module) get_rpo(func Function) []int {
	mut visited := map[int]bool{}
	mut rpo := []int{}
	dfs(mut m, func.blocks[0], mut visited, mut rpo)
	// rpo.reverse_inplace()
	rpo = rpo.reverse()
	return rpo
}

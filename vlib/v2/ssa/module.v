// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v2.types

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
	// For parallel SSA workers: shared reference to main module's type_store.
	// When non-nil, type operations use this instead of the local type_store.
	shared_type_store &TypeStore = unsafe { nil }

	// Type checker environment (optional, for backends that need rich type info)
	env &types.Environment = unsafe { nil }

	// Arenas
	values  []Value
	instrs  []Instruction
	blocks  []BasicBlock
	funcs   []Function
	globals []GlobalVar

	// C struct names: TypeID -> C struct name (e.g., 114 -> "stat" for struct stat)
	// Used by the C gen to emit `typedef struct <name> Struct_N;` instead of
	// generating a custom struct definition that would have the wrong memory layout.
	c_struct_names map[int]string
	// C structs marked with @[typedef] – already a C typedef, not a struct tag.
	c_typedef_structs map[int]bool

	// Constant cache: (type, name) -> ValueID for deduplication
	const_cache map[string]ValueID
}

pub fn Module.new(name string) &Module {
	mut m := &Module{
		name:       name
		type_store: TypeStore.new()
	}
	// Pre-allocate arenas to avoid repeated reallocation during SSA building.
	// A typical hello.v compilation needs ~158K values, ~134K instrs, ~10K blocks, ~1.8K funcs.
	// Pre-allocating avoids ARM64 backend issues with array growth reallocation.
	m.values = []Value{cap: 262144}
	m.instrs = []Instruction{cap: 262144}
	m.blocks = []BasicBlock{cap: 16384}
	m.funcs = []Function{cap: 4096}
	m.globals = []GlobalVar{cap: 2048}
	// Reserve ID 0 to represent "null" or "invalid", avoiding collisions
	// with map lookups returning 0.
	m.values << Value{
		kind: .unknown
		id:   0
	}
	return m
}

pub fn (mut m Module) new_function(name string, ret TypeID, params []TypeID) int {
	// Check if function already exists (avoid duplicates from multiple files)
	for i, f in m.funcs {
		if f.name == name {
			return i
		}
	}
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
	// Avoid m.funcs[func_id].blocks << id -- chained broken in ARM64 self-hosted
	mut f := m.funcs[func_id]
	f.blocks << id
	m.funcs[func_id] = f
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

// --- Helpers to avoid chained struct-array mutations (broken in ARM64 self-hosted) ---

pub fn (mut m Module) func_add_param(func_id int, param_val ValueID) {
	mut f := m.funcs[func_id]
	f.params << param_val
	m.funcs[func_id] = f
}

pub fn (mut m Module) func_set_c_extern(func_id int, val bool) {
	mut f := m.funcs[func_id]
	f.is_c_extern = val
	m.funcs[func_id] = f
}

pub fn (mut m Module) func_clear_blocks(func_id int) {
	mut f := m.funcs[func_id]
	f.blocks.clear()
	m.funcs[func_id] = f
}

pub fn (mut m Module) func_clear_params(func_id int) {
	mut f := m.funcs[func_id]
	f.params.clear()
	m.funcs[func_id] = f
}

pub fn (mut m Module) func_set_params(func_id int, params []ValueID) {
	mut f := m.funcs[func_id]
	f.params = params
	m.funcs[func_id] = f
}

pub fn (mut m Module) block_add_succ(from int, to int) {
	mut blk := m.blocks[from]
	blk.succs << to
	m.blocks[from] = blk
}

pub fn (mut m Module) block_add_pred(to int, from int) {
	mut blk := m.blocks[to]
	blk.preds << from
	m.blocks[to] = blk
}

pub fn (mut m Module) nop_instr(val_idx int) {
	mut instr := m.instrs[val_idx]
	instr.op = .bitcast
	instr.operands = []
	m.instrs[val_idx] = instr
}

// Get or create a constant value, reusing existing ones when possible.
// This maintains SSA's immutability principle by avoiding duplicate constants.
pub fn (mut m Module) get_or_add_const(typ TypeID, name string) ValueID {
	key := '${typ}:${name}'
	if existing := map_get_value_id(m.const_cache, key) {
		return existing
	}
	id := m.add_value_node(.constant, typ, name, 0)
	m.const_cache[key] = id
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

	// 3. Link Block — read whole struct, modify, write back (chained broken in ARM64)
	mut blk := m.blocks[block]
	blk.instrs << val_id
	m.blocks[block] = blk

	// 4. Update Def-Use — same pattern
	for op_id in operands {
		if op_id < m.values.len {
			mut v := m.values[op_id]
			v.uses << val_id
			m.values[op_id] = v
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
		linkage:       .private // Local global, not external
		is_constant:   is_const
		initial_value: initial_value
	}
	m.globals << g

	// FIX: The Value representing a global is a POINTER to the data
	ptr_typ := m.type_store.get_ptr(typ)
	return m.add_value_node(.global, ptr_typ, name, id)
}

pub fn (mut m Module) add_global_with_data(name string, elem_type TypeID, is_const bool, data []u8) int {
	id := m.globals.len
	g := GlobalVar{
		name:         name
		typ:          elem_type
		linkage:      .private
		is_constant:  is_const
		initial_data: data
	}
	m.globals << g

	// The Value is a POINTER to element data (for direct indexing)
	ptr_typ := m.type_store.get_ptr(elem_type)
	return m.add_value_node(.global, ptr_typ, name, id)
}

// add_external_global adds an external global variable (defined outside this module)
// Returns the ValueID for the global pointer
pub fn (mut m Module) add_external_global(name string, typ TypeID) ValueID {
	// Check if this external global already exists
	for v in m.values {
		if v.kind == .global && v.name == name {
			return v.id
		}
	}

	// Create a new external global
	id := m.globals.len
	g := GlobalVar{
		name:    name
		typ:     typ
		linkage: .external
	}
	m.globals << g

	// The Value representing a global is a POINTER to the data
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

	// Prepend to block instructions — read whole struct, modify, write back (chained broken in ARM64)
	mut blk := m.blocks[block]
	blk.instrs.prepend(val_id)
	m.blocks[block] = blk

	// Update Def-Use — same pattern
	for op_id in operands {
		if op_id < m.values.len {
			mut v := m.values[op_id]
			v.uses << val_id
			m.values[op_id] = v
		}
	}
	return val_id
}

// append_phi_operands appends a (val, block_val) pair to a phi instruction's operands.
// Append (val, block_val) pair to phi instruction operands.
// Avoid m.instrs[idx].operands << x — chained append broken in ARM64 self-hosted.
pub fn (mut m Module) append_phi_operands(instr_idx int, val int, block_val int) {
	// Read whole struct, modify, write back (chained broken in ARM64)
	mut instr := m.instrs[instr_idx]
	instr.operands << val
	instr.operands << block_val
	m.instrs[instr_idx] = instr
}

pub fn (mut m Module) replace_uses(old_val int, new_val int) {
	// Copy uses, because we modify instr operands which might change things
	uses := m.values[old_val].uses.clone()
	for use_id in uses {
		use_val := m.values[use_id]
		if use_val.kind == .instruction {
			// Read whole instr, modify operands, write back (chained broken in ARM64)
			mut instr := m.instrs[use_val.index]
			mut replaced := false
			for i in 0 .. instr.operands.len {
				if instr.operands[i] == old_val {
					instr.operands[i] = new_val
					replaced = true
				}
			}
			if replaced {
				m.instrs[use_val.index] = instr
			}
			// Only add to uses list once per user, even if used multiple times
			if replaced && use_id !in m.values[new_val].uses {
				mut nv := m.values[new_val]
				nv.uses << use_id
				m.values[new_val] = nv
			}
		}
	}
	mut ov := m.values[old_val]
	ov.uses = []
	m.values[old_val] = ov
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

// new_worker_module creates a lightweight Module for parallel SSA building.
// Each worker gets its own copy of type_store (COW from main).
// Has its own values/instrs/blocks arenas starting from empty.
pub fn (mut m Module) new_worker_module() &Module {
	// Explicitly clone funcs and globals to avoid COW races between threads.
	mut wf := []Function{cap: m.funcs.len}
	for f in m.funcs {
		wf << f
	}
	mut wg := []GlobalVar{cap: m.globals.len}
	for g in m.globals {
		wg << g
	}
	// Deep-clone TypeStore to avoid COW races on types[] and cache map.
	mut wts := TypeStore{}
	wts.types = []Type{cap: m.type_store.types.len}
	for t in m.type_store.types {
		wts.types << t
	}
	wts.cache = m.type_store.cache.clone()

	mut w := &Module{
		name:              m.name
		shared_type_store: unsafe { nil }
		type_store:        wts
		env:               m.env
		funcs:             wf
		globals:           wg
	}
	// Seed worker with main module's values so that ValueIDs from Phases 1-3
	// (constants, globals, func_refs, params) are valid in the worker.
	w.values = []Value{cap: m.values.len + 32768}
	for v in m.values {
		w.values << v
	}
	// Also seed instrs and blocks (typically empty after Phases 1-3, but just in case).
	w.instrs = []Instruction{cap: m.instrs.len + 32768}
	for instr in m.instrs {
		w.instrs << instr
	}
	w.blocks = []BasicBlock{cap: m.blocks.len + 2048}
	for blk in m.blocks {
		w.blocks << blk
	}
	return w
}

// FuncSSAData holds the SSA data produced by a worker for a single function.
pub struct FuncSSAData {
pub:
	func_idx int // Index into main module's funcs[]
	blocks   []BlockID // Worker-local block IDs
	params   []ValueID // Worker-local param ValueIDs
}

// merge_worker_module merges a worker's SSA arenas into the main module.
// Workers are seeded with the main module's values/instrs/blocks from Phases 1-3.
// seed_values/seed_instrs/seed_blocks are the lengths of the seed data.
// Only data beyond the seed is new and needs to be merged with ID remapping.
// func_data contains (func_idx, blocks, params) for updating main funcs[].
pub fn (mut m Module) merge_worker_module(w &Module, func_data []FuncSSAData, seed_values int, seed_instrs int, seed_blocks int, seed_types int) {
	// Build type remapping: worker type IDs >= seed_types may differ from main.
	// For each new worker type, find or create equivalent in main.
	mut type_remap := []TypeID{len: w.type_store.types.len, init: 0}
	// Seed types map to themselves (identity)
	for ti := 0; ti < seed_types && ti < type_remap.len; ti++ {
		type_remap[ti] = ti
	}
	// Map new worker types to main types
	for ti := seed_types; ti < w.type_store.types.len; ti++ {
		wt := w.type_store.types[ti]
		// Generate cache key matching TypeStore conventions
		mut cache_key := ''
		match wt.kind {
			.int_t {
				cache_key = if wt.is_unsigned { 'u${wt.width}' } else { 'i${wt.width}' }
			}
			.float_t {
				cache_key = 'f${wt.width}'
			}
			.ptr_t {
				// Remap the elem_type first
				remapped_elem := if int(wt.elem_type) < type_remap.len {
					type_remap[int(wt.elem_type)]
				} else {
					wt.elem_type
				}
				cache_key = 'p${remapped_elem}'
			}
			.array_t {
				remapped_elem := if int(wt.elem_type) < type_remap.len {
					type_remap[int(wt.elem_type)]
				} else {
					wt.elem_type
				}
				cache_key = 'a${remapped_elem}_${wt.len}'
			}
			else {}
		}
		// Try cache lookup in main
		if cache_key.len > 0 {
			if existing := map_get_type_id(m.type_store.cache, cache_key) {
				type_remap[ti] = existing
				continue
			}
		}
		// Register new type in main with remapped field types
		mut new_type := Type{
			kind:        wt.kind
			width:       wt.width
			len:         wt.len
			is_c_struct: wt.is_c_struct
			is_union:    wt.is_union
			is_unsigned: wt.is_unsigned
			ret_type:    if int(wt.ret_type) < type_remap.len && int(wt.ret_type) >= seed_types {
				type_remap[int(wt.ret_type)]
			} else {
				wt.ret_type
			}
			elem_type:   if int(wt.elem_type) < type_remap.len && int(wt.elem_type) >= seed_types {
				type_remap[int(wt.elem_type)]
			} else {
				wt.elem_type
			}
		}
		if wt.fields.len > 0 {
			mut new_fields := []TypeID{cap: wt.fields.len}
			for f in wt.fields {
				new_fields << if int(f) < type_remap.len && int(f) >= seed_types {
					type_remap[int(f)]
				} else {
					f
				}
			}
			new_type = Type{
				...new_type
				fields:      new_fields
				field_names: wt.field_names
			}
		}
		if wt.params.len > 0 {
			mut new_params := []TypeID{cap: wt.params.len}
			for p in wt.params {
				new_params << if int(p) < type_remap.len && int(p) >= seed_types {
					type_remap[int(p)]
				} else {
					p
				}
			}
			new_type = Type{
				...new_type
				params: new_params
			}
		}
		new_id := m.type_store.register(new_type)
		if cache_key.len > 0 {
			m.type_store.cache[cache_key] = new_id
		}
		type_remap[ti] = new_id
	}

	// Remapping offsets: worker IDs in [seed..seed+N) → main IDs in [main.len..main.len+N).
	// For seed IDs (< seed_values), no remapping needed — they're the same in both.
	value_off := m.values.len - seed_values
	instr_off := m.instrs.len - seed_instrs
	block_off := m.blocks.len - seed_blocks

	// Append worker values beyond seed
	for wi := seed_values; wi < w.values.len; wi++ {
		wv := w.values[wi]
		mut new_index := wv.index
		if wv.kind == .instruction {
			new_index += instr_off
		} else if wv.kind == .basic_block {
			new_index += block_off
		}
		// Remap uses — only IDs >= seed need remapping
		mut new_uses := []ValueID{cap: wv.uses.len}
		for u in wv.uses {
			new_uses << if u >= seed_values { u + value_off } else { u }
		}
		// Remap type
		new_typ := if int(wv.typ) >= seed_types && int(wv.typ) < type_remap.len {
			type_remap[int(wv.typ)]
		} else {
			wv.typ
		}
		m.values << Value{
			id:    wi + value_off
			kind:  wv.kind
			typ:   new_typ
			name:  wv.name
			index: new_index
			uses:  new_uses
		}
	}

	// Append worker instructions beyond seed
	for ii := seed_instrs; ii < w.instrs.len; ii++ {
		instr := w.instrs[ii]
		mut new_ops := []ValueID{cap: instr.operands.len}
		for op in instr.operands {
			new_ops << if op >= seed_values { op + value_off } else { op }
		}
		// Remap type
		new_typ := if int(instr.typ) >= seed_types && int(instr.typ) < type_remap.len {
			type_remap[int(instr.typ)]
		} else {
			instr.typ
		}
		m.instrs << Instruction{
			op:         instr.op
			block:      if instr.block >= seed_blocks { instr.block + block_off } else { instr.block }
			typ:        new_typ
			operands:   new_ops
			pos:        instr.pos
			atomic_ord: instr.atomic_ord
			inline:     instr.inline
		}
	}

	// Append worker blocks beyond seed
	for bi := seed_blocks; bi < w.blocks.len; bi++ {
		blk := w.blocks[bi]
		mut new_instrs := []ValueID{cap: blk.instrs.len}
		for vi in blk.instrs {
			new_instrs << if vi >= seed_values { vi + value_off } else { vi }
		}
		mut new_preds := []BlockID{cap: blk.preds.len}
		for p in blk.preds {
			new_preds << if p >= seed_blocks { p + block_off } else { p }
		}
		mut new_succs := []BlockID{cap: blk.succs.len}
		for s in blk.succs {
			new_succs << if s >= seed_blocks { s + block_off } else { s }
		}
		m.blocks << BasicBlock{
			id:     blk.id + block_off
			val_id: if blk.val_id >= seed_values { blk.val_id + value_off } else { blk.val_id }
			name:   blk.name
			parent: blk.parent
			instrs: new_instrs
			preds:  new_preds
			succs:  new_succs
		}
	}

	// Update main module's funcs[] with blocks and params from worker
	for fd in func_data {
		if fd.func_idx >= m.funcs.len {
			continue
		}
		mut f := m.funcs[fd.func_idx]
		for blk_id in fd.blocks {
			f.blocks << if blk_id >= seed_blocks { blk_id + block_off } else { blk_id }
		}
		mut new_params := []ValueID{cap: fd.params.len}
		for p in fd.params {
			new_params << if p >= seed_values { p + value_off } else { p }
		}
		f.params = new_params
		m.funcs[fd.func_idx] = f
	}

	// Also add any new functions created by workers (stubs like wyhash, array_eq).
	for fi := m.funcs.len; fi < w.funcs.len; fi++ {
		wfunc := w.funcs[fi]
		mut new_blocks := []BlockID{cap: wfunc.blocks.len}
		for blk_id in wfunc.blocks {
			new_blocks << if blk_id >= seed_blocks { blk_id + block_off } else { blk_id }
		}
		mut new_params := []ValueID{cap: wfunc.params.len}
		for p in wfunc.params {
			new_params << if p >= seed_values { p + value_off } else { p }
		}
		// Remap function return type
		new_typ := if int(wfunc.typ) >= seed_types && int(wfunc.typ) < type_remap.len {
			type_remap[int(wfunc.typ)]
		} else {
			wfunc.typ
		}
		m.funcs << Function{
			name:   wfunc.name
			typ:    new_typ
			blocks: new_blocks
			params: new_params
		}
	}
}

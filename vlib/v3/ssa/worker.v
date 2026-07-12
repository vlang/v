module ssa

// Parallel-SSA worker support, ported from v2's module.v and adapted to v3's
// raw-block-id operand model: branch/phi/switch operands that name blocks are
// remapped by the block offset, while ordinary value operands are remapped by
// the value offset.

// FuncSSAData holds the SSA a worker produced for a single function.
pub struct FuncSSAData {
pub:
	func_idx int       // index into the main module's funcs[]
	blocks   []BlockID // worker-local block ids
	params   []ValueID // worker-local param value ids
}

// is_block_operand reports whether operand `idx` of this instruction names a
// basic block (a raw block id) rather than an SSA value.
pub fn (i &Instruction) is_block_operand(idx int) bool {
	return match i.op {
		.jmp { idx == 0 }
		.br { idx == 1 || idx == 2 }
		// switch_: cond, default_blk, [case_val, blk]... -> blocks at odd indices.
		.switch_ { idx % 2 == 1 }
		// phi: [val, blk]... -> blocks at odd indices.
		.phi { idx % 2 == 1 }
		else { false }
	}
}

// new_worker_module creates a lightweight Module for parallel SSA building. The
// worker gets a deep copy of the type store and is seeded with the main module's
// values/instrs/blocks so ids produced in earlier (serial) phases stay valid.
pub fn (mut m Module) new_worker_module() &Module {
	mut wf := []Function{cap: m.funcs.len}
	for f in m.funcs {
		wf << f
	}
	mut wg := []GlobalVar{cap: m.globals.len}
	for g in m.globals {
		wg << g
	}
	mut wts := TypeStore{
		cache: map[string]TypeID{}
	}
	wts.types = []Type{cap: m.type_store.types.len}
	for t in m.type_store.types {
		wts.types << t
	}
	for k, v in m.type_store.cache {
		wts.cache[k] = v
	}

	mut w := &Module{
		name:              m.name
		target:            m.target
		type_store:        wts
		funcs:             wf
		globals:           wg
		c_struct_names:    map[int]string{}
		c_typedef_structs: map[int]bool{}
		const_cache:       m.const_cache.clone()
	}
	w.values = []Value{cap: m.values.len + 1024}
	for v in m.values {
		w.values << v
	}
	w.instrs = []Instruction{cap: m.instrs.len + 1024}
	for instr in m.instrs {
		w.instrs << instr
	}
	w.blocks = []BasicBlock{cap: m.blocks.len + 256}
	for blk in m.blocks {
		w.blocks << blk
	}
	return w
}

// worker_type_cache_key supports worker type cache key handling for ssa.
fn worker_type_cache_key(t Type, type_remap []TypeID) string {
	return match t.kind {
		.int_t {
			if t.is_unsigned {
				'u${t.width}'
			} else {
				'i${t.width}'
			}
		}
		.float_t {
			'f${t.width}'
		}
		.ptr_t {
			remapped := if int(t.elem_type) < type_remap.len {
				type_remap[int(t.elem_type)]
			} else {
				t.elem_type
			}
			'p${remapped}'
		}
		.array_t {
			remapped := if int(t.elem_type) < type_remap.len {
				type_remap[int(t.elem_type)]
			} else {
				t.elem_type
			}
			'a${remapped}_${t.len}'
		}
		else {
			''
		}
	}
}

// merge_worker_module folds a worker's freshly-built SSA (everything beyond the
// seed lengths) back into the main module, remapping type/value/instr/block/func
// ids. func_data carries each built function's worker-local blocks/params.
pub fn (mut m Module) merge_worker_module(w &Module, func_data []FuncSSAData, seed_values int, seed_instrs int, seed_blocks int, seed_types int, seed_funcs int) {
	// 1. Remap worker types >= seed_types into the main type store.
	mut type_remap := []TypeID{len: w.type_store.types.len, init: 0}
	for ti := 0; ti < seed_types && ti < type_remap.len; ti++ {
		type_remap[ti] = ti
	}
	for ti := seed_types; ti < w.type_store.types.len; ti++ {
		wt := w.type_store.types[ti]
		cache_key := worker_type_cache_key(wt, type_remap)
		if cache_key.len > 0 {
			if existing := m.type_store.cache[cache_key] {
				if existing > 0 {
					type_remap[ti] = existing
					continue
				}
			}
		}
		remap_t := fn (id TypeID, type_remap []TypeID, seed_types int) TypeID {
			if int(id) >= seed_types && int(id) < type_remap.len {
				return type_remap[int(id)]
			}
			return id
		}
		mut new_fields := []TypeID{cap: wt.fields.len}
		for f in wt.fields {
			new_fields << remap_t(f, type_remap, seed_types)
		}
		mut new_params := []TypeID{cap: wt.params.len}
		for p in wt.params {
			new_params << remap_t(p, type_remap, seed_types)
		}
		new_type := Type{
			kind:        wt.kind
			width:       wt.width
			is_unsigned: wt.is_unsigned
			elem_type:   remap_t(wt.elem_type, type_remap, seed_types)
			len:         wt.len
			fields:      new_fields
			field_names: wt.field_names
			params:      new_params
			ret_type:    remap_t(wt.ret_type, type_remap, seed_types)
			is_c_struct: wt.is_c_struct
			is_union:    wt.is_union
		}
		new_id := m.type_store.register(new_type)
		if cache_key.len > 0 {
			m.type_store.cache[cache_key] = new_id
		}
		type_remap[ti] = new_id
	}

	value_off := m.values.len - seed_values
	instr_off := m.instrs.len - seed_instrs
	block_off := m.blocks.len - seed_blocks

	remap_val := fn (id ValueID, seed_values int, value_off int) ValueID {
		return if id >= seed_values { id + value_off } else { id }
	}
	remap_blk := fn (id BlockID, seed_blocks int, block_off int) BlockID {
		return if id >= seed_blocks { id + block_off } else { id }
	}
	remap_typ := fn (id TypeID, seed_types int, type_remap []TypeID) TypeID {
		return if int(id) >= seed_types && int(id) < type_remap.len {
			type_remap[int(id)]
		} else {
			id
		}
	}

	// 2. Append worker values beyond the seed.
	for wi := seed_values; wi < w.values.len; wi++ {
		wv := w.values[wi]
		mut new_index := wv.index
		if wv.kind == .instruction {
			new_index += instr_off
		} else if wv.kind == .basic_block {
			new_index += block_off
		}
		mut new_uses := []ValueID{cap: wv.uses.len}
		for u in wv.uses {
			new_uses << remap_val(u, seed_values, value_off)
		}
		m.values << Value{
			id:    wi + value_off
			kind:  wv.kind
			typ:   remap_typ(wv.typ, seed_types, type_remap)
			name:  wv.name
			index: new_index
			uses:  new_uses
		}
	}

	// 3. Append worker instructions beyond the seed, remapping operands by kind.
	for ii := seed_instrs; ii < w.instrs.len; ii++ {
		instr := w.instrs[ii]
		mut new_ops := []ValueID{cap: instr.operands.len}
		for oi in 0 .. instr.operands.len {
			op := instr.operands[oi]
			if instr.is_block_operand(oi) {
				new_ops << ValueID(remap_blk(BlockID(op), seed_blocks, block_off))
			} else {
				new_ops << remap_val(op, seed_values, value_off)
			}
		}
		m.instrs << Instruction{
			op:         instr.op
			operands:   new_ops
			block:      remap_blk(instr.block, seed_blocks, block_off)
			typ:        remap_typ(instr.typ, seed_types, type_remap)
			pos:        instr.pos
			atomic_ord: instr.atomic_ord
			inline:     instr.inline
		}
	}

	// 4. Append worker blocks beyond the seed.
	for bi := seed_blocks; bi < w.blocks.len; bi++ {
		blk := w.blocks[bi]
		mut new_instrs := []ValueID{cap: blk.instrs.len}
		for vi in blk.instrs {
			new_instrs << remap_val(vi, seed_values, value_off)
		}
		mut new_preds := []BlockID{cap: blk.preds.len}
		for p in blk.preds {
			new_preds << remap_blk(p, seed_blocks, block_off)
		}
		mut new_succs := []BlockID{cap: blk.succs.len}
		for s in blk.succs {
			new_succs << remap_blk(s, seed_blocks, block_off)
		}
		m.blocks << BasicBlock{
			id:     blk.id + block_off
			val_id: remap_val(blk.val_id, seed_values, value_off)
			name:   blk.name
			parent: blk.parent
			instrs: new_instrs
			preds:  new_preds
			succs:  new_succs
		}
	}

	// 5. Patch the built functions' blocks/params in the main module.
	for fd in func_data {
		if fd.func_idx >= m.funcs.len {
			continue
		}
		mut f := m.funcs[fd.func_idx]
		if f.blocks.len > 0 {
			continue // already merged by an earlier worker
		}
		mut new_blocks := []BlockID{cap: fd.blocks.len}
		for blk_id in fd.blocks {
			new_blocks << remap_blk(blk_id, seed_blocks, block_off)
		}
		f.blocks = new_blocks
		mut new_params := []ValueID{cap: fd.params.len}
		for p in fd.params {
			new_params << remap_val(p, seed_values, value_off)
		}
		f.params = new_params
		m.funcs[fd.func_idx] = f
	}

	// 6. Append any brand-new functions a worker created (runtime stubs, etc.).
	for fi := seed_funcs; fi < w.funcs.len; fi++ {
		wfunc := w.funcs[fi]
		mut already := false
		for existing in m.funcs {
			if existing.name == wfunc.name {
				already = true
				break
			}
		}
		if already {
			continue
		}
		mut new_blocks := []BlockID{cap: wfunc.blocks.len}
		for blk_id in wfunc.blocks {
			new_blocks << remap_blk(blk_id, seed_blocks, block_off)
		}
		mut new_params := []ValueID{cap: wfunc.params.len}
		for p in wfunc.params {
			new_params << remap_val(p, seed_values, value_off)
		}
		m.funcs << Function{
			id:           m.funcs.len
			name:         wfunc.name
			typ:          remap_typ(wfunc.typ, seed_types, type_remap)
			blocks:       new_blocks
			params:       new_params
			is_c_extern:  wfunc.is_c_extern
			is_prototype: wfunc.is_prototype
			linkage:      wfunc.linkage
			call_conv:    wfunc.call_conv
		}
	}
}

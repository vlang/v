// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import v2.mir
import v2.ssa
import v2.types
import encoding.binary
import os

pub struct Gen {
pub:
	mod &mir.Module
mut:
	macho &MachOObject
pub mut:
	stack_map      map[int]int
	alloca_offsets map[int]int
	stack_size     int
	curr_offset    int

	block_offsets      []int // indexed by block_id, -1 = not yet visited
	pending_label_blks []int
	pending_label_offs []int
	func_count         int
	total_pending      int
	total_resolved     int

	// Register allocation
	reg_map   map[int]int
	used_regs []int
	next_blk   int
	cur_blk_id int // current block being generated (for phi copy emission)

	// Track which string literals have been materialized (value_id -> str_data offset)
	string_literal_offsets map[int]int

	// Cache for parsed constant integer values (value_id -> parsed i64)
	const_cache map[int]i64

	// Current function's return type (for handling struct returns)
	cur_func_ret_type int
	cur_func_name     string

	// Stack offset where x8 (indirect return pointer) is saved for large struct returns
	x8_save_offset int
	// Cache for deduplicating string data in cstring section (content -> offset)
	string_data_cache map[string]int
	// alloca values whose addresses are stored in sumtype `_data` fields and
	// therefore must outlive the current stack frame.
	sumtype_data_heap_allocas map[int]bool
	// Type layout caches/guards to avoid recursive size/alignment loops.
	type_size_cache  []int  // indexed by type_id, 0 = not cached (valid sizes are > 0 or == 0 only for void)
	type_align_cache []int  // indexed by type_id, 0 = not cached
	type_size_stack  []bool // indexed by type_id (recursion guard)
	type_align_stack []bool // indexed by type_id (recursion guard)
	// Cache for struct field offset calculations (key: typ_id << 16 | field_idx)
	struct_field_offset_cache map[int]int
	// Lookup caches for O(1) name resolution
	func_by_name   map[string]int // function name → index in g.mod.funcs
	global_by_name map[string]int // global name → index in g.mod.globals
	// Per-function cache for alloca pointer analysis (cleared per function)
	alloca_ptr_cache map[int]u8 // alloca_id → 1=has_ptrs, 2=no_ptrs
	// Cached environment variables for debug tracing (read once at init)
	env_dump_funcrefs     string
	env_trace_skip_dead   string
	env_dump_stackmap     string
	env_dump_blocks       string
	env_trace_paramspill  string
	env_trace_val         string
	env_trace_instr       string
	env_trace_cmp         string
	env_trace_store       string
	env_trace_load        string
	env_trace_call        string
	env_trace_ret         string
	env_trace_bitcast     string
	env_trace_assign      string
	env_trace_extract     string
	env_trace_struct_init string
	env_trace_agg_copy    string
	env_trace_insert      string
	env_trace_callcount   string
	env_trace_callarg     string
	env_trace_struct_addr string
	env_trace_strlit      string
	env_trace_storeval    string
	env_trace_regalloc    string
	env_no_regalloc       bool
	// Reverse map: val_id → block_id for block-kind values.
	// Value.index is unreliable in ARM64-compiled binaries, so use this instead.
	val_to_block []int
}

pub fn Gen.new(mod &mir.Module) &Gen {
	n_types := mod.type_store.types.len
	return &Gen{
		mod:                   mod
		macho:                 MachOObject.new()
		type_size_cache:       []int{len: n_types}
		type_align_cache:      []int{len: n_types}
		type_size_stack:       []bool{len: n_types}
		type_align_stack:      []bool{len: n_types}
		env_dump_funcrefs:     os.getenv('V2_ARM64_DUMP_FUNCREFS')
		env_trace_skip_dead:   os.getenv('V2_ARM64_TRACE_SKIP_DEAD')
		env_dump_stackmap:     os.getenv('V2_ARM64_DUMP_STACKMAP')
		env_dump_blocks:       os.getenv('V2_ARM64_DUMP_BLOCKS')
		env_trace_paramspill:  os.getenv('V2_ARM64_TRACE_PARAMSPILL')
		env_trace_val:         os.getenv('V2_ARM64_TRACE_VAL')
		env_trace_instr:       os.getenv('V2_ARM64_TRACE_INSTR')
		env_trace_cmp:         os.getenv('V2_ARM64_TRACE_CMP')
		env_trace_store:       os.getenv('V2_ARM64_TRACE_STORE')
		env_trace_load:        os.getenv('V2_ARM64_TRACE_LOAD')
		env_trace_call:        os.getenv('V2_ARM64_TRACE_CALL')
		env_trace_ret:         os.getenv('V2_ARM64_TRACE_RET')
		env_trace_bitcast:     os.getenv('V2_ARM64_TRACE_BITCAST')
		env_trace_assign:      os.getenv('V2_ARM64_TRACE_ASSIGN')
		env_trace_extract:     os.getenv('V2_ARM64_TRACE_EXTRACT')
		env_trace_struct_init: os.getenv('V2_ARM64_TRACE_STRUCT_INIT')
		env_trace_agg_copy:    os.getenv('V2_ARM64_TRACE_AGG_COPY')
		env_trace_insert:      os.getenv('V2_ARM64_TRACE_INSERT')
		env_trace_callcount:   os.getenv('V2_ARM64_TRACE_CALLCOUNT')
		env_trace_callarg:     os.getenv('V2_ARM64_TRACE_CALLARG')
		env_trace_struct_addr: os.getenv('V2_ARM64_TRACE_STRUCT_ADDR')
		env_trace_strlit:      os.getenv('V2_ARM64_TRACE_STRLIT')
		env_trace_storeval:    os.getenv('V2_ARM64_TRACE_STOREVAL')
		env_trace_regalloc:    os.getenv('V2_ARM64_TRACE_REGALLOC')
		env_no_regalloc:       os.getenv('V2_ARM64_NO_REGALLOC').len > 0
	}
}

pub fn (mut g Gen) gen() {
	g.gen_pre_pass()
	for fi := 0; fi < g.mod.funcs.len; fi++ {
		g.gen_func(g.mod.funcs[fi])
	}
	g.gen_post_pass()
}

// gen_pre_pass registers global symbols and builds lookup caches.
// Must be called before any gen_func calls.
pub fn (mut g Gen) gen_pre_pass() {
	// Pre-register global symbols BEFORE generating functions
	// This ensures add_undefined() finds existing symbols instead of creating undefined ones
	mut data_offset := u64(0)
	for gvar in g.mod.globals {
		// Skip external globals (defined elsewhere, e.g. __stdoutp)
		if gvar.linkage == .external {
			continue
		}
		// Align to 8 bytes
		data_offset = (data_offset + 7) & ~7
		g.macho.add_symbol('_' + gvar.name, data_offset, true, 3)
		size := if gvar.initial_data.len > 0 {
			gvar.initial_data.len
		} else {
			g.type_size(gvar.typ)
		}
		data_offset += u64(size)
	}

	// Build lookup caches for O(1) name resolution
	for fi, func in g.mod.funcs {
		g.func_by_name[func.name] = fi
	}
	for gi, gvar in g.mod.globals {
		g.global_by_name[gvar.name] = gi
	}

	// Build val_to_block once (block data doesn't change between functions).
	// Scan values for basic_block kind instead of using g.mod.blocks[bid].val_id
	// which returns wrong results in ARM64-compiled binaries (large struct copy bug).
	g.val_to_block = []int{len: g.mod.values.len}
	for vtb_i := 0; vtb_i < g.val_to_block.len; vtb_i++ {
		g.val_to_block[vtb_i] = -1
	}
	for vi := 0; vi < g.mod.values.len; vi++ {
		if g.mod.values[vi].kind == .basic_block {
			bid := g.mod.values[vi].index
			if bid >= 0 && bid < g.mod.blocks.len {
				g.val_to_block[vi] = bid
			}
		}
	}

	// Pre-populate type size/align caches so parallel workers can share them read-only
	g.pre_populate_type_caches()
}

// gen_post_pass emits the unresolved stub, global data, and patches symbol addresses.
// Must be called after all gen_func calls.
pub fn (mut g Gen) gen_post_pass() {
	// Add return-zero stub for unresolved symbols.
	// When the linker can't resolve a symbol, it redirects calls here instead of
	// letting them jump to the Mach-O header which corrupts memory.
	g.macho.add_symbol('___unresolved_stub', u64(g.macho.text_data.len), false, 1)
	g.emit(0xD2800000) // mov x0, #0
	g.emit(0xD2800001) // mov x1, #0
	g.emit(0xD65F03C0) // ret

	// Globals in __data (Section 3) - emit actual data
	for gvar in g.mod.globals {
		// Skip external globals (defined elsewhere)
		if gvar.linkage == .external {
			continue
		}
		// Skip globals that collide with function names (same as pre-registration loop)
		if gvar.name in g.func_by_name {
			continue
		}
		for g.macho.data_data.len % 8 != 0 {
			g.macho.data_data << 0
		}
		// Constant arrays: emit raw element data directly
		if gvar.initial_data.len > 0 {
			g.macho.data_data << gvar.initial_data
			continue
		}
		// Calculate actual size of the global variable based on its type.
		size := g.type_size(gvar.typ)
		if gvar.is_constant {
			match size {
				1 {
					g.macho.data_data << u8(gvar.initial_value)
				}
				2 {
					mut bytes := []u8{len: 2}
					binary.little_endian_put_u16(mut bytes, u16(gvar.initial_value))
					g.macho.data_data << bytes
				}
				4 {
					mut bytes := []u8{len: 4}
					binary.little_endian_put_u32(mut bytes, u32(gvar.initial_value))
					g.macho.data_data << bytes
				}
				8 {
					mut bytes := []u8{len: 8}
					binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
					g.macho.data_data << bytes
				}
				else {
					// For struct constants (e.g., sum types), emit initial_value as first 8 bytes
					// (the tag for sum types), then zeros for the rest.
					if gvar.initial_value != 0 && size >= 8 {
						mut bytes := []u8{len: 8}
						binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
						g.macho.data_data << bytes
						for _ in 0 .. size - 8 {
							g.macho.data_data << 0
						}
					} else {
						for _ in 0 .. size {
							g.macho.data_data << 0
						}
					}
				}
			}
		} else {
			// For regular (mutable) globals, emit initial value if set, else zeros.
			if gvar.initial_value != 0 {
				match size {
					1 {
						g.macho.data_data << u8(gvar.initial_value)
					}
					2 {
						mut bytes := []u8{len: 2}
						binary.little_endian_put_u16(mut bytes, u16(gvar.initial_value))
						g.macho.data_data << bytes
					}
					4 {
						mut bytes := []u8{len: 4}
						binary.little_endian_put_u32(mut bytes, u32(gvar.initial_value))
						g.macho.data_data << bytes
					}
					else {
						mut bytes := []u8{len: 8}
						binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
						g.macho.data_data << bytes
						for _ in 0 .. size - 8 {
							g.macho.data_data << 0
						}
					}
				}
			} else {
				for _ in 0 .. size {
					g.macho.data_data << 0
				}
			}
		}
	}

	// Patch symbol addresses
	cstring_base := u64(g.macho.text_data.len)
	// Align data section to 8 bytes
	data_base := (cstring_base + u64(g.macho.str_data.len) + 7) & ~7

	for mut sym in g.macho.symbols {
		if sym.sect == 2 {
			sym.value += cstring_base
		} else if sym.sect == 3 {
			sym.value += data_base
		}
	}
}

// new_worker_clone creates a new Gen instance for parallel code generation.
// The worker shares the read-only MIR module and lookup caches, but has its
// own MachOObject buffers for independent code emission.
// pre_populate_type_caches computes type_size and type_align for ALL types
// in the type store, so that workers can share the caches read-only.
pub fn (mut g Gen) pre_populate_type_caches() {
	for tid := 0; tid < g.mod.type_store.types.len; tid++ {
		g.type_size(tid)
		g.type_align(tid)
	}
}

pub fn (g &Gen) new_worker_clone() &Gen {
	return &Gen{
		mod:                   g.mod
		macho:                 MachOObject.new()
		func_by_name:          g.func_by_name
		global_by_name:        g.global_by_name
		val_to_block:          g.val_to_block
		type_size_cache:       g.type_size_cache
		type_align_cache:      g.type_align_cache
		type_size_stack:       g.type_size_stack
		type_align_stack:      g.type_align_stack
		env_dump_funcrefs:     g.env_dump_funcrefs
		env_trace_skip_dead:   g.env_trace_skip_dead
		env_dump_stackmap:     g.env_dump_stackmap
		env_dump_blocks:       g.env_dump_blocks
		env_trace_paramspill:  g.env_trace_paramspill
		env_trace_val:         g.env_trace_val
		env_trace_instr:       g.env_trace_instr
		env_trace_cmp:         g.env_trace_cmp
		env_trace_store:       g.env_trace_store
		env_trace_load:        g.env_trace_load
		env_trace_call:        g.env_trace_call
		env_trace_ret:         g.env_trace_ret
		env_trace_bitcast:     g.env_trace_bitcast
		env_trace_assign:      g.env_trace_assign
		env_trace_extract:     g.env_trace_extract
		env_trace_struct_init: g.env_trace_struct_init
		env_trace_agg_copy:    g.env_trace_agg_copy
		env_trace_insert:      g.env_trace_insert
		env_trace_callcount:   g.env_trace_callcount
		env_trace_callarg:     g.env_trace_callarg
		env_trace_struct_addr: g.env_trace_struct_addr
		env_trace_strlit:      g.env_trace_strlit
		env_trace_storeval:    g.env_trace_storeval
		env_trace_regalloc:    g.env_trace_regalloc
		env_no_regalloc:       g.env_no_regalloc
	}
}

// merge_worker merges a parallel worker's output buffers into the main Gen.
// text_data, str_data, symbols, and relocations are concatenated with offset adjustment.
pub fn (mut g Gen) merge_worker(w &Gen) {
	text_base := g.macho.text_data.len
	str_base := g.macho.str_data.len

	// Append machine code
	g.macho.text_data << w.macho.text_data

	// Append string literal data
	g.macho.str_data << w.macho.str_data

	// Merge symbols: remap worker symbol indices to main symbol table
	mut sym_remap := []int{len: w.macho.symbols.len}
	for wi, sym in w.macho.symbols {
		mut new_value := sym.value
		if sym.sect == 1 {
			new_value += u64(text_base)
		} else if sym.sect == 2 {
			new_value += u64(str_base)
		}
		// Local symbols (L_str_*, L_cstr_*) are per-worker and must never be
		// deduplicated — each worker's L_str_0 refers to a different string literal.
		is_local := sym.name.len > 2 && sym.name[0] == `L` && sym.name[1] == `_`
		// Check if symbol already exists in main (e.g., pre-registered global or extern)
		if !is_local {
			if existing := g.macho.sym_by_name[sym.name] {
				// Update existing symbol with definition if this one defines it
				if sym.type_ != 0x01 { // not N_UNDF
					mut main_sym := &g.macho.symbols[existing]
					main_sym.type_ = sym.type_
					main_sym.sect = sym.sect
					main_sym.value = new_value
				}
				sym_remap[wi] = existing
				continue
			}
		}
		sym_remap[wi] = g.macho.symbols.len
		name_off := g.macho.str_table.len
		g.macho.str_table << sym.name.bytes()
		g.macho.str_table << 0
		g.macho.symbols << Symbol{
			name:     sym.name
			type_:    sym.type_
			sect:     sym.sect
			desc:     sym.desc
			value:    new_value
			name_off: name_off
		}
		if !is_local {
			g.macho.sym_by_name[sym.name] = sym_remap[wi]
		}
	}

	// Merge relocations with adjusted addresses and remapped symbol indices
	for rel in w.macho.relocs {
		g.macho.relocs << RelocationInfo{
			addr:    rel.addr + text_base
			sym_idx: sym_remap[rel.sym_idx]
			pcrel:   rel.pcrel
			length:  rel.length
			extern:  rel.extern
			type_:   rel.type_
		}
	}
}

pub fn (mut g Gen) gen_func(func mir.Function) {
	if func.is_c_extern {
		// C extern functions are provided by external libraries (libc, etc.).
		// Don't emit any local symbol — let the linker resolve them as undefined externals.
		return
	}
	if func.blocks.len == 0 {
		// Emit a minimal stub: just a ret instruction.
		// This handles functions registered in Phase 3 but not built in Phase 4
		// (dead code elimination), or functions with empty bodies.
		g.curr_offset = g.macho.text_data.len
		sym_name := '_' + func.name
		g.macho.add_symbol(sym_name, u64(g.curr_offset), false, 1)
		g.emit(0xd65f03c0) // ret
		return
	}
	g.curr_offset = g.macho.text_data.len
	g.stack_map.clear()
	g.alloca_offsets.clear()
	g.alloca_ptr_cache.clear()
	// Reuse block_offsets array, grow if needed, only zero this function's blocks
	n_blks := g.mod.blocks.len
	if g.block_offsets.len < n_blks {
		g.block_offsets = []int{len: n_blks}
		// Fresh allocation needs full -1 init
		for bo_idx := 0; bo_idx < n_blks; bo_idx++ {
			g.block_offsets[bo_idx] = -1
		}
	} else {
		// Only reset blocks belonging to this function
		for fbi := 0; fbi < func.blocks.len; fbi++ {
			bid := func.blocks[fbi]
			if bid >= 0 && bid < g.block_offsets.len {
				g.block_offsets[bid] = -1
			}
		}
	}
	// val_to_block is built once in gen(), not per function
	g.pending_label_blks.clear()
	g.pending_label_offs.clear()
	g.func_count++
	g.total_pending = 0
	g.total_resolved = 0
	g.reg_map.clear()
	g.used_regs.clear()
	g.string_literal_offsets.clear()
	g.const_cache.clear()
	g.sumtype_data_heap_allocas.clear()
	g.cur_func_ret_type = func.typ
	g.cur_func_name = func.name
	g.x8_save_offset = 0
	g.mark_sumtype_data_heap_allocas(func)
	g.allocate_registers(func)
	if g.env_dump_funcrefs.len > 0
		&& (g.env_dump_funcrefs == '*' || func.name == g.env_dump_funcrefs) {
		eprintln('ARM64 FUNCREFS fn=${func.name} begin')
		for i, vv in g.mod.values {
			if vv.kind != .func_ref {
				continue
			}
			if vv.name.contains('cleanc__Gen__expr') {
				eprintln('ARM64 FUNCREF val=${i} name=${vv.name} typ=${vv.typ}')
			}
		}
		for f in g.mod.funcs {
			if f.name.contains('cleanc__Gen__expr') {
				eprintln('ARM64 FUNCDECL id=${f.id} name=${f.name} typ=${f.typ} params=${f.params}')
			}
		}
		eprintln('ARM64 FUNCREFS fn=${func.name} end')
	}

	// Check if function requires indirect return pointer preservation in x8.
	fn_ret_typ := g.mod.type_store.types[func.typ]
	fn_ret_size := g.type_size(func.typ)
	needs_x8_save := func.abi_ret_indirect || (fn_ret_typ.kind == .struct_t && fn_ret_size > 16)

	// Callee-saved registers are pushed at [fp - 8], [fp - 16], etc.
	// We need to account for this when computing stack offsets
	callee_saved_size := ((g.used_regs.len + 1) / 2) * 16

	// Stack Frame - start after callee-saved register area
	mut slot_offset := 8 + callee_saved_size

	// If function returns large struct, reserve slot for saving x8
	if needs_x8_save {
		g.x8_save_offset = -slot_offset
		slot_offset += 8
	}

	for pi, pid in func.params {
		// For struct parameters, allocate full struct size on the stack.
		// On ARM64, structs > 16 bytes are passed by pointer (indirect),
		// and structs 9-16 bytes are passed in 2 consecutive registers.
		param_typ := g.mod.values[pid].typ
		param_type_info := g.mod.type_store.types[param_typ]
		param_size := g.type_size(param_typ)
		is_indirect_param := pi < func.abi_param_class.len && func.abi_param_class[pi] == .indirect
		if is_indirect_param || (param_type_info.kind == .struct_t && param_size > 16) {
			// Align to 16 bytes and allocate full struct size
			slot_offset = (slot_offset + 15) & ~0xF
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
			// Reserve one more scalar slot so following values do not overlap
			// with the first field at the base offset.
			slot_offset += 8
		} else if param_type_info.kind == .struct_t && param_size > 8 {
			// Small struct (9-16 bytes) passed in 2 registers - allocate full size
			slot_offset = (slot_offset + 7) & ~0x7
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		} else {
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		}
	}

	// Pre-pass: find string_literal values used in this function and allocate stack for them
	mut used_string_literals := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			// Check all operands for string_literal references
			for op in instr.operands {
				op_val := g.mod.values[op]
				if op_val.kind == .string_literal {
					used_string_literals[op] = true
				}
			}
			// Also check return values - if function returns a string_literal directly
			if instr.op == .ret && instr.operands.len > 0 {
				ret_val := g.mod.values[instr.operands[0]]
				if ret_val.kind == .string_literal {
					used_string_literals[instr.operands[0]] = true
				}
			}
		}
	}

	// Allocate stack slots for used string_literal values
	for str_lit_id, _ in used_string_literals {
		mut str_size := g.type_size(g.mod.values[str_lit_id].typ)
		if str_size <= 0 {
			str_size = 24
		}
		slot_offset = (slot_offset + 15) & ~0xF
		slot_offset += str_size
		g.stack_map[str_lit_id] = -slot_offset
		// Keep subsequent scalar slots below the aggregate base.
		slot_offset += 8
	}

	trace_skip_dead := g.env_trace_skip_dead.len > 0
		&& (g.env_trace_skip_dead == '*' || func.name == g.env_trace_skip_dead)

	for i, blk_id in func.blocks {
		g.next_blk = if i + 1 < func.blocks.len { func.blocks[i + 1] } else { -1 }
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			opcode := g.selected_opcode(instr)
			// Phi lowering can leave placeholder bitcasts/copies that are fully dead.
			// Do not reserve stack slots for these values; in large recursive
			// functions this can cause pathological frame growth and stack overflow.
			if val.uses.len == 0 {
				if opcode == .bitcast && instr.operands.len == 0 {
					if trace_skip_dead {
						eprintln('ARM64 SKIP_DEAD fn=${func.name} val=${val_id} op=bitcast ops=${instr.operands} uses_len=${val.uses.len} uses=${val.uses}')
					}
					continue
				}
				if opcode == .assign {
					if trace_skip_dead {
						eprintln('ARM64 SKIP_DEAD fn=${func.name} val=${val_id} op=assign ops=${instr.operands} uses_len=${val.uses.len} uses=${val.uses}')
					}
					continue
				}
			}

			if instr.op == .alloca {
				if val_id !in g.sumtype_data_heap_allocas {
					// Calculate allocation size based on the type
					// The alloca result type is ptr(T), so get the element type
					ptr_type := g.mod.type_store.types[val.typ]
					elem_size := g.type_size(ptr_type.elem_type)
					mut alloc_size := if elem_size > 0 { elem_size } else { 8 }
					// Check for array alloca: operand[0] is element count
					if instr.operands.len > 0 {
						count_val := g.mod.values[instr.operands[0]]
						count := count_val.name.int()
						if count > 1 {
							alloc_size = elem_size * count
						}
					}

					// Align to 16 bytes.
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += alloc_size
					g.alloca_offsets[val_id] = -slot_offset

					// Ensure the next instruction does not use the slot
					// overlapping with the base of the alloca data.
					slot_offset += 8
				}
			}

			if instr.op == .inline_string_init {
				// Reserve payload bytes plus a separate pointer slot.
				// The payload size follows the lowered string type layout.
				mut string_size := g.type_size(instr.typ)
				if string_size <= 0 {
					string_size = 24
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += string_size // struct data
				slot_offset += 8 // pointer slot (separate from struct)
				g.stack_map[val_id] = -slot_offset
				continue
			}

			if instr.op == .insertvalue || instr.op == .struct_init {
				// Tuple/struct needs full ABI size, not just fields.len * 8.
				tuple_typ := g.mod.type_store.types[instr.typ]
				mut tuple_size := g.type_size(instr.typ)
				if tuple_size <= 0 {
					tuple_size = tuple_typ.fields.len * 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += tuple_size
				g.stack_map[val_id] = -slot_offset
				// Keep following scalar slots from overlapping field 0.
				slot_offset += 8
				continue
			}

			// Keep full stack storage for struct/array values so aggregate copies have
			// stable backing bytes even when values are register-allocated.
			val_typ := g.mod.type_store.types[val.typ]
			if val_typ.kind == .struct_t || val_typ.kind == .array_t {
				mut struct_size := g.type_size(val.typ)
				if struct_size <= 0 {
					struct_size = if val_typ.fields.len > 0 { val_typ.fields.len * 8 } else { 8 }
				}
				// Some large aggregate producers are represented as data pointers in stack
				// slots (one word), not inline bytes. Reserve pointer-sized storage for
				// those values to avoid pathological frame growth in recursive functions.
				if val_typ.kind == .struct_t && struct_size > 16
					&& g.large_struct_stack_value_is_pointer(val_id) {
					g.stack_map[val_id] = -slot_offset
					slot_offset += 8
					continue
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += struct_size
				g.stack_map[val_id] = -slot_offset
				// Keep following scalar slots below the aggregate base.
				slot_offset += 8
				continue
			}

			if instr.op == .call {
				// Check if call returns a tuple
				result_typ := g.mod.type_store.types[val.typ]
				mut is_multi_reg_call := result_typ.kind == .struct_t && result_typ.fields.len > 1
				mut call_tuple_size := g.type_size(val.typ)
				// Also check callee's registered return type
				if !is_multi_reg_call && instr.operands.len > 0 {
					callee_val := g.mod.values[instr.operands[0]]
					if callee_val.kind == .func_ref {
						if f := g.get_function_by_name(callee_val.name) {
							callee_ret_typ := g.mod.type_store.types[f.typ]
							callee_ret_size := g.type_size(f.typ)
							if callee_ret_typ.kind == .struct_t && callee_ret_size > 8
								&& callee_ret_size <= 16 {
								is_multi_reg_call = true
								call_tuple_size = callee_ret_size
							}
						}
					}
				}
				if is_multi_reg_call {
					if call_tuple_size <= 0 {
						call_tuple_size = 16
					}
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += call_tuple_size
					g.stack_map[val_id] = -slot_offset
					// Keep following scalar slots below the aggregate base.
					slot_offset += 8
					continue
				}
			} else if instr.op == .call_sret {
				// call_sret returns an aggregate indirectly into the destination slot.
				result_typ := g.mod.type_store.types[val.typ]
				if result_typ.kind == .struct_t {
					result_size := g.type_size(val.typ)
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += result_size
					g.stack_map[val_id] = -slot_offset
					// Keep following scalar slots below the aggregate base.
					slot_offset += 8
					continue
				}
			}

			if val_id in g.reg_map {
				continue
			}
			// Assign slot for result of instruction (or pointer for alloca)
			g.stack_map[val_id] = -slot_offset
			slot_offset += 8
		}
	}

	g.stack_size = (slot_offset + 16) & ~0xF

	if g.env_dump_stackmap.len > 0
		&& (g.env_dump_stackmap == '*' || func.name == g.env_dump_stackmap) {
		eprintln('ARM64 FRAME ${func.name} stack_size=${g.stack_size} x8_save_offset=${g.x8_save_offset}')
		eprintln('ARM64 STACKMAP ${func.name} begin')
		for vid, off in g.stack_map {
			mut typ_kind := 'na'
			mut typ_size := 0
			mut typ_id := ssa.TypeID(0)
			mut typ_desc := ''
			mut kind := 'na'
			mut name := ''
			mut op := 'na'
			mut blk := ssa.BlockID(-1)
			mut operands := ''
			mut uses := ''
			if vid > 0 && vid < g.mod.values.len {
				vv := g.mod.values[vid]
				kind = '${vv.kind}'
				name = vv.name
				if vv.typ > 0 && vv.typ < g.mod.type_store.types.len {
					typ_id = vv.typ
					typ := g.mod.type_store.types[vv.typ]
					typ_kind = '${typ.kind}'
					typ_size = g.type_size(vv.typ)
					if typ.kind == .struct_t {
						typ_desc = 'fields=${typ.field_names} ftypes=${typ.fields}'
					} else if typ.kind == .ptr_t {
						typ_desc = 'elem=${typ.elem_type}'
					}
				}
				if vv.kind == .instruction {
					instr := g.mod.instrs[vv.index]
					op = '${g.selected_opcode(instr)}'
					blk = instr.block
					operands = '${instr.operands}'
				}
				uses = '${vv.uses}'
			}
			eprintln('ARM64 STACKMAP ${func.name} val=${vid} off=${off} kind=${kind} blk=${blk} op=${op} ops=${operands} uses=${uses} typ=${typ_id}/${typ_kind} size=${typ_size} tdesc=`${typ_desc}` name=`${name}`')
		}
		for vid, off in g.alloca_offsets {
			mut elem_typ_id := ssa.TypeID(0)
			mut elem_typ_kind := 'na'
			mut elem_typ_size := 0
			mut alloca_ops := ''
			if vid > 0 && vid < g.mod.values.len {
				vv := g.mod.values[vid]
				if vv.kind == .instruction {
					instr := g.mod.instrs[vv.index]
					alloca_ops = '${instr.operands}'
					if vv.typ > 0 && vv.typ < g.mod.type_store.types.len {
						typ := g.mod.type_store.types[vv.typ]
						if typ.kind == .ptr_t && typ.elem_type > 0
							&& typ.elem_type < g.mod.type_store.types.len {
							elem_typ_id = typ.elem_type
							elem_typ := g.mod.type_store.types[elem_typ_id]
							elem_typ_kind = '${elem_typ.kind}'
							elem_typ_size = g.type_size(elem_typ_id)
						}
					}
				}
			}
			eprintln('ARM64 ALLOCA ${func.name} val=${vid} off=${off} ops=${alloca_ops} elem=${elem_typ_id}/${elem_typ_kind} size=${elem_typ_size}')
		}
		eprintln('ARM64 STACKMAP ${func.name} end')
	}
	if g.env_dump_blocks.len > 0 && (g.env_dump_blocks == '*' || func.name == g.env_dump_blocks) {
		eprintln('ARM64 BLOCKS ${func.name} begin')
		for bi, blk_id in func.blocks {
			blk := g.mod.blocks[blk_id]
			eprintln('ARM64 BLOCK ${func.name} order=${bi} id=${blk_id} val=${blk.val_id} preds=${blk.preds} succs=${blk.succs} instrs=${blk.instrs}')
			for val_id in blk.instrs {
				if val_id <= 0 || val_id >= g.mod.values.len {
					continue
				}
				val := g.mod.values[val_id]
				mut op := 'na'
				mut operands := '[]'
				if val.kind == .instruction {
					instr := g.mod.instrs[val.index]
					op = '${g.selected_opcode(instr)}'
					operands = '${instr.operands}'
				}
				mut callee_info := ''
				if val.kind == .instruction {
					instr := g.mod.instrs[val.index]
					opcode := g.selected_opcode(instr)
					if opcode in [.call, .call_indirect, .call_sret] && instr.operands.len > 0 {
						callee_id := instr.operands[0]
						if callee_id > 0 && callee_id < g.mod.values.len {
							callee_val := g.mod.values[callee_id]
							callee_info = ' callee=${callee_id}:${callee_val.kind}:${callee_val.name}:${callee_val.typ}'
						} else {
							callee_info = ' callee=${callee_id}:invalid'
						}
					}
				}
				eprintln('ARM64 BLOCK INSTR ${func.name} blk=${blk_id} val=${val_id} kind=${val.kind} op=${op} ops=${operands} uses=${val.uses}${callee_info}')
			}
		}
		eprintln('ARM64 BLOCKS ${func.name} end')
	}
	g.macho.add_symbol('_' + func.name, u64(g.curr_offset), true, 1)

	// Prologue
	g.emit(asm_stp_fp_lr_pre())
	g.emit(asm_mov_fp_sp())

	// Save callee-saved regs (pushed below fp using pre-decrement)
	for i := 0; i < g.used_regs.len; i += 2 {
		r1 := g.used_regs[i]
		mut r2 := 31 // xzr
		if i + 1 < g.used_regs.len {
			r2 = g.used_regs[i + 1]
		}
		g.emit(asm_stp_pair_pre(Reg(r1), Reg(r2)))
	}

	g.emit_sub_sp(g.stack_size)

	// Save x8 if this function returns a large struct
	// x8 contains the indirect return pointer from the caller
	// Save it at a fixed offset from fp (below callee-saved registers)
	if g.x8_save_offset != 0 {
		g.emit_str_reg_offset(8, 29, g.x8_save_offset)
	}

	// The Mach-O LC_MAIN entrypoint invokes `main` with C-style argc/argv in
	// x0/x1. Persist them to builtin globals so `os.args` / `arguments()` work.
	if func.name == 'main' {
		g.store_entry_arg_to_global(0, 'g_main_argc')
		g.store_entry_arg_to_global(1, 'g_main_argv')
		// Call _vinit to initialize dynamic array constants
		g.emit_call_to_named_fn('_vinit')
	}

	// Spill params
	// ARM64 ABI: args in x0..x7, args 8+ on caller stack.
	// Struct params ≤ 16 bytes occupy ceil(size/8) consecutive registers.
	// Struct params > 16 bytes are passed by pointer (one register).
	// ARM64 ABI: integer params in x0-x7, float params in d0-d7 (independent allocation)
	mut reg_idx := 0
	mut float_reg_idx := 0
	trace_paramspill := g.env_trace_paramspill.len > 0
		&& (g.env_trace_paramspill == '*' || func.name == g.env_trace_paramspill)
	for i, pid in func.params {
		param_typ := g.mod.values[pid].typ
		param_type_info := g.mod.type_store.types[param_typ]
		param_size := g.type_size(param_typ)
		is_indirect_param := i < func.abi_param_class.len && func.abi_param_class[i] == .indirect
		if trace_paramspill {
			eprintln('ARM64 PARAMSPILL fn=${func.name} idx=${i} pid=${pid} typ=${param_typ} kind=${int(param_type_info.kind)} size=${param_size} reg_idx=${reg_idx} indirect=${is_indirect_param}')
		}

		// Float parameters arrive in d-registers; move to x-register for storage
		if param_type_info.kind == .float_t && !is_indirect_param {
			if float_reg_idx < 8 {
				// fmov xN, dN to get float bits into integer register
				g.emit(asm_fmov_x_d(Reg(9), float_reg_idx))
				if reg := g.reg_map[pid] {
					g.emit_mov_reg(reg, 9)
				} else {
					offset := g.stack_map[pid]
					g.emit_str_reg_offset(9, 29, offset)
				}
			}
			float_reg_idx++
			continue
		}

		mut src_reg := reg_idx
		if reg_idx >= 8 {
			stack_arg_off := 16 + ((reg_idx - 8) * 8)
			g.emit_ldr_reg_offset(9, 29, stack_arg_off)
			src_reg = 9
		}

		// For large struct parameters (> 16 bytes), the argument value is a pointer.
		// Copy pointed struct bytes into the function-local spill slot.
		if is_indirect_param || (param_type_info.kind == .struct_t && param_size > 16) {
			if src_reg != 9 {
				g.emit_mov_reg(9, src_reg)
			}
			offset := g.stack_map[pid]
			num_fields := (param_size + 7) / 8
			for field_idx in 0 .. num_fields {
				g.emit(asm_ldr_imm(Reg(10), Reg(9), u32(field_idx)))
				g.emit_str_reg_offset(10, 29, offset + field_idx * 8)
			}
			// Large/indirect params are represented as addresses in registers.
			// Materialize the local spill address for any register-allocated uses.
			if reg := g.reg_map[pid] {
				g.emit_add_fp_imm(reg, offset)
			}
			reg_idx += 1
		} else if param_type_info.kind == .struct_t && param_size > 8 {
			// Small struct (9-16 bytes) passed in 2 consecutive registers.
			offset := g.stack_map[pid]
			num_regs := (param_size + 7) / 8
			if trace_paramspill {
				eprintln('ARM64 PARAMSPILL fn=${func.name} idx=${i} mode=small_struct offset=${offset} num_regs=${num_regs}')
			}
			for ri in 0 .. num_regs {
				mut cur_reg := reg_idx + ri
				if cur_reg >= 8 {
					stack_arg_off := 16 + ((cur_reg - 8) * 8)
					g.emit_ldr_reg_offset(9, 29, stack_arg_off)
					g.emit_str_reg_offset(9, 29, offset + ri * 8)
				} else {
					g.emit_str_reg_offset(cur_reg, 29, offset + ri * 8)
				}
			}
			if reg := g.reg_map[pid] {
				g.emit_add_fp_imm(reg, offset)
			}
			reg_idx += num_regs
		} else if reg := g.reg_map[pid] {
			if reg != src_reg {
				g.emit_mov_reg(reg, src_reg)
			}
			reg_idx += 1
		} else {
			offset := g.stack_map[pid]
			g.emit_str_reg_offset(src_reg, 29, offset)
			reg_idx += 1
		}
	}

	// Run SSA lowered global initializers before entering user main.
	// This mirrors the C backend behavior where __v2_global_init() is invoked from main.
	if func.name == 'main' && g.has_function_named('__v2_global_init') {
		sym_idx := g.macho.add_undefined('_' + '__v2_global_init')
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
		g.emit(asm_bl_reloc())
	}

	// Materialize all string literals unconditionally in the function prologue.
	// Relying on first-use codegen order can leave literal slots uninitialized when
	// control flow reaches a "reuse" site before the first emitted init site.
	mut lit_ids := []int{}
	for lit_id, _ in used_string_literals {
		lit_ids << lit_id
	}
	lit_ids.sort(a < b)
	for lit_id in lit_ids {
		g.load_val_to_reg(8, lit_id)
	}

	for i := 0; i < func.blocks.len; i++ {
		blk_id := int(func.blocks[i])
		g.next_blk = if i + 1 < func.blocks.len { int(func.blocks[i + 1]) } else { -1 }
		g.cur_blk_id = blk_id
		blk := g.mod.blocks[blk_id]
		g.block_offsets[blk_id] = g.macho.text_data.len - g.curr_offset

		for pi := 0; pi < g.pending_label_blks.len; pi++ {
			if g.pending_label_blks[pi] != blk_id {
				continue
			}
			off := g.pending_label_offs[pi]
			target := g.block_offsets[blk_id]
			rel := (target - off) / 4
			abs_off := g.curr_offset + off
			instr := g.read_u32(abs_off)

			mut new_instr := u32(0)
			// Check for CBNZ (0xB5...) vs B (0x14...) vs B.cond (0x54...)
			if (instr & 0xFF000000) == 0xB5000000 {
				// CBNZ
				new_instr = (instr & 0xFF000000) | ((u32(rel) & 0x7FFFF) << 5) | (instr & 0x1F)
			} else if (instr & 0xFC000000) == 0x14000000 {
				// B imm26
				new_instr = (instr & 0xFC000000) | (u32(rel) & 0x3FFFFFF)
			} else {
				// B.cond
				new_instr = (instr & 0xFF000000) | ((u32(rel) & 0x7FFFF) << 5) | (instr & 0x1F)
			}
			g.write_u32(abs_off, new_instr)
			g.total_resolved++
		}

		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}
	unresolved := g.total_pending - g.total_resolved
	if unresolved > 0 {
		eprintln('BRANCH: fn=${func.name} pending=${g.total_pending} resolved=${g.total_resolved} unresolved=${unresolved} pending_blks_len=${g.pending_label_blks.len}')
	}
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.mod.instrs[g.mod.values[val_id].index]
	op := g.selected_opcode(instr)
	trace_val := g.env_trace_val.len > 0
		&& (g.env_trace_val == '*' || g.cur_func_name == g.env_trace_val)
	if trace_val {
		eprintln('ARM64 VAL fn=${g.cur_func_name} val=${val_id} opi=${int(op)} off=${g.macho.text_data.len - g.curr_offset} sel=`${instr.selected_op}` ops=${instr.operands}')
	}
	trace_instr := g.env_trace_instr.len > 0
		&& (g.env_trace_instr == '*' || g.cur_func_name == g.env_trace_instr)
	if trace_instr {
		typ_id := g.mod.values[val_id].typ
		mut kind := ssa.TypeKind.void_t
		mut width := 0
		mut is_unsigned := false
		if typ_id > 0 && typ_id < g.mod.type_store.types.len {
			typ := g.mod.type_store.types[typ_id]
			kind = typ.kind
			width = typ.width
			is_unsigned = typ.is_unsigned
		}
		eprintln('ARM64 INSTR fn=${g.cur_func_name} val=${val_id} op=${op} orig=${instr.op} sel=${instr.selected_op} typ=${typ_id} kind=${kind} width=${width} unsigned=${is_unsigned} ops=${instr.operands}')
	}
	match op {
		.fadd, .fsub, .fmul, .fdiv, .frem {
			// Float operations using scalar SIMD instructions (d0-d7)
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// For now, load operands as float constants or from memory
			// Load LHS to d0
			g.load_float_operand(instr.operands[0], 0) // d0
			// Load RHS to d1
			g.load_float_operand(instr.operands[1], 1) // d1

			// Perform float operation: result in d0
			match op {
				.fadd {
					g.emit(asm_fadd_d0_d0_d1())
				}
				.fsub {
					g.emit(asm_fsub_d0_d0_d1())
				}
				.fmul {
					g.emit(asm_fmul_d0_d0_d1())
				}
				.fdiv {
					g.emit(asm_fdiv_d0_d0_d1())
				}
				.frem {
					// No single instruction for frem on ARM64
					// Use: d0 = d0 - trunc(d0/d1) * d1
					g.emit(asm_fdiv_d2_d0_d1())
					g.emit(asm_frintz_d2())
					g.emit(asm_fnmsub_d0_d2_d1_d0())
				}
				else {}
			}

			// Convert d0 result back to integer register for storage
			// Store the float bits in the result (for later int() conversion)
			g.emit(asm_fmov_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.fptosi {
			// Float to signed integer conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load float operand to d0
			g.load_float_operand(instr.operands[0], 0)

			// FCVTZS Xd, Dn (convert to signed int, truncate toward zero)
			g.emit(asm_fcvtzs_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.sitofp {
			// Signed integer to float conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load integer operand to x8
			src_reg := g.get_operand_reg(instr.operands[0], 8)

			// Check if target is f32
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32

			// SCVTF Dd, Xn (convert signed int to double)
			g.emit(asm_scvtf_d_x(0, Reg(src_reg)))

			if result_is_f32 {
				// Convert f64→f32 and move to integer register as 32-bit pattern
				g.emit(asm_fcvt_s_d(0, 0))
				g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
			} else {
				// FMOV Xd, D0 (copy f64 bit pattern to integer reg for storage)
				g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
			}

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.uitofp {
			// Unsigned integer to float conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load integer operand to x8
			src_reg := g.get_operand_reg(instr.operands[0], 8)

			// Check if target is f32
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32

			// UCVTF Dd, Xn (convert unsigned int to double)
			g.emit(asm_ucvtf_d_x(0, Reg(src_reg)))

			if result_is_f32 {
				// Convert f64→f32 and move to integer register as 32-bit pattern
				g.emit(asm_fcvt_s_d(0, 0))
				g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
			} else {
				// FMOV Xd, D0 (copy float bit pattern to integer reg for storage)
				g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
			}

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.fptoui {
			// Float to unsigned integer conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load float operand to d0
			g.load_float_operand(instr.operands[0], 0)

			// FCVTZU Xd, Dn (convert to unsigned int, truncate toward zero)
			g.emit(asm_fcvtzu_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq,
		.ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			// Optimization: Use actual registers if allocated, avoid shuffling to x8/x9
			// Dest register
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Op0 (LHS)
			lhs_reg := g.get_operand_reg(instr.operands[0], 8)

			// Op1 (RHS) - Check immediate optimization
			mut is_imm := false
			mut imm_val := i64(0)
			mut rhs_reg := 9 // Default scratch for RHS

			op1 := g.mod.values[instr.operands[1]]
			if op1.kind == .constant && op in [.add, .sub] {
				v := g.get_const_int(instr.operands[1])
				if v >= 0 && v < 4096 {
					is_imm = true
					imm_val = v
				}
			}

			if !is_imm {
				// Don't use x8 as scratch if LHS is in x8
				scratch := if lhs_reg == 8 { 9 } else { 8 }
				rhs_reg = g.get_operand_reg(instr.operands[1], scratch)
			}
			mut emitted_types_sum_is_check := false

			match op {
				.add {
					// Some frontend paths can leave `is` checks over `types.Type` as malformed
					// `add` i1 over sumtype wrappers. Lower these to direct `_tag == variant_tag`
					// checks in arm64 so branch conditions remain semantically correct.
					if g.try_emit_types_type_ischeck_add(dest_reg, lhs_reg, val_id, instr) {
						emitted_types_sum_is_check = true
					} else if is_imm {
						g.emit(asm_add_imm(Reg(dest_reg), Reg(lhs_reg), u32(imm_val)))
					} else {
						g.emit(asm_add_reg(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
					}
				}
				.sub {
					if is_imm {
						g.emit(asm_sub_imm(Reg(dest_reg), Reg(lhs_reg), u32(imm_val)))
					} else {
						g.emit(asm_sub_reg(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
					}
				}
				.mul {
					g.emit(asm_mul(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.sdiv {
					g.emit(asm_sdiv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.udiv {
					g.emit(asm_udiv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.srem {
					// Signed modulo: a % b = a - (a / b) * b
					// Choose temp register for quotient that doesn't conflict with inputs
					mut temp_reg := 10
					if lhs_reg == 10 || rhs_reg == 10 {
						temp_reg = 11
						if lhs_reg == 11 || rhs_reg == 11 {
							temp_reg = 12
						}
					}
					g.emit(asm_sdiv(Reg(temp_reg), Reg(lhs_reg), Reg(rhs_reg)))
					g.emit(asm_msub(Reg(dest_reg), Reg(temp_reg), Reg(rhs_reg), Reg(lhs_reg)))
				}
				.urem {
					// Unsigned modulo: a % b = a - (a / b) * b
					mut temp_reg := 10
					if lhs_reg == 10 || rhs_reg == 10 {
						temp_reg = 11
						if lhs_reg == 11 || rhs_reg == 11 {
							temp_reg = 12
						}
					}
					g.emit(asm_udiv(Reg(temp_reg), Reg(lhs_reg), Reg(rhs_reg)))
					g.emit(asm_msub(Reg(dest_reg), Reg(temp_reg), Reg(rhs_reg), Reg(lhs_reg)))
				}
				.and_ {
					g.emit(asm_and(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.or_ {
					g.emit(asm_orr(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.xor {
					g.emit(asm_eor(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.shl {
					g.emit(asm_lslv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.ashr {
					g.emit(asm_asrv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.lshr {
					g.emit(asm_lsrv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
					trace_cmp := g.env_trace_cmp.len > 0
						&& (g.env_trace_cmp == '*' || g.cur_func_name == g.env_trace_cmp)
					lhs_typ := g.mod.values[instr.operands[0]].typ
					is_float := lhs_typ > 0 && lhs_typ < g.mod.type_store.types.len
						&& g.mod.type_store.types[lhs_typ].kind == .float_t
					mut handled_large_struct_zero_cmp := false
					mut large_struct_cmp_operand := 0
					if is_float {
						// Float comparison: load to FP regs, use FCMP
						g.load_float_operand(instr.operands[0], 0) // d0
						g.load_float_operand(instr.operands[1], 1) // d1
						g.emit(asm_fcmp_d(Reg(0), Reg(1)))
					} else {
						// For `eq/ne` against zero on large struct values, compare the
						// struct truth word (`[addr + 0]`) instead of the slot address.
						// This keeps comparison semantics consistent with `.br` lowering.
						if op in [.eq, .ne] {
							lhs_id := instr.operands[0]
							rhs_id := instr.operands[1]
							if g.value_is_large_struct(lhs_id) && g.is_known_zero_value(rhs_id, 0) {
								large_struct_cmp_operand = lhs_id
							} else if g.value_is_large_struct(rhs_id)
								&& g.is_known_zero_value(lhs_id, 0) {
								large_struct_cmp_operand = rhs_id
							}
							if large_struct_cmp_operand > 0 {
								g.load_large_struct_truth_word_to_reg(9, large_struct_cmp_operand)
								g.emit(asm_cmp_reg(Reg(9), Reg(31)))
								handled_large_struct_zero_cmp = true
							}
						}
						if !handled_large_struct_zero_cmp {
							// Integer comparison
							// Use 32-bit CMP for i32 operands to preserve sign semantics.
							use_32bit := lhs_typ > 0 && lhs_typ < g.mod.type_store.types.len
								&& g.mod.type_store.types[lhs_typ].kind == .int_t
								&& g.mod.type_store.types[lhs_typ].width == 32
							if use_32bit {
								g.emit(asm_cmp_reg_w(Reg(lhs_reg), Reg(rhs_reg)))
							} else {
								g.emit(asm_cmp_reg(Reg(lhs_reg), Reg(rhs_reg)))
							}
						}
					}
					if trace_cmp {
						eprintln('ARM64 CMP fn=${g.cur_func_name} val=${val_id} op=${op} lhs_id=${instr.operands[0]} rhs_id=${instr.operands[1]} lhs_reg=${lhs_reg} rhs_reg=${rhs_reg} lhs_typ=${lhs_typ} float=${is_float} large_struct_zero_cmp=${handled_large_struct_zero_cmp} large_struct_id=${large_struct_cmp_operand}')
					}

					// CSET Rd, cond (works for both integer and float NZCV flags)
					match op {
						.eq { g.emit(asm_cset_eq(Reg(dest_reg))) }
						.ne { g.emit(asm_cset_ne(Reg(dest_reg))) }
						.lt { g.emit(asm_cset_lt(Reg(dest_reg))) }
						.gt { g.emit(asm_cset_gt(Reg(dest_reg))) }
						.le { g.emit(asm_cset_le(Reg(dest_reg))) }
						.ge { g.emit(asm_cset_ge(Reg(dest_reg))) }
						.ugt { g.emit(asm_cset_hi(Reg(dest_reg))) }
						.uge { g.emit(asm_cset_hs(Reg(dest_reg))) }
						.ult { g.emit(asm_cset_lo(Reg(dest_reg))) }
						.ule { g.emit(asm_cset_ls(Reg(dest_reg))) }
						else {}
					}
				}
				else {}
			}
			// Keep narrow integer results (i1/i8/i16/i32) canonical after 64-bit
			// ALU ops so upper garbage bits do not leak through later uses.
			result_typ_id := g.mod.values[val_id].typ
			if result_typ_id > 0 && result_typ_id < g.mod.type_store.types.len {
				result_typ := g.mod.type_store.types[result_typ_id]
				if result_typ.kind == .int_t && !emitted_types_sum_is_check {
					g.canonicalize_narrow_int_result(dest_reg, result_typ_id)
				}
			}
			// If dest_reg was not the allocated one (e.g. was 8), move it.
			// Only if spilled (not in reg_map) do we need to store.
			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.store {
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]
			trace_store := g.env_trace_store.len > 0
				&& (g.env_trace_store == '*' || g.cur_func_name == g.env_trace_store)
			// ValueID 0 is the SSA null/invalid sentinel.
			if src_id <= 0 || src_id >= g.mod.values.len {
				return
			}
			if ptr_id <= 0 || ptr_id >= g.mod.values.len {
				return
			}
			mut src_addr_override_id := 0
			if src_id > 0 && src_id < g.mod.values.len {
				src_val2 := g.mod.values[src_id]
				if src_val2.kind == .instruction {
					src_instr2 := g.mod.instrs[src_val2.index]
					if src_instr2.op == .bitcast && src_instr2.operands.len > 0 {
						bitcast_src := src_instr2.operands[0]
						if bitcast_src > 0 && bitcast_src < g.mod.values.len {
							bitcast_src_val := g.mod.values[bitcast_src]
							if bitcast_src_val.kind == .instruction {
								extract_instr := g.mod.instrs[bitcast_src_val.index]
								if extract_instr.op == .extractvalue
									&& extract_instr.operands.len >= 2 {
									idx_val_id := extract_instr.operands[1]
									if idx_val_id > 0 && idx_val_id < g.mod.values.len {
										idx_val := g.mod.values[idx_val_id]
										if idx_val.kind == .constant && idx_val.name == '0' {
											base_id := extract_instr.operands[0]
											if base_id > 0 && base_id < g.mod.values.len {
												base_val := g.mod.values[base_id]
												if base_val.kind == .instruction {
													load_instr := g.mod.instrs[base_val.index]
													if load_instr.op == .load
														&& load_instr.operands.len > 0 {
														load_src := load_instr.operands[0]
														if load_src > 0
															&& load_src < g.mod.values.len
															&& g.mod.values[load_src].kind == .string_literal {
															// Sumtype string payload lowering can arrive as:
															// bitcast(extractvalue(load(string_literal), 0)).
															// Preserve pointer-to-string-struct, not string.str.
															src_addr_override_id = load_src
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}

			// Check if we're storing a large struct value (> 16 bytes)
			// In this case, the value is a pointer to the struct and we need to copy
			val_val := g.mod.values[src_id]
			val_typ := g.mod.type_store.types[val_val.typ]
			val_size := g.type_size(val_val.typ)
			is_undef_aggregate := val_val.kind == .constant && val_val.name == 'undef'
			src_has_storage := src_id in g.reg_map || src_id in g.stack_map
				|| val_val.kind in [.global, .string_literal]
			mut dst_struct_size := 0
			mut dst_is_large_struct := false
			mut dst_is_small_struct := false
			mut dst_struct_typ_id := ssa.TypeID(0)
			mut dst_elem_is_ptrlike := false
			ptr_val := g.mod.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.mod.type_store.types.len {
					elem_typ := g.mod.type_store.types[ptr_typ.elem_type]
					elem_size := g.type_size(ptr_typ.elem_type)
					if elem_typ.kind in [.ptr_t, .func_t] {
						dst_elem_is_ptrlike = true
					}
					if elem_typ.kind == .struct_t || elem_typ.kind == .array_t {
						dst_struct_typ_id = ptr_typ.elem_type
						if elem_size > 16 {
							dst_is_large_struct = true
							dst_struct_size = elem_size
						} else if elem_size > 0 {
							dst_is_small_struct = true
							dst_struct_size = elem_size
						}
					}
				}
			}
			if trace_store {
				mut dst_kind := ssa.TypeKind.void_t
				mut dst_size := 0
				mut src_off_dbg := 0
				mut src_has_off_dbg := false
				if src_off := g.stack_map[src_id] {
					src_off_dbg = src_off
					src_has_off_dbg = true
				}
				mut src_op_dbg := 'na'
				if val_val.kind == .instruction {
					src_op_dbg = '${g.selected_opcode(g.mod.instrs[val_val.index])}'
				}
				if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
					ptr_typ := g.mod.type_store.types[ptr_val.typ]
					if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
						&& ptr_typ.elem_type < g.mod.type_store.types.len {
						dst_kind = g.mod.type_store.types[ptr_typ.elem_type].kind
						dst_size = g.type_size(ptr_typ.elem_type)
					}
				}
				eprintln('ARM64 STORE fn=${g.cur_func_name} src=${src_id} sop=${src_op_dbg} ptr=${ptr_id} styp=${val_val.typ}/${val_typ.kind} ssz=${val_size} src_has_storage=${src_has_storage} src_has_off=${src_has_off_dbg} src_off=${src_off_dbg} dst_kind=${dst_kind} dst_size=${dst_size} dst_small=${dst_is_small_struct} dst_ptrlike=${dst_elem_is_ptrlike}')
			}
			mut should_zero_large_store := is_undef_aggregate
			if !should_zero_large_store && (dst_is_large_struct
				|| (val_typ.kind in [.struct_t, .array_t] && val_size > 16 && !dst_elem_is_ptrlike))
				&& !src_has_storage {
				should_zero_large_store = true
			}

			// Load source first, then preserve it in a register that will not be clobbered
			// when loading the destination pointer (which may use x9 plus x11/x12 scratch).
			mut val_reg := if src_addr_override_id > 0 {
				g.get_operand_reg(src_addr_override_id, 8)
			} else {
				g.get_operand_reg(src_id, 8)
			}
			if val_reg == 9 || val_reg == 11 || val_reg == 12 {
				if val_reg != 8 {
					g.emit_mov_reg(8, val_reg)
				}
				val_reg = 8
			}
			ptr_reg := g.get_operand_reg(ptr_id, 9)

			if dst_is_large_struct {
				// Destination expects a large struct by value.
				// Large structs are represented as pointers in registers, so copy pointee bytes.
				if should_zero_large_store {
					g.zero_ptr_bytes(ptr_reg, dst_struct_size)
				} else {
					mut can_copy_from_src_ptr := false
					mut src_ptr_reg := if ptr_reg == 11 { 12 } else { 11 }
					mut src_is_unwrapped_wrapper := false
					if val_typ.kind == .struct_t && g.is_sumtype_wrapper_struct_type(val_val.typ)
						&& g.load_sumtype_data_ptr_to_reg(src_ptr_reg, src_id) {
						can_copy_from_src_ptr = true
						src_is_unwrapped_wrapper = true
					}
					if !can_copy_from_src_ptr {
						if src_off := g.stack_map[src_id] {
							if g.large_struct_stack_value_is_pointer(src_id)
								|| g.large_aggregate_stack_value_is_pointer(src_id) {
								g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
							} else if src_id in g.reg_map {
								if val_reg != src_ptr_reg {
									g.emit_mov_reg(src_ptr_reg, val_reg)
								}
							} else {
								g.emit_add_fp_imm(src_ptr_reg, src_off)
							}
							can_copy_from_src_ptr = true
						} else {
							src_ptr_reg = val_reg
							can_copy_from_src_ptr = true
						}
					}
					if can_copy_from_src_ptr {
						mut large_copy_size := dst_struct_size
						if !src_is_unwrapped_wrapper && val_typ.kind in [.struct_t, .array_t] {
							src_inline_size := g.type_size(val_val.typ)
							// Some MIR paths store a small aggregate into a pointer typed as a
							// larger aggregate (e.g. wrapper field updates through opaque GEPs).
							// Copy only initialized source bytes and clear the destination tail.
							if src_inline_size > 0 && src_inline_size < large_copy_size {
								large_copy_size = src_inline_size
							}
						}
						if large_copy_size <= 0 {
							large_copy_size = dst_struct_size
						}
						g.copy_ptr_to_ptr_bytes(src_ptr_reg, ptr_reg, large_copy_size)
						if large_copy_size < dst_struct_size {
							g.zero_ptr_range_bytes(ptr_reg, large_copy_size, dst_struct_size)
						}
					} else {
						g.zero_ptr_bytes(ptr_reg, dst_struct_size)
					}
				}
			} else if dst_is_small_struct {
				// Destination expects a small multi-field struct by value.
				num_fields := (dst_struct_size + 7) / 8
				mut src_points_to_struct := false
				if dst_struct_typ_id > 0 && val_typ.kind == .ptr_t && val_typ.elem_type > 0
					&& val_typ.elem_type < g.mod.type_store.types.len {
					// Only treat pointer sources as by-value struct bytes when the
					// pointee type exactly matches the destination struct type.
					// Size-only matches can copy unrelated payload structs into wrapper
					// structs (e.g. ast.Expr), corrupting tag/data words.
					if val_typ.elem_type == dst_struct_typ_id {
						if g.is_sumtype_wrapper_struct_type(dst_struct_typ_id)
							&& g.scalar_value_is_pointer_payload(src_id, 0) {
							src_points_to_struct = false
						} else {
							src_points_to_struct = true
						}
					}
				}
				mut src_copy_chunks := num_fields
				if !src_points_to_struct {
					src_size_for_copy := g.aggregate_source_size_bytes(src_id)
					if src_size_for_copy > 0 {
						src_chunks := (src_size_for_copy + 7) / 8
						if src_chunks > 0 && src_chunks < src_copy_chunks {
							src_copy_chunks = src_chunks
						}
					}
				}
				if src_copy_chunks < 1 {
					src_copy_chunks = 1
				}
				if trace_store {
					eprintln('ARM64 STORE_SMALL_COPY fn=${g.cur_func_name} src=${src_id} ptr=${ptr_id} src_chunks=${src_copy_chunks}/${num_fields} src_points_to_struct=${src_points_to_struct}')
				}
				if !src_has_storage && !src_points_to_struct {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_fields {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else {
					mut can_copy_from_src_ptr := false
					mut src_ptr_reg := if ptr_reg == 11 { 12 } else { 11 }
					if src_points_to_struct {
						if val_reg != src_ptr_reg {
							g.emit_mov_reg(src_ptr_reg, val_reg)
						}
						can_copy_from_src_ptr = true
					} else if src_off := g.stack_map[src_id] {
						g.emit_add_fp_imm(src_ptr_reg, src_off)
						can_copy_from_src_ptr = true
					}
					if can_copy_from_src_ptr {
						for i in 0 .. src_copy_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
						if src_copy_chunks < num_fields {
							g.emit_mov_reg(10, 31)
							for i in src_copy_chunks .. num_fields {
								g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
							}
						}
					} else if num_fields == 1 {
						// Single-slot struct values in registers can be stored directly.
						g.emit(asm_str(Reg(val_reg), Reg(ptr_reg)))
					} else if val_typ.kind in [.struct_t, .array_t] && src_id > 0
						&& src_id < g.mod.values.len {
						// Source is a struct/array value whose register holds a pointer
						// (e.g., from a .load of a struct without a stack slot).
						// Use the register as a source pointer for field-by-field copy.
						src_val_info := g.mod.values[src_id]
						if src_val_info.kind == .instruction
							&& g.mod.instrs[src_val_info.index].op == .load {
							if val_reg != src_ptr_reg {
								g.emit_mov_reg(src_ptr_reg, val_reg)
							}
							for i in 0 .. src_copy_chunks {
								g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
								g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
							}
							if src_copy_chunks < num_fields {
								g.emit_mov_reg(10, 31)
								for i in src_copy_chunks .. num_fields {
									g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
								}
							}
						} else {
							g.emit_mov_reg(10, 31)
							for i in 0 .. num_fields {
								g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
							}
						}
					} else {
						// Keep behavior deterministic when aggregate source bytes are unavailable.
						g.emit_mov_reg(10, 31)
						for i in 0 .. num_fields {
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
					}
				}
			} else if dst_struct_typ_id > 0 && val_typ.kind in [.struct_t, .array_t]
				&& val_size > 16 && !dst_elem_is_ptrlike {
				// Large struct source with non-pointer destination slot:
				// copy pointee bytes into destination memory.
				if should_zero_large_store {
					g.zero_ptr_bytes(ptr_reg, val_size)
				} else {
					mut can_copy_from_src_ptr := false
					mut src_ptr_reg := if ptr_reg == 11 { 12 } else { 11 }
					if src_off := g.stack_map[src_id] {
						if g.large_struct_stack_value_is_pointer(src_id)
							|| g.large_aggregate_stack_value_is_pointer(src_id) {
							g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
						} else if src_id in g.reg_map {
							if val_reg != src_ptr_reg {
								g.emit_mov_reg(src_ptr_reg, val_reg)
							}
						} else {
							g.emit_add_fp_imm(src_ptr_reg, src_off)
						}
						can_copy_from_src_ptr = true
					} else {
						src_ptr_reg = val_reg
						can_copy_from_src_ptr = true
					}
					if can_copy_from_src_ptr {
						g.copy_ptr_to_ptr_bytes(src_ptr_reg, ptr_reg, val_size)
					} else {
						g.zero_ptr_bytes(ptr_reg, val_size)
					}
				}
			} else if val_typ.kind in [.struct_t, .array_t] && val_size > 0 && val_size <= 16
				&& !dst_elem_is_ptrlike {
				// Small aggregate source stored through an opaque destination pointer.
				// Copy value-sized 8-byte chunks (not field count), otherwise packed
				// structs like `{u32,u8}` can over-copy and corrupt adjacent memory.
				num_chunks := (val_size + 7) / 8
				if !src_has_storage {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_chunks {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else if src_off := g.stack_map[src_id] {
					for i in 0 .. num_chunks {
						g.emit_ldr_reg_offset(10, 29, src_off + i * 8)
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else if num_chunks == 1 {
					g.emit(asm_str(Reg(val_reg), Reg(ptr_reg)))
				} else if val_typ.kind in [.struct_t, .array_t] && src_id > 0
					&& src_id < g.mod.values.len {
					src_val_info := g.mod.values[src_id]
					if src_val_info.kind == .instruction
						&& g.mod.instrs[src_val_info.index].op == .load {
						if val_reg != 11 {
							g.emit_mov_reg(11, val_reg)
						}
						for i in 0 .. num_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(11), u32(i)))
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
					} else {
						g.emit_mov_reg(10, 31)
						for i in 0 .. num_chunks {
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
					}
				} else {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_chunks {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				}
			} else {
				mut store_size := g.mem_access_size_bytes(val_val.typ, ptr_id)
				// Sumtype payload pointers can flow through i64-typed SSA values.
				// Do not narrow these stores based on imprecise pointer element widths.
				if store_size < 8 && g.scalar_value_is_pointer_payload(src_id, 0) {
					store_size = 8
				}
				match store_size {
					1 { g.emit(asm_str_b(Reg(val_reg), Reg(ptr_reg))) }
					2 { g.emit(asm_str_h(Reg(val_reg), Reg(ptr_reg))) }
					4 { g.emit(asm_str_w(Reg(val_reg), Reg(ptr_reg))) }
					else { g.emit(asm_str(Reg(val_reg), Reg(ptr_reg))) }
				}
			}
		}
		.load {
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }
			ptr_id := instr.operands[0]
			trace_load := g.env_trace_load.len > 0
				&& (g.env_trace_load == '*' || g.cur_func_name == g.env_trace_load)
			mut loaded_into_aggregate_slot := false
			mut force_spill_small_struct := false
			mut handled_sumtype_data_word_load := false
			mut ptr_is_null_const := false
			// ValueID 0 is the SSA null/invalid sentinel.
			if ptr_id <= 0 || ptr_id >= g.mod.values.len {
				g.emit_mov_imm64(dest_reg, 0)
			} else if data_word_id := g.sumtype_data_word_load_source(ptr_id, g.mod.values[val_id].typ) {
				if trace_load {
					eprintln('ARM64 LOAD fn=${g.cur_func_name} val=${val_id} ptr=${ptr_id} sumtype_data_word=${data_word_id}')
				}
				g.load_val_to_reg(dest_reg, data_word_id)
				handled_sumtype_data_word_load = true
			} else {
				ptr_is_null_const = g.is_effective_null_pointer_value(ptr_id)
				ptr_reg := g.get_operand_reg(ptr_id, 9)
				if trace_load {
					mut rkind := ssa.TypeKind.void_t
					mut rsize := 0
					mut rtyp := ssa.TypeID(0)
					mut pkind := mir.ValueKind.constant
					mut ptyp := ssa.TypeID(0)
					mut pop := 'na'
					mut pname := ''
					mut gtyp := ssa.TypeID(0)
					mut gkind := ssa.TypeKind.void_t
					mut gsize := 0
					mut gconst := false
					mut ginit_len := 0
					if val_id > 0 && val_id < g.mod.values.len {
						rtyp = g.mod.values[val_id].typ
						if rtyp > 0 && rtyp < g.mod.type_store.types.len {
							rkind = g.mod.type_store.types[rtyp].kind
							rsize = g.type_size(rtyp)
						}
					}
					if ptr_id > 0 && ptr_id < g.mod.values.len {
						ptr_val_dbg := g.mod.values[ptr_id]
						pkind = ptr_val_dbg.kind
						ptyp = ptr_val_dbg.typ
						pname = ptr_val_dbg.name
						if ptr_val_dbg.kind == .instruction {
							pop = '${g.selected_opcode(g.mod.instrs[ptr_val_dbg.index])}'
						} else if ptr_val_dbg.kind == .global {
							for gvar in g.mod.globals {
								if gvar.name == ptr_val_dbg.name {
									gtyp = gvar.typ
									gconst = gvar.is_constant
									ginit_len = gvar.initial_data.len
									if gtyp > 0 && gtyp < g.mod.type_store.types.len {
										gkind = g.mod.type_store.types[gtyp].kind
										gsize = g.type_size(gtyp)
									}
									break
								}
							}
						}
					}
					eprintln('ARM64 LOAD fn=${g.cur_func_name} val=${val_id} rtyp=${rtyp} ptr=${ptr_id} ptr_name=`${pname}` ptr_kind=${pkind} ptr_typ=${ptyp} ptr_op=${pop} ptr_reg=${ptr_reg} ptr_null=${ptr_is_null_const} rkind=${rkind} rsize=${rsize} gtyp=${gtyp}/${gkind} gsize=${gsize} gconst=${gconst} ginit=${ginit_len}')
				}
				mut load_src_ptr_reg := ptr_reg
				if ptr_id > 0 && ptr_id < g.mod.values.len {
					ptr_val := g.mod.values[ptr_id]
					if ptr_val.kind == .instruction {
						ptr_instr := g.mod.instrs[ptr_val.index]
						if g.selected_opcode(ptr_instr) == .alloca {
							slot_has_ptr := g.alloca_slot_stores_pointer_like_values(ptr_id,
								g.mod.values[val_id].typ)
							if trace_load {
								mut rkind := ssa.TypeKind.void_t
								mut rsize := 0
								if g.mod.values[val_id].typ > 0
									&& g.mod.values[val_id].typ < g.mod.type_store.types.len {
									rkind = g.mod.type_store.types[g.mod.values[val_id].typ].kind
									rsize = g.type_size(g.mod.values[val_id].typ)
								}
								eprintln('ARM64 LOAD fn=${g.cur_func_name} val=${val_id} ptr=${ptr_id} alloca_slot_ptr=${slot_has_ptr} rkind=${rkind} rsize=${rsize}')
							}
							if slot_has_ptr {
								load_src_ptr_reg = if ptr_reg == 11 { 12 } else { 11 }
								g.emit(asm_ldr(Reg(load_src_ptr_reg), Reg(ptr_reg)))
							}
						}
					}
				}
				result_typ_id := g.mod.values[val_id].typ
				if result_typ_id > 0 && result_typ_id < g.mod.type_store.types.len {
					result_typ := g.mod.type_store.types[result_typ_id]
					result_size := g.type_size(result_typ_id)
					if (result_typ.kind == .struct_t || result_typ.kind == .array_t)
						&& result_size > 8 && result_size <= 16 {
						if result_offset := g.stack_map[val_id] {
							if trace_load {
								eprintln('ARM64 LOAD_SMALL_AGG fn=${g.cur_func_name} val=${val_id} ptr=${ptr_id} result_off=${result_offset} size=${result_size} mode=copy')
							}
							if ptr_is_null_const {
								g.zero_fp_bytes(result_offset, result_size)
							} else {
								g.copy_ptr_to_fp_bytes(load_src_ptr_reg, result_offset,
									result_size)
							}
							loaded_into_aggregate_slot = true
						} else if dest_reg != load_src_ptr_reg {
							if trace_load {
								eprintln('ARM64 LOAD_SMALL_AGG fn=${g.cur_func_name} val=${val_id} ptr=${ptr_id} size=${result_size} mode=address')
							}
							// Fallback when no aggregate slot is available.
							if ptr_is_null_const {
								g.emit_mov_reg(dest_reg, 31)
							} else {
								g.emit_mov_reg(dest_reg, load_src_ptr_reg)
							}
						}
					} else if (result_typ.kind == .struct_t || result_typ.kind == .array_t)
						&& result_size > 16 {
						if result_offset := g.stack_map[val_id] {
							if g.large_struct_stack_value_is_pointer(val_id) {
								// Pointer-backed large struct value: keep source address in the
								// value slot instead of copying full bytes into the frame.
								if ptr_is_null_const {
									g.emit_mov_reg(dest_reg, 31)
								} else if dest_reg != load_src_ptr_reg {
									g.emit_mov_reg(dest_reg, load_src_ptr_reg)
								}
								g.store_reg_to_val(dest_reg, val_id)
							} else {
								// Materialize large load results by value in their stack slot.
								if ptr_is_null_const {
									g.zero_fp_bytes(result_offset, result_size)
								} else {
									g.copy_ptr_to_fp_bytes(load_src_ptr_reg, result_offset,
										result_size)
								}
								if val_id in g.reg_map {
									g.emit_add_fp_imm(dest_reg, result_offset)
								}
							}
							loaded_into_aggregate_slot = true
						} else if dest_reg != load_src_ptr_reg {
							// Fallback when no spill slot is available: keep address form.
							if ptr_is_null_const {
								g.emit_mov_reg(dest_reg, 31)
							} else {
								g.emit_mov_reg(dest_reg, load_src_ptr_reg)
							}
						}
					} else {
						if ptr_is_null_const {
							g.emit_mov_reg(dest_reg, 31)
						} else {
							mut load_size := g.mem_access_size_bytes(result_typ_id, ptr_id)
							// Sumtype payload pointers can be represented as i64 scalar words.
							// Preserve pointer-width loads for these values even when the
							// intermediate pointer type appears byte-sized.
							if load_size < 8 && g.scalar_value_is_pointer_payload(val_id, 0) {
								load_size = 8
							}
							match load_size {
								1 { g.emit(asm_ldr_b(Reg(dest_reg), Reg(ptr_reg))) }
								2 { g.emit(asm_ldr_h(Reg(dest_reg), Reg(ptr_reg))) }
								4 { g.emit(asm_ldr_w(Reg(dest_reg), Reg(ptr_reg))) }
								else { g.emit(asm_ldr(Reg(dest_reg), Reg(ptr_reg))) }
							}
						}
						if result_typ.kind == .struct_t && result_size <= 8 && val_id in g.stack_map {
							force_spill_small_struct = true
						}
					}
				} else {
					if ptr_is_null_const {
						g.emit_mov_reg(dest_reg, 31)
					} else {
						g.emit(asm_ldr(Reg(dest_reg), Reg(ptr_reg)))
					}
				}
			}

			if !loaded_into_aggregate_slot && (handled_sumtype_data_word_load
				|| val_id !in g.reg_map || force_spill_small_struct) {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.alloca {
			if val_id in g.sumtype_data_heap_allocas {
				ptr_type := g.mod.type_store.types[g.mod.values[val_id].typ]
				mut alloc_size := g.type_size(ptr_type.elem_type)
				if alloc_size <= 0 {
					alloc_size = 8
				}
				// alloca can request multiple elements via operand[0].
				if instr.operands.len > 0 {
					count_id := instr.operands[0]
					if count_id > 0 && count_id < g.mod.values.len {
						count_val := g.mod.values[count_id]
						count := count_val.name.int()
						if count > 1 {
							alloc_size *= count
						}
					}
				}
				g.emit_mov_imm(0, 1)
				g.emit_mov_imm(1, u64(alloc_size))
				sym_idx := g.macho.add_undefined('_calloc')
				g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
					true)
				g.emit(asm_bl_reloc())
				g.store_reg_to_val(0, val_id)
			} else {
				data_off := g.alloca_offsets[val_id]
				g.emit_add_fp_imm(8, data_off)
				g.store_reg_to_val(8, val_id)
			}
		}
		.heap_alloc {
			// Heap-allocate memory for a struct type.
			// Result type is ptr(T), compute sizeof(T) and call calloc(1, size).
			mut alloc_size := 8
			ha_val := g.mod.values[val_id]
			if ha_val.typ > 0 && ha_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ha_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0 {
					alloc_size = g.type_size(ptr_typ.elem_type)
					if alloc_size <= 0 {
						alloc_size = 8
					}
				}
			}
			// calloc(1, size) → x0 = 1, x1 = size
			g.emit_mov_imm(0, 1)
			g.emit_mov_imm(1, u64(alloc_size))
			sym_idx := g.macho.add_undefined('_calloc')
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
			g.emit(asm_bl_reloc())
			// calloc returns heap pointer in x0
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			// GEP: Base + scaled index (or struct field offset for aggregate pointers)
			idx_id := instr.operands[1]
			base_typ_id := g.mod.values[instr.operands[0]].typ
			mut pointee_typ_id := ssa.TypeID(0)
			mut base_elem_typ_id := ssa.TypeID(0)
			mut gep_done := false
			if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
				base_typ := g.mod.type_store.types[base_typ_id]
				if base_typ.kind == .ptr_t {
					pointee_typ_id = base_typ.elem_type
					base_elem_typ_id = base_typ.elem_type
				}
			}
			mut base_reg := g.get_operand_reg(instr.operands[0], 8)

			// Struct field GEP with constant index: use real field byte offsets.
			// Distinguish from array-style GEP: if the GEP result type equals the
			// base pointer type, this is array indexing (ptr(struct)[i] -> ptr(struct)),
			// not struct field access (ptr(struct), field_idx -> ptr(field_type)).
			mut is_array_gep := false
			base_val_typ := g.mod.values[instr.operands[0]].typ
			if instr.typ == base_val_typ {
				is_array_gep = true
			}
			// Some lowered flows keep pointer payloads in alloca-backed scalar slots.
			// For GEP over such values, first load the payload pointer from the slot.
			mut idx_is_zero_const := false
			if idx_id > 0 && idx_id < g.mod.values.len {
				idx_val_dbg := g.mod.values[idx_id]
				if idx_val_dbg.kind == .constant && idx_val_dbg.name == '0' {
					idx_is_zero_const = true
				}
			}
			base_val := g.mod.values[instr.operands[0]]
			mut gep_base_slot_has_ptr := false
			if base_val.kind == .instruction {
				base_instr := g.mod.instrs[base_val.index]
				if g.selected_opcode(base_instr) == .alloca {
					mut target_elem_typ_id := base_elem_typ_id
					if instr.typ > 0 && instr.typ < g.mod.type_store.types.len {
						res_typ := g.mod.type_store.types[instr.typ]
						if res_typ.kind == .ptr_t && res_typ.elem_type > 0
							&& res_typ.elem_type < g.mod.type_store.types.len {
							target_elem_typ_id = res_typ.elem_type
						}
					}
					gep_base_slot_has_ptr = target_elem_typ_id > 0
						&& g.alloca_slot_stores_pointer_like_values(instr.operands[0], target_elem_typ_id)
					if gep_base_slot_has_ptr && !(instr.typ == base_val_typ && idx_is_zero_const) {
						g.emit(asm_ldr(Reg(base_reg), Reg(base_reg)))
					}
				}
			}
			if !is_array_gep && idx_id > 0 && idx_id < g.mod.values.len && pointee_typ_id > 0
				&& pointee_typ_id < g.mod.type_store.types.len {
				idx_val := g.mod.values[idx_id]
				pointee_typ := g.mod.type_store.types[pointee_typ_id]
				if idx_val.kind == .constant && pointee_typ.kind == .struct_t {
					field_idx := idx_val.name.int()
					field_off := g.struct_field_offset_bytes(pointee_typ_id, field_idx)
					if field_off <= 0xFFF {
						g.emit(asm_add_imm(Reg(8), Reg(base_reg), u32(field_off)))
					} else {
						g.emit_mov_imm64(9, i64(field_off))
						g.emit(asm_add_reg(Reg(8), Reg(base_reg), Reg(9)))
					}
					g.store_gep_result_from_addr(8, val_id)
					gep_done = true
				}
			}
			if !gep_done {
				// Array/pointer-style GEP: scale by element size.
				mut scale := 8
				mut base_ptr_reg := base_reg
				if pointee_typ_id > 0 && pointee_typ_id < g.mod.type_store.types.len {
					pointee_typ := g.mod.type_store.types[pointee_typ_id]
					elem_size := if pointee_typ.kind == .array_t {
						g.type_size(pointee_typ.elem_type)
					} else {
						g.type_size(pointee_typ_id)
					}
					if elem_size > 0 {
						scale = elem_size
					}
				}
				// Ensure index load doesn't clobber base if base is 8
				idx_scratch := if base_ptr_reg == 8 { 9 } else { 8 }
				idx_reg := g.get_operand_reg(idx_id, idx_scratch)
				// Sign-extend index from 32-bit to 64-bit. GEP indices may have
				// been stored as 32-bit values (e.g. from map lookups or
				// extractvalue of int fields) with undefined upper 32 bits.
				// The ARM64 ABI does not guarantee upper bits are zeroed for
				// sub-64-bit values, so always extend before scaling.
				g.emit(asm_sxtw(Reg(idx_reg), Reg(idx_reg)))
				if scale == 8 {
					g.emit(asm_add_reg_lsl3(Reg(8), Reg(base_ptr_reg), Reg(idx_reg)))
				} else if scale == 1 {
					g.emit(asm_add_reg(Reg(8), Reg(base_ptr_reg), Reg(idx_reg)))
				} else {
					mut scale_reg := 10
					if scale_reg == base_ptr_reg || scale_reg == idx_reg {
						scale_reg = 11
						if scale_reg == base_ptr_reg || scale_reg == idx_reg {
							scale_reg = 12
						}
					}
					g.emit_mov_imm64(scale_reg, scale)
					g.emit(asm_mul(Reg(scale_reg), Reg(idx_reg), Reg(scale_reg)))
					g.emit(asm_add_reg(Reg(8), Reg(base_ptr_reg), Reg(scale_reg)))
				}
				g.store_gep_result_from_addr(8, val_id)
			}
		}
		.call {
			fn_val := g.mod.values[instr.operands[0]]
			fn_name := fn_val.name
			trace_call := g.env_trace_call.len > 0
				&& (g.env_trace_call == '*' || g.cur_func_name == g.env_trace_call)
			if trace_call {
				eprintln('ARM64 CALL fn=${g.cur_func_name} val=${val_id} callee_id=${instr.operands[0]} callee=`${fn_name}` args=${instr.operands.len - 1}')
			}
			// Skip calls with empty function names (shouldn't happen, but safety check)
			if fn_name != '' {
				// On ARM64 macOS (Apple Silicon), variadic arguments must be
				// passed on the stack, not in registers.
				is_variadic := fn_name in ['sprintf', 'printf', 'snprintf', 'fprintf', 'sscanf']
				num_fixed_args := if fn_name == 'sprintf' {
					2 // buffer, format
				} else if fn_name == 'printf' {
					1 // format
				} else if fn_name in ['snprintf', 'fprintf'] {
					3 // buffer/file, size, format
				} else if fn_name == 'sscanf' {
					2 // string, format
				} else {
					8 // default: all in registers
				}

				num_args := instr.operands.len - 1

				// Check if return type is a large struct (> 16 bytes) requiring indirect return
				result_typ := g.mod.type_store.types[g.mod.values[val_id].typ]
				result_size := g.type_size(g.mod.values[val_id].typ)
				is_indirect_return := result_typ.kind == .struct_t && result_size > 16
				// For indirect struct returns, set x8 to point to result storage BEFORE the call
				if is_indirect_return {
					result_offset := g.stack_map[val_id]
					g.emit_add_fp_imm(8, result_offset)
				}

				if is_variadic && num_args > num_fixed_args {
					// Variadic call: push variadic args to stack, fixed args to registers
					num_variadic := num_args - num_fixed_args

					// Allocate stack space for variadic args (8 bytes each, 16-byte aligned)
					stack_space := ((num_variadic * 8) + 15) & ~0xF
					if stack_space > 0 {
						g.emit_sub_sp(stack_space)
					}

					// Store variadic arguments to stack (in order)
					for i := 0; i < num_variadic; i++ {
						arg_idx := num_fixed_args + 1 + i // +1 because operands[0] is the function
						// Anonymous variadic args do not have a declared parameter slot.
						// Pass their promoted value representation directly instead of
						// reusing fixed-arg lowering, which can incorrectly turn a loaded
						// scalar into the address of its spill slot.
						g.load_val_to_reg(9, instr.operands[arg_idx]) // Use x9 to avoid clobbering x8
						// STR x9, [sp, #offset]
						offset := i * 8
						imm12 := u32(offset / 8)
						g.emit(asm_str_imm(Reg(9), sp, imm12))
					}

					// Load fixed arguments to registers (in reverse order to avoid clobbering)
					for i := num_fixed_args; i >= 1; i-- {
						g.load_call_arg_to_reg(i - 1, instr.operands[i], i - 1, instr)
					}

					// Call function
					sym_idx := g.macho.add_undefined('_' + fn_name)
					g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
						true)
					g.emit(asm_bl_reloc())

					// Restore stack
					if stack_space > 0 {
						if stack_space <= 0xFFF {
							g.emit(asm_add_imm(sp, sp, u32(stack_space)))
						} else {
							g.emit_mov_imm(10, u64(stack_space))
							g.emit(asm_add_sp_reg(Reg(10)))
						}
					}
				} else {
					// Non-variadic call:
					// ARM64 ABI: integer args in x0-x7, float args in d0-d7
					// Integer and float registers are allocated independently.

					// Classify each argument as float or integer
					mut is_float_arg := []bool{len: num_args}
					mut arg_int_reg := []int{len: num_args, init: -1}
					mut arg_float_reg := []int{len: num_args, init: -1}
					mut arg_int_cnt := []int{len: num_args}
					mut int_reg_idx := 0
					mut float_reg_idx := 0

					for a in 0 .. num_args {
						arg_val := g.mod.values[instr.operands[a + 1]]
						mut is_float := false
						if arg_val.typ > 0 && int(arg_val.typ) < g.mod.type_store.types.len {
							arg_typ := g.mod.type_store.types[arg_val.typ]
							if arg_typ.kind == .float_t {
								is_float = true
							}
						}
						if is_float {
							is_float_arg[a] = true
							arg_float_reg[a] = float_reg_idx
							float_reg_idx++
						} else {
							cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
							arg_int_reg[a] = int_reg_idx
							arg_int_cnt[a] = cnt
							int_reg_idx += cnt
						}
					}

					// Handle stack-spilled integer args (>8 int regs)
					num_int_stack := if int_reg_idx > 8 { int_reg_idx - 8 } else { 0 }
					num_float_stack := if float_reg_idx > 8 { float_reg_idx - 8 } else { 0 }
					total_stack_slots := num_int_stack + num_float_stack
					stack_space := ((total_stack_slots * 8) + 15) & ~0xF
					if stack_space > 0 {
						g.emit_sub_sp(stack_space)
						mut stack_idx := 0
						for a in 0 .. num_args {
							if is_float_arg[a] {
								if arg_float_reg[a] >= 8 {
									g.load_val_to_reg(9, instr.operands[a + 1])
									g.emit(asm_str_imm(Reg(9), sp, u32(stack_idx)))
									stack_idx++
								}
							} else {
								cnt := arg_int_cnt[a]
								start_reg := arg_int_reg[a]
								expected_struct_typ := g.call_param_type(instr, a) or {
									ssa.TypeID(0)
								}
								for ri in 0 .. cnt {
									if start_reg + ri < 8 {
										continue
									}
									if cnt > 1 {
										g.load_struct_arg_word_to_reg(9, instr.operands[a + 1],
											ri, expected_struct_typ, instr.operands[0])
									} else {
										g.load_call_arg_to_reg(9, instr.operands[a + 1],
											a, instr)
									}
									g.emit(asm_str_imm(Reg(9), sp, u32(stack_idx)))
									stack_idx++
								}
							}
						}
					}

					// Load integer args to x-registers (reverse order)
					for a := num_args - 1; a >= 0; a-- {
						if is_float_arg[a] {
							continue
						}
						reg := arg_int_reg[a]
						if reg >= 8 && arg_int_cnt[a] == 1 {
							continue // stack arg
						}
						if arg_int_cnt[a] > 1 {
							expected_struct_typ := g.call_param_type(instr, a) or { ssa.TypeID(0) }
							for ri := arg_int_cnt[a] - 1; ri >= 0; ri-- {
								target_reg := reg + ri
								if target_reg >= 8 {
									continue
								}
								g.load_struct_arg_word_to_reg(target_reg, instr.operands[a + 1],
									ri, expected_struct_typ, instr.operands[0])
							}
						} else {
							g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
						}
					}

					// Load float args to d-registers
					for a in 0 .. num_args {
						if !is_float_arg[a] {
							continue
						}
						freg := arg_float_reg[a]
						if freg >= 8 {
							continue // stack float arg
						}
						// Load value bits to x9, then fmov to dN
						g.load_val_to_reg(9, instr.operands[a + 1])
						g.emit(asm_fmov_d_x(freg, Reg(9)))
					}

					sym_idx := g.macho.add_undefined('_' + fn_name)
					g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
						true)
					g.emit(asm_bl_reloc())

					if stack_space > 0 {
						if stack_space <= 0xFFF {
							g.emit(asm_add_imm(sp, sp, u32(stack_space)))
						} else {
							g.emit_mov_imm(10, u64(stack_space))
							g.emit(asm_add_sp_reg(Reg(10)))
						}
					}
				}

				if result_typ.kind != .void_t {
					// Check if this is a float return: result comes in d0 instead of x0
					mut is_float_return := result_typ.kind == .float_t
					if !is_float_return {
						// Also check the callee's registered return type
						callee_fn_val := g.mod.values[instr.operands[0]]
						if callee_fn_val.kind == .func_ref {
							if f := g.get_function_by_name(callee_fn_val.name) {
								callee_ret := g.mod.type_store.types[f.typ]
								if callee_ret.kind == .float_t {
									is_float_return = true
								}
							}
						}
					}
					if is_float_return {
						// Float return: move d0 to x0 for integer storage
						g.emit(asm_fmov_x_d(Reg(0), 0))
						g.store_reg_to_val(0, val_id)
					} else {
						// Also check callee's registered return type: when the SSA value type
						// isn't struct_t (e.g. i64 fallback), use the callee's return type.
						mut call_ret_is_multi_reg := result_typ.kind == .struct_t && result_size > 8
						mut actual_call_ret_size := result_size
						if !call_ret_is_multi_reg && !is_indirect_return
							&& result_typ.kind == .int_t {
							callee_val2 := g.mod.values[instr.operands[0]]
							if callee_val2.kind == .func_ref {
								if f := g.get_function_by_name(callee_val2.name) {
									callee_ret_typ := g.mod.type_store.types[f.typ]
									callee_ret_size := g.type_size(f.typ)
									if callee_ret_typ.kind == .struct_t && callee_ret_size > 8
										&& callee_ret_size <= 16 {
										call_ret_is_multi_reg = true
										actual_call_ret_size = callee_ret_size
									}
								}
							}
						}
						if call_ret_is_multi_reg {
							if !is_indirect_return {
								if val_id in g.stack_map {
									result_offset := g.stack_map[val_id]
									num_chunks := (actual_call_ret_size + 7) / 8
									// Use ABI return size, not SSA value type size. Some MIR
									// paths merge small-struct call results to `int`, but the
									// call still returns x0/x1 that must both be materialized.
									for i in 0 .. num_chunks {
										if i < 8 {
											g.emit_str_reg_offset(i, 29, result_offset + i * 8)
										}
									}
								} else {
									g.store_reg_to_val(0, val_id)
								}
							}
						} else {
							g.canonicalize_narrow_int_result(0, g.mod.values[val_id].typ)
							g.store_reg_to_val(0, val_id)
						}
					}
				}
			}
		}
		.call_indirect {
			// Indirect call through function pointer
			// operands[0] is the function pointer, rest are arguments
			num_args := instr.operands.len - 1

			// Compute register mapping for multi-register struct args.
			mut ci_arg_reg_start := []int{len: num_args}
			mut ci_arg_reg_cnt := []int{len: num_args}
			mut ci_total_reg_slots := 0
			for a in 0 .. num_args {
				ci_arg_reg_start[a] = ci_total_reg_slots
				cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
				ci_arg_reg_cnt[a] = cnt
				ci_total_reg_slots += cnt
			}
			num_stack_slots := if ci_total_reg_slots > 8 {
				ci_total_reg_slots - 8
			} else {
				0
			}
			stack_space := ((num_stack_slots * 8) + 15) & ~0xF
			if stack_space > 0 {
				g.emit_sub_sp(stack_space)
				mut stack_idx := 0
				for a in 0 .. num_args {
					cnt := ci_arg_reg_cnt[a]
					start_reg := ci_arg_reg_start[a]
					expected_struct_typ := g.call_param_type(instr, a) or { ssa.TypeID(0) }
					for ri in 0 .. cnt {
						if start_reg + ri < 8 {
							continue
						}
						if cnt > 1 {
							g.load_struct_arg_word_to_reg(9, instr.operands[a + 1], ri,
								expected_struct_typ, instr.operands[0])
						} else {
							g.load_call_arg_to_reg(9, instr.operands[a + 1], a, instr)
						}
						imm12 := u32(stack_idx)
						g.emit(asm_str_imm(Reg(9), sp, imm12))
						stack_idx++
					}
				}
			}

			for a := num_args - 1; a >= 0; a-- {
				reg := ci_arg_reg_start[a]
				if reg >= 8 && ci_arg_reg_cnt[a] == 1 {
					continue
				}
				if ci_arg_reg_cnt[a] > 1 {
					expected_struct_typ := g.call_param_type(instr, a) or { ssa.TypeID(0) }
					for ri := ci_arg_reg_cnt[a] - 1; ri >= 0; ri-- {
						target_reg := reg + ri
						if target_reg >= 8 {
							continue
						}
						g.load_struct_arg_word_to_reg(target_reg, instr.operands[a + 1],
							ri, expected_struct_typ, instr.operands[0])
					}
				} else {
					g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
				}
			}

			// Load function pointer to x9 (scratch register).
			// Do not use generic value loading here: it can materialize an address
			// for large struct-like values instead of the actual callable pointer.
			g.load_fnptr_to_reg(9, instr.operands[0])

			// BLR x9 - branch and link to register
			g.emit(asm_blr(Reg(9)))

			if stack_space > 0 {
				if stack_space <= 0xFFF {
					g.emit(asm_add_imm(sp, sp, u32(stack_space)))
				} else {
					g.emit_mov_imm(10, u64(stack_space))
					g.emit(asm_add_sp_reg(Reg(10)))
				}
			}

			ci_result_typ_id := g.mod.values[val_id].typ
			ci_result_typ := g.mod.type_store.types[ci_result_typ_id]
			if ci_result_typ.kind != .void_t {
				ci_result_size := g.type_size(ci_result_typ_id)
				if ci_result_typ.kind == .struct_t && ci_result_size > 8 {
					// Small struct return: store x0, x1 into stack slot
					ci_result_offset := g.stack_map[val_id]
					num_chunks := (ci_result_size + 7) / 8
					for i in 0 .. num_chunks {
						if i < 8 {
							g.emit_str_reg_offset(i, 29, ci_result_offset + i * 8)
						}
					}
				} else {
					g.canonicalize_narrow_int_result(0, ci_result_typ_id)
					g.store_reg_to_val(0, val_id)
				}
			}
		}
		.call_sret {
			// Call with struct return lowered by ABI pass.
			// operands: [fn, arg1, arg2, ...], destination is val_id's stack slot.
			num_args := instr.operands.len - 1
			trace_call := g.env_trace_call.len > 0
				&& (g.env_trace_call == '*' || g.cur_func_name == g.env_trace_call)
			if trace_call {
				callee_id := instr.operands[0]
				mut callee_name := ''
				if callee_id > 0 && callee_id < g.mod.values.len {
					callee_name = g.mod.values[callee_id].name
				}
				eprintln('ARM64 CALL_SRET fn=${g.cur_func_name} val=${val_id} callee_id=${callee_id} callee=`${callee_name}` args=${num_args}')
			}

			// Set x8 to destination address for indirect return.
			result_offset := g.stack_map[val_id]
			g.emit_add_fp_imm(8, result_offset)

			// Compute register mapping for multi-register struct args.
			mut sr_arg_reg_start := []int{len: num_args}
			mut sr_arg_reg_cnt := []int{len: num_args}
			mut sr_total_reg_slots := 0
			for a in 0 .. num_args {
				sr_arg_reg_start[a] = sr_total_reg_slots
				cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
				sr_arg_reg_cnt[a] = cnt
				sr_total_reg_slots += cnt
			}
			sr_num_stack_slots := if sr_total_reg_slots > 8 {
				sr_total_reg_slots - 8
			} else {
				0
			}
			stack_space := ((sr_num_stack_slots * 8) + 15) & ~0xF
			if stack_space > 0 {
				g.emit_sub_sp(stack_space)
				mut stack_idx := 0
				for a in 0 .. num_args {
					cnt := sr_arg_reg_cnt[a]
					start_reg := sr_arg_reg_start[a]
					expected_struct_typ := g.call_param_type(instr, a) or { ssa.TypeID(0) }
					for ri in 0 .. cnt {
						if start_reg + ri < 8 {
							continue
						}
						if cnt > 1 {
							g.load_struct_arg_word_to_reg(9, instr.operands[a + 1], ri,
								expected_struct_typ, instr.operands[0])
						} else {
							g.load_call_arg_to_reg(9, instr.operands[a + 1], a, instr)
						}
						imm12 := u32(stack_idx)
						g.emit(asm_str_imm(Reg(9), sp, imm12))
						stack_idx++
					}
				}
			}

			for a := num_args - 1; a >= 0; a-- {
				reg := sr_arg_reg_start[a]
				if reg >= 8 && sr_arg_reg_cnt[a] == 1 {
					continue
				}
				if sr_arg_reg_cnt[a] > 1 {
					expected_struct_typ := g.call_param_type(instr, a) or { ssa.TypeID(0) }
					for ri := sr_arg_reg_cnt[a] - 1; ri >= 0; ri-- {
						target_reg := reg + ri
						if target_reg >= 8 {
							continue
						}
						g.load_struct_arg_word_to_reg(target_reg, instr.operands[a + 1],
							ri, expected_struct_typ, instr.operands[0])
					}
				} else {
					g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
				}
			}

			fn_val := g.mod.values[instr.operands[0]]
			if fn_val.name != '' && fn_val.kind in [.unknown, .func_ref] {
				// Direct call by symbol.
				sym_idx := g.macho.add_undefined('_' + fn_val.name)
				g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
					true)
				g.emit(asm_bl_reloc())
			} else {
				// Indirect call through function pointer value.
				g.load_fnptr_to_reg(9, instr.operands[0])
				g.emit(asm_blr(Reg(9)))
			}

			if stack_space > 0 {
				if stack_space <= 0xFFF {
					g.emit(asm_add_imm(sp, sp, u32(stack_space)))
				} else {
					g.emit_mov_imm(10, u64(stack_space))
					g.emit(asm_add_sp_reg(Reg(10)))
				}
			}
		}
		.ret {
			if instr.operands.len > 0 {
				mut ret_val_id := instr.operands[0]
				mut ret_val := g.mod.values[ret_val_id]
				mut ret_typ := g.mod.type_store.types[ret_val.typ]
				trace_ret := g.env_trace_ret.len > 0
					&& (g.env_trace_ret == '*' || g.cur_func_name == g.env_trace_ret)

				// Get the function's declared return type
				fn_ret_type := g.cur_func_ret_type
				fn_ret_typ := g.mod.type_store.types[fn_ret_type]
				fn_ret_size := g.type_size(fn_ret_type)
				mut ret_val_size := g.type_size(ret_val.typ)
				if trace_ret {
					eprintln('ARM64 RET fn=${g.cur_func_name} ret_val=${ret_val_id} rtyp=${ret_val.typ}/${ret_typ.kind} rsz=${ret_val_size} fn_typ=${fn_ret_type}/${fn_ret_typ.kind} fn_sz=${fn_ret_size} roff=${g.stack_map[ret_val_id]}')
				}
				// Sumtype wrapper returns must produce `{_tag, _data}`.
				// When the lowered return value is a payload/pointer/etc., recover
				// the originating wrapper from the unwrapped value chain.
				if fn_ret_typ.kind == .struct_t && g.is_sumtype_wrapper_struct_type(fn_ret_type)
					&& fn_ret_size > 0 && fn_ret_size <= 16 && ret_val.typ != fn_ret_type {
					if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(ret_val_id,
						fn_ret_type, 0)
					{
						ret_val_id = wrapper_id
						ret_val = g.mod.values[ret_val_id]
						ret_typ = g.mod.type_store.types[ret_val.typ]
						ret_val_size = g.type_size(ret_val.typ)
					}
				}
				// Some lowered return paths re-wrap an already-optional `types.Type`
				// value as `Type(OptionType{ base_type: <Type> })`. For `return inner()`
				// this turns `none` into a fake `some`. Recover the original wrapper.
				if fn_ret_typ.kind == .struct_t && g.is_sumtype_wrapper_struct_type(fn_ret_type) {
					if forwarded_wrapper_id := g.forwarded_optiontype_wrapper_return_source(ret_val_id,
						fn_ret_type)
					{
						if trace_ret {
							eprintln('ARM64 RET fn=${g.cur_func_name} rewrite=forward_option_wrapper from=${ret_val_id} to=${forwarded_wrapper_id}')
						}
						ret_val_id = forwarded_wrapper_id
						ret_val = g.mod.values[ret_val_id]
						ret_typ = g.mod.type_store.types[ret_val.typ]
						ret_val_size = g.type_size(ret_val.typ)
					}
				}

				// Check if we're returning a pointer but the function expects a struct
				// This happens when returning local struct variables (expr_init returns pointers)
				mut is_indirect_struct_return := false
				if ret_typ.kind == .ptr_t && fn_ret_typ.kind == .struct_t {
					elem_type := ret_typ.elem_type
					if elem_type == fn_ret_type {
						is_indirect_struct_return = true
					}
				}

				// For large struct returns (> 16 bytes), use indirect return via x8
				// The caller provides the destination address in x8
				if fn_ret_typ.kind == .struct_t && fn_ret_size > 16 {
					// Restore x8 from the saved location (fp-relative)
					if g.x8_save_offset != 0 {
						g.emit_ldr_reg_offset(8, 29, g.x8_save_offset)
					}

					// Check if returning a zero/none value (e.g., `return 0` from `return none`).
					// In this case, zero-fill the return area instead of trying to copy
					// from address 0 (which would be a null pointer dereference).
					is_zero_const := ret_val.kind == .constant && ret_val.name == '0'
					if is_zero_const {
						num_fields := (fn_ret_size + 7) / 8
						for i in 0 .. num_fields {
							// STR xzr, [x8, #i*8]
							g.emit(asm_str_imm(Reg(31), Reg(8), u32(i)))
						}
					} else {
						// string_literal values need to be materialized on the stack
						// before we can copy them to the return pointer.
						if ret_val.kind == .string_literal {
							g.load_val_to_reg(9, ret_val_id)
						}

						// Get the source address of the struct
						if is_indirect_struct_return {
							// Return value is a pointer to struct - use it as source
							g.load_val_to_reg(9, ret_val_id)
						} else if ret_offset := g.stack_map[ret_val_id] {
							if g.large_struct_stack_value_is_pointer(ret_val_id) {
								// Some large-struct temporaries are represented as pointers in stack slots.
								g.emit_ldr_reg_offset(9, 29, ret_offset)
							} else {
								// Struct is materialized by value on stack.
								g.emit_add_fp_imm(9, ret_offset)
							}
						} else {
							// Fallback
							g.load_val_to_reg(9, ret_val_id)
						}
						// Copy struct from [x9] to [x8] (x8 was restored from saved location)
						num_fields := (fn_ret_size + 7) / 8
						for i in 0 .. num_fields {
							// LDR x10, [x9, #i*8]
							g.emit(asm_ldr_imm(Reg(10), Reg(9), u32(i)))
							// STR x10, [x8, #i*8]
							g.emit(asm_str_imm(Reg(10), Reg(8), u32(i)))
						}
					}
				} else if (ret_typ.kind == .struct_t && g.type_size(ret_val.typ) > 8)
					|| is_indirect_struct_return
					|| (fn_ret_typ.kind == .struct_t && fn_ret_size > 8 && fn_ret_size <= 16) {
					// Small struct (≤ 16 bytes) - return in registers x0, x1
					// Use type size (not field count) to determine multi-register returns,
					// since a struct with 1 nested struct field can still span 2 registers.
					// Also handle the case where the SSA value's type doesn't match the
					// function's return type (e.g., after PHI node type merging).
					use_declared_ret_layout := fn_ret_typ.kind == .struct_t
						&& ret_typ.kind == .struct_t && ret_val_size != fn_ret_size
					actual_struct_typ_id := if is_indirect_struct_return
						|| ret_typ.kind != .struct_t || use_declared_ret_layout {
						fn_ret_type
					} else {
						ret_val.typ
					}
					actual_struct_size := g.type_size(actual_struct_typ_id)
					num_chunks := (actual_struct_size + 7) / 8
					mut source_chunks := num_chunks
					if !is_indirect_struct_return {
						mut source_size := 0
						if ret_typ.kind in [.struct_t, .array_t] {
							source_size = g.type_size(ret_val.typ)
						} else {
							// Some return values are scalar-typed in SSA while still carrying
							// multi-word struct bytes in their stack slot (e.g. phi-merged
							// small structs). Preserve all carried chunks in x0/x1 returns.
							source_size = g.aggregate_source_size_bytes(ret_val_id)
							if source_size <= 0 {
								source_size = g.type_size(ret_val.typ)
							}
						}
						if source_size > 0 {
							source_chunks = (source_size + 7) / 8
							if source_chunks < 1 {
								source_chunks = 1
							} else if source_chunks > num_chunks {
								source_chunks = num_chunks
							}
						}
					}
					if trace_ret {
						eprintln('ARM64 RET_SMALL_STRUCT fn=${g.cur_func_name} ret_val=${ret_val_id} actual_typ=${actual_struct_typ_id} chunks=${source_chunks}/${num_chunks} indirect=${is_indirect_struct_return}')
					}
					is_zero_struct_const := ret_val.kind == .constant && ret_val.name == '0'
					mut unwrapped_small_sumtype_ret := false
					if !is_indirect_struct_return && use_declared_ret_layout
						&& actual_struct_size > 0 && actual_struct_size <= 8
						&& g.is_sumtype_wrapper_struct_type(ret_val.typ) {
						if g.load_sumtype_payload_word_to_reg(0, ret_val_id) {
							unwrapped_small_sumtype_ret = true
						}
					}

					// Ensure string literals are materialized on the stack
					// before we try to load their fields into return registers.
					if ret_val.kind == .string_literal {
						g.load_val_to_reg(9, ret_val_id)
					}

					if is_zero_struct_const {
						// `return 0`/`return none` for multi-register struct returns must
						// clear all return registers, not just x0.
						for i in 0 .. num_chunks {
							if i < 8 {
								g.emit_mov_reg(i, 31)
							}
						}
					} else if unwrapped_small_sumtype_ret {
						// x0 already materialized from sumtype._data payload
					} else if is_indirect_struct_return {
						// Return value is a pointer to struct - load each field via the pointer
						g.load_val_to_reg(8, ret_val_id)
						for i in 0 .. num_chunks {
							if i < 8 {
								g.emit(asm_ldr_imm(Reg(i), Reg(8), u32(i)))
							}
						}
					} else if ret_offset := g.stack_map[ret_val_id] {
						for i in 0 .. num_chunks {
							if i < 8 {
								if i < source_chunks {
									g.emit_ldr_reg_offset(i, 29, ret_offset + i * 8)
								} else {
									g.emit_mov_reg(i, 31)
								}
							}
						}
					} else {
						if source_chunks > 1 || ret_typ.kind == .struct_t {
							g.load_struct_src_address_to_reg(8, ret_val_id, actual_struct_typ_id,
								0)
							for i in 0 .. num_chunks {
								if i < 8 {
									if i < source_chunks {
										g.emit(asm_ldr_imm(Reg(i), Reg(8), u32(i)))
									} else {
										g.emit_mov_reg(i, 31)
									}
								}
							}
						} else {
							g.load_val_to_reg(0, ret_val_id)
							for i in 1 .. num_chunks {
								if i < 8 {
									g.emit_mov_reg(i, 31)
								}
							}
						}
					}
				} else if fn_ret_typ.kind == .struct_t && ret_val.kind == .constant
					&& ret_val.name == '0' {
					// Returning zero/none from a function that returns a small struct.
					// Zero all return registers for the struct to avoid garbage in x1+.
					num_ret_chunks := (fn_ret_size + 7) / 8
					for i in 0 .. num_ret_chunks {
						if i < 8 {
							g.emit_mov_reg(i, 31) // xN = xzr (zero)
						}
					}
				} else {
					g.load_val_to_reg(0, ret_val_id)
				}
				// For float return types, move result from x0 to d0
				if fn_ret_typ.kind == .float_t {
					g.emit(asm_fmov_d_x(0, Reg(0))) // fmov d0, x0
				}
			}
			if g.mod.type_store.types[g.cur_func_ret_type].kind == .void_t {
				// Keep void returns deterministic; process entry uses x0 as exit status.
				g.emit_mov_reg(0, 31)
			}
			// Reset SP to the bottom of the callee-saved registers area
			// SP = FP - callee_saved_size
			callee_size := ((g.used_regs.len + 1) / 2) * 16
			g.emit(asm_sub_imm(sp, fp, u32(callee_size)))
			// Restore callee-saved regs
			mut j := g.used_regs.len
			if j % 2 != 0 {
				j += 1
			}
			for j > 0 {
				base := j - 2
				r1 := g.used_regs[base]
				mut r2 := 31 // xzr
				if base + 1 < g.used_regs.len {
					r2 = g.used_regs[base + 1]
				}
				g.emit(asm_ldp_pair_post(Reg(r1), Reg(r2)))
				j -= 2
			}
			g.emit(asm_ldp_fp_lr_post())
			g.emit(asm_ret())
		}
		.jmp {
			target_blk := instr.operands[0]
			target_idx := if target_blk >= 0 && target_blk < g.val_to_block.len {
				g.val_to_block[target_blk]
			} else {
				g.mod.values[target_blk].index
			}

			// Fallthrough optimization: Don't jump if target is next block
			if target_idx != g.next_blk {
				if target_idx >= 0 && target_idx < g.block_offsets.len
					&& g.block_offsets[target_idx] != -1 {
					off := g.block_offsets[target_idx]
					rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
					g.emit(asm_b(rel))
				} else {
					g.record_pending_label(target_idx)
					g.emit(asm_b(0))
				}
			}
		}
		.br {
			// Load condition value into x8 for branch.
			// Large struct conditions are lowered through their truth word (`[addr + 0]`)
			// to keep branch semantics aligned with `eq/ne` comparisons against zero.
			cond_id := instr.operands[0]
			cond_val := g.mod.values[cond_id]
			if g.value_is_large_struct(cond_id) {
				g.load_large_struct_truth_word_to_reg(8, cond_id)
			} else {
				g.load_val_to_reg(8, cond_id)
			}
			// IR i1 conditions are 1-bit values. Force truncation so non-canonical
			// upper bits from previous producers do not flip branch direction.
			cond_is_i1 := cond_val.typ > 0 && cond_val.typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[cond_val.typ].kind == .int_t
				&& g.mod.type_store.types[cond_val.typ].width == 1
			if cond_is_i1 {
				g.emit_mov_imm64(9, 1)
				g.emit(asm_and(Reg(8), Reg(8), Reg(9)))
			}

			true_blk := if instr.operands[1] >= 0 && instr.operands[1] < g.val_to_block.len {
				g.val_to_block[instr.operands[1]]
			} else {
				g.mod.values[instr.operands[1]].index
			}
			false_blk := if instr.operands[2] >= 0 && instr.operands[2] < g.val_to_block.len {
				g.val_to_block[instr.operands[2]]
			} else {
				g.mod.values[instr.operands[2]].index
			}

			if true_blk >= 0 && true_blk < g.block_offsets.len && g.block_offsets[true_blk] != -1 {
				off := g.block_offsets[true_blk]
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				if rel >= -262144 && rel < 262144 {
					g.emit(asm_cbnz(Reg(8), rel))
				} else {
					g.emit(asm_cbz(Reg(8), 2))
					g.emit(asm_b(rel - 1))
				}
			} else {
				g.emit(asm_cbz(Reg(8), 2))
				g.record_pending_label(true_blk)
				g.emit(asm_b(0))
			}

			if false_blk != g.next_blk {
				if false_blk >= 0 && false_blk < g.block_offsets.len
					&& g.block_offsets[false_blk] != -1 {
					off := g.block_offsets[false_blk]
					rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
					g.emit(asm_b(rel))
				} else {
					g.record_pending_label(false_blk)
					g.emit(asm_b(0))
				}
			}
		}
		.switch_ {
			g.load_val_to_reg(8, instr.operands[0]) // Cond -> x8

			// Iterate cases: pairs of (val, blk) starting at index 2
			for i := 2; i < instr.operands.len; i += 2 {
				// We need val in a register. x9.
				g.load_val_to_reg(9, instr.operands[i])
				g.emit(asm_cmp_reg(Reg(8), Reg(9)))

				// b.eq target
				target_blk_val := instr.operands[i + 1]
				target_blk_idx := if target_blk_val >= 0 && target_blk_val < g.val_to_block.len {
					g.val_to_block[target_blk_val]
				} else {
					g.mod.values[target_blk_val].index
				}

				if target_blk_idx >= 0 && target_blk_idx < g.block_offsets.len
					&& g.block_offsets[target_blk_idx] != -1 {
					off := g.block_offsets[target_blk_idx]
					rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
					if rel >= -262144 && rel < 262144 {
						g.emit(asm_b_cond(cond_eq, rel))
					} else {
						// Trampoline: b.ne skip; B target; skip:
						g.emit(asm_b_cond(cond_ne, 2)) // skip over next B
						g.emit(asm_b(rel - 1))
					}
				} else {
					// Forward reference: use trampoline for safety
					g.emit(asm_b_cond(cond_ne, 2)) // skip over next B
					g.record_pending_label(target_blk_idx)
					g.emit(asm_b(0))
				}
			}

			// Default (Unconditional Branch)
			def_blk_val := instr.operands[1]
			def_idx := if def_blk_val >= 0 && def_blk_val < g.val_to_block.len {
				g.val_to_block[def_blk_val]
			} else {
				g.mod.values[def_blk_val].index
			}
			if def_idx >= 0 && def_idx < g.block_offsets.len && g.block_offsets[def_idx] != -1 {
				off := g.block_offsets[def_idx]
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(asm_b(rel))
			} else {
				g.record_pending_label(def_idx)
				g.emit(asm_b(0))
			}
		}
		.trunc, .zext {
			if instr.operands.len > 0 {
				// Check if this is a float-to-float conversion
				src_val := g.mod.values[instr.operands[0]]
				src_is_float := src_val.typ > 0 && src_val.typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[src_val.typ].kind == .float_t
				dst_is_float := g.mod.values[val_id].typ > 0
					&& g.mod.values[val_id].typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				if src_is_float && dst_is_float {
					dest_reg := if r := g.reg_map[val_id] { r } else { 8 }
					if instr.op == .trunc {
						// f64 → f32: load f64 into d0, convert to s0, move bits to int reg
						g.load_float_operand(instr.operands[0], 0)
						g.emit(asm_fcvt_s_d(0, 0))
						g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
					} else {
						// f32 → f64: load_float_operand already widens f32→f64
						g.load_float_operand(instr.operands[0], 0)
						g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
					}
					if val_id !in g.reg_map {
						g.store_reg_to_val(dest_reg, val_id)
					}
				} else {
					// Integer conversions: just copy (registers are 64-bit)
					g.load_val_to_reg(8, instr.operands[0])
					g.store_reg_to_val(8, val_id)
				}
			}
		}
		.bitcast, .sext {
			// For arm64: all registers are 64-bit, so integer type conversions
			// are mostly just copies. Sign extension would matter for 8/16 bit
			// values but we operate on full 64-bit registers throughout.
			if instr.operands.len > 0 {
				mut src_id := instr.operands[0]
				// Keep sumtype wrapper semantics: if the source is an unwrapped payload
				// derived from an existing wrapper, copy the wrapper value instead of
				// reinterpreting payload struct bytes as `{_tag,_data}`.
				if val_id > 0 && val_id < g.mod.values.len {
					dest_typ_id := g.mod.values[val_id].typ
					if g.is_sumtype_wrapper_struct_type(dest_typ_id) {
						if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(src_id,
							dest_typ_id, 0)
						{
							src_id = wrapper_id
						}
					}
				}
				trace_bitcast := g.env_trace_bitcast.len > 0
					&& (g.env_trace_bitcast == '*' || g.cur_func_name == g.env_trace_bitcast)
				if trace_bitcast {
					mut src_kind := ssa.TypeKind.void_t
					mut src_size := 0
					mut dest_kind := ssa.TypeKind.void_t
					mut dest_size := 0
					mut src_name := ''
					if src_id > 0 && src_id < g.mod.values.len {
						src_val_dbg := g.mod.values[src_id]
						src_name = src_val_dbg.name
						if src_val_dbg.typ > 0 && src_val_dbg.typ < g.mod.type_store.types.len {
							src_kind = g.mod.type_store.types[src_val_dbg.typ].kind
							src_size = g.type_size(src_val_dbg.typ)
						}
					}
					if val_id > 0 && val_id < g.mod.values.len {
						dst_typ_id := g.mod.values[val_id].typ
						if dst_typ_id > 0 && dst_typ_id < g.mod.type_store.types.len {
							dest_kind = g.mod.type_store.types[dst_typ_id].kind
							dest_size = g.type_size(dst_typ_id)
						}
					}
					eprintln('ARM64 BITCAST fn=${g.cur_func_name} val=${val_id} src=${src_id} sname=`${src_name}` skind=${src_kind} ssz=${src_size} dkind=${dest_kind} dsz=${dest_size} src_off=${g.stack_map[src_id]} dst_off=${g.stack_map[val_id]}')
				}
				mut copied_aggregate := false
				if val_id > 0 && val_id < g.mod.values.len {
					dest_typ_id := g.mod.values[val_id].typ
					if dest_typ_id > 0 && dest_typ_id < g.mod.type_store.types.len {
						dest_typ := g.mod.type_store.types[dest_typ_id]
						dest_size := g.type_size(dest_typ_id)
						if dest_typ.kind in [.struct_t, .array_t] && dest_size > 8 {
							if dest_off := g.stack_map[val_id] {
								mut src_ptr_reg := 11
								mut can_copy := false
								mut src_is_null_ptr := false
								mut src_copy_uses_pointee := false
								src_is_ptr_carried := g.large_aggregate_stack_value_is_pointer(src_id)
								if src_id > 0 && src_id < g.mod.values.len {
									src_typ_id := g.mod.values[src_id].typ
									if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
										src_typ := g.mod.type_store.types[src_typ_id]
										if src_typ.kind == .ptr_t && src_typ.elem_type > 0
											&& src_typ.elem_type < g.mod.type_store.types.len {
											src_elem_size := g.type_size(src_typ.elem_type)
											mut can_reinterpret_ptr :=
												src_typ.elem_type == dest_typ_id
												|| src_elem_size == dest_size
											// Sumtype wrappers must never reinterpret same-size,
											// different pointee structs as wrapper bytes.
											if can_reinterpret_ptr
												&& g.is_sumtype_wrapper_struct_type(dest_typ_id)
												&& src_typ.elem_type != dest_typ_id {
												can_reinterpret_ptr = false
											}
											if can_reinterpret_ptr
												&& g.is_sumtype_wrapper_struct_type(dest_typ_id)
												&& g.scalar_value_is_pointer_payload(src_id, 0) {
												can_reinterpret_ptr = false
											}
											if can_reinterpret_ptr {
												src_is_null_ptr = g.is_effective_null_pointer_value(src_id)
												if src_reg := g.reg_map[src_id] {
													src_ptr_reg = src_reg
												} else if src_off := g.stack_map[src_id] {
													g.emit_ldr_reg_offset(src_ptr_reg,
														29, src_off)
												} else {
													g.load_val_to_reg(src_ptr_reg, src_id)
												}
												can_copy = true
												src_copy_uses_pointee = true
											}
										}
									}
								}
								if !can_copy {
									if src_off := g.stack_map[src_id] {
										if src_is_ptr_carried {
											// Source slot carries an address to aggregate payload.
											g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
										} else {
											// For bitcasted aggregate values, reinterpret bytes from
											// the source value storage. Pointer-typed sources are
											// handled above; non-pointer sources must copy inline
											// bytes from their stack slot, not dereference slot data.
											g.emit_add_fp_imm(src_ptr_reg, src_off)
										}
										can_copy = true
									} else if src_reg := g.reg_map[src_id] {
										if src_is_ptr_carried {
											src_ptr_reg = src_reg
											can_copy = true
										}
									} else if src_id > 0 && src_id < g.mod.values.len {
										src_is_null_ptr = g.is_effective_null_pointer_value(src_id)
										g.load_address_of_val_to_reg(src_ptr_reg, src_id)
										can_copy = true
									}
								}
								if can_copy {
									mut src_size := if src_copy_uses_pointee {
										// Pointer source was accepted as by-value aggregate address.
										// Size must follow the pointee layout.
										if src_id > 0 && src_id < g.mod.values.len {
											src_typ_id := g.mod.values[src_id].typ
											if src_typ_id > 0
												&& src_typ_id < g.mod.type_store.types.len {
												src_typ := g.mod.type_store.types[src_typ_id]
												if src_typ.kind == .ptr_t && src_typ.elem_type > 0
													&& src_typ.elem_type < g.mod.type_store.types.len {
													g.type_size(src_typ.elem_type)
												} else {
													g.type_size(g.mod.values[src_id].typ)
												}
											} else {
												0
											}
										} else {
											0
										}
									} else {
										// Copying from source value storage: use source storage size,
										// not pointer pointee size, to avoid over-reading adjacent slots.
										g.aggregate_source_size_bytes(src_id)
									}
									copy_size := if src_size > 0 && src_size < dest_size {
										src_size
									} else {
										dest_size
									}
									if src_is_null_ptr {
										g.zero_fp_bytes(dest_off, dest_size)
									} else {
										g.copy_ptr_to_fp_bytes(src_ptr_reg, dest_off,
											copy_size)
										if copy_size < dest_size {
											g.zero_fp_bytes(dest_off + copy_size, dest_size - copy_size)
										}
									}
									copied_aggregate = true
								}
							}
						}
					}
				}
				if !copied_aggregate {
					g.load_val_to_reg(8, src_id)
					g.store_reg_to_val(8, val_id)
				}
			}
		}
		.phi {
			// Phi nodes are handled by elim_phi_nodes inserting assignments in predecessors.
			// We just need to ensure the slot exists (handled in gen_func loop).
		}
		.assign {
			// assign dest_id, src_id
			// Used for Phi elimination: store src into dest's slot
			dest_id := instr.operands[0]
			mut src_id := instr.operands[1]
			trace_assign := g.env_trace_assign.len > 0
				&& (g.env_trace_assign == '*' || g.cur_func_name == g.env_trace_assign)
			mut handled_aggregate_copy := false
			if dest_id > 0 && dest_id < g.mod.values.len {
				dest_typ_id := g.mod.values[dest_id].typ
				if dest_typ_id > 0 && dest_typ_id < g.mod.type_store.types.len {
					dest_typ := g.mod.type_store.types[dest_typ_id]
					dest_size := g.type_size(dest_typ_id)
					if g.is_sumtype_wrapper_struct_type(dest_typ_id) {
						if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(src_id,
							dest_typ_id, 0)
						{
							src_id = wrapper_id
						}
					}
					if trace_assign {
						mut src_kind := ssa.TypeKind.void_t
						mut src_size := 0
						mut src_op_dbg := 'na'
						mut src_name_dbg := ''
						mut src_has_off_dbg := false
						mut src_off_dbg := 0
						mut dest_has_off_dbg := false
						mut dest_off_dbg := 0
						if src_id > 0 && src_id < g.mod.values.len {
							src_typ_id := g.mod.values[src_id].typ
							if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
								src_kind = g.mod.type_store.types[src_typ_id].kind
								src_size = g.type_size(src_typ_id)
							}
							src_val := g.mod.values[src_id]
							src_name_dbg = src_val.name
							if src_val.kind == .instruction {
								src_op_dbg = '${g.selected_opcode(g.mod.instrs[src_val.index])}'
							} else {
								src_op_dbg = '${src_val.kind}'
							}
						}
						if src_off := g.stack_map[src_id] {
							src_has_off_dbg = true
							src_off_dbg = src_off
						}
						if dest_off := g.stack_map[dest_id] {
							dest_has_off_dbg = true
							dest_off_dbg = dest_off
						}
						eprintln('ARM64 ASSIGN fn=${g.cur_func_name} dest=${dest_id} dtyp=${dest_typ_id} dkind=${dest_typ.kind} dsz=${dest_size} dest_has_off=${dest_has_off_dbg} dest_off=${dest_off_dbg} src=${src_id} sop=${src_op_dbg} sname=`${src_name_dbg}` skind=${src_kind} ssz=${src_size} src_has_off=${src_has_off_dbg} src_off=${src_off_dbg}')
					}
					if dest_typ.kind in [.struct_t, .array_t] && dest_size > 8 {
						if dest_off := g.stack_map[dest_id] {
							// Use x12 as source pointer: emit_str_reg_offset can clobber x11.
							mut src_ptr_reg := 12
							mut can_copy := false
							mut src_is_null_ptr := false
							mut src_copy_uses_pointee := false
							src_is_ptr_carried := g.large_aggregate_stack_value_is_pointer(src_id)
							if src_id > 0 && src_id < g.mod.values.len {
								src_typ_id := g.mod.values[src_id].typ
								if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
									src_typ := g.mod.type_store.types[src_typ_id]
									// Aggregate sources in registers/slots are often address-typed
									// (including bitcasted pointers). Treat pointer sources as
									// source addresses for by-value struct copies.
									if src_typ.kind == .ptr_t {
										mut can_use_ptr_source := true
										// For sumtype wrappers, only copy from pointer sources
										// when the pointee type exactly matches the wrapper type.
										if g.is_sumtype_wrapper_struct_type(dest_typ_id)
											&& src_typ.elem_type != dest_typ_id {
											can_use_ptr_source = false
										}
										if can_use_ptr_source {
											if g.is_sumtype_wrapper_struct_type(dest_typ_id)
												&& g.scalar_value_is_pointer_payload(src_id, 0) {
												can_use_ptr_source = false
											}
										}
										if can_use_ptr_source {
											src_is_null_ptr = g.is_effective_null_pointer_value(src_id)
											if src_reg := g.reg_map[src_id] {
												src_ptr_reg = src_reg
											} else if src_off := g.stack_map[src_id] {
												g.emit_ldr_reg_offset(src_ptr_reg, 29,
													src_off)
											} else {
												g.load_val_to_reg(src_ptr_reg, src_id)
											}
											can_copy = true
											src_copy_uses_pointee = true
										}
									}
								}
							}
							if !can_copy {
								if src_off := g.stack_map[src_id] {
									// Materialize string_literal values before reading from their stack slot.
									if src_id > 0 && src_id < g.mod.values.len
										&& g.mod.values[src_id].kind == .string_literal
										&& src_id !in g.string_literal_offsets {
										g.load_val_to_reg(src_ptr_reg, src_id)
									}
									if src_is_ptr_carried {
										// Source slot carries an address to aggregate payload.
										g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
									} else {
										// Pointer-typed sources are handled above.
										// Non-pointer aggregate assigns should copy bytes from
										// the source value stack slot directly.
										g.emit_add_fp_imm(src_ptr_reg, src_off)
									}
									can_copy = true
								} else if src_reg := g.reg_map[src_id] {
									if src_is_ptr_carried {
										src_ptr_reg = src_reg
										can_copy = true
									}
								} else if src_id > 0 && src_id < g.mod.values.len {
									// Keep aggregate-by-value semantics for non-stack sources
									// (e.g. globals, arguments, and address-carried values).
									src_is_null_ptr = g.is_effective_null_pointer_value(src_id)
									g.load_address_of_val_to_reg(src_ptr_reg, src_id)
									can_copy = true
								}
							}
							if can_copy {
								mut src_sz := if src_copy_uses_pointee {
									// Pointer source was accepted as by-value aggregate address.
									if src_id > 0 && src_id < g.mod.values.len {
										src_typ_id := g.mod.values[src_id].typ
										if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
											src_typ := g.mod.type_store.types[src_typ_id]
											if src_typ.kind == .ptr_t && src_typ.elem_type > 0
												&& src_typ.elem_type < g.mod.type_store.types.len {
												g.type_size(src_typ.elem_type)
											} else {
												g.type_size(g.mod.values[src_id].typ)
											}
										} else {
											0
										}
									} else {
										0
									}
								} else {
									// Copying from source value storage: do not use pointer pointee
									// size here, otherwise we over-read stale neighbouring slots.
									g.aggregate_source_size_bytes(src_id)
								}
								copy_size := if src_sz > 0 && src_sz < dest_size {
									src_sz
								} else {
									dest_size
								}
								if src_is_null_ptr {
									g.zero_fp_bytes(dest_off, dest_size)
								} else {
									g.copy_ptr_to_fp_bytes(src_ptr_reg, dest_off, copy_size)
									if copy_size < dest_size {
										g.zero_fp_bytes(dest_off + copy_size, dest_size - copy_size)
									}
								}
								handled_aggregate_copy = true
							}
						}
					}
				}
			}
			if !handled_aggregate_copy {
				// For multi-word struct destinations with constant sources (undef/0),
				// zero-fill all chunks instead of storing a single register.
				if dest_id > 0 && dest_id < g.mod.values.len {
					d_typ_id := g.mod.values[dest_id].typ
					if d_typ_id > 0 && d_typ_id < g.mod.type_store.types.len {
						d_sz := g.type_size(d_typ_id)
						if d_sz > 8 {
							is_const_src := src_id > 0 && src_id < g.mod.values.len
								&& g.mod.values[src_id].kind == .constant
							if is_const_src {
								if d_off := g.stack_map[dest_id] {
									g.zero_fp_bytes(d_off, d_sz)
									handled_aggregate_copy = true
								}
							}
						}
					}
				}
			}
			if !handled_aggregate_copy {
				g.load_val_to_reg(8, src_id)
				g.store_reg_to_val(8, dest_id)
			}
		}
		.unreachable {
			// Emit UDF #0 instruction (undefined trap)
			g.emit(asm_udf())
		}
		.inline_string_init {
			// Create string struct by value: { str, len, is_lit }
			// operands: [str_ptr, len, is_lit]
			// This instruction creates a string struct on the stack
			// The result is a pointer to the struct
			str_ptr_id := instr.operands[0]
			len_id := instr.operands[1]
			is_lit_id := instr.operands[2]

			// stack_map[val_id] points to the 8-byte pointer slot.
			// The string payload data lives right above it (at +8).
			base_offset := g.stack_map[val_id]
			struct_offset := base_offset + 8
			str_off, _ := g.struct_field_offset_and_size(instr.typ, 0, 0, 8)
			len_off, len_size := g.struct_field_offset_and_size(instr.typ, 1, 8, 8)
			is_lit_off, is_lit_size := g.struct_field_offset_and_size(instr.typ, 2, 16,
				8)

			// Store str field
			g.load_val_to_reg(8, str_ptr_id)
			g.emit_str_reg_offset_sized(8, 29, struct_offset + str_off, 8)

			// Store len field
			g.load_val_to_reg(9, len_id)
			g.emit_str_reg_offset_sized(9, 29, struct_offset + len_off, len_size)

			// Store is_lit field
			g.load_val_to_reg(10, is_lit_id)
			g.emit_str_reg_offset_sized(10, 29, struct_offset + is_lit_off, is_lit_size)

			// Return pointer to struct (stored at base_offset, separate from struct data)
			g.emit_add_fp_imm(8, struct_offset) // x8 = fp + struct_offset
			g.store_reg_to_val(8, val_id)
		}
		.extractvalue {
			// Extract element from tuple/struct
			// operands: [tuple_val, index]
			tuple_id := instr.operands[0]
			idx_val := g.mod.values[instr.operands[1]]
			idx := idx_val.name.int()
			trace_extract := g.env_trace_extract.len > 0
				&& (g.env_trace_extract == '*' || g.cur_func_name == g.env_trace_extract)
			tuple_val := g.mod.values[tuple_id]
			mut tuple_is_agg_typ := false
			mut tuple_is_large_agg := false
			mut field_byte_off := idx * 8
			mut field_elem_size := 8
			mut scalar_load_agg_src_ptr_id := 0
			mut scalar_load_agg_ptr_elem_typ_id := ssa.TypeID(0)
			mut scalar_load_agg_needs_slot_deref := false
			if tuple_val.typ > 0 && tuple_val.typ < g.mod.type_store.types.len {
				tuple_typ := g.mod.type_store.types[tuple_val.typ]
				tuple_is_agg_typ = tuple_typ.kind in [.struct_t, .array_t]
				tuple_is_large_agg = g.type_size(tuple_val.typ) > 16
					&& tuple_typ.kind in [.struct_t, .array_t]
				if tuple_typ.kind == .struct_t && idx >= 0 {
					field_byte_off = g.struct_field_offset_bytes(tuple_val.typ, idx)
					if idx < tuple_typ.fields.len {
						field_elem_size = g.type_size(tuple_typ.fields[idx])
						if field_elem_size <= 0 {
							field_elem_size = 8
						}
					}
				}
			} else {
				// typ out of range — use default field_byte_off and field_elem_size
			}
			// Some lowered aggregate loads are scalar-typed in SSA (e.g. pointer-width
			// temporaries). Recover the aggregate element type from the load pointer so
			// field offsets/sizes stay correct for mixed-size structs like `map`.
			if !tuple_is_agg_typ && tuple_val.kind == .instruction {
				tuple_ins0 := g.mod.instrs[tuple_val.index]
				if g.selected_opcode(tuple_ins0) == .load && tuple_ins0.operands.len > 0 {
					load_ptr_id := tuple_ins0.operands[0]
					if load_ptr_id > 0 && load_ptr_id < g.mod.values.len {
						load_ptr_val := g.mod.values[load_ptr_id]
						if load_ptr_val.typ > 0 && load_ptr_val.typ < g.mod.type_store.types.len {
							load_ptr_typ := g.mod.type_store.types[load_ptr_val.typ]
							if load_ptr_typ.kind == .ptr_t && load_ptr_typ.elem_type > 0
								&& load_ptr_typ.elem_type < g.mod.type_store.types.len {
								load_elem_typ := g.mod.type_store.types[load_ptr_typ.elem_type]
								if load_elem_typ.kind in [.struct_t, .array_t] {
									scalar_load_agg_src_ptr_id = load_ptr_id
									scalar_load_agg_ptr_elem_typ_id = load_ptr_typ.elem_type
									tuple_is_agg_typ = true
									tuple_is_large_agg = g.type_size(load_ptr_typ.elem_type) > 16
									if load_elem_typ.kind == .struct_t && idx >= 0 {
										field_byte_off = g.struct_field_offset_bytes(load_ptr_typ.elem_type,
											idx)
										if idx < load_elem_typ.fields.len {
											field_elem_size = g.type_size(load_elem_typ.fields[idx])
											if field_elem_size <= 0 {
												field_elem_size = 8
											}
										}
									}
									if load_ptr_val.kind == .instruction {
										load_ptr_instr := g.mod.instrs[load_ptr_val.index]
										if g.selected_opcode(load_ptr_instr) == .alloca
											&& g.alloca_slot_stores_pointer_like_values(load_ptr_id, load_ptr_typ.elem_type) {
											scalar_load_agg_needs_slot_deref = true
										}
									}
								}
							}
						}
					}
				}
			}
			mut result_is_agg := false
			mut result_size := 0
			if instr.typ > 0 && instr.typ < g.mod.type_store.types.len {
				result_typ := g.mod.type_store.types[instr.typ]
				result_size = g.type_size(instr.typ)
				result_is_agg = result_typ.kind in [.struct_t, .array_t]
			}
			if trace_extract {
				eprintln('ARM64 EXTRACT fn=${g.cur_func_name} val=${val_id} tuple=${tuple_id} tuple_kind=${tuple_val.kind} tuple_name=`${tuple_val.name}` tuple_typ=${tuple_val.typ} idx=${idx} field_off=${field_byte_off} field_size=${field_elem_size} result_typ=${instr.typ} result_size=${result_size} tuple_agg=${tuple_is_agg_typ} tuple_large=${tuple_is_large_agg} scalar_ptr=${scalar_load_agg_src_ptr_id} scalar_elem_typ=${scalar_load_agg_ptr_elem_typ_id} needs_slot_deref=${scalar_load_agg_needs_slot_deref} tuple_off=${g.stack_map[tuple_id]}')
			}
			mut handled_scalar_load_struct_extract := false
			if scalar_load_agg_src_ptr_id > 0 && scalar_load_agg_ptr_elem_typ_id > 0 {
				base_ptr_reg := g.get_operand_reg(scalar_load_agg_src_ptr_id, 9)
				mut agg_ptr_reg := base_ptr_reg
				if scalar_load_agg_needs_slot_deref {
					agg_ptr_reg = if base_ptr_reg == 11 { 12 } else { 11 }
					g.emit(asm_ldr(Reg(agg_ptr_reg), Reg(base_ptr_reg)))
				}
				if result_is_agg && result_size > 8 {
					if dst_offset := g.stack_map[val_id] {
						if g.is_effective_null_pointer_value(scalar_load_agg_src_ptr_id) {
							g.zero_fp_bytes(dst_offset, result_size)
						} else {
							g.copy_ptr_offset_to_fp_bytes(agg_ptr_reg, field_byte_off,
								dst_offset, result_size)
						}
						handled_scalar_load_struct_extract = true
					}
				} else {
					if g.is_effective_null_pointer_value(scalar_load_agg_src_ptr_id) {
						g.emit_mov_reg(8, 31)
					} else if field_elem_size in [1, 2, 4] {
						mut field_addr_reg := 9
						if field_addr_reg == agg_ptr_reg {
							field_addr_reg = 10
						}
						if field_byte_off >= 0 && field_byte_off <= 0xFFF {
							g.emit(asm_add_imm(Reg(field_addr_reg), Reg(agg_ptr_reg),
								u32(field_byte_off)))
						} else {
							g.emit_mov_imm64(field_addr_reg, field_byte_off)
							g.emit(asm_add_reg(Reg(field_addr_reg), Reg(agg_ptr_reg),
								Reg(field_addr_reg)))
						}
						field_is_unsigned := if instr.typ > 0
							&& instr.typ < g.mod.type_store.types.len {
							g.mod.type_store.types[instr.typ].is_unsigned
						} else {
							false
						}
						match field_elem_size {
							1 {
								g.emit(asm_ldr_b(Reg(8), Reg(field_addr_reg)))
							}
							2 {
								g.emit(asm_ldr_h(Reg(8), Reg(field_addr_reg)))
							}
							4 {
								if field_is_unsigned {
									g.emit(asm_ldr_w(Reg(8), Reg(field_addr_reg)))
								} else {
									g.emit(asm_ldrsw(Reg(8), Reg(field_addr_reg)))
								}
							}
							else {}
						}
					} else {
						g.emit_ldr_reg_offset(8, agg_ptr_reg, field_byte_off)
					}
					g.store_reg_to_val(8, val_id)
					handled_scalar_load_struct_extract = true
				}
			}
			mut handled_scalar_load_agg_extract := false
			// Some lowered paths represent aggregate loads through scalar-typed
			// temporaries (typically pointer-sized). If extractvalue asks for a
			// multi-word aggregate from such a temporary, read directly from the
			// original load pointer instead of interpreting scalar spill bytes.
			if result_is_agg && result_size > 8 && !tuple_is_agg_typ
				&& tuple_val.kind == .instruction {
				tuple_ins := g.mod.instrs[tuple_val.index]
				if tuple_ins.op == .load && tuple_ins.operands.len > 0 {
					load_src_id := tuple_ins.operands[0]
					mut scalar_load_is_ptr_payload := false
					if load_src_id > 0 && load_src_id < g.mod.values.len {
						load_src_val := g.mod.values[load_src_id]
						if load_src_val.kind == .instruction {
							load_src_instr := g.mod.instrs[load_src_val.index]
							if g.selected_opcode(load_src_instr) == .alloca
								&& g.alloca_slot_stores_pointer_like_values(load_src_id, instr.typ) {
								scalar_load_is_ptr_payload = true
							}
						}
					}
					if scalar_load_is_ptr_payload {
						// The scalar load result carries a pointer payload stored in an
						// alloca-backed slot. Extract aggregate bytes from that pointer.
						src_ptr_id := tuple_id
						if dst_offset := g.stack_map[val_id] {
							if g.is_effective_null_pointer_value(src_ptr_id) {
								g.zero_fp_bytes(dst_offset, result_size)
							} else {
								src_ptr_reg := g.get_operand_reg(src_ptr_id, 9)
								g.copy_ptr_offset_to_fp_bytes(src_ptr_reg, field_byte_off,
									dst_offset, result_size)
							}
							handled_scalar_load_agg_extract = true
						}
					}
				}
			}
			mut handled_sumtype_tag_payload_extract := false
			// Some malformed MIR paths unwrap sumtype wrappers through `_tag` first, then
			// request a pointer/aggregate extract from that scalar word. Recover by reading
			// from the wrapper `_data` pointer directly.
			if !handled_scalar_load_struct_extract && !handled_scalar_load_agg_extract
				&& !tuple_is_agg_typ && tuple_val.kind == .instruction && instr.typ > 0
				&& instr.typ < g.mod.type_store.types.len {
				dst_typ := g.mod.type_store.types[instr.typ]
				if (result_is_agg && result_size > 8) || dst_typ.kind == .ptr_t {
					if wrapper_id := g.sumtype_wrapper_source_from_tag_word(tuple_id) {
						if g.load_sumtype_data_ptr_to_reg(9, wrapper_id) {
							if result_is_agg && result_size > 8 {
								if dst_offset := g.stack_map[val_id] {
									g.copy_ptr_offset_to_fp_bytes(9, field_byte_off, dst_offset,
										result_size)
									handled_sumtype_tag_payload_extract = true
								}
							} else if dst_typ.kind == .ptr_t {
								if field_byte_off == 0 {
									g.emit_mov_reg(8, 9)
								} else if field_byte_off >= 0 && field_byte_off <= 0xFFF {
									g.emit(asm_add_imm(Reg(8), Reg(9), u32(field_byte_off)))
								} else {
									g.emit_mov_imm64(8, field_byte_off)
									g.emit(asm_add_reg(Reg(8), Reg(9), Reg(8)))
								}
								g.store_reg_to_val(8, val_id)
								handled_sumtype_tag_payload_extract = true
							}
						}
					}
				}
			}
			if !handled_scalar_load_struct_extract && !handled_scalar_load_agg_extract
				&& !handled_sumtype_tag_payload_extract {
				// If the tuple source is a string_literal (e.g. after mem2reg
				// promotion of alloca → store → load), ensure the string struct
				// has been materialized on the stack before we read from it.
				if tuple_val.kind == .string_literal {
					if _ := g.string_literal_offsets[tuple_id] {
						// already materialized
					} else {
						// Force materialization by loading into a scratch register.
						g.load_val_to_reg(10, tuple_id)
					}
				}
				mut handled_scalar_payload_extract := false
				if !tuple_is_agg_typ && g.scalar_value_is_pointer_payload(tuple_id, 0) {
					base_ptr_reg := g.get_operand_reg(tuple_id, 9)
					if result_is_agg && result_size > 8 {
						if dst_offset := g.stack_map[val_id] {
							if g.is_effective_null_pointer_value(tuple_id) {
								g.zero_fp_bytes(dst_offset, result_size)
							} else {
								g.copy_ptr_offset_to_fp_bytes(base_ptr_reg, field_byte_off,
									dst_offset, result_size)
							}
							handled_scalar_payload_extract = true
						}
					} else if instr.typ > 0 && instr.typ < g.mod.type_store.types.len
						&& g.mod.type_store.types[instr.typ].kind == .ptr_t {
						if g.is_effective_null_pointer_value(tuple_id) {
							g.emit_mov_reg(8, 31)
						} else if field_byte_off == 0 {
							if base_ptr_reg != 8 {
								g.emit_mov_reg(8, base_ptr_reg)
							}
						} else if field_byte_off >= 0 && field_byte_off <= 0xFFF {
							g.emit(asm_add_imm(Reg(8), Reg(base_ptr_reg), u32(field_byte_off)))
						} else {
							g.emit_mov_imm64(8, field_byte_off)
							g.emit(asm_add_reg(Reg(8), Reg(base_ptr_reg), Reg(8)))
						}
						g.store_reg_to_val(8, val_id)
						handled_scalar_payload_extract = true
					} else {
						if g.is_effective_null_pointer_value(tuple_id) {
							g.emit_mov_reg(8, 31)
						} else {
							load_size := if result_size in [1, 2, 4, 8] {
								result_size
							} else {
								8
							}
							mut field_addr_reg := 9
							if field_addr_reg == base_ptr_reg {
								field_addr_reg = 10
							}
							if field_byte_off == 0 {
								if field_addr_reg != base_ptr_reg {
									g.emit_mov_reg(field_addr_reg, base_ptr_reg)
								}
							} else if field_byte_off >= 0 && field_byte_off <= 0xFFF {
								g.emit(asm_add_imm(Reg(field_addr_reg), Reg(base_ptr_reg),
									u32(field_byte_off)))
							} else {
								g.emit_mov_imm64(field_addr_reg, field_byte_off)
								g.emit(asm_add_reg(Reg(field_addr_reg), Reg(base_ptr_reg),
									Reg(field_addr_reg)))
							}
							match load_size {
								1 {
									g.emit(asm_ldr_b(Reg(8), Reg(field_addr_reg)))
								}
								2 {
									g.emit(asm_ldr_h(Reg(8), Reg(field_addr_reg)))
								}
								4 {
									field_is_unsigned := if instr.typ > 0
										&& instr.typ < g.mod.type_store.types.len {
										g.mod.type_store.types[instr.typ].is_unsigned
									} else {
										false
									}
									if field_is_unsigned {
										g.emit(asm_ldr_w(Reg(8), Reg(field_addr_reg)))
									} else {
										g.emit(asm_ldrsw(Reg(8), Reg(field_addr_reg)))
									}
								}
								else {
									g.emit(asm_ldr(Reg(8), Reg(field_addr_reg)))
								}
							}
						}
						g.store_reg_to_val(8, val_id)
						handled_scalar_payload_extract = true
					}
				}
				mut direct_global_load_ptr_id := -1
				if !handled_scalar_payload_extract && idx == 0 && tuple_val.kind == .instruction {
					tuple_ins := g.mod.instrs[tuple_val.index]
					if tuple_ins.op == .load && tuple_ins.operands.len > 0 && instr.typ > 0
						&& instr.typ < g.mod.type_store.types.len {
						src_ptr_id := tuple_ins.operands[0]
						if src_ptr_id > 0 && src_ptr_id < g.mod.values.len
							&& g.mod.values[src_ptr_id].kind == .global {
							src_is_aggregate := tuple_val.typ > 0
								&& tuple_val.typ < g.mod.type_store.types.len
								&& g.mod.type_store.types[tuple_val.typ].kind in [.struct_t, .array_t]
							dst_typ := g.mod.type_store.types[instr.typ]
							if !src_is_aggregate && dst_typ.kind in [.struct_t, .array_t] {
								direct_global_load_ptr_id = src_ptr_id
							}
						}
					}
				}

				// Get tuple's stack location and load from offset
				if handled_scalar_payload_extract {
					// handled above
				} else if direct_global_load_ptr_id > 0 {
					src_reg := g.get_operand_reg(direct_global_load_ptr_id, 9)
					dst_size := g.type_size(instr.typ)
					if dst_size > 8 {
						if dst_offset := g.stack_map[val_id] {
							num_words := (dst_size + 7) / 8
							for w in 0 .. num_words {
								g.emit_ldr_reg_offset(8, src_reg, w * 8)
								g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
							}
						} else {
							if src_reg != 8 {
								g.emit_mov_reg(8, src_reg)
							}
							g.store_reg_to_val(8, val_id)
						}
					} else {
						load_size := if dst_size in [1, 2, 4, 8] { dst_size } else { 8 }
						match load_size {
							1 { g.emit(asm_ldr_b(Reg(8), Reg(src_reg))) }
							2 { g.emit(asm_ldr_h(Reg(8), Reg(src_reg))) }
							4 { g.emit(asm_ldr_w(Reg(8), Reg(src_reg))) }
							else { g.emit_ldr_reg_offset(8, src_reg, 0) }
						}
						g.store_reg_to_val(8, val_id)
					}
				} else if tuple_offset := g.stack_map[tuple_id] {
					mut handled_wrapper_payload_extract := false
					if tuple_val.typ > 0 && tuple_val.typ < g.mod.type_store.types.len {
						tuple_typ2 := g.mod.type_store.types[tuple_val.typ]
						// Sumtype wrappers encode payload pointers in `_data`.
						// `extractvalue` with indices beyond wrapper fields means
						// "read from payload pointer", not "read past wrapper bytes".
						if tuple_typ2.kind == .struct_t && tuple_typ2.field_names.len == 2
							&& tuple_typ2.field_names[0] == '_tag'
							&& tuple_typ2.field_names[1] == '_data' && idx >= tuple_typ2.fields.len {
							data_off := g.struct_field_offset_bytes(tuple_val.typ, 1)
							g.emit_ldr_reg_offset(9, 29, tuple_offset + data_off)
							payload_field_idx := idx - tuple_typ2.fields.len
							payload_off := payload_field_idx * 8
							if result_is_agg && result_size > 8 {
								if dst_offset := g.stack_map[val_id] {
									g.copy_ptr_offset_to_fp_bytes(9, payload_off, dst_offset,
										result_size)
									handled_wrapper_payload_extract = true
								}
							} else if instr.typ > 0 && instr.typ < g.mod.type_store.types.len
								&& g.mod.type_store.types[instr.typ].kind == .ptr_t {
								if payload_off == 0 {
									g.emit_mov_reg(8, 9)
								} else if payload_off >= 0 && payload_off <= 0xFFF {
									g.emit(asm_add_imm(Reg(8), Reg(9), u32(payload_off)))
								} else {
									g.emit_mov_imm64(8, payload_off)
									g.emit(asm_add_reg(Reg(8), Reg(9), Reg(8)))
								}
								g.store_reg_to_val(8, val_id)
								handled_wrapper_payload_extract = true
							} else {
								load_size := if result_size in [1, 2, 4, 8] {
									result_size
								} else {
									8
								}
								if payload_off == 0 {
									g.emit_mov_reg(10, 9)
								} else if payload_off >= 0 && payload_off <= 0xFFF {
									g.emit(asm_add_imm(Reg(10), Reg(9), u32(payload_off)))
								} else {
									g.emit_mov_imm64(10, payload_off)
									g.emit(asm_add_reg(Reg(10), Reg(9), Reg(10)))
								}
								match load_size {
									1 {
										g.emit(asm_ldr_b(Reg(8), Reg(10)))
									}
									2 {
										g.emit(asm_ldr_h(Reg(8), Reg(10)))
									}
									4 {
										field_is_unsigned := if instr.typ > 0
											&& instr.typ < g.mod.type_store.types.len {
											g.mod.type_store.types[instr.typ].is_unsigned
										} else {
											false
										}
										if field_is_unsigned {
											g.emit(asm_ldr_w(Reg(8), Reg(10)))
										} else {
											g.emit(asm_ldrsw(Reg(8), Reg(10)))
										}
									}
									else {
										g.emit(asm_ldr(Reg(8), Reg(10)))
									}
								}
								g.store_reg_to_val(8, val_id)
								handled_wrapper_payload_extract = true
							}
						}
					}
					if handled_wrapper_payload_extract {
						// handled above
					} else if tuple_is_large_agg && idx >= 0
						&& g.large_aggregate_stack_value_is_pointer(tuple_id) {
						g.emit_ldr_reg_offset(9, 29, tuple_offset)
						if field_elem_size > 8 {
							// Multi-word struct field: copy all words from pointer
							if dst_offset := g.stack_map[val_id] {
								num_words := (field_elem_size + 7) / 8
								for w in 0 .. num_words {
									g.emit_ldr_reg_offset(8, 9, field_byte_off + w * 8)
									g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
								}
							} else {
								g.emit_ldr_reg_offset(8, 9, field_byte_off)
								g.store_reg_to_val(8, val_id)
							}
						} else {
							g.emit_ldr_reg_offset(8, 9, field_byte_off)
							g.store_reg_to_val(8, val_id)
						}
					} else if field_elem_size > 8 {
						// Multi-word struct field stored inline: copy all words
						if trace_extract && ((tuple_val.typ == 140 && instr.typ == 10)
							|| (tuple_val.typ == 143 && instr.typ == 123)) {
							eprintln('ARM64 EXTRACT PATH inline tuple=${tuple_id} val=${val_id} src=${
								tuple_offset + field_byte_off} dst=${g.stack_map[val_id]} field_off=${field_byte_off}')
						}
						if dst_offset := g.stack_map[val_id] {
							src_offset := tuple_offset + field_byte_off
							num_words := (field_elem_size + 7) / 8
							for w in 0 .. num_words {
								g.emit_ldr_reg_offset(8, 29, src_offset + w * 8)
								g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
							}
						} else {
							field_offset := tuple_offset + field_byte_off
							g.emit_ldr_reg_offset(8, 29, field_offset)
							g.store_reg_to_val(8, val_id)
						}
					} else if field_elem_size in [1, 2, 4] {
						// Use sized load to avoid reading adjacent packed fields.
						// For signed integer fields, use sign-extending loads.
						field_offset := tuple_offset + field_byte_off
						g.emit_add_fp_imm(9, field_offset)
						field_is_unsigned := if instr.typ > 0
							&& instr.typ < g.mod.type_store.types.len {
							g.mod.type_store.types[instr.typ].is_unsigned
						} else {
							false
						}
						match field_elem_size {
							1 {
								g.emit(asm_ldr_b(Reg(8), Reg(9)))
							}
							2 {
								g.emit(asm_ldr_h(Reg(8), Reg(9)))
							}
							4 {
								if field_is_unsigned {
									g.emit(asm_ldr_w(Reg(8), Reg(9)))
								} else {
									g.emit(asm_ldrsw(Reg(8), Reg(9)))
								}
							}
							else {}
						}
						g.store_reg_to_val(8, val_id)
					} else {
						field_offset := tuple_offset + field_byte_off
						g.emit_ldr_reg_offset(8, 29, field_offset)
						g.store_reg_to_val(8, val_id)
					}
				} else if reg := g.reg_map[tuple_id] {
					// Large aggregates in registers are represented by their address.
					if tuple_is_large_agg && idx >= 0 {
						if field_elem_size > 8 {
							// Multi-word struct field: copy all words from address
							if dst_offset := g.stack_map[val_id] {
								num_words := (field_elem_size + 7) / 8
								for w in 0 .. num_words {
									g.emit_ldr_reg_offset(8, reg, field_byte_off + w * 8)
									g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
								}
							} else {
								g.emit_ldr_reg_offset(8, reg, field_byte_off)
								g.store_reg_to_val(8, val_id)
							}
						} else {
							g.emit_ldr_reg_offset(8, reg, field_byte_off)
							g.store_reg_to_val(8, val_id)
						}
					} else if idx == 0 {
						// Tuple is in a register (e.g., scalarized first field).
						if reg != 8 {
							g.emit_mov_reg(8, reg)
						}
						// Mask to field width for sub-8-byte fields packed in register.
						if field_elem_size in [1, 2, 4] {
							g.emit(asm_ubfx_lower(Reg(8), Reg(8), u32(field_elem_size * 8)))
						}
						g.store_reg_to_val(8, val_id)
					} else {
						// Higher indices packed in same register — shift then mask.
						g.load_val_to_reg(8, tuple_id)
						if field_byte_off > 0 && field_byte_off < 8 {
							g.emit(asm_lsr_imm(Reg(8), Reg(8), u32(field_byte_off * 8)))
						}
						if field_elem_size in [1, 2, 4] {
							g.emit(asm_ubfx_lower(Reg(8), Reg(8), u32(field_elem_size * 8)))
						}
						g.store_reg_to_val(8, val_id)
					}
				} else {
					// Tuple not in stack_map or reg_map - fallback
					g.load_val_to_reg(8, tuple_id)
					g.store_reg_to_val(8, val_id)
				}
			}
		}
		.struct_init {
			// Create struct from field values: operands are field values in order
			result_offset := g.stack_map[val_id]
			struct_typ := g.mod.type_store.types[instr.typ]
			struct_size := g.type_size(instr.typ)
			trace_struct_init := g.env_trace_struct_init.len > 0
				&& (g.env_trace_struct_init == '*' || g.cur_func_name == g.env_trace_struct_init)
			if trace_struct_init {
				mut op_dbg := []string{}
				for field_id in instr.operands {
					if field_id > 0 && field_id < g.mod.values.len {
						fv := g.mod.values[field_id]
						op_dbg << '${field_id}:${fv.kind}:${fv.typ}:`${fv.name}`'
					} else {
						op_dbg << '${field_id}:invalid:0:` `'
					}
				}
				eprintln('ARM64 STRUCT_INIT fn=${g.cur_func_name} val=${val_id} typ=${instr.typ} size=${struct_size} ops=${op_dbg}')
			}
			zero_size := if struct_size > 0 { struct_size } else { 8 }

			// Zero-initialize the entire struct first
			g.emit_add_fp_imm(9, result_offset)
			g.emit_mov_reg(10, 31) // xzr
			mut off := 0
			for off + 8 <= zero_size {
				g.emit_str_reg_offset_sized(10, 9, off, 8)
				off += 8
			}
			if off + 4 <= zero_size {
				g.emit_str_reg_offset_sized(10, 9, off, 4)
				off += 4
			}
			if off + 2 <= zero_size {
				g.emit_str_reg_offset_sized(10, 9, off, 2)
				off += 2
			}
			if off < zero_size {
				g.emit_str_reg_offset_sized(10, 9, off, 1)
			}

			// Store each field value at its proper offset
			for fi, field_id in instr.operands {
				mut field_off := fi * 8
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_off = g.struct_field_offset_bytes(instr.typ, fi)
				}

				mut field_typ_id := ssa.TypeID(0)
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_typ_id = struct_typ.fields[fi]
				} else if field_id > 0 && field_id < g.mod.values.len {
					field_typ_id = g.mod.values[field_id].typ
				}
				mut field_size := if field_typ_id > 0 { g.type_size(field_typ_id) } else { 8 }
				if field_size <= 0 {
					field_size = 8
				}
				if trace_struct_init {
					eprintln('ARM64 STRUCT_INIT FIELD fn=${g.cur_func_name} val=${val_id} fi=${fi} foff=${field_off} fsz=${field_size} ftyp=${field_typ_id} src=${field_id}')
				}

				// Skip zero constants (already zeroed above)
				field_val := g.mod.values[field_id]
				if field_val.kind == .constant && field_val.name == '0' {
					continue
				}

				if field_size <= 8 {
					g.load_val_to_reg(8, field_id)
					store_off := result_offset + field_off
					if field_size in [1, 2, 4] {
						g.emit_add_fp_imm(9, store_off)
						match field_size {
							1 { g.emit(asm_str_b(Reg(8), Reg(9))) }
							2 { g.emit(asm_str_h(Reg(8), Reg(9))) }
							4 { g.emit(asm_str_w(Reg(8), Reg(9))) }
							else {}
						}
					} else {
						g.emit_str_reg_offset(8, 29, store_off)
					}
				} else {
					// Multi-word field (nested struct)
					// Ensure string_literal values are materialized before reading from stack
					if field_val.kind == .string_literal {
						if _ := g.string_literal_offsets[field_id] {
							// already materialized
						} else {
							// Force materialization by loading into a scratch register
							g.load_val_to_reg(10, field_id)
						}
					}
					field_chunks := (field_size + 7) / 8
					src_typ_id := if field_id > 0 && field_id < g.mod.values.len {
						g.mod.values[field_id].typ
					} else {
						ssa.TypeID(0)
					}
					mut src_ptr_matches_field := false
					mut src_size := if src_typ_id > 0 { g.type_size(src_typ_id) } else { 0 }
					if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
						src_typ := g.mod.type_store.types[src_typ_id]
						if src_typ.kind == .ptr_t && src_typ.elem_type > 0
							&& src_typ.elem_type < g.mod.type_store.types.len {
							// Pointer-typed field sources are valid by-value aggregates only when
							// pointee type matches the destination field type exactly.
							if src_typ.elem_type == field_typ_id {
								if g.is_sumtype_wrapper_struct_type(field_typ_id)
									&& g.scalar_value_is_pointer_payload(field_id, 0) {
									src_ptr_matches_field = false
								} else {
									src_ptr_matches_field = true
								}
								src_size = g.type_size(src_typ.elem_type)
							}
						}
					}
					src_chunks := if src_size > 0 { (src_size + 7) / 8 } else { 0 }
					src_is_ptr_carried := g.large_aggregate_stack_value_is_pointer(field_id)
					trace_agg_copy := g.env_trace_agg_copy.len > 0
						&& (g.env_trace_agg_copy == '*' || g.cur_func_name == g.env_trace_agg_copy)
					if trace_agg_copy && src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
						src_typ := g.mod.type_store.types[src_typ_id]
						if src_typ.kind == .ptr_t {
							eprintln('ARM64 AGG struct_init fn=${g.cur_func_name} field=${fi} fsz=${field_size} src=${field_id} src_typ=${src_typ_id} src_sz=${src_size} chunks=${src_chunks}')
						}
					}
					copy_chunks := if src_chunks > 0 && src_chunks < field_chunks {
						src_chunks
					} else {
						field_chunks
					}
					mut copied_field := false

					// Size-mismatched scalar sources (e.g. from unresolved wraps) must not
					// be treated as aggregate addresses.
					if src_size > 0 && src_size <= 8 {
						g.load_val_to_reg(8, field_id)
						g.emit_str_reg_offset(8, 29, result_offset + field_off)
						copied_field = true
					} else if field_offset := g.stack_map[field_id] {
						mut src_ptr_reg := 12
						if src_ptr_matches_field || src_is_ptr_carried {
							g.emit_ldr_reg_offset(src_ptr_reg, 29, field_offset)
						} else {
							g.emit_add_fp_imm(src_ptr_reg, field_offset)
						}
						for w in 0 .. copy_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(w)))
							g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
						}
						copied_field = true
					} else if src_reg := g.reg_map[field_id] {
						if src_ptr_matches_field || src_is_ptr_carried {
							for w in 0 .. copy_chunks {
								g.emit(asm_ldr_imm(Reg(10), Reg(src_reg), u32(w)))
								g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
							}
							copied_field = true
						}
					}
					if !copied_field && g.is_sumtype_wrapper_struct_type(field_typ_id) {
						// Some MIR paths carry small wrapper structs (like `types.Type`)
						// through scalar-typed values. Recover the wrapper address and
						// copy the full bytes instead of falling back to a single-word store.
						mut src_ptr_reg := 12
						g.load_struct_src_address_to_reg(src_ptr_reg, field_id, field_typ_id,
							0)
						for w in 0 .. field_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(w)))
							g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
						}
						copied_field = true
					}

					if !copied_field {
						// Deterministic fallback when aggregate backing storage is unavailable.
						g.load_val_to_reg(8, field_id)
						g.emit_str_reg_offset(8, 29, result_offset + field_off)
						copied_field = true
						// Ensure upper words are initialized; otherwise stale stack bytes
						// can leak into small struct fields (e.g. sumtype wrappers).
						if field_chunks > 1 {
							g.emit_mov_reg(10, 31)
							for w in 1 .. field_chunks {
								g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
							}
						}
					}
					if copied_field && copy_chunks < field_chunks {
						g.emit_mov_reg(10, 31)
						for w in copy_chunks .. field_chunks {
							g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
						}
					}
				}
			}
		}
		.insertvalue {
			// Insert element into tuple/struct
			// operands: [tuple_val, elem_val, index]
			tuple_id := instr.operands[0]
			elem_id := instr.operands[1]
			idx_val := g.mod.values[instr.operands[2]]
			idx := idx_val.name.int()
			trace_insert := g.env_trace_insert.len > 0
				&& (g.env_trace_insert == '*' || g.cur_func_name == g.env_trace_insert)

			// Get result's stack location
			result_offset := g.stack_map[val_id]
			tuple_typ := g.mod.type_store.types[instr.typ]
			tuple_size := g.type_size(instr.typ)
			num_chunks := if tuple_size > 0 { (tuple_size + 7) / 8 } else { 1 }
			mut elem_off := idx * 8
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_off = g.struct_field_offset_bytes(instr.typ, idx)
			}
			if trace_insert {
				tuple_val_dbg := g.mod.values[tuple_id]
				elem_val_dbg := g.mod.values[elem_id]
				eprintln('ARM64 INSERT fn=${g.cur_func_name} val=${val_id} typ=${instr.typ} size=${tuple_size} idx=${idx} elem_off=${elem_off} tuple=${tuple_id}:${tuple_val_dbg.kind}:${tuple_val_dbg.typ} elem=${elem_id}:${elem_val_dbg.kind}:${elem_val_dbg.typ} roff=${result_offset} toff=${g.stack_map[tuple_id]} eoff=${g.stack_map[elem_id]}')
			}

			// Copy existing tuple data if not undef.
			tuple_val := g.mod.values[tuple_id]
			if !(tuple_val.kind == .constant && tuple_val.name == 'undef') {
				mut copied_tuple := false
				if tuple_offset := g.stack_map[tuple_id] {
					if tuple_size > 16 && g.large_aggregate_stack_value_is_pointer(tuple_id) {
						// Keep source pointer in x12 so stores can use x11 scratch safely.
						g.emit_ldr_reg_offset(12, 29, tuple_offset)
						for i in 0 .. num_chunks {
							g.emit_ldr_reg_offset(9, 12, i * 8)
							g.emit_str_reg_offset(9, 29, result_offset + i * 8)
						}
					} else {
						for i in 0 .. num_chunks {
							g.emit_ldr_reg_offset(9, 29, tuple_offset + i * 8)
							g.emit_str_reg_offset(9, 29, result_offset + i * 8)
						}
					}
					copied_tuple = true
				} else if src_reg := g.reg_map[tuple_id] {
					for i in 0 .. num_chunks {
						g.emit(asm_ldr_imm(Reg(9), Reg(src_reg), u32(i)))
						g.emit_str_reg_offset(9, 29, result_offset + i * 8)
					}
					copied_tuple = true
				}
				if !copied_tuple {
					// Deterministic fallback for missing aggregate backing bytes.
					g.emit_mov_reg(9, 31)
					for i in 0 .. num_chunks {
						g.emit_str_reg_offset(9, 29, result_offset + i * 8)
					}
				}
			} else {
				// Start from zeroed storage for `insertvalue(undef, ...)`.
				g.emit_mov_reg(9, 31)
				for i in 0 .. num_chunks {
					g.emit_str_reg_offset(9, 29, result_offset + i * 8)
				}
			}

			// Store the new element at the specified index. Aggregate fields need
			// full-width copies, not a single 8-byte store.
			mut elem_typ_id := ssa.TypeID(0)
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_typ_id = tuple_typ.fields[idx]
			} else if elem_id > 0 && elem_id < g.mod.values.len {
				elem_typ_id = g.mod.values[elem_id].typ
			}
			mut elem_size := if elem_typ_id > 0 { g.type_size(elem_typ_id) } else { 8 }
			if elem_size <= 0 {
				elem_size = 8
			}
			if elem_size <= 8 {
				g.load_val_to_reg(8, elem_id)
				store_off := result_offset + elem_off
				if elem_size in [1, 2, 4] {
					g.emit_add_fp_imm(9, store_off)
					match elem_size {
						1 { g.emit(asm_str_b(Reg(8), Reg(9))) }
						2 { g.emit(asm_str_h(Reg(8), Reg(9))) }
						4 { g.emit(asm_str_w(Reg(8), Reg(9))) }
						else {}
					}
				} else {
					g.emit_str_reg_offset(8, 29, store_off)
				}
			} else {
				elem_chunks := (elem_size + 7) / 8
				src_typ_id := if elem_id > 0 && elem_id < g.mod.values.len {
					g.mod.values[elem_id].typ
				} else {
					ssa.TypeID(0)
				}
				mut src_ptr_matches_elem := false
				mut src_size := if src_typ_id > 0 { g.type_size(src_typ_id) } else { 0 }
				if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
					src_typ := g.mod.type_store.types[src_typ_id]
					if src_typ.kind == .ptr_t && src_typ.elem_type > 0
						&& src_typ.elem_type < g.mod.type_store.types.len {
						// Pointer-typed element sources can be copied by-value only for
						// exact pointee-type matches with the destination element type.
						if src_typ.elem_type == elem_typ_id {
							if g.is_sumtype_wrapper_struct_type(elem_typ_id)
								&& g.scalar_value_is_pointer_payload(elem_id, 0) {
								src_ptr_matches_elem = false
							} else {
								src_ptr_matches_elem = true
							}
							src_size = g.type_size(src_typ.elem_type)
						}
					}
				}
				src_chunks := if src_size > 0 { (src_size + 7) / 8 } else { 0 }
				src_is_ptr_carried := g.large_aggregate_stack_value_is_pointer(elem_id)
				trace_agg_copy := g.env_trace_agg_copy.len > 0
					&& (g.env_trace_agg_copy == '*' || g.cur_func_name == g.env_trace_agg_copy)
				if trace_agg_copy && src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
					src_typ := g.mod.type_store.types[src_typ_id]
					if src_typ.kind == .ptr_t {
						eprintln('ARM64 AGG insertvalue fn=${g.cur_func_name} idx=${idx} esz=${elem_size} src=${elem_id} src_typ=${src_typ_id} src_sz=${src_size} chunks=${src_chunks}')
					}
				}
				copy_chunks := if src_chunks > 0 && src_chunks < elem_chunks {
					src_chunks
				} else {
					elem_chunks
				}
				mut copied_elem := false

				// Avoid treating scalar mismatch values as aggregate addresses.
				if src_size > 0 && src_size <= 8 {
					g.load_val_to_reg(8, elem_id)
					g.emit_str_reg_offset(8, 29, result_offset + elem_off)
					copied_elem = true
				} else if elem_offset := g.stack_map[elem_id] {
					mut src_ptr_reg := 12
					if src_ptr_matches_elem || src_is_ptr_carried {
						g.emit_ldr_reg_offset(src_ptr_reg, 29, elem_offset)
					} else {
						g.emit_add_fp_imm(src_ptr_reg, elem_offset)
					}
					for i in 0 .. copy_chunks {
						g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
					copied_elem = true
				} else if src_reg := g.reg_map[elem_id] {
					if src_ptr_matches_elem || src_is_ptr_carried {
						for i in 0 .. copy_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_reg), u32(i)))
							g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
						}
						copied_elem = true
					}
				}
				if !copied_elem && g.is_sumtype_wrapper_struct_type(elem_typ_id) {
					// Keep full wrapper bytes for small sumtype fields carried through
					// scalar temporaries instead of truncating to the first word.
					mut src_ptr_reg := 12
					g.load_struct_src_address_to_reg(src_ptr_reg, elem_id, elem_typ_id,
						0)
					for i in 0 .. elem_chunks {
						g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
					copied_elem = true
				}
				if !copied_elem {
					// Best-effort fallback: store first word when source aggregate
					// backing storage is unavailable.
					g.load_val_to_reg(8, elem_id)
					g.emit_str_reg_offset(8, 29, result_offset + elem_off)
					copied_elem = true
					// Initialize remaining words to avoid leaking stale bytes when
					// multi-word values are only partially materialized.
					if elem_chunks > 1 {
						g.emit_mov_reg(10, 31)
						for i in 1 .. elem_chunks {
							g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
						}
					}
				}
				if copied_elem && copy_chunks < elem_chunks {
					g.emit_mov_reg(10, 31)
					for i in copy_chunks .. elem_chunks {
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
				}
			}
		}
		else {
			eprintln('arm64: unknown instruction ${int(op)} (${instr.selected_op})')
			exit(1)
		}
	}
}

fn (g Gen) selected_opcode(instr mir.Instruction) ssa.OpCode {
	if instr.selected_op == '' {
		return instr.op
	}
	suffix := if instr.selected_op.contains('.') {
		instr.selected_op.all_after('.')
	} else {
		instr.selected_op
	}
	return match suffix {
		'add_rr' { .add }
		'sub_rr' { .sub }
		'mul_rr' { .mul }
		'sdiv_rr' { .sdiv }
		'and_rr' { .and_ }
		'or_rr' { .or_ }
		'xor_rr' { .xor }
		'load_mr' { .load }
		'store_rm' { .store }
		'call' { .call }
		'call_indirect' { .call_indirect }
		'call_sret' { .call_sret }
		'ret' { .ret }
		'br' { .br }
		'jmp' { .jmp }
		'switch' { .switch_ }
		'copy' { .assign }
		else { instr.op }
	}
}

fn (g &Gen) has_function_named(name string) bool {
	return name in g.func_by_name
}

fn (g &Gen) get_function_by_name(name string) ?&mir.Function {
	if fi := g.func_by_name[name] {
		return &g.mod.funcs[fi]
	}
	return none
}

fn (g &Gen) call_param_type(instr mir.Instruction, arg_idx int) ?ssa.TypeID {
	if arg_idx < 0 || instr.operands.len == 0 {
		return none
	}
	fn_id := instr.operands[0]
	if fn_id <= 0 || fn_id >= g.mod.values.len {
		return none
	}
	fn_val := g.mod.values[fn_id]
	op := g.selected_opcode(instr)
	// Direct calls should follow the declared callee signature.
	// MIR transformations can leave a drifted `fn_val.typ` on direct call targets,
	// which would otherwise misclassify aggregate arguments.
	if op in [.call, .call_sret] && fn_val.name != '' {
		if f := g.get_function_by_name(fn_val.name) {
			if arg_idx < f.params.len {
				param_id := f.params[arg_idx]
				if param_id > 0 && param_id < g.mod.values.len {
					return g.mod.values[param_id].typ
				}
			}
			return none
		}
	}
	// For indirect calls (and as a fallback), use the function type attached to
	// the call target value.
	if fn_val.typ > 0 && fn_val.typ < g.mod.type_store.types.len {
		fn_typ := g.mod.type_store.types[fn_val.typ]
		if fn_typ.kind == .func_t && arg_idx < fn_typ.params.len {
			return fn_typ.params[arg_idx]
		}
	}
	return none
}

fn (g &Gen) call_result_type(instr mir.Instruction) ?ssa.TypeID {
	op := g.selected_opcode(instr)
	if op in [.call, .call_indirect] && instr.operands.len > 0 {
		fn_id := instr.operands[0]
		if fn_id > 0 && fn_id < g.mod.values.len {
			fn_val := g.mod.values[fn_id]
			// Keep direct calls aligned with declared function signatures first.
			if fn_val.name != '' {
				if f := g.get_function_by_name(fn_val.name) {
					return f.typ
				}
			}
			if fn_val.typ > 0 && fn_val.typ < g.mod.type_store.types.len {
				fn_typ := g.mod.type_store.types[fn_val.typ]
				if fn_typ.kind == .func_t && fn_typ.ret_type > 0 {
					return fn_typ.ret_type
				}
			}
		}
	}
	if instr.typ > 0 {
		return instr.typ
	}
	return none
}

fn (g &Gen) alloca_store_value_is_pointer_like(val_id int, for_alloca_id int, depth int) bool {
	mut seen_allocas := map[int]bool{}
	return g.alloca_store_value_is_pointer_like_inner(val_id, for_alloca_id, depth, mut
		seen_allocas)
}

fn (g &Gen) alloca_store_value_is_pointer_like_inner(val_id int, for_alloca_id int, depth int, mut seen_allocas map[int]bool) bool {
	if depth > 64 || val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[val.typ]
		if typ.kind in [.ptr_t, .func_t] {
			return true
		}
	}
	if val.kind == .constant {
		// `0` may be used as a null pointer value in mixed pointer/non-pointer phis.
		// Only treat as pointer-like if the constant type could hold a pointer
		// (pointer type or pointer-width integer). Small integer constants like
		// i8(0) or i16(0) are definitely NOT pointers.
		if val.name == '0' {
			if val.typ > 0 && val.typ < g.mod.type_store.types.len {
				ct := g.mod.type_store.types[val.typ]
				if ct.kind == .ptr_t {
					return true
				}
				if ct.kind == .int_t && ct.width >= 64 {
					return true
				}
			}
		}
		return false
	}
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.bitcast, .zext, .sext, .trunc {
			if instr.operands.len > 0 {
				return g.alloca_store_value_is_pointer_like_inner(instr.operands[0], for_alloca_id,
					depth + 1, mut seen_allocas)
			}
		}
		.assign {
			if instr.operands.len >= 2 {
				return g.alloca_store_value_is_pointer_like_inner(instr.operands[1], for_alloca_id,
					depth + 1, mut seen_allocas)
			}
		}
		.phi {
			if instr.operands.len == 0 {
				return false
			}
			for op_id in instr.operands {
				if !g.alloca_store_value_is_pointer_like_inner(op_id, for_alloca_id, depth + 1, mut
					seen_allocas) {
					return false
				}
			}
			return true
		}
		.load {
			if instr.operands.len > 0 {
				ptr_id := instr.operands[0]
				if ptr_id > 0 && ptr_id < g.mod.values.len {
					ptr_val := g.mod.values[ptr_id]
					if ptr_val.kind == .instruction {
						ptr_instr := g.mod.instrs[ptr_val.index]
						if g.selected_opcode(ptr_instr) == .alloca && ptr_id != for_alloca_id {
							if g.alloca_slot_stores_pointer_like_values_inner(ptr_id,
								ssa.TypeID(0), depth + 1, mut seen_allocas)
							{
								return true
							}
						}
					}
				}
			}
		}
		else {}
	}
	// Some SSA pseudo-values are mutable placeholders whose current value is
	// carried by `assign dest, src` instructions. Follow such assignments so
	// pointer payloads stored through these placeholders are detected.
	mut saw_assign_to_val := false
	for use_id in val.uses {
		if use_id <= 0 || use_id >= g.mod.values.len {
			continue
		}
		use_val := g.mod.values[use_id]
		if use_val.kind != .instruction {
			continue
		}
		use_instr := g.mod.instrs[use_val.index]
		if g.selected_opcode(use_instr) != .assign || use_instr.operands.len < 2
			|| use_instr.operands[0] != val_id {
			continue
		}
		saw_assign_to_val = true
		src_id := use_instr.operands[1]
		if src_id <= 0 || src_id >= g.mod.values.len {
			return false
		}
		if !g.alloca_store_value_is_pointer_like_inner(src_id, for_alloca_id, depth + 1, mut
			seen_allocas) {
			return false
		}
	}
	if saw_assign_to_val {
		return true
	}
	return false
}

fn (mut g Gen) alloca_slot_stores_pointer_like_values(alloca_id int, param_elem_typ_id ssa.TypeID) bool {
	if cached := g.alloca_ptr_cache[alloca_id] {
		return cached == 1
	}
	mut seen_allocas := map[int]bool{}
	result := g.alloca_slot_stores_pointer_like_values_inner(alloca_id, param_elem_typ_id,
		0, mut seen_allocas)
	g.alloca_ptr_cache[alloca_id] = if result { u8(1) } else { u8(2) }
	return result
}

fn (g &Gen) alloca_slot_stores_pointer_like_values_inner(alloca_id int, param_elem_typ_id ssa.TypeID, depth int, mut seen_allocas map[int]bool) bool {
	_ = param_elem_typ_id
	if depth > 64 {
		return false
	}
	if alloca_id in seen_allocas {
		return false
	}
	seen_allocas[alloca_id] = true
	defer {
		seen_allocas.delete(alloca_id)
	}
	if alloca_id <= 0 || alloca_id >= g.mod.values.len {
		return false
	}
	alloca_val := g.mod.values[alloca_id]
	if alloca_val.kind != .instruction {
		return false
	}
	alloca_instr := g.mod.instrs[alloca_val.index]
	if g.selected_opcode(alloca_instr) != .alloca {
		return false
	}
	mut saw_ptr_like_store := false
	mut saw_non_ptr_like_store := false
	for use_id in alloca_val.uses {
		if use_id <= 0 || use_id >= g.mod.values.len {
			continue
		}
		use_val := g.mod.values[use_id]
		if use_val.kind != .instruction {
			continue
		}
		use_instr := g.mod.instrs[use_val.index]
		if g.selected_opcode(use_instr) != .store || use_instr.operands.len < 2
			|| use_instr.operands[1] != alloca_id {
			continue
		}
		src_id := use_instr.operands[0]
		if src_id <= 0 || src_id >= g.mod.values.len {
			continue
		}
		if g.alloca_store_value_is_pointer_like_inner(src_id, alloca_id, depth + 1, mut
			seen_allocas)
		{
			saw_ptr_like_store = true
		} else {
			saw_non_ptr_like_store = true
		}
	}
	return saw_ptr_like_store && !saw_non_ptr_like_store
}

fn (g &Gen) scalar_value_is_pointer_payload(val_id int, depth int) bool {
	if depth > 8 || val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[val.typ]
		if typ.kind !in [.int_t, .ptr_t] {
			return false
		}
	} else {
		return false
	}
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.extractvalue {
			if instr.operands.len < 2 {
				return false
			}
			src_id := instr.operands[0]
			idx_id := instr.operands[1]
			if src_id <= 0 || src_id >= g.mod.values.len || idx_id <= 0
				|| idx_id >= g.mod.values.len {
				return false
			}
			src_typ_id := g.mod.values[src_id].typ
			if src_typ_id <= 0 || src_typ_id >= g.mod.type_store.types.len {
				return false
			}
			src_typ := g.mod.type_store.types[src_typ_id]
			if src_typ.kind != .struct_t {
				return false
			}
			idx_val := g.mod.values[idx_id]
			idx := idx_val.name.int()
			if idx < 0 || idx >= src_typ.field_names.len {
				return false
			}
			// Sumtype payloads are extracted from `_data` scalar fields and are
			// represented as pointer-width integers in SSA.
			return src_typ.field_names[idx] == '_data'
		}
		.get_element_ptr {
			if instr.operands.len < 2 {
				return false
			}
			base_id := instr.operands[0]
			idx_id := instr.operands[1]
			if idx_id <= 0 || idx_id >= g.mod.values.len {
				return false
			}
			idx_val := g.mod.values[idx_id]
			if idx_val.kind != .constant {
				return false
			}
			idx := idx_val.name.int()
			if idx == 0 {
				// Preserve classification through no-op GEP hops.
				return g.scalar_value_is_pointer_payload(base_id, depth + 1)
			}
			if base_id <= 0 || base_id >= g.mod.values.len {
				return false
			}
			base_typ_id := g.mod.values[base_id].typ
			if base_typ_id <= 0 || base_typ_id >= g.mod.type_store.types.len {
				return false
			}
			base_typ := g.mod.type_store.types[base_typ_id]
			if base_typ.kind != .ptr_t || base_typ.elem_type <= 0
				|| base_typ.elem_type >= g.mod.type_store.types.len {
				return false
			}
			elem_typ := g.mod.type_store.types[base_typ.elem_type]
			if elem_typ.kind != .struct_t || idx < 0 || idx >= elem_typ.field_names.len {
				return false
			}
			// GEP to sumtype wrapper `_data` field keeps pointer payload semantics.
			return elem_typ.field_names[idx] == '_data'
		}
		.load {
			// Payload pointers often flow through `load(bitcast(extractvalue(...,_data)))`.
			// Follow the source pointer to classify only those flows as payload words.
			if instr.operands.len < 1 {
				return false
			}
			return g.scalar_value_is_pointer_payload(instr.operands[0], depth + 1)
		}
		.bitcast, .assign {
			if instr.operands.len == 0 {
				return false
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			return g.scalar_value_is_pointer_payload(instr.operands[src_idx], depth + 1)
		}
		.phi {
			if instr.operands.len == 0 {
				return false
			}
			for op_id in instr.operands {
				if !g.scalar_value_is_pointer_payload(op_id, depth + 1) {
					return false
				}
			}
			return true
		}
		else {}
	}
	return false
}

fn (mut g Gen) get_operand_reg(val_id int, fallback int) int {
	// If value is in a register, return it
	if r := g.reg_map[val_id] {
		return r
	}
	// Otherwise load it into fallback
	g.load_val_to_reg(fallback, val_id)
	return fallback
}

// Returns the number of registers a call argument occupies on ARM64.
// Struct args 9-16 bytes use 2 consecutive registers; all others use 1.
fn (mut g Gen) call_arg_reg_count(val_id int, arg_idx int, instr mir.Instruction) int {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	trace_callcount := g.env_trace_callcount.len > 0
		&& (g.env_trace_callcount == '*' || g.cur_func_name == g.env_trace_callcount)
	if is_indirect {
		if trace_callcount {
			eprintln('ARM64 CALLCOUNT fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} indirect=1 count=1')
		}
		return 1 // pointer
	}
	// Prefer the callee signature over the SSA value type when deciding
	// how many argument registers are needed. Some transformed calls carry
	// a widened/narrowed SSA value type that does not match the declared param.
	if param_typ_id := g.call_param_type(instr, arg_idx) {
		if param_typ_id > 0 && param_typ_id < g.mod.type_store.types.len {
			param_typ := g.mod.type_store.types[param_typ_id]
			if param_typ.kind == .struct_t {
				size := g.type_size(param_typ_id)
				if size > 8 && size <= 16 {
					if trace_callcount {
						eprintln('ARM64 CALLCOUNT fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} source=param typ=${param_typ_id} kind=${int(param_typ.kind)} size=${size} count=2')
					}
					return 2
				}
			}
		}
	}
	if val_id <= 0 || val_id >= g.mod.values.len {
		return 1
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		val_typ := g.mod.type_store.types[val.typ]
		if val_typ.kind == .struct_t {
			size := g.type_size(val.typ)
			if size > 8 && size <= 16 {
				if trace_callcount {
					eprintln('ARM64 CALLCOUNT fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} source=val typ=${val.typ} kind=${int(val_typ.kind)} size=${size} count=2')
				}
				return 2
			}
		}
	}
	if trace_callcount {
		mut typ_id := ssa.TypeID(0)
		mut kind := -1
		mut size := 0
		if val_id > 0 && val_id < g.mod.values.len {
			typ_id = g.mod.values[val_id].typ
			if typ_id > 0 && typ_id < g.mod.type_store.types.len {
				kind = int(g.mod.type_store.types[typ_id].kind)
				size = g.type_size(typ_id)
			}
		}
		eprintln('ARM64 CALLCOUNT fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} typ=${typ_id} kind=${kind} size=${size} count=1')
	}
	return 1
}

// Loads one 8-byte word of a small struct argument (9-16 bytes) into `dest_reg`.
fn (mut g Gen) load_struct_arg_word_to_reg(dest_reg int, val_id int, word_idx int, expected_struct_typ ssa.TypeID, call_callee_id int) {
	if g.is_known_zero_value(val_id, 0) {
		g.emit_mov_reg(dest_reg, 31)
		return
	}
	// Get the source address of the struct value.
	// expected_struct_typ lets us correctly handle pointer-backed struct values.
	mut addr_reg := 9
	if addr_reg == dest_reg {
		addr_reg = 10
		if addr_reg == dest_reg {
			addr_reg = 11
		}
	}
	if expected_struct_typ > 0 {
		g.load_struct_src_address_to_reg(addr_reg, val_id, expected_struct_typ, call_callee_id)
	} else {
		g.load_address_of_val_to_reg(addr_reg, val_id)
	}
	g.emit(asm_ldr_imm(Reg(dest_reg), Reg(addr_reg), u32(word_idx)))
}

// Loads a multi-register struct argument (9-16 bytes) into consecutive regs.
fn (mut g Gen) load_struct_arg_to_regs(start_reg int, val_id int, expected_struct_typ ssa.TypeID, call_callee_id int) {
	g.load_struct_arg_word_to_reg(start_reg, val_id, 0, expected_struct_typ, call_callee_id)
	g.load_struct_arg_word_to_reg(start_reg + 1, val_id, 1, expected_struct_typ, call_callee_id)
}

fn (mut g Gen) load_call_arg_to_reg(reg int, val_id int, arg_idx int, instr mir.Instruction) {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	trace_callarg := g.env_trace_callarg.len > 0
		&& (g.env_trace_callarg == '*' || g.cur_func_name == g.env_trace_callarg)
	if !is_indirect {
		if param_typ_id := g.call_param_type(instr, arg_idx) {
			if param_typ_id > 0 && param_typ_id < g.mod.type_store.types.len && val_id > 0
				&& val_id < g.mod.values.len {
				param_typ := g.mod.type_store.types[param_typ_id]
				arg_val := g.mod.values[val_id]
				if trace_callarg {
					mut arg_op := 'na'
					if arg_val.kind == .instruction {
						arg_op = '${g.selected_opcode(g.mod.instrs[arg_val.index])}'
					}
					eprintln('ARM64 CALLARG fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} param_typ=${param_typ_id} val_typ=${arg_val.typ} val_kind=${int(arg_val.kind)} arg_op=${arg_op} indirect=${is_indirect}')
				}
				if arg_val.typ > 0 && arg_val.typ < g.mod.type_store.types.len {
					arg_typ := g.mod.type_store.types[arg_val.typ]
					if param_typ.kind == .ptr_t && arg_val.kind == .instruction {
						arg_instr := g.mod.instrs[arg_val.index]
						if g.selected_opcode(arg_instr) == .alloca && arg_typ.kind == .ptr_t {
							slot_has_ptr := g.alloca_slot_stores_pointer_like_values(val_id,
								param_typ.elem_type)
							if trace_callarg {
								eprintln('ARM64 CALLARG alloca-ptr fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} slot_has_ptr=${slot_has_ptr}')
							}
							if slot_has_ptr {
								g.load_val_to_reg(reg, val_id)
								g.emit(asm_ldr(Reg(reg), Reg(reg)))
							} else {
								g.load_address_of_val_to_reg(reg, val_id)
							}
							return
						}
					}
					// Some pointer arguments come from alloca-backed pointer-variable slots
					// (slot element is often i64 in MIR). For `&T` params, pass the slot
					// contents (`*slot`) rather than the slot address.
					if param_typ.kind == .ptr_t && param_typ.elem_type > 0
						&& param_typ.elem_type < g.mod.type_store.types.len
						&& arg_val.kind == .instruction {
						param_elem_typ := g.mod.type_store.types[param_typ.elem_type]
						arg_instr := g.mod.instrs[arg_val.index]
						if param_elem_typ.kind != .ptr_t && g.selected_opcode(arg_instr) == .alloca {
							slot_has_ptr := g.alloca_slot_stores_pointer_like_values(val_id,
								param_typ.elem_type)
							if trace_callarg {
								eprintln('ARM64 CALLARG slot-check fn=${g.cur_func_name} arg_idx=${arg_idx} val=${val_id} param_typ=${param_typ_id} param_elem_typ=${param_typ.elem_type} alloca_slot_ptr=${slot_has_ptr}')
							}
							if slot_has_ptr {
								g.load_val_to_reg(reg, val_id)
								g.emit(asm_ldr(Reg(reg), Reg(reg)))
								return
							}
						}
					}
					// Mutable locals are often represented as `alloca` pointers. When the
					// slot stores a pointer and the callee expects that pointer type, pass
					// the loaded pointer value (not the alloca slot address).
					if param_typ.kind == .ptr_t && arg_val.kind == .instruction {
						arg_instr := g.mod.instrs[arg_val.index]
						if arg_instr.op == .alloca && arg_typ.kind == .ptr_t
							&& arg_typ.elem_type > 0
							&& arg_typ.elem_type < g.mod.type_store.types.len {
							slot_typ := g.mod.type_store.types[arg_typ.elem_type]
							if slot_typ.kind == .ptr_t && arg_typ.elem_type == param_typ_id {
								g.load_val_to_reg(reg, val_id)
								g.emit(asm_ldr(Reg(reg), Reg(reg)))
								return
							}
						}
					}
					// Some transformed pointer locals are materialized in generic i64-sized
					// alloca slots (slot type appears as int_t) even though stores carry
					// `&Struct`/`&Array` values. Treat these slots as pointer containers and
					// pass the loaded pointer value.
					if param_typ.kind == .ptr_t && arg_val.kind == .instruction {
						arg_instr := g.mod.instrs[arg_val.index]
						if arg_instr.op == .alloca && arg_typ.kind == .ptr_t
							&& arg_typ.elem_type > 0
							&& arg_typ.elem_type < g.mod.type_store.types.len
							&& param_typ.elem_type > 0
							&& param_typ.elem_type < g.mod.type_store.types.len {
							slot_elem_typ := g.mod.type_store.types[arg_typ.elem_type]
							param_elem_typ := g.mod.type_store.types[param_typ.elem_type]
							if slot_elem_typ.kind == .int_t && g.type_size(arg_typ.elem_type) == 8
								&& param_elem_typ.kind in [.struct_t, .array_t] {
								g.load_val_to_reg(reg, val_id)
								g.emit(asm_ldr(Reg(reg), Reg(reg)))
								return
							}
						}
					}
					// Loop-carried pointer variables can be represented as pointers
					// to pointer slots (typically from alloca-backed mutable vars).
					// When the callee expects `&T` and we have `&&T`, pass `*arg`.
					if param_typ.kind == .ptr_t && arg_typ.kind == .ptr_t && arg_typ.elem_type > 0
						&& arg_typ.elem_type < g.mod.type_store.types.len {
						arg_elem_typ := g.mod.type_store.types[arg_typ.elem_type]
						if arg_elem_typ.kind == .ptr_t
							&& arg_elem_typ.elem_type == param_typ.elem_type {
							g.load_val_to_reg(reg, val_id)
							g.emit(asm_ldr(Reg(reg), Reg(reg)))
							return
						}
					}
					// If call lowering asks for a pointer parameter but the argument is
					// a struct value, pass the address of the struct instead of
					// loading its first field. This handles call results, load results,
					// and other struct-producing instructions.
					// Applies to ALL struct sizes (e.g., sumtype=16 bytes, string=24 bytes).
					if param_typ.kind == .ptr_t && arg_typ.kind == .struct_t {
						arg_size := g.type_size(arg_val.typ)
						if arg_size > 16 {
							// Large aggregates are address-carried in ARM64 codegen.
							// `load_val_to_reg` preserves whether the producer already
							// materialized a pointer, avoiding accidental pointer-to-pointer
							// arguments when the SSA value is stack-spilled.
							g.load_val_to_reg(reg, val_id)
						} else {
							g.load_address_of_val_to_reg(reg, val_id)
						}
						return
					}
				}
			}
		}
	}
	if is_indirect {
		if param_typ_id := g.call_param_type(instr, arg_idx) {
			g.load_struct_src_address_to_reg(reg, val_id, param_typ_id, instr.operands[0])
		} else {
			g.load_address_of_val_to_reg(reg, val_id)
		}
		return
	}
	g.load_val_to_reg(reg, val_id)
}

fn (mut g Gen) canonicalize_narrow_int_result(reg int, typ_id ssa.TypeID) {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return
	}
	typ := g.mod.type_store.types[typ_id]
	if typ.kind != .int_t || typ.width <= 0 || typ.width >= 64 {
		return
	}
	if typ.width == 1 {
		// i1 arithmetic is modulo 2; force truncation to the low bit.
		g.emit_mov_imm64(11, 1)
		g.emit(asm_and(Reg(reg), Reg(reg), Reg(11)))
	} else if typ.is_unsigned {
		// Unsigned narrow integers: zero-extend by masking.
		if typ.width <= 32 {
			mask := (u64(1) << typ.width) - 1
			g.emit_mov_imm64(11, i64(mask))
			g.emit(asm_and(Reg(reg), Reg(reg), Reg(11)))
		}
		// Unsigned 33-63 bit: no action needed (upper bits already zero)
	} else {
		// For signed types, sign-extend via LSL + ASR
		shift := 64 - typ.width
		mut shreg := 11
		if reg == shreg {
			shreg = 12
		}
		g.emit_mov_imm64(shreg, shift)
		g.emit(asm_lslv(Reg(reg), Reg(reg), Reg(shreg)))
		g.emit(asm_asrv(Reg(reg), Reg(reg), Reg(shreg)))
	}
}

fn (g &Gen) is_i1_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	return typ.kind == .int_t && typ.width == 1
}

fn (g &Gen) types_type_variant_name_from_ssa_typ(typ_id ssa.TypeID) ?string {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return none
	}
	typ := g.mod.type_store.types[typ_id]
	if typ.kind == .struct_t {
		if typ.field_names.len == 2 && typ.field_names[0] == 'name'
			&& typ.field_names[1] == 'base_type' {
			return 'Alias'
		}
		if typ.field_names.len == 2 && typ.field_names[0] == 'len'
			&& typ.field_names[1] == 'elem_type' {
			return 'ArrayFixed'
		}
		if typ.field_names.len == 1 && typ.field_names[0] == 'elem_type' {
			return 'Array'
		}
		if typ.field_names.len == 1 && typ.field_names[0] == 'base_type' {
			return 'Pointer'
		}
		return none
	}
	if typ.kind == .int_t && typ.width == 8 {
		return 'String'
	}
	return none
}

fn (g &Gen) sumtype_variant_tag_index(sumtype_name string, module_name string, variant_name string) ?int {
	if sum_obj := g.lookup_type_from_env(sumtype_name, module_name) {
		if sum_obj is types.SumType {
			for i, variant in sum_obj.get_variants() {
				full_name := types.type_name(variant)
				short_name := if full_name.contains('__') {
					full_name.all_after_last('__')
				} else {
					full_name
				}
				if full_name == variant_name || short_name == variant_name {
					return i
				}
			}
		}
	}
	return none
}

fn (g &Gen) try_match_types_type_ischeck_rhs(rhs_id int) ?string {
	if rhs_id <= 0 || rhs_id >= g.mod.values.len {
		return none
	}
	rhs_val := g.mod.values[rhs_id]
	if rhs_val.kind != .instruction {
		return none
	}
	rhs_instr := g.mod.instrs[rhs_val.index]
	if g.selected_opcode(rhs_instr) != .extractvalue || rhs_instr.operands.len < 2 {
		return none
	}
	idx_id := rhs_instr.operands[1]
	if idx_id <= 0 || idx_id >= g.mod.values.len {
		return none
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant || idx_val.name != '0' {
		return none
	}
	tuple_id := rhs_instr.operands[0]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len {
		return none
	}
	tuple_val := g.mod.values[tuple_id]
	if tuple_val.kind != .constant || tuple_val.name != '0' {
		return none
	}
	if variant_name := g.types_type_variant_name_from_ssa_typ(rhs_val.typ) {
		return variant_name
	}
	return none
}

fn (mut g Gen) try_emit_types_type_ischeck_add(dest_reg int, lhs_reg int, val_id int, instr mir.Instruction) bool {
	if instr.operands.len < 2 || !g.is_i1_type(g.mod.values[val_id].typ) {
		return false
	}
	lhs_id := instr.operands[0]
	if lhs_id <= 0 || lhs_id >= g.mod.values.len {
		return false
	}
	lhs_typ_id := g.mod.values[lhs_id].typ
	if !g.is_sumtype_wrapper_struct_type(lhs_typ_id) {
		return false
	}
	variant_name := g.try_match_types_type_ischeck_rhs(instr.operands[1]) or { return false }
	tag_idx := g.sumtype_variant_tag_index('Type', 'types', variant_name) or { return false }
	mut tag_reg := 10
	if tag_reg == lhs_reg || tag_reg == dest_reg {
		tag_reg = 11
		if tag_reg == lhs_reg || tag_reg == dest_reg {
			tag_reg = 12
		}
	}
	g.emit_mov_imm64(tag_reg, i64(tag_idx))
	g.emit(asm_cmp_reg(Reg(lhs_reg), Reg(tag_reg)))
	g.emit(asm_cset_eq(Reg(dest_reg)))
	return true
}

fn (g &Gen) call_targets_current_function(callee_id int) bool {
	if callee_id <= 0 || callee_id >= g.mod.values.len {
		return false
	}
	callee := g.mod.values[callee_id]
	return callee.name != '' && callee.name == g.cur_func_name
}

fn (mut g Gen) load_struct_src_address_to_reg(reg int, val_id int, expected_struct_typ ssa.TypeID, call_callee_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	if expected_struct_typ > 0 && expected_struct_typ < g.mod.type_store.types.len {
		expected_typ := g.mod.type_store.types[expected_struct_typ]
		// Some call sites pass unwrapped sumtype words (`_tag`/`_data`) while the
		// callee expects the full wrapper struct. Re-associate such values back to
		// their wrapper so argument passing uses stable `{_tag, _data}` storage.
		if expected_typ.kind == .struct_t && g.is_sumtype_wrapper_struct_type(expected_struct_typ) {
			trace_struct_addr := g.env_trace_struct_addr.len > 0
				&& (g.env_trace_struct_addr == '*' || g.cur_func_name == g.env_trace_struct_addr)
			// A raw `_data` word extracted from a wrapper can represent an address to
			// bytes whose prefix is the desired wrapper argument (for example, when
			// selecting a nested `ast.Expr` field). In that case pass the payload
			// pointer directly as the struct source address.
			if tuple_id := g.sumtype_extractvalue_data_tuple_id(val_id, expected_struct_typ) {
				if trace_struct_addr {
					eprintln('ARM64 STRUCT_ADDR fn=${g.cur_func_name} mode=data_word_payload val=${val_id} wrapper=${tuple_id} expected=${expected_struct_typ}')
				}
				if g.load_sumtype_data_ptr_to_reg(reg, tuple_id) {
					return
				}
			}
			if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(val_id, expected_struct_typ,
				0)
			{
				if trace_struct_addr {
					eprintln('ARM64 STRUCT_ADDR fn=${g.cur_func_name} mode=unwrap_match val=${val_id} wrapper=${wrapper_id} expected=${expected_struct_typ}')
				}
				g.load_address_of_val_to_reg(reg, wrapper_id)
				return
			}
			if wrapper_id := g.sumtype_wrapper_source_from_tag_word(val_id) {
				if wrapper_id > 0 && wrapper_id < g.mod.values.len
					&& g.mod.values[wrapper_id].typ == expected_struct_typ {
					// `_tag` extracted from the same wrapper type: recover that wrapper.
					// Passing payload bytes here corrupts `{_tag, _data}` arguments.
					if trace_struct_addr {
						eprintln('ARM64 STRUCT_ADDR fn=${g.cur_func_name} mode=tag_match val=${val_id} wrapper=${wrapper_id} expected=${expected_struct_typ}')
					}
					g.load_address_of_val_to_reg(reg, wrapper_id)
					return
				}
				// If wrapper types differ and there is a dominating variant guard,
				// the payload can carry the expected nested wrapper bytes.
				if wrapper_id > 0 && wrapper_id < g.mod.values.len
					&& g.sumtype_tag_word_has_variant_guard(val_id) {
					if trace_struct_addr {
						eprintln('ARM64 STRUCT_ADDR fn=${g.cur_func_name} mode=tag_payload_guarded val=${val_id} wrapper=${wrapper_id} wrapper_typ=${g.mod.values[wrapper_id].typ} expected=${expected_struct_typ}')
					}
					if g.load_sumtype_data_ptr_to_reg(reg, wrapper_id) {
						return
					}
				}
			}
		}
		if expected_typ.kind == .struct_t && g.type_size(expected_struct_typ) > 16
			&& g.value_is_large_struct(val_id) {
			// Large struct values may already be represented as data pointers.
			// Preserve that representation for indirect ABI arguments.
			g.load_val_to_reg(reg, val_id)
			return
		}
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		val_typ := g.mod.type_store.types[val.typ]
		if val_typ.kind == .ptr_t && val_typ.elem_type == expected_struct_typ {
			g.load_val_to_reg(reg, val_id)
			return
		}
	}
	g.load_address_of_val_to_reg(reg, val_id)
}

fn (mut g Gen) load_address_of_val_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	// string_literal values need full materialization (create cstring data,
	// store str/len/is_lit to stack) before their address can be taken.
	// load_val_to_reg handles this and returns the struct address.
	if val.kind == .string_literal {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		if instr.op == .alloca {
			if data_off := g.alloca_offsets[val_id] {
				g.emit_add_fp_imm(reg, data_off)
			} else {
				g.load_val_to_reg(reg, val_id)
			}
			return
		}
		if instr.op == .load && instr.operands.len > 0 && val.typ > 0
			&& val.typ < g.mod.type_store.types.len {
			loaded_typ := g.mod.type_store.types[val.typ]
			if loaded_typ.kind == .struct_t && val_id !in g.stack_map {
				// Only forward the original pointer when this load value has no
				// materialized stack storage. Otherwise use the loaded by-value slot.
				g.load_val_to_reg(reg, instr.operands[0])
				return
			}
		}
	}
	if offset := g.stack_map[val_id] {
		g.emit_add_fp_imm(reg, offset)
		return
	}
	// Value already represents or contains a pointer.
	g.load_val_to_reg(reg, val_id)
}

// load_float_operand loads a value into a float register (d0-d7).
// For constants, parses the float value and loads via literal pool or immediate.
// For memory values, loads from stack into integer reg then moves to float reg.
fn (mut g Gen) load_float_operand(val_id int, dreg int) {
	val := g.mod.values[val_id]
	is_f32 := val.typ > 0 && val.typ < g.mod.type_store.types.len
		&& g.mod.type_store.types[val.typ].kind == .float_t
		&& g.mod.type_store.types[val.typ].width == 32
	if val.kind == .constant {
		// Parse float constant and load into float register
		if is_f32 {
			// f32 constant: parse as f64, convert to f32, load via s-register
			f_val := f32(val.name.f64())
			bits := *unsafe { &u32(&f_val) }
			g.emit_mov_imm64(8, i64(bits))
			g.emit(asm_fmov_s_w(dreg, Reg(8)))
			g.emit(asm_fcvt_d_s(dreg, dreg))
		} else {
			// f64 constant: load 64-bit bit pattern
			f_val := val.name.f64()
			bits := *unsafe { &u64(&f_val) }
			g.emit_mov_imm64(8, i64(bits))
			g.emit(asm_fmov_d_x(dreg, Reg(8)))
		}
	} else {
		if is_f32 {
			// f32 from stack: load 32-bit value, move to s-register, convert to double
			g.load_val_to_reg(8, val_id)
			g.emit(asm_fmov_s_w(dreg, Reg(8)))
			g.emit(asm_fcvt_d_s(dreg, dreg))
		} else {
			// f64 from stack: load 64-bit value, move to d-register
			g.load_val_to_reg(8, val_id)
			g.emit(asm_fmov_d_x(dreg, Reg(8)))
		}
	}
}

// Get constant integer value with caching to avoid repeated string parsing
fn (mut g Gen) get_const_int(val_id int) i64 {
	if cached := g.const_cache[val_id] {
		return cached
	}
	// Parse as u64 first to handle values with bit 63 set that would overflow i64 parsing.
	// Then reinterpret as i64 to preserve the bit pattern.
	name := g.mod.values[val_id].name
	int_val := if name.len > 2 && name[0] == `0` && (name[1] == `x` || name[1] == `X`) {
		i64(name.u64())
	} else if name.len > 0 && name[0] != `-` {
		// Parse unsigned to handle values > i64.max (e.g., 14695981039346656037)
		i64(name.u64())
	} else {
		name.i64()
	}
	g.const_cache[val_id] = int_val
	return int_val
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	// Handle func_ref early: function pointers use ADRP+ADD regardless of their declared type
	if val.kind == .func_ref {
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
		return
	}
	if val.typ < 0 || val.typ >= g.mod.type_store.types.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val_typ := g.mod.type_store.types[val.typ]
	if val_typ.kind == .void_t {
		g.emit_mov_imm64(reg, 0)
		return
	}
	if val.kind == .constant {
		// Quick check: if first char is '"' it's a string
		if val.name.len > 0 && val.name[0] == `"` {
			raw_content := val.name.trim('"')
			// Handle escape sequences
			mut str_content := []u8{}
			mut i := 0
			for i < raw_content.len {
				if raw_content[i] == `\\` && i + 1 < raw_content.len {
					next_char := raw_content[i + 1]
					match next_char {
						`n` { str_content << 10 }
						`t` { str_content << 9 }
						`r` { str_content << 13 }
						`\\` { str_content << 92 }
						`"` { str_content << 34 }
						`'` { str_content << 39 }
						else { str_content << next_char }
					}
					i += 2
				} else {
					str_content << raw_content[i]
					i++
				}
			}

			str_offset := g.macho.str_data.len
			g.macho.str_data << str_content
			g.macho.str_data << 0

			sym_idx := g.macho.add_symbol('L_str_${str_offset}', u64(str_offset), false,
				2)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
		} else if val_typ.kind == .float_t {
			// Float constant: load IEEE 754 bit pattern, not truncated integer
			if val_typ.width == 32 {
				// f32 constant: store 32-bit IEEE 754 pattern
				f_val := f32(val.name.f64())
				bits := *unsafe { &u32(&f_val) }
				g.emit_mov_imm64(reg, i64(bits))
			} else {
				f_val := val.name.f64()
				bits := *unsafe { &u64(&f_val) }
				g.emit_mov_imm64(reg, i64(bits))
			}
		} else {
			int_val := g.get_const_int(val_id)
			g.emit_mov_imm64(reg, int_val)
		}
	} else if val.kind == .global {
		// Check if this is an external global (needs GOT access)
		mut is_external := false
		if gi := g.global_by_name[val.name] {
			is_external = g.mod.globals[gi].linkage == .external
		}

		if is_external {
			// External global: load address through GOT
			sym_idx := g.macho.add_undefined('_' + val.name)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_got_load_page21,
				true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_got_load_pageoff12,
				false)
			g.emit(asm_ldr_pageoff(Reg(reg)))
		} else {
			// Local global: direct address
			sym_idx := g.macho.add_undefined('_' + val.name)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
		}
	} else if val.kind == .string_literal {
		// String literal: create string struct { str, len, is_lit } on stack
		// val.name contains the string content, val.index contains the length

		// Get stack slot for this string struct
		base_offset := g.stack_map[val_id]
		trace_strlit := g.env_trace_strlit != ''

		// Check if we've already materialized this string literal in this function.
		if _ := g.string_literal_offsets[val_id] {
			// Already materialized - just load pointer to the struct
			g.emit_add_fp_imm(reg, base_offset)
			if trace_strlit && g.cur_func_name == 'transformer__Transformer__transform_if_expr' {
				eprintln('ARM64 STRLIT reuse fn=${g.cur_func_name} val=${val_id} typ=${val.typ} base=${base_offset}')
			}
		} else {
			// First time - create string data and initialize struct
			str_content := val.name
			str_len := val.index

			// Create the string data in cstring section (deduplicate identical strings)
			str_offset2 := if existing := g.string_data_cache[str_content] {
				existing
			} else {
				offset := g.macho.str_data.len
				g.macho.str_data << str_content.bytes()
				g.macho.str_data << 0 // null terminator
				g.string_data_cache[str_content] = offset
				offset
			}

			// Track that we've materialized this string literal
			g.string_literal_offsets[val_id] = str_offset2

			str_off, _ := g.struct_field_offset_and_size(val.typ, 0, 0, 8)
			len_off, len_size := g.struct_field_offset_and_size(val.typ, 1, 8, 8)
			is_lit_off, is_lit_size := g.struct_field_offset_and_size(val.typ, 2, 16,
				8)
			if trace_strlit && g.cur_func_name == 'transformer__Transformer__transform_if_expr' {
				eprintln('ARM64 STRLIT init fn=${g.cur_func_name} val=${val_id} typ=${val.typ} base=${base_offset} str_off=${str_off} len_off=${len_off} len_size=${len_size} lit_off=${is_lit_off} lit_size=${is_lit_size} slen=${str_len} name=`${str_content}`')
			}

			// Store str pointer: load address of string data
			sym_idx := g.macho.add_symbol('L_str_${str_offset2}', u64(str_offset2), false,
				2)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
			g.emit_str_reg_offset_sized(reg, 29, base_offset + str_off, 8)

			// Store len
			g.emit_mov_imm64(9, str_len)
			g.emit_str_reg_offset_sized(9, 29, base_offset + len_off, len_size)

			// Store is_lit = 1
			g.emit_mov_imm64(10, 1)
			g.emit_str_reg_offset_sized(10, 29, base_offset + is_lit_off, is_lit_size)

			// Load pointer to string struct into reg
			g.emit_add_fp_imm(reg, base_offset)
		}
	} else if val.kind == .c_string_literal {
		// C string literal: just a raw char* pointer to string data
		// Process C escape sequences (\n, \t, etc.) into actual bytes
		raw_bytes := val.name.bytes()
		str_offset2 := g.macho.str_data.len
		mut i2 := 0
		for i2 < raw_bytes.len {
			if raw_bytes[i2] == `\\` && i2 + 1 < raw_bytes.len {
				match raw_bytes[i2 + 1] {
					`n` { g.macho.str_data << 0x0A } // newline
					`t` { g.macho.str_data << 0x09 } // tab
					`r` { g.macho.str_data << 0x0D } // carriage return
					`0` { g.macho.str_data << 0x00 } // null
					`\\` { g.macho.str_data << 0x5C } // backslash
					`'` { g.macho.str_data << 0x27 } // single quote
					`"` { g.macho.str_data << 0x22 } // double quote
					else { g.macho.str_data << raw_bytes[i2 + 1] }
				}
				i2 += 2
			} else {
				g.macho.str_data << raw_bytes[i2]
				i2++
			}
		}
		g.macho.str_data << 0 // null terminator
		sym_idx := g.macho.add_symbol('L_cstr_${str_offset2}', u64(str_offset2), false,
			2)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
	} else if val.kind == .func_ref {
		// Function pointer reference - load address of the function
		// Add symbol and use ADRP + ADD to get the address
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
	} else {
		// Handles .instruction, .argument, etc.
		// For large struct types (> 16 bytes like string), ARM64 ABI requires
		// passing by pointer, so load the address instead of the value
		typ := val_typ
		val_size := g.type_size(val.typ)
		if (typ.kind == .struct_t || typ.kind == .array_t) && val_size > 16 {
			// Large structs/arrays can be materialized either by-value (slot contains bytes)
			// or indirectly (slot contains a pointer to bytes). Preserve the producer's
			// representation when loading from stack.
			if reg_idx := g.reg_map[val_id] {
				if reg_idx != reg {
					g.emit_mov_reg(reg, reg_idx)
				}
			} else if offset := g.stack_map[val_id] {
				if typ.kind == .array_t {
					// Array values in stack slots are always by-value (data bytes directly).
					g.emit_add_fp_imm(reg, offset)
				} else if g.large_struct_stack_value_is_pointer(val_id) {
					g.emit_ldr_reg_offset(reg, 29, offset)
				} else {
					g.emit_add_fp_imm(reg, offset)
				}
			} else {
				g.emit_mov_imm64(reg, 0)
			}
		} else if reg_idx := g.reg_map[val_id] {
			if reg_idx != reg {
				g.emit_mov_reg(reg, reg_idx)
			}
		} else {
			if offset := g.stack_map[val_id] {
				if g.env_trace_load.len > 0
					&& (g.env_trace_load == '*' || g.cur_func_name == g.env_trace_load) {
					eprintln('LOAD_VAL fn=${g.cur_func_name} val=${val_id} off=${offset} reg=${reg} kind=${val.kind} name=${val.name}')
				}
				g.emit_ldr_reg_offset(reg, 29, offset)
				g.canonicalize_narrow_int_result(reg, val.typ)
			} else {
				g.emit_mov_imm64(reg, 0)
			}
		}
	}
}

fn (mut g Gen) load_fnptr_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	if val.kind in [.constant, .func_ref, .global] {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg(reg, reg_idx)
		}
		return
	}
	if offset := g.stack_map[val_id] {
		g.emit_ldr_reg_offset(reg, 29, offset)
	} else {
		g.emit_mov_imm64(reg, 0)
	}
}

fn (mut g Gen) store_reg_to_val(reg int, val_id int) {
	mut stored_reg := reg
	trace_storeval := g.env_trace_storeval.len > 0
		&& (g.env_trace_storeval == '*' || g.cur_func_name == g.env_trace_storeval)
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg(reg_idx, reg)
		}
		stored_reg = reg_idx
	}
	if offset := g.stack_map[val_id] {
		if val_id > 0 && val_id < g.mod.values.len {
			val_typ_id := g.mod.values[val_id].typ
			if val_typ_id > 0 && val_typ_id < g.mod.type_store.types.len {
				val_typ := g.mod.type_store.types[val_typ_id]
				if val_typ.kind in [.struct_t, .array_t] {
					val_size := g.type_size(val_typ_id)
					if val_size > 8 && val_size <= 16 {
						// For 9-16 byte structs, the register holds a pointer to the
						// stack location containing the struct data. Copy it.
						if trace_storeval {
							eprintln('ARM64 STOREVAL ptr-copy fn=${g.cur_func_name} val=${val_id} typ=${val_typ_id}/${val_typ.kind} size=${val_size} reg=${stored_reg} off=${offset}')
						}
						g.copy_ptr_to_fp_bytes(stored_reg, offset, val_size)
						return
					}
				}
				if val_typ.kind == .struct_t && g.type_size(val_typ_id) <= 8 {
					g.emit_str_reg_offset(stored_reg, 29, offset)
					return
				}
			}
		}
		if val_id !in g.reg_map {
			g.emit_str_reg_offset(stored_reg, 29, offset)
		}
	}
}

fn (mut g Gen) store_gep_result_from_addr(addr_reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.store_reg_to_val(addr_reg, val_id)
		return
	}
	val_typ_id := g.mod.values[val_id].typ
	if val_typ_id <= 0 || val_typ_id >= g.mod.type_store.types.len {
		g.store_reg_to_val(addr_reg, val_id)
		return
	}
	val_typ := g.mod.type_store.types[val_typ_id]
	if val_typ.kind !in [.struct_t, .array_t] {
		g.store_reg_to_val(addr_reg, val_id)
		return
	}
	val_size := g.type_size(val_typ_id)
	if val_size <= 0 {
		g.store_reg_to_val(addr_reg, val_id)
		return
	}
	if dst_off := g.stack_map[val_id] {
		g.copy_ptr_to_fp_bytes(addr_reg, dst_off, val_size)
		return
	}
	// Fallback when no aggregate spill slot exists.
	if val_size <= 8 {
		g.emit_ldr_reg_offset_sized(9, addr_reg, 0, val_size)
		g.store_reg_to_val(9, val_id)
		return
	}
	g.store_reg_to_val(addr_reg, val_id)
}

fn (mut g Gen) emit_sub_sp(imm int) {
	if imm <= 0xFFF {
		g.emit(asm_sub_imm(sp, sp, u32(imm)))
	} else if imm <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts
		high := (imm >> 12) & 0xFFF
		low := imm & 0xFFF
		if high > 0 {
			g.emit(asm_sub_imm_lsl12(sp, sp, u32(high)))
		}
		if low > 0 {
			g.emit(asm_sub_imm(sp, sp, u32(low)))
		}
	} else {
		// Very large stack: use register
		g.emit_mov_imm(10, u64(imm))
		g.emit(asm_sub_sp_reg(Reg(10)))
	}
}

// block_has_phis returns true if the given block contains any phi instructions.
fn (g Gen) block_has_phis(blk_id int) bool {
	if blk_id < 0 || blk_id >= g.mod.blocks.len {
		return false
	}
	blk := g.mod.blocks[blk_id]
	for vid in blk.instrs {
		v := g.mod.values[vid]
		if v.kind != .instruction {
			continue
		}
		if g.mod.instrs[v.index].op == .phi {
			return true
		}
	}
	return false
}

// emit_phi_copies emits register/stack copies for phi nodes in the target block.
// Called before jmp/br to ensure phi values from the current predecessor block
// are written to the phi output slots. In -O0 mode, phi nodes are not eliminated
// by the optimizer, so the codegen must lower them directly.
fn (mut g Gen) emit_phi_copies(target_blk_id int) {
	if target_blk_id < 0 || target_blk_id >= g.mod.blocks.len {
		return
	}
	target_blk := g.mod.blocks[target_blk_id]
	cur_blk_id := g.cur_blk_id

	for phi_val_id in target_blk.instrs {
		phi_val := g.mod.values[phi_val_id]
		if phi_val.kind != .instruction {
			continue
		}
		phi_instr := g.mod.instrs[phi_val.index]
		if phi_instr.op != .phi {
			continue
		}
		// Phi operands are [value, block, value, block, ...]
		// Find the operand pair matching our current block
		for pi := 0; pi + 1 < phi_instr.operands.len; pi += 2 {
			src_val_id := phi_instr.operands[pi]
			blk_val_id := phi_instr.operands[pi + 1]
			src_blk := if blk_val_id >= 0 && blk_val_id < g.val_to_block.len {
				g.val_to_block[blk_val_id]
			} else {
				g.mod.values[blk_val_id].index
			}
			if src_blk != cur_blk_id {
				continue
			}
			// Copy src_val_id → phi_val_id slot
			phi_size := g.type_size(phi_val.typ)
			if phi_size > 8 {
				// Aggregate copy: load source address, copy bytes to dest slot
				if src_off := g.stack_map[src_val_id] {
					g.emit_add_fp_imm(9, src_off)
					if dst_off := g.stack_map[phi_val_id] {
						g.copy_ptr_to_fp_bytes(9, dst_off, phi_size)
					}
				}
			} else {
				g.load_val_to_reg(8, src_val_id)
				g.store_reg_to_val(8, phi_val_id)
			}
			break
		}
	}
}

fn (mut g Gen) emit_add_fp_imm(rd int, imm int) {
	val := -imm
	if val <= 0xFFF {
		g.emit(asm_sub_imm(Reg(rd), fp, u32(val)))
	} else if val <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts
		high := (val >> 12) & 0xFFF
		low := val & 0xFFF
		g.emit(asm_sub_imm_lsl12(Reg(rd), fp, u32(high)))
		if low > 0 {
			g.emit(asm_sub_imm(Reg(rd), Reg(rd), u32(low)))
		}
	} else {
		// Very large offset: use register
		g.emit_mov_imm(10, u64(val))
		g.emit(asm_sub_fp_to_reg(Reg(rd), Reg(10)))
	}
}

fn (mut g Gen) emit_str_reg_offset(rt int, rn int, offset int) {
	g.emit_str_reg_offset_sized(rt, rn, offset, 8)
}

fn (mut g Gen) emit_str_reg_offset_sized(rt int, rn int, offset int, size int) {
	if offset >= -255 && offset <= 255 {
		match size {
			1 { g.emit(asm_stur_b(Reg(rt), Reg(rn), offset)) }
			2 { g.emit(asm_stur_h(Reg(rt), Reg(rn), offset)) }
			4 { g.emit(asm_stur_w(Reg(rt), Reg(rn), offset)) }
			else { g.emit(asm_stur(Reg(rt), Reg(rn), offset)) }
		}
	} else {
		// Large offset: materialize effective address in a non-conflicting scratch register.
		mut scratch := 11
		if rt == scratch || rn == scratch {
			scratch = 12
		}
		if offset < 0 {
			g.emit_mov_imm64(scratch, i64(-offset))
			g.emit(asm_sub_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		} else {
			g.emit_mov_imm64(scratch, i64(offset))
			g.emit(asm_add_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		}
		match size {
			1 { g.emit(asm_str_b(Reg(rt), Reg(scratch))) }
			2 { g.emit(asm_str_h(Reg(rt), Reg(scratch))) }
			4 { g.emit(asm_str_w(Reg(rt), Reg(scratch))) }
			else { g.emit(asm_str(Reg(rt), Reg(scratch))) }
		}
	}
}

fn (mut g Gen) emit_ldr_reg_offset(rt int, rn int, offset int) {
	g.emit_ldr_reg_offset_sized(rt, rn, offset, 8)
}

fn (mut g Gen) emit_ldr_reg_offset_sized(rt int, rn int, offset int, size int) {
	if offset >= -255 && offset <= 255 {
		match size {
			1 { g.emit(asm_ldur_b(Reg(rt), Reg(rn), offset)) }
			2 { g.emit(asm_ldur_h(Reg(rt), Reg(rn), offset)) }
			4 { g.emit(asm_ldur_w(Reg(rt), Reg(rn), offset)) }
			else { g.emit(asm_ldur(Reg(rt), Reg(rn), offset)) }
		}
	} else {
		// Large offset: materialize effective address in a non-conflicting scratch register.
		mut scratch := 11
		if rt == scratch || rn == scratch {
			scratch = 12
		}
		if offset < 0 {
			g.emit_mov_imm64(scratch, i64(-offset))
			g.emit(asm_sub_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		} else {
			g.emit_mov_imm64(scratch, i64(offset))
			g.emit(asm_add_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		}
		match size {
			1 { g.emit(asm_ldr_b(Reg(rt), Reg(scratch))) }
			2 { g.emit(asm_ldr_h(Reg(rt), Reg(scratch))) }
			4 { g.emit(asm_ldr_w(Reg(rt), Reg(scratch))) }
			else { g.emit(asm_ldr(Reg(rt), Reg(scratch))) }
		}
	}
}

fn (mut g Gen) zero_fp_bytes(dst_off int, size int) {
	if size <= 0 {
		return
	}
	g.emit_mov_reg(10, 31)
	mut off := 0
	for off + 8 <= size {
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 8)
		off += 8
	}
	if off + 4 <= size {
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 4)
		off += 4
	}
	if off + 2 <= size {
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 2)
		off += 2
	}
	if off < size {
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 1)
	}
}

fn (mut g Gen) copy_ptr_to_fp_bytes(src_reg int, dst_off int, size int) {
	if size <= 0 {
		return
	}
	// Keep source base in x13 so large-offset stack stores (which use x11/x12 scratch)
	// never clobber the active source pointer mid-copy.
	mut sreg := src_reg
	if sreg != 13 {
		g.emit_mov_reg(13, sreg)
		sreg = 13
	}
	// Null source pointers represent zero aggregate payloads.
	g.emit(asm_cmp_reg(Reg(sreg), Reg(31)))
	br_zero_off := g.macho.text_data.len
	g.emit(asm_b_cond(cond_eq, 0))
	mut off := 0
	for off + 8 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 8)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 8)
		off += 8
	}
	if off + 4 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 4)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 4)
		off += 4
	}
	if off + 2 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 2)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 2)
		off += 2
	}
	if off < size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 1)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 1)
	}
	br_end_off := g.macho.text_data.len
	g.emit(asm_b(0))
	zero_off := g.macho.text_data.len
	g.zero_fp_bytes(dst_off, size)
	end_off := g.macho.text_data.len
	rel_zero := (zero_off - br_zero_off) / 4
	g.write_u32(br_zero_off, asm_b_cond(cond_eq, rel_zero))
	rel_end := (end_off - br_end_off) / 4
	g.write_u32(br_end_off, asm_b(rel_end))
}

fn (mut g Gen) copy_ptr_offset_to_fp_bytes(src_reg int, src_off int, dst_off int, size int) {
	if size <= 0 {
		return
	}
	// Keep source base in x13 so large-offset helpers cannot clobber it.
	mut sreg := src_reg
	if sreg != 13 {
		g.emit_mov_reg(13, sreg)
		sreg = 13
	}
	// Null source pointers represent zero aggregate payloads.
	g.emit(asm_cmp_reg(Reg(sreg), Reg(31)))
	br_zero_off := g.macho.text_data.len
	g.emit(asm_b_cond(cond_eq, 0))
	mut off := 0
	for off + 8 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, src_off + off, 8)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 8)
		off += 8
	}
	if off + 4 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, src_off + off, 4)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 4)
		off += 4
	}
	if off + 2 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, src_off + off, 2)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 2)
		off += 2
	}
	if off < size {
		g.emit_ldr_reg_offset_sized(10, sreg, src_off + off, 1)
		g.emit_str_reg_offset_sized(10, 29, dst_off + off, 1)
	}
	br_end_off := g.macho.text_data.len
	g.emit(asm_b(0))
	zero_off := g.macho.text_data.len
	g.zero_fp_bytes(dst_off, size)
	end_off := g.macho.text_data.len
	rel_zero := (zero_off - br_zero_off) / 4
	g.write_u32(br_zero_off, asm_b_cond(cond_eq, rel_zero))
	rel_end := (end_off - br_end_off) / 4
	g.write_u32(br_end_off, asm_b(rel_end))
}

fn (mut g Gen) zero_ptr_bytes(dst_reg int, size int) {
	if size <= 0 {
		return
	}
	g.emit_mov_reg(10, 31)
	mut off := 0
	for off + 8 <= size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 8)
		off += 8
	}
	if off + 4 <= size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 4)
		off += 4
	}
	if off + 2 <= size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 2)
		off += 2
	}
	if off < size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 1)
	}
}

fn (mut g Gen) zero_ptr_range_bytes(dst_reg int, start_off int, total_size int) {
	if total_size <= 0 || start_off >= total_size {
		return
	}
	if start_off < 0 {
		g.zero_ptr_bytes(dst_reg, total_size)
		return
	}
	g.emit_mov_reg(10, 31)
	mut off := start_off
	for off + 8 <= total_size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 8)
		off += 8
	}
	if off + 4 <= total_size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 4)
		off += 4
	}
	if off + 2 <= total_size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 2)
		off += 2
	}
	if off < total_size {
		g.emit_str_reg_offset_sized(10, dst_reg, off, 1)
	}
}

fn (mut g Gen) copy_ptr_to_ptr_bytes(src_reg int, dst_reg int, size int) {
	if size <= 0 {
		return
	}
	// Keep source/destination bases out of x11/x12 so large-offset helpers cannot
	// overwrite active pointers between load/store pairs.
	mut sreg := src_reg
	if sreg != 13 {
		g.emit_mov_reg(13, sreg)
		sreg = 13
	}
	mut dreg := dst_reg
	if dreg != 14 {
		g.emit_mov_reg(14, dreg)
		dreg = 14
	}
	// Null source pointers represent zero aggregate payloads.
	g.emit(asm_cmp_reg(Reg(sreg), Reg(31)))
	br_zero_off := g.macho.text_data.len
	g.emit(asm_b_cond(cond_eq, 0))
	mut off := 0
	for off + 8 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 8)
		g.emit_str_reg_offset_sized(10, dreg, off, 8)
		off += 8
	}
	if off + 4 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 4)
		g.emit_str_reg_offset_sized(10, dreg, off, 4)
		off += 4
	}
	if off + 2 <= size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 2)
		g.emit_str_reg_offset_sized(10, dreg, off, 2)
		off += 2
	}
	if off < size {
		g.emit_ldr_reg_offset_sized(10, sreg, off, 1)
		g.emit_str_reg_offset_sized(10, dreg, off, 1)
	}
	br_end_off := g.macho.text_data.len
	g.emit(asm_b(0))
	zero_off := g.macho.text_data.len
	g.zero_ptr_bytes(dreg, size)
	end_off := g.macho.text_data.len
	rel_zero := (zero_off - br_zero_off) / 4
	g.write_u32(br_zero_off, asm_b_cond(cond_eq, rel_zero))
	rel_end := (end_off - br_end_off) / 4
	g.write_u32(br_end_off, asm_b(rel_end))
}

fn (mut g Gen) emit_call_to_named_fn(fn_name string) {
	sym_idx := g.macho.add_undefined('_' + fn_name)
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
	g.emit(asm_bl_reloc())
}

fn (mut g Gen) store_entry_arg_to_global(reg int, global_name string) {
	if global_name == '' {
		return
	}
	sym_idx := g.macho.add_undefined('_' + global_name)
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
	g.emit(asm_adrp(Reg(9)))
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
	g.emit(asm_add_pageoff(Reg(9)))
	g.emit_str_reg_offset(reg, 9, 0)
}

fn (mut g Gen) emit(code u32) {
	write_u32_le(mut g.macho.text_data, code)
}

fn (mut g Gen) record_pending_label(blk int) {
	off := g.macho.text_data.len - g.curr_offset
	g.pending_label_blks << blk
	g.pending_label_offs << off
	g.total_pending++
}

fn (g Gen) read_u32(off int) u32 {
	return u32(g.macho.text_data[off]) | (u32(g.macho.text_data[off + 1]) << 8) | (u32(g.macho.text_data[
		off + 2]) << 16) | (u32(g.macho.text_data[off + 3]) << 24)
}

fn (mut g Gen) write_u32(off int, v u32) {
	g.macho.text_data[off] = u8(v)
	g.macho.text_data[off + 1] = u8(v >> 8)
	g.macho.text_data[off + 2] = u8(v >> 16)
	g.macho.text_data[off + 3] = u8(v >> 24)
}

pub fn (mut g Gen) write_file(path string) {
	g.macho.write(path)
}

pub fn (mut g Gen) link_executable(output_path string) {
	mut linker := Linker.new(g.macho)
	linker.link(output_path, '_main')
}

// extract_function extracts the raw machine code bytes for a named function.
// Used for hot code reloading: the extracted bytes can be loaded into a JIT page
// and called via a function pointer.
pub fn (mut g Gen) extract_function(fn_name string) []u8 {
	target_sym := '_' + fn_name

	// Collect text section symbol offsets, sorted
	mut sym_names := []string{}
	mut sym_values := []u64{}
	for sym in g.macho.symbols {
		if sym.sect == 1 && (sym.type_ & 0x0E) == 0x0E {
			sym_names << sym.name
			sym_values << sym.value
		}
	}
	// Bubble sort by value (small N)
	for i in 0 .. sym_values.len {
		for j in 0 .. sym_values.len - 1 - i {
			if sym_values[j] > sym_values[j + 1] {
				sym_values[j], sym_values[j + 1] = sym_values[j + 1], sym_values[j]
				sym_names[j], sym_names[j + 1] = sym_names[j + 1], sym_names[j]
			}
		}
	}

	// Find target function boundaries
	mut fn_start := u64(0)
	mut fn_end := u64(g.macho.text_data.len)
	mut found := false
	for i, name in sym_names {
		if name == target_sym {
			fn_start = sym_values[i]
			if i + 1 < sym_values.len {
				fn_end = sym_values[i + 1]
			}
			found = true
			break
		}
	}

	if !found {
		eprintln('extract_function: symbol "${target_sym}" not found')
		return []u8{}
	}

	code_size := int(fn_end - fn_start)
	if code_size <= 0 || fn_start + u64(code_size) > u64(g.macho.text_data.len) {
		eprintln('extract_function: invalid range ${fn_start}..${fn_end}')
		return []u8{}
	}

	return g.macho.text_data[int(fn_start)..int(fn_end)].clone()
}

fn (mut g Gen) emit_mov_imm(rd int, imm u64) {
	// Assume imm < 65536; use MOVZ xd, #imm
	g.emit(asm_movz(Reg(rd), u32(imm & 0xFFFF)))
	// For larger imm, add MOVK(s), but not needed for stack sizes.
}

fn (mut g Gen) emit_mov_imm64(rd int, val i64) {
	// Handle 0 specifically
	if val == 0 {
		g.emit_mov_reg(rd, 31) // mov reg, xzr
		return
	}

	// For small positive values (0 to 65535), use MOVZ
	if val > 0 && val <= 0xFFFF {
		g.emit(asm_movz(Reg(rd), u32(val)))
		return
	}

	// For small negative values (-1 to -65536), use MOVN
	// MOVN xd, #imm16 sets xd to NOT(imm16 << shift)
	// For -1: MOVN xd, #0 -> ~0 = -1
	// For -42: MOVN xd, #41 -> ~41 = -42
	if val >= -65536 && val < 0 {
		not_val := u32(~val) // ~(-42) = 41
		g.emit(asm_movn(Reg(rd), not_val))
		return
	}

	// For larger values, use MOVZ followed by MOVK instructions
	uval := u64(val)

	// Emit MOVZ for first chunk (always at shift 0)
	chunk0 := u32(uval & 0xFFFF)
	g.emit(asm_movz(Reg(rd), chunk0))

	// Emit MOVK for remaining non-zero chunks
	for shift := 16; shift < 64; shift += 16 {
		chunk := u32((uval >> shift) & 0xFFFF)
		if chunk != 0 {
			g.emit(asm_movk(Reg(rd), chunk, shift))
		}
	}
}

fn (mut g Gen) emit_mov_reg(rd int, rm int) {
	g.emit(asm_mov_reg(Reg(rd), Reg(rm)))
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

fn (mut g Gen) ptr_elem_size_bytes(ptr_val_id int) int {
	if ptr_val_id <= 0 || ptr_val_id >= g.mod.values.len {
		return 8
	}
	ptr_typ_id := g.mod.values[ptr_val_id].typ
	if ptr_typ_id <= 0 || ptr_typ_id >= g.mod.type_store.types.len {
		return 8
	}
	ptr_typ := g.mod.type_store.types[ptr_typ_id]
	if ptr_typ.kind != .ptr_t || ptr_typ.elem_type <= 0
		|| ptr_typ.elem_type >= g.mod.type_store.types.len {
		return 8
	}
	elem_size := g.type_size(ptr_typ.elem_type)
	return if elem_size in [1, 2, 4] { elem_size } else { 8 }
}

fn (mut g Gen) mem_access_size_bytes(val_typ_id ssa.TypeID, ptr_val_id int) int {
	if val_typ_id <= 0 || val_typ_id >= g.mod.type_store.types.len {
		return g.ptr_elem_size_bytes(ptr_val_id)
	}
	val_typ := g.mod.type_store.types[val_typ_id]
	// The pointer element type indicates the actual memory width. When the
	// value was promoted to a wider register type (e.g., i64 for a u8 store),
	// the pointer element size takes precedence.
	ptr_elem_size := g.ptr_elem_size_bytes(ptr_val_id)
	match val_typ.kind {
		.ptr_t, .func_t {
			return 8
		}
		.int_t, .float_t {
			val_size := g.type_size(val_typ_id)
			if val_size in [1, 2, 4, 8] {
				if ptr_elem_size in [1, 2, 4] && ptr_elem_size < val_size {
					return ptr_elem_size
				}
				return val_size
			}
		}
		else {}
	}
	return ptr_elem_size
}

fn (mut g Gen) pointer_carried_aggregate_size_bytes(ptr_id int, depth int, mut seen map[int]bool) int {
	if depth > 16 || ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return 0
	}
	if ptr_id in seen {
		return 0
	}
	seen[ptr_id] = true
	defer {
		seen.delete(ptr_id)
	}
	ptr_val := g.mod.values[ptr_id]
	if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
		ptr_typ := g.mod.type_store.types[ptr_val.typ]
		if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
			&& ptr_typ.elem_type < g.mod.type_store.types.len {
			elem_typ := g.mod.type_store.types[ptr_typ.elem_type]
			if elem_typ.kind in [.struct_t, .array_t] {
				elem_size := g.type_size(ptr_typ.elem_type)
				if elem_size > 8 {
					return elem_size
				}
			}
		}
	}
	if ptr_val.kind != .instruction || ptr_val.index < 0 || ptr_val.index >= g.mod.instrs.len {
		return 0
	}
	ptr_instr := g.mod.instrs[ptr_val.index]
	op := g.selected_opcode(ptr_instr)
	match op {
		.bitcast, .zext, .sext, .trunc {
			if ptr_instr.operands.len > 0 {
				return g.pointer_carried_aggregate_size_bytes(ptr_instr.operands[0], depth + 1, mut
					seen)
			}
		}
		.assign {
			if ptr_instr.operands.len > 1 {
				return g.pointer_carried_aggregate_size_bytes(ptr_instr.operands[1], depth + 1, mut
					seen)
			}
		}
		.get_element_ptr {
			if ptr_instr.operands.len > 0 {
				base_id := ptr_instr.operands[0]
				mut field_size := 0
				if ptr_instr.operands.len > 1 {
					idx_id := ptr_instr.operands[1]
					if idx_id > 0 && idx_id < g.mod.values.len && base_id > 0
						&& base_id < g.mod.values.len {
						idx_val := g.mod.values[idx_id]
						if idx_val.kind == .constant {
							idx := idx_val.name.int()
							base_typ_id := g.mod.values[base_id].typ
							if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
								base_typ := g.mod.type_store.types[base_typ_id]
								if base_typ.kind == .ptr_t && base_typ.elem_type > 0
									&& base_typ.elem_type < g.mod.type_store.types.len {
									base_elem_typ := g.mod.type_store.types[base_typ.elem_type]
									if base_elem_typ.kind == .struct_t && idx >= 0
										&& idx < base_elem_typ.fields.len {
										field_typ_id := base_elem_typ.fields[idx]
										if field_typ_id > 0
											&& field_typ_id < g.mod.type_store.types.len {
											field_typ := g.mod.type_store.types[field_typ_id]
											if field_typ.kind in [.struct_t, .array_t] {
												field_size = g.type_size(field_typ_id)
											}
										}
									}
								}
							}
						}
					}
				}
				base_size := g.pointer_carried_aggregate_size_bytes(base_id, depth + 1, mut
					seen)
				if field_size > 8 {
					return field_size
				}
				if base_size > 8 {
					return base_size
				}
			}
		}
		.load {
			if ptr_instr.operands.len > 0 {
				return g.pointer_carried_aggregate_size_bytes(ptr_instr.operands[0], depth + 1, mut
					seen)
			}
		}
		.phi {
			if ptr_instr.operands.len == 0 {
				return 0
			}
			mut min_size := 0
			for op_id in ptr_instr.operands {
				op_size := g.pointer_carried_aggregate_size_bytes(op_id, depth + 1, mut
					seen)
				if op_size <= 8 {
					return 0
				}
				if min_size == 0 || op_size < min_size {
					min_size = op_size
				}
			}
			return min_size
		}
		else {}
	}
	return 0
}

fn (mut g Gen) extractvalue_carried_field_size_bytes(instr mir.Instruction, depth int, mut seen map[int]bool) int {
	if instr.operands.len < 2 {
		return 0
	}
	tuple_id := instr.operands[0]
	idx_id := instr.operands[1]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len || idx_id <= 0 || idx_id >= g.mod.values.len {
		return 0
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant {
		return 0
	}
	idx := idx_val.name.int()
	if idx < 0 {
		return 0
	}
	mut field_size := 0
	tuple_typ_id := g.mod.values[tuple_id].typ
	if tuple_typ_id > 0 && tuple_typ_id < g.mod.type_store.types.len {
		tuple_typ := g.mod.type_store.types[tuple_typ_id]
		if tuple_typ.kind == .struct_t && idx < tuple_typ.fields.len {
			field_typ_id := tuple_typ.fields[idx]
			if field_typ_id > 0 && field_typ_id < g.mod.type_store.types.len {
				field_typ := g.mod.type_store.types[field_typ_id]
				if field_typ.kind in [.struct_t, .array_t] {
					field_size = g.type_size(field_typ_id)
				}
			}
		} else if tuple_typ.kind == .array_t {
			field_typ_id := tuple_typ.elem_type
			if field_typ_id > 0 && field_typ_id < g.mod.type_store.types.len {
				field_typ := g.mod.type_store.types[field_typ_id]
				if field_typ.kind in [.struct_t, .array_t] {
					field_size = g.type_size(field_typ_id)
				}
			}
		}
	}
	if field_size > 8 {
		return field_size
	}
	tuple_val := g.mod.values[tuple_id]
	if tuple_val.kind == .instruction && tuple_val.index >= 0 && tuple_val.index < g.mod.instrs.len {
		tuple_instr := g.mod.instrs[tuple_val.index]
		if g.selected_opcode(tuple_instr) == .load && tuple_instr.operands.len > 0 {
			ptr_id := tuple_instr.operands[0]
			return g.pointer_carried_aggregate_size_bytes(ptr_id, depth + 1, mut seen)
		}
	}
	return g.inferred_aggregate_carried_size_bytes(tuple_id, depth + 1, mut seen)
}

fn (mut g Gen) inferred_aggregate_carried_size_bytes(val_id int, depth int, mut seen map[int]bool) int {
	if depth > 16 || val_id <= 0 || val_id >= g.mod.values.len {
		return 0
	}
	if val_id in seen {
		return 0
	}
	seen[val_id] = true
	defer {
		seen.delete(val_id)
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[val.typ]
		if typ.kind in [.struct_t, .array_t] {
			size := g.type_size(val.typ)
			if size > 8 {
				return size
			}
		}
	}
	if val.kind != .instruction || val.index < 0 || val.index >= g.mod.instrs.len {
		return 0
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.bitcast, .zext, .sext, .trunc {
			if instr.operands.len == 0 {
				return 0
			}
			src_id := instr.operands[0]
			mut inferred := g.inferred_aggregate_carried_size_bytes(src_id, depth + 1, mut
				seen)
			if op == .bitcast && src_id > 0 && src_id < g.mod.values.len {
				src_typ_id := g.mod.values[src_id].typ
				if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
					src_typ := g.mod.type_store.types[src_typ_id]
					if src_typ.kind in [.struct_t, .array_t] {
						src_size := g.type_size(src_typ_id)
						if src_size > inferred {
							inferred = src_size
						}
					} else if src_typ.kind == .ptr_t && src_typ.elem_type > 0
						&& src_typ.elem_type < g.mod.type_store.types.len {
						elem_typ := g.mod.type_store.types[src_typ.elem_type]
						if elem_typ.kind in [.struct_t, .array_t] {
							elem_size := g.type_size(src_typ.elem_type)
							if elem_size > inferred {
								inferred = elem_size
							}
						}
					}
				}
			}
			return inferred
		}
		.assign {
			if instr.operands.len > 1 {
				return g.inferred_aggregate_carried_size_bytes(instr.operands[1], depth + 1, mut
					seen)
			}
		}
		.load {
			if instr.operands.len == 0 {
				return 0
			}
			// Sumtype `_data` payload words are pointer-sized scalars and must not
			// be widened as aggregate-bytes sources.
			if g.scalar_value_is_pointer_payload(val_id, 0) {
				return 0
			}
			return g.pointer_carried_aggregate_size_bytes(instr.operands[0], depth + 1, mut
				seen)
		}
		.extractvalue {
			return g.extractvalue_carried_field_size_bytes(instr, depth + 1, mut seen)
		}
		.call, .call_indirect, .call_sret {
			if ret_typ_id := g.call_result_type(instr) {
				if ret_typ_id > 0 && ret_typ_id < g.mod.type_store.types.len {
					ret_typ := g.mod.type_store.types[ret_typ_id]
					if ret_typ.kind in [.struct_t, .array_t] {
						ret_size := g.type_size(ret_typ_id)
						if ret_size > 8 {
							return ret_size
						}
					}
				}
			}
		}
		.phi {
			if instr.operands.len == 0 {
				return 0
			}
			mut min_size := 0
			for op_id in instr.operands {
				op_size := g.inferred_aggregate_carried_size_bytes(op_id, depth + 1, mut
					seen)
				if op_size <= 8 {
					return 0
				}
				if min_size == 0 || op_size < min_size {
					min_size = op_size
				}
			}
			return min_size
		}
		else {}
	}
	return 0
}

fn (mut g Gen) aggregate_source_size_bytes(src_id int) int {
	if src_id <= 0 || src_id >= g.mod.values.len {
		return 0
	}
	src_val := g.mod.values[src_id]
	src_typ_id := src_val.typ
	mut src_size := if src_typ_id > 0 { g.type_size(src_typ_id) } else { 0 }
	mut src_kind := ssa.TypeKind.void_t
	if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
		src_kind = g.mod.type_store.types[src_typ_id].kind
	}
	mut seen := map[int]bool{}
	inferred_size := g.inferred_aggregate_carried_size_bytes(src_id, 0, mut seen)
	if inferred_size > src_size {
		src_size = inferred_size
	}
	// Scalar-width sources (plain ints/pointers) occupy a single stack slot.
	// Keep this clamp for true scalars, but allow widening when MIR carries
	// aggregate bytes through scalar-typed temporaries.
	if src_size > 0 && src_size <= 8 && src_kind in [.int_t, .ptr_t] {
		return src_size
	}
	return src_size
}

// Calculate the size of a type in bytes
fn (mut g Gen) type_size(typ_id ssa.TypeID) int {
	if typ_id == 0 {
		return 0 // void
	}
	if typ_id < 0 || typ_id >= g.mod.type_store.types.len {
		return 8
	}
	// Flat array cache: check if already computed (cached values are offset by +1, 0 = not cached)
	if typ_id < g.type_size_cache.len && g.type_size_cache[typ_id] != 0 {
		return g.type_size_cache[typ_id] - 1
	}
	if typ_id < g.type_size_stack.len && g.type_size_stack[typ_id] {
		// Break recursive layout cycles (e.g. malformed self-referential aggregates).
		return 8
	}
	if typ_id < g.type_size_stack.len {
		g.type_size_stack[typ_id] = true
	}
	typ := g.mod.type_store.types[typ_id]
	mut size := 0
	match typ.kind {
		.void_t {
			size = 0
		}
		.int_t {
			size = if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.float_t {
			size = if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.ptr_t {
			size = 8
		} // 64-bit pointers on arm64
		.array_t {
			elem_size := g.type_size(typ.elem_type)
			size = typ.len * elem_size
		}
		.struct_t {
			if typ.is_union {
				// Union: size = max(field_sizes), aligned to largest field
				mut max_size := 0
				for field_typ in typ.fields {
					fs := g.type_size(field_typ)
					if fs > max_size {
						max_size = fs
					}
				}
				size = if max_size > 0 { max_size } else { 8 }
			} else {
				mut total := 0
				mut max_align := 1
				for field_typ in typ.fields {
					align := g.type_align(field_typ)
					if align > max_align {
						max_align = align
					}
					if align > 1 && total % align != 0 {
						total = (total + align - 1) & ~(align - 1)
					}
					total += g.type_size(field_typ)
				}
				// Align struct size to its largest field alignment
				if max_align > 1 && total % max_align != 0 {
					total = (total + max_align - 1) & ~(max_align - 1)
				}
				size = if total > 0 { total } else { 8 }
			}
		}
		.func_t {
			size = 8
		} // function pointer
		.label_t {
			size = 0
		}
		.metadata_t {
			size = 0
		}
	}
	if typ_id < g.type_size_stack.len {
		g.type_size_stack[typ_id] = false
	}
	// Store size+1 so that 0 means "not cached" (size 0 is valid for void/label/metadata)
	if typ_id < g.type_size_cache.len {
		g.type_size_cache[typ_id] = size + 1
	}
	return size
}

fn (mut g Gen) type_align(typ_id ssa.TypeID) int {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return if typ_id == 0 { 1 } else { 8 }
	}
	// Flat array cache: cached values stored as align+1, 0 = not cached
	if typ_id < g.type_align_cache.len && g.type_align_cache[typ_id] != 0 {
		return g.type_align_cache[typ_id] - 1
	}
	if typ_id < g.type_align_stack.len && g.type_align_stack[typ_id] {
		return 8
	}
	if typ_id < g.type_align_stack.len {
		g.type_align_stack[typ_id] = true
	}
	typ := g.mod.type_store.types[typ_id]
	mut align := 1
	if typ.kind == .array_t {
		// Array alignment = element alignment (matching C semantics)
		align = g.type_align(typ.elem_type)
	} else {
		size := g.type_size(typ_id)
		if size >= 8 {
			align = 8
		} else if size >= 4 {
			align = 4
		} else if size >= 2 {
			align = 2
		}
	}
	if typ_id < g.type_align_stack.len {
		g.type_align_stack[typ_id] = false
	}
	if typ_id < g.type_align_cache.len {
		g.type_align_cache[typ_id] = align + 1
	}
	return align
}

fn (mut g Gen) struct_field_offset_bytes(struct_typ_id ssa.TypeID, field_idx int) int {
	if struct_typ_id <= 0 || struct_typ_id >= g.mod.type_store.types.len {
		return field_idx * 8
	}
	// Cache key: (typ_id << 16) | field_idx — supports up to 65535 fields per struct
	cache_key := (struct_typ_id << 16) | field_idx
	if cache_key in g.struct_field_offset_cache {
		return g.struct_field_offset_cache[cache_key]
	}
	typ := g.mod.type_store.types[struct_typ_id]
	if typ.kind != .struct_t || field_idx < 0 || field_idx >= typ.fields.len {
		return field_idx * 8
	}
	// Union types: all fields overlap at offset 0
	if typ.is_union {
		return 0
	}
	mut offset := 0
	for i, field_typ in typ.fields {
		align := g.type_align(field_typ)
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
		if i == field_idx {
			g.struct_field_offset_cache[cache_key] = offset
			return offset
		}
		field_size := g.type_size(field_typ)
		offset += if field_size > 0 { field_size } else { 8 }
	}
	return field_idx * 8
}

fn (mut g Gen) struct_field_offset_and_size(struct_typ_id ssa.TypeID, field_idx int, default_off int, default_size int) (int, int) {
	mut off := default_off
	mut size := default_size
	if struct_typ_id > 0 && struct_typ_id < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[struct_typ_id]
		if typ.kind == .struct_t && field_idx >= 0 && field_idx < typ.fields.len {
			off = g.struct_field_offset_bytes(struct_typ_id, field_idx)
			field_size := g.type_size(typ.fields[field_idx])
			if field_size > 0 {
				size = field_size
			}
		}
	}
	if size !in [1, 2, 4, 8] {
		size = 8
	}
	return off, size
}

fn (mut g Gen) large_struct_stack_value_is_pointer(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.typ <= 0 || val.typ >= g.mod.type_store.types.len {
		return false
	}
	val_typ := g.mod.type_store.types[val.typ]
	if val_typ.kind != .struct_t || g.type_size(val.typ) <= 16 {
		return false
	}
	// Values materialized directly into aggregate storage.
	if val.kind == .string_literal {
		return false
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		op := g.selected_opcode(instr)
		if op == .load {
			if instr.operands.len > 0
				&& g.ptr_originates_from_sumtype_data_word(instr.operands[0], 0) {
				// Sumtype `_data` words can hold either inline immediates or heap pointers.
				// Keep large loads from these sources pointer-carried so payload bytes are
				// only dereferenced in guarded, variant-specific paths.
				return true
			}
			// Keep very large struct loads address-carried in stack slots. Copying these
			// values by-value can explode frame sizes in recursive code paths.
			return g.type_size(val.typ) > 256
		}
		return op !in [.call, .call_sret, .inline_string_init, .insertvalue, .struct_init,
			.extractvalue, .assign, .phi, .bitcast, .load]
	}
	// Arguments and globals are treated as by-value stack storage.
	return false
}

fn (g &Gen) value_has_only_extractvalue_uses(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.uses.len == 0 {
		return false
	}
	for use_id in val.uses {
		if use_id <= 0 || use_id >= g.mod.values.len {
			return false
		}
		use_val := g.mod.values[use_id]
		if use_val.kind != .instruction {
			return false
		}
		use_instr := g.mod.instrs[use_val.index]
		if g.selected_opcode(use_instr) != .extractvalue {
			return false
		}
	}
	return true
}

fn (mut g Gen) large_aggregate_stack_value_is_pointer(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	typ_id := g.mod.values[val_id].typ
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	if typ.kind != .struct_t || g.type_size(typ_id) <= 16 {
		return false
	}
	return g.large_struct_stack_value_is_pointer(val_id)
}

fn (mut g Gen) value_is_large_struct(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	typ_id := g.mod.values[val_id].typ
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	return typ.kind == .struct_t && g.type_size(typ_id) > 16
}

fn (mut g Gen) load_large_struct_truth_word_to_reg(reg int, val_id int) {
	g.load_val_to_reg(reg, val_id)
	g.emit(asm_ldr_imm(Reg(reg), Reg(reg), 0))
}

fn (g &Gen) is_known_zero_value(val_id int, depth int) bool {
	if depth > 8 || val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind == .constant {
		return val.name == '0'
	}
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.bitcast, .zext, .sext, .trunc, .assign {
			if instr.operands.len == 0 {
				return false
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			return g.is_known_zero_value(instr.operands[src_idx], depth + 1)
		}
		.phi {
			if instr.operands.len == 0 {
				return false
			}
			for op_id in instr.operands {
				if !g.is_known_zero_value(op_id, depth + 1) {
					return false
				}
			}
			return true
		}
		else {}
	}
	return false
}

fn (g &Gen) is_effective_null_pointer_value(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return true
	}
	if g.is_known_zero_value(val_id, 0) {
		return true
	}
	val := g.mod.values[val_id]
	has_storage := val_id in g.reg_map || val_id in g.stack_map
	is_symbol_like := val.kind in [.global, .string_literal, .c_string_literal, .func_ref, .constant]
	return !has_storage && !is_symbol_like
}

fn (g &Gen) is_sumtype_wrapper_struct_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	return typ.kind == .struct_t && typ.field_names.len == 2 && typ.field_names[0] == '_tag'
		&& typ.field_names[1] == '_data'
}

fn (g &Gen) is_sumtype_data_extract_value(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	if g.selected_opcode(instr) != .extractvalue || instr.operands.len < 2 {
		return false
	}
	tuple_id := instr.operands[0]
	idx_id := instr.operands[1]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len || idx_id <= 0 || idx_id >= g.mod.values.len {
		return false
	}
	tuple_typ_id := g.mod.values[tuple_id].typ
	if !g.is_sumtype_wrapper_struct_type(tuple_typ_id) {
		return false
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant {
		return false
	}
	idx := idx_val.name.int()
	wrapper_typ := g.mod.type_store.types[tuple_typ_id]
	if idx < 0 || idx >= wrapper_typ.field_names.len {
		return false
	}
	return wrapper_typ.field_names[idx] == '_data'
}

fn (g &Gen) ptr_originates_from_sumtype_data_word(ptr_id int, depth int) bool {
	if depth > 16 || ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return false
	}
	ptr_val := g.mod.values[ptr_id]
	if ptr_val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[ptr_val.index]
	op := g.selected_opcode(instr)
	match op {
		.extractvalue {
			return g.is_sumtype_data_extract_value(ptr_id)
		}
		.get_element_ptr {
			if instr.operands.len < 2 {
				return false
			}
			base_id := instr.operands[0]
			// Preserve `_data` provenance through pointer arithmetic. Once a pointer
			// is derived from sumtype `_data`, nested GEPs (field/array indexing)
			// still refer to that payload region and must remain pointer-carried.
			if g.ptr_originates_from_sumtype_data_word(base_id, depth + 1) {
				return true
			}
			idx_id := instr.operands[1]
			if idx_id <= 0 || idx_id >= g.mod.values.len {
				return false
			}
			idx_val := g.mod.values[idx_id]
			if idx_val.kind != .constant {
				return false
			}
			idx := idx_val.name.int()
			if base_id <= 0 || base_id >= g.mod.values.len {
				return false
			}
			base_typ_id := g.mod.values[base_id].typ
			if base_typ_id <= 0 || base_typ_id >= g.mod.type_store.types.len {
				return false
			}
			base_typ := g.mod.type_store.types[base_typ_id]
			if base_typ.kind != .ptr_t || base_typ.elem_type <= 0
				|| base_typ.elem_type >= g.mod.type_store.types.len {
				return false
			}
			elem_typ := g.mod.type_store.types[base_typ.elem_type]
			if elem_typ.kind != .struct_t || idx < 0 || idx >= elem_typ.field_names.len {
				return false
			}
			return elem_typ.field_names[idx] == '_data'
		}
		.load, .bitcast, .zext, .sext, .trunc, .assign {
			if instr.operands.len == 0 {
				return false
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			return g.ptr_originates_from_sumtype_data_word(instr.operands[src_idx], depth + 1)
		}
		.phi {
			for op_id in instr.operands {
				if g.ptr_originates_from_sumtype_data_word(op_id, depth + 1) {
					return true
				}
			}
			return false
		}
		else {}
	}
	return false
}

fn (g &Gen) sumtype_data_word_load_source(ptr_id int, result_typ_id ssa.TypeID) ?int {
	// Detect `load(bitcast(extractvalue(wrapper, _data)))` and use the `_data`
	// word directly for scalar results. Primitive sumtype variants (e.g. `u8`)
	// are stored inline in `_data`; dereferencing as a pointer crashes on values
	// like `1` (the sentinel used by `types.Void(1)`).
	if ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return none
	}
	if result_typ_id <= 0 || result_typ_id >= g.mod.type_store.types.len {
		return none
	}
	result_typ := g.mod.type_store.types[result_typ_id]
	if result_typ.kind != .int_t {
		return none
	}
	ptr_val := g.mod.values[ptr_id]
	if ptr_val.kind != .instruction {
		return none
	}
	ptr_instr := g.mod.instrs[ptr_val.index]
	if g.selected_opcode(ptr_instr) != .bitcast || ptr_instr.operands.len < 1 {
		return none
	}
	data_word_id := ptr_instr.operands[0]
	if data_word_id <= 0 || data_word_id >= g.mod.values.len {
		return none
	}
	data_word_val := g.mod.values[data_word_id]
	if data_word_val.kind != .instruction {
		return none
	}
	data_word_instr := g.mod.instrs[data_word_val.index]
	if g.selected_opcode(data_word_instr) != .extractvalue || data_word_instr.operands.len < 2 {
		return none
	}
	tuple_id := data_word_instr.operands[0]
	idx_id := data_word_instr.operands[1]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len || idx_id <= 0 || idx_id >= g.mod.values.len {
		return none
	}
	tuple_typ_id := g.mod.values[tuple_id].typ
	if !g.is_sumtype_wrapper_struct_type(tuple_typ_id) {
		return none
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant {
		return none
	}
	wrapper_typ := g.mod.type_store.types[tuple_typ_id]
	idx := idx_val.name.int()
	if idx < 0 || idx >= wrapper_typ.field_names.len {
		return none
	}
	if wrapper_typ.field_names[idx] != '_data' {
		return none
	}
	return data_word_id
}

fn (mut g Gen) load_sumtype_data_ptr_to_reg(reg int, val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val_typ_id := g.mod.values[val_id].typ
	if !g.is_sumtype_wrapper_struct_type(val_typ_id) {
		return false
	}
	if val_off := g.stack_map[val_id] {
		g.emit_ldr_reg_offset(reg, 29, val_off + 8)
		return true
	}
	g.load_address_of_val_to_reg(reg, val_id)
	g.emit(asm_ldr_imm(Reg(reg), Reg(reg), 1))
	return true
}

fn (mut g Gen) load_sumtype_payload_word_to_reg(reg int, val_id int) bool {
	if !g.load_sumtype_data_ptr_to_reg(reg, val_id) {
		return false
	}
	g.emit(asm_ldr_imm(Reg(reg), Reg(reg), 0))
	return true
}

fn (g &Gen) sumtype_extractvalue_data_tuple_id(val_id int, expected_wrapper_typ ssa.TypeID) ?int {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return none
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return none
	}
	instr := g.mod.instrs[val.index]
	if g.selected_opcode(instr) != .extractvalue || instr.operands.len < 2 {
		return none
	}
	idx_id := instr.operands[1]
	if idx_id <= 0 || idx_id >= g.mod.values.len {
		return none
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant || idx_val.name != '1' {
		return none
	}
	tuple_id := instr.operands[0]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len {
		return none
	}
	tuple_typ := g.mod.values[tuple_id].typ
	if tuple_typ != expected_wrapper_typ || !g.is_sumtype_wrapper_struct_type(tuple_typ) {
		return none
	}
	return tuple_id
}

fn (g &Gen) sumtype_extractvalue_tag_tuple_id(val_id int, expected_wrapper_typ ssa.TypeID) ?int {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return none
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return none
	}
	instr := g.mod.instrs[val.index]
	if g.selected_opcode(instr) != .extractvalue || instr.operands.len < 2 {
		return none
	}
	idx_id := instr.operands[1]
	if idx_id <= 0 || idx_id >= g.mod.values.len {
		return none
	}
	idx_val := g.mod.values[idx_id]
	if idx_val.kind != .constant {
		return none
	}
	idx := idx_val.name.int()
	tuple_id := instr.operands[0]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len {
		return none
	}
	tuple_typ := g.mod.values[tuple_id].typ
	if tuple_typ != expected_wrapper_typ || !g.is_sumtype_wrapper_struct_type(tuple_typ) {
		return none
	}
	wrapper_typ := g.mod.type_store.types[tuple_typ]
	if idx < 0 || idx >= wrapper_typ.field_names.len {
		return none
	}
	if wrapper_typ.field_names[idx] != '_tag' {
		return none
	}
	return tuple_id
}

fn (g &Gen) sumtype_wrapper_source_from_tag_word(val_id int) ?int {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return none
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return none
	}
	instr := g.mod.instrs[val.index]
	if g.selected_opcode(instr) != .extractvalue || instr.operands.len < 2 {
		return none
	}
	tuple_id := instr.operands[0]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len {
		return none
	}
	tuple_typ := g.mod.values[tuple_id].typ
	if !g.is_sumtype_wrapper_struct_type(tuple_typ) {
		return none
	}
	if wrapper_id := g.sumtype_extractvalue_tag_tuple_id(val_id, tuple_typ) {
		return wrapper_id
	}
	return none
}

fn (g &Gen) sumtype_tag_word_has_variant_guard(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	if g.selected_opcode(instr) != .extractvalue || instr.operands.len < 2 {
		return false
	}
	tuple_id := instr.operands[0]
	if tuple_id <= 0 || tuple_id >= g.mod.values.len {
		return false
	}
	for tuple_use_id in g.mod.values[tuple_id].uses {
		if tuple_use_id <= 0 || tuple_use_id >= g.mod.values.len {
			continue
		}
		tuple_use_val := g.mod.values[tuple_use_id]
		if tuple_use_val.kind != .instruction {
			continue
		}
		tuple_use_instr := g.mod.instrs[tuple_use_val.index]
		if g.selected_opcode(tuple_use_instr) != .extractvalue || tuple_use_instr.operands.len < 2 {
			continue
		}
		idx_id := tuple_use_instr.operands[1]
		if idx_id <= 0 || idx_id >= g.mod.values.len {
			continue
		}
		idx_val := g.mod.values[idx_id]
		if idx_val.kind != .constant || idx_val.name != '0' {
			continue
		}
		for use_id in tuple_use_val.uses {
			if use_id <= 0 || use_id >= g.mod.values.len {
				continue
			}
			use_val := g.mod.values[use_id]
			if use_val.kind != .instruction {
				continue
			}
			use_instr := g.mod.instrs[use_val.index]
			op := g.selected_opcode(use_instr)
			if op !in [.eq, .ne] || use_instr.operands.len < 2 {
				continue
			}
			mut other_id := 0
			if use_instr.operands[0] == tuple_use_id {
				other_id = use_instr.operands[1]
			} else if use_instr.operands[1] == tuple_use_id {
				other_id = use_instr.operands[0]
			}
			if other_id <= 0 || other_id >= g.mod.values.len {
				continue
			}
			if g.mod.values[other_id].kind != .constant {
				continue
			}
			for cmp_use_id in use_val.uses {
				if cmp_use_id <= 0 || cmp_use_id >= g.mod.values.len {
					continue
				}
				cmp_use_val := g.mod.values[cmp_use_id]
				if cmp_use_val.kind != .instruction {
					continue
				}
				cmp_use_instr := g.mod.instrs[cmp_use_val.index]
				if g.selected_opcode(cmp_use_instr) == .br {
					return true
				}
			}
		}
	}
	return false
}

fn (g &Gen) sumtype_wrapper_source_from_data_ptr(ptr_id int, expected_wrapper_typ ssa.TypeID, depth int) ?int {
	if depth > 8 || ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return none
	}
	ptr_val := g.mod.values[ptr_id]
	if ptr_val.kind != .instruction {
		return none
	}
	instr := g.mod.instrs[ptr_val.index]
	op := g.selected_opcode(instr)
	match op {
		.bitcast, .assign {
			if instr.operands.len == 0 {
				return none
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			src_id := instr.operands[src_idx]
			if tuple_id := g.sumtype_extractvalue_data_tuple_id(src_id, expected_wrapper_typ) {
				return tuple_id
			}
			if wrapper_id := g.sumtype_wrapper_source_from_data_ptr(src_id, expected_wrapper_typ,
				depth + 1)
			{
				return wrapper_id
			}
			if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(src_id, expected_wrapper_typ,
				depth + 1)
			{
				return wrapper_id
			}
		}
		.extractvalue {
			if tuple_id := g.sumtype_extractvalue_data_tuple_id(ptr_id, expected_wrapper_typ) {
				return tuple_id
			}
		}
		.phi {
			for op_id in instr.operands {
				if wrapper_id := g.sumtype_wrapper_source_from_data_ptr(op_id, expected_wrapper_typ,
					depth + 1)
				{
					return wrapper_id
				}
			}
		}
		else {}
	}
	return none
}

fn (g &Gen) sumtype_wrapper_source_from_unwrapped_value(val_id int, expected_wrapper_typ ssa.TypeID, depth int) ?int {
	if depth > 8 || val_id <= 0 || val_id >= g.mod.values.len {
		return none
	}
	val := g.mod.values[val_id]
	if val.typ == expected_wrapper_typ && g.is_sumtype_wrapper_struct_type(val.typ) {
		return val_id
	}
	if val.kind != .instruction {
		return none
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.load {
			if instr.operands.len > 0 {
				if wrapper_id := g.sumtype_wrapper_source_from_data_ptr(instr.operands[0],
					expected_wrapper_typ, depth + 1)
				{
					return wrapper_id
				}
			}
		}
		.extractvalue {
			// For extracted words, recurse through the source value and recover the
			// wrapper/payload at the call site where we also have guard context.
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				if wrapper_id := g.sumtype_wrapper_source_from_data_ptr(src_id, expected_wrapper_typ,
					depth + 1)
				{
					return wrapper_id
				}
			}
		}
		.bitcast, .assign {
			if instr.operands.len == 0 {
				return none
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			src_id := instr.operands[src_idx]
			if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(src_id, expected_wrapper_typ,
				depth + 1)
			{
				return wrapper_id
			}
			if wrapper_id := g.sumtype_wrapper_source_from_data_ptr(src_id, expected_wrapper_typ,
				depth + 1)
			{
				return wrapper_id
			}
		}
		.phi {
			mut found_wrapper_id := 0
			for op_id in instr.operands {
				if wrapper_id := g.sumtype_wrapper_source_from_unwrapped_value(op_id,
					expected_wrapper_typ, depth + 1)
				{
					if found_wrapper_id == 0 {
						found_wrapper_id = wrapper_id
					} else if found_wrapper_id != wrapper_id {
						return none
					}
				}
			}
			if found_wrapper_id != 0 {
				return found_wrapper_id
			}
		}
		else {}
	}
	return none
}

fn (g &Gen) unwrap_passthrough_value(val_id int, depth int) int {
	if depth > 8 || val_id <= 0 || val_id >= g.mod.values.len {
		return val_id
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction {
		return val_id
	}
	instr := g.mod.instrs[val.index]
	op := g.selected_opcode(instr)
	match op {
		.bitcast, .assign {
			if instr.operands.len == 0 {
				return val_id
			}
			src_idx := if op == .assign && instr.operands.len > 1 { 1 } else { 0 }
			src_id := instr.operands[src_idx]
			return g.unwrap_passthrough_value(src_id, depth + 1)
		}
		else {}
	}
	return val_id
}

fn (g &Gen) heap_alloc_store_source(alloc_id int) ?int {
	if alloc_id <= 0 || alloc_id >= g.mod.values.len {
		return none
	}
	alloc_val := g.mod.values[alloc_id]
	if alloc_val.kind != .instruction {
		return none
	}
	alloc_instr := g.mod.instrs[alloc_val.index]
	if g.selected_opcode(alloc_instr) != .heap_alloc {
		return none
	}
	for use_id in alloc_val.uses {
		if use_id <= 0 || use_id >= g.mod.values.len {
			continue
		}
		use_val := g.mod.values[use_id]
		if use_val.kind != .instruction {
			continue
		}
		use_instr := g.mod.instrs[use_val.index]
		if g.selected_opcode(use_instr) != .store || use_instr.operands.len < 2 {
			continue
		}
		if use_instr.operands[1] != alloc_id {
			continue
		}
		src_id := use_instr.operands[0]
		if src_id > 0 && src_id < g.mod.values.len {
			return src_id
		}
	}
	return none
}

fn (g &Gen) forwarded_optiontype_wrapper_return_source(ret_val_id int, expected_wrapper_typ ssa.TypeID) ?int {
	if ret_val_id <= 0 || ret_val_id >= g.mod.values.len {
		return none
	}
	ret_val := g.mod.values[ret_val_id]
	if ret_val.kind != .instruction {
		return none
	}
	ret_instr := g.mod.instrs[ret_val.index]
	if g.selected_opcode(ret_instr) != .struct_init || ret_instr.typ != expected_wrapper_typ
		|| ret_instr.operands.len < 2 {
		return none
	}
	tag_idx := g.sumtype_variant_tag_index('Type', 'types', 'OptionType') or { return none }
	tag_id := ret_instr.operands[0]
	if tag_id <= 0 || tag_id >= g.mod.values.len {
		return none
	}
	tag_val := g.mod.values[tag_id]
	if tag_val.kind != .constant || tag_val.name.int() != tag_idx {
		return none
	}
	data_id := ret_instr.operands[1]
	alloc_id := g.unwrap_passthrough_value(data_id, 0)
	if src_id := g.heap_alloc_store_source(alloc_id) {
		if src_id > 0 && src_id < g.mod.values.len
			&& g.mod.values[src_id].typ == expected_wrapper_typ {
			return src_id
		}
	}
	return none
}

fn (mut g Gen) mark_sumtype_data_heap_allocas(func mir.Function) {
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			if instr.op == .struct_init {
				if !g.is_sumtype_wrapper_struct_type(instr.typ) || instr.operands.len < 2 {
					continue
				}
				g.try_mark_sumtype_data_heap_alloca(instr.operands[1])
				continue
			}
			if instr.op == .insertvalue {
				if !g.is_sumtype_wrapper_struct_type(instr.typ) || instr.operands.len < 3 {
					continue
				}
				idx_id := instr.operands[2]
				if idx_id <= 0 || idx_id >= g.mod.values.len {
					continue
				}
				idx_val := g.mod.values[idx_id]
				if idx_val.kind != .constant || idx_val.name != '1' {
					continue
				}
				g.try_mark_sumtype_data_heap_alloca(instr.operands[1])
			}
		}
	}
}

fn (mut g Gen) try_mark_sumtype_data_heap_alloca(data_id int) {
	if data_id <= 0 || data_id >= g.mod.values.len {
		return
	}
	data_val := g.mod.values[data_id]
	if data_val.kind != .instruction {
		return
	}
	data_instr := g.mod.instrs[data_val.index]
	if data_instr.op != .bitcast || data_instr.operands.len == 0 {
		return
	}
	src_ptr_id := data_instr.operands[0]
	if src_ptr_id <= 0 || src_ptr_id >= g.mod.values.len {
		return
	}
	src_ptr_val := g.mod.values[src_ptr_id]
	if src_ptr_val.kind != .instruction {
		return
	}
	src_ptr_instr := g.mod.instrs[src_ptr_val.index]
	if src_ptr_instr.op == .alloca {
		g.sumtype_data_heap_allocas[src_ptr_id] = true
	}
}

// lookup_type_from_env looks up a type by name from the Environment.
// Returns the types.Type if found, none otherwise.
fn (g &Gen) lookup_type_from_env(name string, module_name string) ?types.Type {
	if g.mod.env == unsafe { nil } {
		return none
	}
	mut scope := &types.Scope(unsafe { nil })
	if s := g.mod.env.get_scope(module_name) {
		scope = unsafe { s }
	} else if s := g.mod.env.get_scope('builtin') {
		scope = unsafe { s }
	} else {
		return none
	}
	if obj := scope.lookup_parent(name, 0) {
		if obj is types.Type {
			return obj
		}
	}
	return none
}

// lookup_struct_from_env looks up a struct type by name from the Environment.
fn (g &Gen) lookup_struct_from_env(name string) ?types.Struct {
	if typ := g.lookup_type_from_env(name, 'builtin') {
		if typ is types.Struct {
			return typ
		}
	}
	return none
}

fn (mut g Gen) allocate_registers(func mir.Function) {
	if g.env_no_regalloc {
		return
	}
	trace_ra := g.env_trace_regalloc.len > 0
		&& (g.env_trace_regalloc == '*' || func.name == g.env_trace_regalloc)
	mut intervals := map[int]&Interval{}
	mut call_indices := []int{}
	mut instr_idx := 0
	// Phi elimination lowers edge copies as `.assign dest, src` and leaves
	// placeholder `.bitcast` values for former phis. Keep these carried values
	// on the stack so loop-carried state does not depend on linearized interval
	// approximations in the register allocator.
	mut phi_related_vals := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			if instr.op == .assign {
				phi_related_vals[val_id] = true
				if instr.operands.len > 0 {
					phi_related_vals[instr.operands[0]] = true
				}
				if instr.operands.len > 1 {
					phi_related_vals[instr.operands[1]] = true
				}
			} else if instr.op == .bitcast && instr.operands.len == 0 {
				phi_related_vals[val_id] = true
			}
		}
	}

	// Map block index to instruction range
	mut block_start := map[int]int{}
	mut block_end := map[int]int{}

	// Don't register-allocate function parameters.
	// Parameters have special spilling behavior managed by prologue code.

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		block_start[blk_id] = instr_idx
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction || val.kind == .argument {
				// Check if this is a call with indirect struct return (> 16 bytes)
				// These values must stay on the stack - don't register-allocate them
				mut skip_interval := val_id in phi_related_vals
				if val.kind == .instruction {
					instr := g.mod.instrs[val.index]
					// Skip instructions that build results directly on the stack.
					// These ops write to the stack slot without going through a register,
					// so register-allocating them leaves the register uninitialized.
					if instr.op in [.struct_init, .insertvalue, .inline_string_init,
						.call_sret] {
						skip_interval = true
					}
					if instr.op in [.call, .call_indirect, .call_sret] {
						result_typ := g.mod.type_store.types[val.typ]
						if result_typ.kind == .struct_t {
							result_size := g.type_size(val.typ)
							if result_size > 16 {
								skip_interval = true
							}
						}
					}
				}
				if val.typ > 0 && val.typ < g.mod.type_store.types.len {
					val_typ := g.mod.type_store.types[val.typ]
					// Keep all multi-word aggregates in stack slots. Treating a >8-byte
					// aggregate as a single GP register loses words and breaks
					// insertvalue/extractvalue flows (notably wrappers around `types.Type`).
					if val_typ.kind in [.struct_t, .array_t] {
						val_size := g.type_size(val.typ)
						if val_size > 8 {
							skip_interval = true
						}
					}
				}

				if !skip_interval && unsafe { intervals[val_id] == nil } {
					intervals[val_id] = &Interval{
						val_id: val_id
						start:  instr_idx
						end:    instr_idx
					}
				}
			}

			instr := g.mod.instrs[val.index]
			if instr.op in [.call, .call_indirect, .call_sret, .heap_alloc] {
				call_indices << instr_idx
			}

			for op in instr.operands {
				if op in phi_related_vals {
					continue
				}
				if g.mod.values[op].kind in [.instruction, .argument] {
					if mut interval := intervals[op] {
						if instr_idx > interval.end {
							interval.end = instr_idx
						}
					}
				}
			}
			instr_idx++
		}
		block_end[blk_id] = instr_idx
	}

	// Conservative approach: don't register-allocate values that cross block boundaries
	// This is slower but correct for loops
	mut block_of_def := map[int]int{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			block_of_def[val_id] = blk_id
		}
	}

	// Mark values used in different blocks than defined - extend to entire function
	total_instrs := instr_idx
	// Parameters can be consumed in any CFG block (including loop bodies).
	// Keep them live for the full function so they are not re-used by
	// later temporaries before all dynamic iterations/branches are done.
	for pid in func.params {
		if mut interval := intervals[pid] {
			interval.end = total_instrs
		}
	}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			for op in instr.operands {
				if op in phi_related_vals {
					continue
				}
				if g.mod.values[op].kind in [.instruction, .argument] {
					if def_blk := block_of_def[op] {
						if def_blk != blk_id {
							// Value crosses block boundary - extend to entire function
							if mut interval := intervals[op] {
								interval.end = total_instrs
							}
						}
					}
				}
			}
		}
	}

	// Mark intervals that cross a function call
	for _, mut interval in intervals {
		for call_idx in call_indices {
			if interval.start < call_idx && interval.end > call_idx {
				interval.has_call = true
				break
			}
		}
	}

	mut sorted := []&Interval{cap: intervals.len}
	for _, i in intervals {
		sorted << i
	}
	sorted.sort(a.start < b.start)

	mut active := []&Interval{cap: 32}

	// Registers
	// Caller-saved (Temporaries): x9..x15
	// Callee-saved (Preserved): x19..x28
	// Reserve x8/x9 as backend data path scratch registers.
	// Reserve x10/x11/x12 for helper temporaries (large offset materialization,
	// address arithmetic, and spill-free internal moves).
	short_regs := [19, 20, 21, 22, 23, 24, 25, 26, 27, 28]
	long_regs := [19, 20, 21, 22, 23, 24, 25, 26, 27, 28]

	// Reusable arrays to avoid allocation in the hot loop
	mut used := []bool{len: 32, init: false}
	mut used_regs_set := []bool{len: 32, init: false}

	for i in sorted {
		// Remove expired intervals from active list
		mut j := 0
		for j < active.len {
			if active[j].end < i.start {
				active.delete(j)
			} else {
				j++
			}
		}

		// Reset used array
		for k in 0 .. 32 {
			used[k] = false
		}
		for a in active {
			used[g.reg_map[a.val_id]] = true
		}

		// Decide which pool to use (avoid clone)
		pool := if i.has_call { long_regs } else { short_regs }

		for r in pool {
			if !used[r] {
				g.reg_map[i.val_id] = r
				active << i
				// Only track used callee-saved regs for prologue saving
				if r >= 19 && r <= 28 && !used_regs_set[r] {
					used_regs_set[r] = true
					g.used_regs << r
				}
				break
			}
		}
	}
	g.used_regs.sort()
	if trace_ra {
		eprintln('REGALLOC fn=${func.name} intervals=${sorted.len} calls=${call_indices.len} allocated=${g.reg_map.len} used_regs=${g.used_regs} total_instrs=${total_instrs}')
		for val_id, reg in g.reg_map {
			if mut iv := intervals[val_id] {
				eprintln('  val=${val_id} -> x${reg} [${iv.start},${iv.end}] has_call=${iv.has_call}')
			}
		}
	}
}

module wasm

import os
import v.flat
import v.ssa

// SSAGen lowers SSA to a WebAssembly binary with WASI output support.
// A dispatch loop represents arbitrary SSA control flow without requiring a
// reducible graph. Each invocation owns its locals and linear-memory frame.
@[heap]
pub struct SSAGen {
mut:
	m             &ssa.Module = unsafe { nil }
	mod           &Module     = unsafe { nil }
	configured    bool
	exports       map[string]string
	init_fns      []string
	main_fn       string
	fn_index      map[string]int
	fn_table      map[string]int
	global_addr   map[string]int
	strings       map[int]int
	data          []u8
	stack_global  int
	heap_global   int
	stack_bottom  int
	write_index   int
	exit_index    int
	alloc_index   int
	cur           Code
	locals        []u8
	value_local   map[int]int
	slots         map[int]int
	phi_snapshots map[int]int
	frame_size    int
	frame_local   int
	pc_local      int
	sret_local    int = -1
	nparams       int
	cur_ret       ssa.TypeID
	warnings      []string
}

// new creates a WebAssembly generator for an SSA module.
pub fn SSAGen.new(m &ssa.Module) &SSAGen {
	return &SSAGen{ m: m, mod: Module.new() }
}

// configure supplies entry points and export names collected by the frontend.
pub fn (mut g SSAGen) configure(exports map[string]string, init_fns []string, main_fn string) {
	g.configured = true
	g.exports = exports.clone()
	g.init_fns = init_fns.clone()
	g.main_fn = main_fn
}

// gen emits only functions reachable from the selected SSA entry points.
pub fn (mut g SSAGen) gen() ! {
	functions := g.reachable_functions()!
	g.collect_data()
	mut init := Code{}
	g.stack_bottom = (data_base + g.data.len + 15) & ~15
	stack_top := g.stack_bottom + 1024 * 1024
	init.i32_const(stack_top)
	init.end()
	g.stack_global = g.mod.add_global(valtype_i32, init.bytes)
	g.heap_global = g.mod.add_global(valtype_i32, init.bytes)
	g.mod.set_mem_min((stack_top + 65535) / 65536 + 1)
	mut uses_write := false
	mut uses_exit := false
	for fid in functions {
		for bid in g.m.funcs[fid].blocks {
			for vid in g.m.blocks[bid].instrs {
				instr := g.m.instrs[g.m.values[vid].index]
				if instr.op == .call && instr.operands.len > 0 {
					if name := g.intrinsic_name(g.m.values[instr.operands[0]]) {
						uses_write = uses_write || name == 'write'
						uses_exit = uses_exit || name in ['exit', 'abort']
					}
				}
			}
		}
	}
	mut helper_count := 1
	mut fd_write := -1
	if uses_write {
		write_type := g.mod.add_type([valtype_i32, valtype_i32, valtype_i32, valtype_i32], [valtype_i32])
		fd_write = g.mod.add_import_func('wasi_snapshot_preview1', 'fd_write', write_type)
	}
	if uses_exit {
		exit_type := g.mod.add_type([valtype_i32], [])
		g.exit_index = g.mod.add_import_func('wasi_snapshot_preview1', 'proc_exit', exit_type)
	}
	if uses_write {
		g.write_index = g.mod.reserve_func_index(0)
		mut runtime := &Gen{ mod: g.mod }
		runtime.emit_write_helper(fd_write)
		helper_count++
	}
	g.alloc_index = g.mod.reserve_func_index(helper_count - 1)
	g.emit_allocator()
	mut table := []int{}
	for i, fid in functions {
		f := g.m.funcs[fid]
		g.fn_index[f.name] = g.mod.reserve_func_index(helper_count + i)
		g.fn_table[f.name] = i + 1
		table << g.fn_index[f.name]
	}
	g.mod.set_table(table)
	for fid in functions {
		g.emit_function(fid)!
	}
	g.mod.add_export('memory', export_mem, 0)
	mut used_exports := map[string]bool{}
	used_exports['memory'] = true
	used_exports['_start'] = true
	for fid in functions {
		f := g.m.funcs[fid]
		if g.configured && f.name !in g.exports {
			continue
		}
		name := if g.configured {
			g.exports[f.name]
		} else {
			f.name.trim_string_left('main__').replace('.', '__')
		}
		if name in used_exports {
			g.warnings << 'not exporting ${f.name.all_after_last('.')}: export name `${name}` is already in use'
			continue
		}
		used_exports[name] = true
		g.mod.add_export(name, export_func, g.fn_index[f.name])
	}
	mut entry := Code{}
	for name in g.init_fns {
		idx := g.fn_index[name] or { return error('wasm: missing init function `${name}`') }
		entry.call(idx)
	}
	if g.main_fn.len > 0 {
		idx := g.fn_index[g.main_fn] or { return error('wasm: missing main function `${g.main_fn}`') }
		entry.call(idx)
	}
	entry.end()
	entry_type := g.mod.add_type([], [])
	entry_index := g.mod.add_func(entry_type, [], entry.bytes)
	if g.main_fn.len > 0 {
		g.mod.add_export('_start', export_func, entry_index)
	} else if g.init_fns.len > 0 {
		g.mod.set_start(entry_index)
	}
	if g.data.len > 0 {
		g.mod.add_data(data_base, g.data)
	}
	g.mod.add_data(nl_ptr, [u8(10)])
}

// write writes the generated module to path.
pub fn (g &SSAGen) write(path string) ! {
	os.write_file_array(path, g.mod.compile())!
}

// warnings_list returns non-fatal export diagnostics.
pub fn (g &SSAGen) warnings_list() []string {
	return g.warnings
}

fn (mut g SSAGen) reachable_functions() ![]int {
	mut names := map[string]int{}
	for i, f in g.m.funcs {
		names[f.name] = i
	}
	mut roots := []string{}
	if g.configured {
		for name, _ in g.exports {
			roots << name
		}
		roots << g.init_fns
		if g.main_fn.len > 0 {
			roots << g.main_fn
		}
	} else {
		for f in g.m.funcs {
			if !f.is_c_extern && f.blocks.len > 0 {
				roots << f.name
			}
			if f.name in ['main', 'main__main'] {
				g.main_fn = f.name
			}
		}
	}
	mut reached := map[int]bool{}
	for roots.len > 0 {
		name := roots.pop()
		fid := names[name] or { return error('wasm: unknown SSA function `${name}`') }
		if fid in reached {
			continue
		}
		f := g.m.funcs[fid]
		if f.is_c_extern || f.blocks.len == 0 {
			return error('wasm: unsupported external function `${name}`')
		}
		reached[fid] = true
		for bid in f.blocks {
			for vid in g.m.blocks[bid].instrs {
				instr := g.m.instrs[g.m.values[vid].index]
				for oi, operand in instr.operands {
					if instr.is_value_operand(oi) && operand > 0 && g.m.values[operand].kind == .func_ref {
						value := g.m.values[operand]
						if _ := g.intrinsic_name(value) { continue }
						roots << g.function_name(value)
					}
				}
			}
		}
	}
	mut functions := []int{}
	for i in 0 .. g.m.funcs.len {
		if i in reached {
			functions << i
		}
	}
	return functions
}

fn (mut g SSAGen) reserve_data(size int, alignment int) int {
	for (data_base + g.data.len) % alignment != 0 {
		g.data << 0
	}
	addr := data_base + g.data.len
	g.data << []u8{len: size}
	return addr
}

fn (mut g SSAGen) collect_data() {
	for global in g.m.globals {
		size := g.m.type_size(global.typ)
		addr := g.reserve_data(if size > 0 { size } else { 8 }, 8)
		g.global_addr[global.name] = addr
		for i in 0 .. size {
			g.data[addr - data_base + i] = if i < global.initial_data.len {
				global.initial_data[i]
			} else if i < 8 {
				u8(u64(global.initial_value) >> (i * 8))
			} else {
				u8(0)
			}
		}
	}
	for i, value in g.m.values {
		if value.kind !in [.string_literal, .c_string_literal] {
			continue
		}
		text := g.reserve_data(value.name.len + 1, 1)
		for j, byte in value.name.bytes() {
			g.data[text - data_base + j] = byte
		}
		if value.kind == .c_string_literal {
			g.strings[i] = text
			continue
		}
		size := g.m.type_size(value.typ)
		addr := g.reserve_data(size, 8)
		g.strings[i] = addr
		for j in 0 .. g.m.target.ptr_size {
			g.data[addr - data_base + j] = u8(u64(text) >> (j * 8))
		}
		len_offset := g.m.struct_field_offset(value.typ, 1)
		lit_offset := g.m.struct_field_offset(value.typ, 2)
		for j in 0 .. 4 {
			g.data[addr - data_base + len_offset + j] = u8(u32(value.name.len) >> (j * 8))
		}
		if lit_offset > 0 && lit_offset < size {
			g.data[addr - data_base + lit_offset] = 1
		}
	}
}

fn (g &SSAGen) aggregate(typ ssa.TypeID) bool {
	return typ > 0 && g.m.type_store.types[typ].kind in [.struct_t, .array_t]
}

fn (g &SSAGen) wtype(typ ssa.TypeID) WType {
	if typ <= 0 { return .void }
	t := g.m.type_store.types[typ]
	return match t.kind {
		.void_t { .void }
		.float_t {
			if t.width == 32 { WType.f32 } else { WType.f64 }
		}
		.int_t {
			if t.width > 32 { WType.i64 } else { WType.i32 }
		}
		else { .i32 }
	}
}

fn (mut g SSAGen) temp(w WType) int {
	idx := g.nparams + g.locals.len
	g.locals << wt_valtype(w)
	return idx
}

fn (mut g SSAGen) reserve_value(id int) {
	if id <= 0 || id in g.value_local { return }
	value := g.m.values[id]
	w := g.wtype(value.typ)
	if w == .void { return }
	g.value_local[id] = g.temp(w)
	if g.aggregate(value.typ) {
		g.frame_size = (g.frame_size + 7) & ~7
		g.slots[id] = g.frame_size
		g.frame_size += g.m.type_size(value.typ)
	}
}

fn (mut g SSAGen) frame_addr(offset int) {
	g.cur.local_get(g.frame_local)
	if offset > 0 {
		g.cur.i32_const(offset)
		g.cur.raw(0x6a)
	}
}

fn (mut g SSAGen) emit_function(fid int) ! {
	f := g.m.funcs[fid]
	g.cur = Code{}
	g.locals = []u8{}
	g.value_local = map[int]int{}
	g.slots = map[int]int{}
	g.phi_snapshots = map[int]int{}
	g.frame_size = 0
	g.cur_ret = f.typ
	g.nparams = f.params.len + if g.aggregate(f.typ) { 1 } else { 0 }
	g.sret_local = if g.aggregate(f.typ) { f.params.len } else { -1 }
	mut params := []u8{}
	for i, param in f.params {
		params << wt_valtype(g.wtype(g.m.values[param].typ))
		g.value_local[param] = i
		if g.aggregate(g.m.values[param].typ) {
			g.frame_size = (g.frame_size + 7) & ~7
			g.slots[param] = g.frame_size
			g.frame_size += g.m.type_size(g.m.values[param].typ)
		}
	}
	if g.sret_local >= 0 { params << valtype_i32 }
	for bid in f.blocks {
		for vid in g.m.blocks[bid].instrs {
			instr := g.m.instrs[g.m.values[vid].index]
			g.reserve_value(vid)
			if instr.op == .phi && g.aggregate(instr.typ) {
				g.frame_size = (g.frame_size + 7) & ~7
				g.phi_snapshots[vid] = g.frame_size
				g.frame_size += g.m.type_size(instr.typ)
			}
			if instr.op == .assign && instr.operands.len > 0 {
				g.reserve_value(instr.operands[0])
			}
			if instr.op == .alloca {
				elem := g.m.type_store.types[instr.typ].elem_type
				mut count := 1
				if instr.operands.len > 0 {
					value := g.m.values[instr.operands[0]]
					if value.kind != .constant {
						return error('wasm: dynamic alloca in `${f.name}`')
					}
					count = int(parse_int_literal(value.name))
				}
				align := g.m.type_align(elem)
				g.frame_size = (g.frame_size + align - 1) / align * align
				g.slots[vid] = g.frame_size
				g.frame_size += g.m.type_size(elem) * if count > 0 { count } else { 1 }
			}
		}
	}
	g.frame_size = (g.frame_size + 15) & ~15
	g.frame_local = g.temp(.i32)
	g.pc_local = g.temp(.i32)
	g.cur.global_get(g.stack_global)
	g.cur.i32_const(g.frame_size)
	g.cur.raw(0x6b)
	g.cur.local_tee(g.frame_local)
	g.cur.i32_const(g.stack_bottom)
	g.cur.raw(0x49)
	g.cur.if_void()
	g.cur.raw(0x00)
	g.cur.end()
	g.cur.local_get(g.frame_local)
	g.cur.global_set(g.stack_global)
	for id, offset in g.slots {
		if g.aggregate(g.m.values[id].typ) {
			g.frame_addr(offset)
			if id in f.params {
				g.cur.local_get(g.value_local[id])
				g.cur.i32_const(g.m.type_size(g.m.values[id].typ))
				g.memory_copy()
				g.frame_addr(offset)
			}
			g.cur.local_set(g.value_local[id])
		}
	}
	g.cur.i32_const(f.blocks[0])
	g.cur.local_set(g.pc_local)
	g.cur.loop_void()
	for bid in f.blocks {
		g.cur.local_get(g.pc_local)
		g.cur.i32_const(bid)
		g.cur.raw(0x46)
		g.cur.if_void()
		for vid in g.m.blocks[bid].instrs {
			g.emit_instruction(vid)!
		}
		g.cur.raw(0x00) // A block must end in an SSA terminator.
		g.cur.end()
	}
	g.cur.raw(0x00) // Invalid dispatch target.
	g.cur.end()
	g.cur.raw(0x00)
	g.cur.end()
	mut results := []u8{}
	if g.wtype(f.typ) != .void && !g.aggregate(f.typ) { results << wt_valtype(g.wtype(f.typ)) }
	ti := g.mod.add_type(params, results)
	g.mod.add_func(ti, g.locals, g.cur.bytes)
}

fn (mut g SSAGen) value(id int) ! {
	if id <= 0 || id >= g.m.values.len { return error('wasm: invalid SSA value ${id}') }
	value := g.m.values[id]
	match value.kind {
		.constant {
			match g.wtype(value.typ) {
				.i64 { g.cur.i64_const(parse_int_literal(value.name)) }
				.f32 { g.cur.f32_const(f32(value.name.f64())) }
				.f64 { g.cur.f64_const(value.name.f64()) }
				else { g.cur.i32_const(parse_int_literal(value.name)) }
			}
			g.narrow(value.typ)
		}
		.string_literal, .c_string_literal { g.cur.i32_const(g.strings[id]) }
		.global {
			addr := g.global_addr[value.name] or { return error('wasm: unknown global `${value.name}`') }
			g.cur.i32_const(addr)
		}
		.func_ref {
			index := g.fn_table[g.function_name(value)] or { return error('wasm: unknown function reference `${value.name}`') }
			if g.wtype(value.typ) == .i64 {
				g.cur.i64_const(index)
			} else {
				g.cur.i32_const(index)
			}
		}
		else {
			idx := g.value_local[id] or { return error('wasm: missing local for SSA value ${id}') }
			g.cur.local_get(idx)
		}
	}
}

fn (mut g SSAGen) value_as(id int, typ ssa.TypeID) ! {
	g.value(id)!
	from := g.wtype(g.m.values[id].typ)
	to := g.wtype(typ)
	signed := !g.m.type_store.types[g.m.values[id].typ].is_unsigned
	g.convert(from, to, signed)
}

fn (mut g SSAGen) convert(from WType, to WType, signed bool) {
	mut runtime := &Gen{ cur: g.cur }
	runtime.coerce(from, to, signed)
	g.cur = runtime.cur
}

fn (mut g SSAGen) narrow(typ ssa.TypeID) {
	if typ <= 0 { return }
	t := g.m.type_store.types[typ]
	if t.kind != .int_t || t.width >= 32 { return }
	if t.width == 1 {
		g.cur.i32_const(1)
		g.cur.raw(0x71)
	} else {
		mut runtime := &Gen{ cur: g.cur }
		runtime.emit_narrow(t.width, t.is_unsigned)
		g.cur = runtime.cur
	}
}

fn (mut g SSAGen) result(id int) {
	g.narrow(g.m.values[id].typ)
	g.cur.local_set(g.value_local[id])
}

fn (mut g SSAGen) memory_copy() {
	g.cur.raw(0xfc)
	g.cur.raw(0x0a)
	g.cur.raw(0x00)
	g.cur.raw(0x00)
}

fn (mut g SSAGen) memory_fill() {
	g.cur.raw(0xfc)
	g.cur.raw(0x0b)
	g.cur.raw(0x00)
}

fn (mut g SSAGen) copy_value(dest int, src int) ! {
	if g.aggregate(g.m.values[dest].typ) {
		g.value(dest)!
		g.copy_aggregate_source(src, g.m.values[dest].typ)!
	} else {
		g.value_as(src, g.m.values[dest].typ)!
		g.result(dest)
	}
}

fn (mut g SSAGen) copy_aggregate_source(src int, typ ssa.TypeID) ! {
	if g.m.values[src].kind == .constant {
		g.cur.i32_const(0)
		g.cur.i32_const(g.m.type_size(typ))
		g.memory_fill()
	} else {
		g.value(src)!
		g.cur.i32_const(g.m.type_size(typ))
		g.memory_copy()
	}
}

fn (mut g SSAGen) edge(from int, to int, depth int) ! {
	mut destinations := []int{}
	mut temporaries := []int{}
	for vid in g.m.blocks[to].instrs {
		instr := g.m.instrs[g.m.values[vid].index]
		if instr.op != .phi { continue }
		for oi := 0; oi + 1 < instr.operands.len; oi += 2 {
			if int(instr.operands[oi + 1]) != from { continue }
			tmp := g.temp(g.wtype(instr.typ))
			if g.aggregate(instr.typ) {
				g.frame_addr(g.phi_snapshots[vid])
				g.copy_aggregate_source(instr.operands[oi], instr.typ)!
				g.frame_addr(g.phi_snapshots[vid])
			} else {
				g.value_as(instr.operands[oi], instr.typ)!
			}
			g.cur.local_set(tmp)
			destinations << vid
			temporaries << tmp
		}
	}
	for i, dest in destinations {
		if g.aggregate(g.m.values[dest].typ) {
			g.value(dest)!
			g.cur.local_get(temporaries[i])
			g.cur.i32_const(g.m.type_size(g.m.values[dest].typ))
			g.memory_copy()
		} else {
			g.cur.local_get(temporaries[i])
			g.result(dest)
		}
	}
	g.cur.i32_const(to)
	g.cur.local_set(g.pc_local)
	g.cur.br(depth)
}

fn (mut g SSAGen) load_scalar(typ ssa.TypeID, offset int) {
	t := g.m.type_store.types[typ]
	if t.kind in [.ptr_t, .func_t] && g.m.target.ptr_size == 8 {
		g.cur.load(0x29, 0, offset)
		g.cur.raw(0xa7)
		return
	}
	op := match g.wtype(typ) {
		.i64 { u8(0x29) }
		.f32 { u8(0x2a) }
		.f64 { u8(0x2b) }
		else {
			if t.kind == .int_t && t.width <= 8 {
				if t.is_unsigned || t.width == 1 { u8(0x2d) } else { u8(0x2c) }
			} else if t.kind == .int_t && t.width <= 16 {
				if t.is_unsigned { u8(0x2f) } else { u8(0x2e) }
			} else {
				u8(0x28)
			}
		}
	}
	g.cur.load(op, 0, offset)
}

fn (mut g SSAGen) store_scalar(typ ssa.TypeID, offset int) {
	t := g.m.type_store.types[typ]
	if t.kind in [.ptr_t, .func_t] && g.m.target.ptr_size == 8 {
		g.cur.raw(0xad)
		g.cur.store(0x37, 0, offset)
		return
	}
	op := match g.wtype(typ) {
		.i64 { u8(0x37) }
		.f32 { u8(0x38) }
		.f64 { u8(0x39) }
		else {
			if t.kind == .int_t && t.width <= 8 {
				u8(0x3a)
			} else if t.kind == .int_t && t.width <= 16 {
				u8(0x3b)
			} else {
				u8(0x36)
			}
		}
	}
	g.cur.store(op, 0, offset)
}

fn (mut g SSAGen) emit_instruction(id int) ! {
	instr := g.m.instrs[g.m.values[id].index]
	ops := instr.operands
	match instr.op {
		.alloca {
			g.frame_addr(g.slots[id])
			g.result(id)
			// V aggregate and local storage starts zeroed.
			g.value(id)!
			g.cur.i32_const(0)
			elem := g.m.type_store.types[instr.typ].elem_type
			mut count := 1
			if ops.len > 0 { count = int(parse_int_literal(g.m.values[ops[0]].name)) }
			g.cur.i32_const(g.m.type_size(elem) * if count > 0 { count } else { 1 })
			g.memory_fill()
		}
		.heap_alloc {
			elem := g.m.type_store.types[instr.typ].elem_type
			g.cur.i32_const(g.m.type_size(elem))
			g.cur.call(g.alloc_index)
			g.result(id)
		}
		.get_element_ptr {
			g.value(ops[0])!
			g.value(ops[1])!
			g.convert(g.wtype(g.m.values[ops[1]].typ), .i32, false)
			g.cur.raw(0x6a)
			g.result(id)
		}
		.load {
			if g.aggregate(instr.typ) {
				g.value(id)!
				g.value(ops[0])!
				g.cur.i32_const(g.m.type_size(instr.typ))
				g.memory_copy()
			} else {
				g.value(ops[0])!
				g.load_scalar(instr.typ, 0)
				g.result(id)
			}
		}
		.store {
			ptr_type := g.m.type_store.types[g.m.values[ops[1]].typ]
			typ := if ptr_type.kind == .ptr_t { ptr_type.elem_type } else { g.m.values[ops[0]].typ }
			g.value(ops[1])!
			if g.aggregate(typ) {
				if g.m.values[ops[0]].kind == .constant {
					g.cur.i32_const(0)
					g.cur.i32_const(g.m.type_size(typ))
					g.memory_fill()
				} else {
					g.value(ops[0])!
					g.cur.i32_const(g.m.type_size(typ))
					g.memory_copy()
				}
			} else {
				g.value_as(ops[0], typ)!
				g.store_scalar(typ, 0)
			}
		}
		.shl, .ashr, .lshr {
			w := g.wtype(instr.typ)
			count_w := g.wtype(g.m.values[ops[1]].typ)
			count := g.temp(count_w)
			g.value_as(ops[0], instr.typ)!
			if instr.op == .lshr && w == .i32 {
				lhs := g.m.type_store.types[g.m.values[ops[0]].typ]
				if lhs.kind == .int_t && lhs.width > 0 && lhs.width < 32 {
					g.cur.i32_const((i64(1) << lhs.width) - 1)
					g.cur.raw(0x71)
				}
			}
			g.value(ops[1])!
			g.cur.local_tee(count)
			g.convert(count_w, w, false)
			g.cur.raw(ssa_wasm_binary(instr.op, w)!)
			init_push_zero(w, mut g.cur)
			g.cur.local_get(count)
			width := if w == .i64 { 64 } else { 32 }
			if count_w == .i64 {
				g.cur.i64_const(width)
				g.cur.raw(0x54)
			} else {
				g.cur.i32_const(width)
				g.cur.raw(0x49)
			}
			g.cur.raw(0x1b)
			g.result(id)
		}
		.add, .sub, .mul, .sdiv, .srem, .udiv, .urem, .and_, .or_, .xor,
		.fadd, .fsub, .fmul, .fdiv {
			g.value_as(ops[0], instr.typ)!
			g.value_as(ops[1], instr.typ)!
			g.cur.raw(ssa_wasm_binary(instr.op, g.wtype(instr.typ))!)
			g.result(id)
		}
		.lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge, .eq, .ne {
			typ := g.m.values[ops[0]].typ
			g.value(ops[0])!
			g.value_as(ops[1], typ)!
			g.cur.raw(ssa_wasm_compare(instr.op, g.wtype(typ)))
			g.result(id)
		}
		.neg {
			w := g.wtype(instr.typ)
			if w in [.f32, .f64] {
				g.value(ops[0])!
				g.cur.raw(if w == .f32 { u8(0x8c) } else { u8(0x9a) })
			} else {
				init_push_zero(w, mut g.cur)
				g.value(ops[0])!
				g.cur.raw(if w == .i64 { u8(0x7d) } else { u8(0x6b) })
			}
			g.result(id)
		}
		.trunc, .sext, .zext, .fptoui, .fptosi, .uitofp, .sitofp, .bitcast {
			if g.aggregate(instr.typ) {
				g.copy_value(id, ops[0])!
				return
			}
			g.value(ops[0])!
			from := g.wtype(g.m.values[ops[0]].typ)
			to := g.wtype(instr.typ)
			if instr.op in [.zext, .sext] && from == .i32 {
				source_width := g.m.type_store.types[g.m.values[ops[0]].typ].width
				if source_width > 0 && source_width < 32 {
					if instr.op == .zext || source_width == 1 {
						g.cur.i32_const((i64(1) << source_width) - 1)
						g.cur.raw(0x71)
					} else if source_width == 8 {
						g.cur.raw(0xc0)
					} else if source_width == 16 {
						g.cur.raw(0xc1)
					}
				}
			}
			float_width_cast := from in [.f32, .f64] && to in [.f32, .f64]
			if instr.op == .bitcast && from != to && !float_width_cast && ((from in [
				.f32,
				.f64,
			]) || (to in [
				.f32,
				.f64,
			])) {
				op := if from == .f32 && to == .i32 {
					u8(0xbc)
				} else if from == .f64 && to == .i64 {
					u8(0xbd)
				} else if from == .i32 && to == .f32 {
					u8(0xbe)
				} else if from == .i64 && to == .f64 {
					u8(0xbf)
				} else {
					return error('wasm: incompatible bitcast ${from} to ${to}')
				}
				g.cur.raw(op)
			} else {
				source := g.m.type_store.types[g.m.values[ops[0]].typ]
				signed := if instr.op == .bitcast {
					source.kind !in [.ptr_t, .func_t] && !source.is_unsigned
				} else {
					instr.op !in [.zext, .fptoui, .uitofp]
				}
				g.convert(from, to, signed)
			}
			g.result(id)
		}
		.select {
			if g.aggregate(instr.typ) {
				g.value(ops[0])!
				g.cur.if_void()
				g.copy_value(id, ops[1])!
				g.cur.else_()
				g.copy_value(id, ops[2])!
				g.cur.end()
				return
			}
			g.value_as(ops[1], instr.typ)!
			g.value_as(ops[2], instr.typ)!
			g.value(ops[0])!
			g.cur.raw(0x1b)
			g.result(id)
		}
		.assign { g.copy_value(ops[0], ops[1])! }
		.phi {}
		.call, .call_indirect, .call_sret { g.emit_call(id, instr)! }
		.ret {
			if g.sret_local >= 0 {
				g.cur.local_get(g.sret_local)
				g.copy_aggregate_source(ops[0], g.cur_ret)!
			} else if g.wtype(g.cur_ret) != .void {
				g.value_as(ops[0], g.cur_ret)!
				g.narrow(g.cur_ret)
			}
			g.cur.local_get(g.frame_local)
			g.cur.i32_const(g.frame_size)
			g.cur.raw(0x6a)
			g.cur.global_set(g.stack_global)
			g.cur.ret()
		}
		.jmp { g.edge(instr.block, ops[0], 1)! }
		.br {
			g.value(ops[0])!
			g.cur.if_void()
			g.edge(instr.block, ops[1], 2)!
			g.cur.else_()
			g.edge(instr.block, ops[2], 2)!
			g.cur.end()
		}
		.switch_ {
			for oi := 2; oi + 1 < ops.len; oi += 2 {
				g.value(ops[0])!
				g.value_as(ops[oi], g.m.values[ops[0]].typ)!
				g.cur.raw(if g.wtype(g.m.values[ops[0]].typ) == .i64 { u8(0x51) } else { u8(0x46) })
				g.cur.if_void()
				g.edge(instr.block, ops[oi + 1], 2)!
				g.cur.end()
			}
			g.edge(instr.block, ops[1], 1)!
		}
		.unreachable { g.cur.raw(0x00) }
		.fence {}
		else { return error('wasm: unsupported SSA opcode `${instr.op}`') }
	}
}

fn (g &SSAGen) function_name(value ssa.Value) string {
	if value.index >= 0 && value.index < g.m.funcs.len {
		return g.m.funcs[value.index].name
	}
	return value.name
}

fn (g &SSAGen) intrinsic_name(value ssa.Value) ?string {
	if value.kind != .func_ref || value.index < 0 || value.index >= g.m.funcs.len || !g.m.funcs[value.index].is_c_extern {
		return none
	}
	name := g.function_name(value).trim_string_left('C.')
	if name in ['write', 'malloc', 'calloc', 'free', 'memcpy', 'memmove', 'memset', 'exit', 'abort'] {
		return name
	}
	return none
}

fn (mut g SSAGen) emit_call(id int, instr ssa.Instruction) ! {
	ops := instr.operands
	callee := g.m.values[ops[0]]
	if instr.op != .call_indirect {
		if name := g.intrinsic_name(callee) {
			g.emit_intrinsic(id, instr, name)!
			return
		}
	}
	mut params := []u8{}
	for oi in 1 .. ops.len {
		mut typ := g.m.values[ops[oi]].typ
		if instr.op != .call_indirect && callee.index >= 0 && callee.index < g.m.funcs.len {
			f := g.m.funcs[callee.index]
			if oi - 1 < f.params.len { typ = g.m.values[f.params[oi - 1]].typ }
		}
		g.value_as(ops[oi], typ)!
		params << wt_valtype(g.wtype(typ))
	}
	if g.aggregate(instr.typ) {
		g.value(id)!
		params << valtype_i32
	}
	if instr.op == .call_indirect {
		mut results := []u8{}
		if g.wtype(instr.typ) != .void && !g.aggregate(instr.typ) {
			results << wt_valtype(g.wtype(instr.typ))
		}
		ti := g.mod.add_type(params, results)
		g.value(ops[0])!
		g.convert(g.wtype(callee.typ), .i32, false)
		g.cur.raw(0x11)
		leb_u(mut g.cur.bytes, u64(ti))
		g.cur.raw(0x00)
	} else {
		index := g.fn_index[g.function_name(callee)] or { return error('wasm: missing callee `${callee.name}`') }
		g.cur.call(index)
	}
	if g.wtype(instr.typ) != .void && !g.aggregate(instr.typ) { g.result(id) }
}

fn (mut g SSAGen) intrinsic_arg(id int) ! {
	g.value(id)!
	g.convert(g.wtype(g.m.values[id].typ), .i32, false)
}

fn (mut g SSAGen) emit_intrinsic(id int, instr ssa.Instruction, name string) ! {
	ops := instr.operands
	match name {
		'write' {
			g.intrinsic_arg(ops[2])!
			g.intrinsic_arg(ops[3])!
			g.intrinsic_arg(ops[1])!
			g.cur.call(g.write_index)
			if g.wtype(instr.typ) != .void {
				g.value_as(ops[3], instr.typ)!
				g.result(id)
			}
		}
		'malloc', 'calloc' {
			g.intrinsic_arg(ops[1])!
			if name == 'calloc' {
				g.intrinsic_arg(ops[2])!
				g.cur.raw(0x6c)
			}
			g.cur.call(g.alloc_index)
			g.result(id)
		}
		'free' {}
		'memcpy', 'memmove' {
			g.intrinsic_arg(ops[1])!
			g.intrinsic_arg(ops[2])!
			g.intrinsic_arg(ops[3])!
			g.memory_copy()
			if g.wtype(instr.typ) != .void {
				g.value_as(ops[1], instr.typ)!
				g.result(id)
			}
		}
		'memset' {
			g.intrinsic_arg(ops[1])!
			g.intrinsic_arg(ops[2])!
			g.intrinsic_arg(ops[3])!
			g.memory_fill()
			if g.wtype(instr.typ) != .void {
				g.value_as(ops[1], instr.typ)!
				g.result(id)
			}
		}
		'exit', 'abort' {
			if name == 'exit' {
				g.intrinsic_arg(ops[1])!
			} else {
				g.cur.i32_const(1)
			}
			g.cur.call(g.exit_index)
			g.cur.raw(0x00)
		}
		else { return error('wasm: unsupported intrinsic `${name}`') }
	}
}

fn (mut g SSAGen) emit_allocator() {
	mut c := Code{}
	// malloc(size): align the bump pointer, grow memory, and zero the allocation.
	c.global_get(g.heap_global)
	c.local_set(1)
	c.local_get(1)
	c.local_get(0)
	c.raw(0x6a)
	c.i32_const(15)
	c.raw(0x6a)
	c.i32_const(-16)
	c.raw(0x71)
	c.local_tee(2)
	c.local_get(1)
	c.raw(0x49)
	c.if_void()
	c.raw(0x00)
	c.end()
	c.local_get(2)
	c.i32_const(65535)
	c.raw(0x6a)
	c.i32_const(16)
	c.raw(0x76)
	c.raw(0x3f)
	c.raw(0x00)
	c.local_tee(3)
	c.raw(0x4b)
	c.if_void()
	c.local_get(2)
	c.i32_const(65535)
	c.raw(0x6a)
	c.i32_const(16)
	c.raw(0x76)
	c.local_get(3)
	c.raw(0x6b)
	c.raw(0x40)
	c.raw(0x00)
	c.i32_const(-1)
	c.raw(0x46)
	c.if_void()
	c.raw(0x00)
	c.end()
	c.end()
	c.local_get(2)
	c.global_set(g.heap_global)
	c.local_get(1)
	c.i32_const(0)
	c.local_get(0)
	c.raw(0xfc)
	c.raw(0x0b)
	c.raw(0x00)
	c.local_get(1)
	c.end()
	ti := g.mod.add_type([valtype_i32], [valtype_i32])
	g.mod.add_func(ti, [valtype_i32, valtype_i32, valtype_i32], c.bytes)
}

fn ssa_wasm_binary(op ssa.OpCode, w WType) !u8 {
	if w in [.f32, .f64] {
		base := if w == .f32 { u8(0x92) } else { u8(0xa0) }
		return match op {
			.add, .fadd { base }
			.sub, .fsub { base + 1 }
			.mul, .fmul { base + 2 }
			.sdiv, .udiv, .fdiv { base + 3 }
			else { return error('wasm: unsupported floating operation `${op}`') }
		}
	}
	base := if w == .i64 { u8(0x7c) } else { u8(0x6a) }
	return match op {
		.add { base }
		.sub { base + 1 }
		.mul { base + 2 }
		.sdiv { base + 3 }
		.udiv { base + 4 }
		.srem { base + 5 }
		.urem { base + 6 }
		.and_ { base + 7 }
		.or_ { base + 8 }
		.xor { base + 9 }
		.shl { base + 10 }
		.ashr { base + 11 }
		.lshr { base + 12 }
		else { return error('wasm: unsupported integer operation `${op}`') }
	}
}

fn ssa_wasm_compare(op ssa.OpCode, w WType) u8 {
	flat_op := match op {
		.eq { flat.Op.eq }
		.ne { flat.Op.ne }
		.lt, .ult { flat.Op.lt }
		.gt, .ugt { flat.Op.gt }
		.le, .ule { flat.Op.le }
		else { flat.Op.ge }
	}
	return cmp_op(flat_op, w, op !in [.ult, .ugt, .ule, .uge])
}

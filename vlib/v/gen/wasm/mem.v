// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import wasm
import v.ast

struct Var {
	name       string
mut:
	typ        ast.Type
	idx        wasm.LocalIndex
	is_pointer bool
	is_global  bool
	g_idx      wasm.GlobalIndex
	offset     int
}

fn (mut g Gen) get_var_from_ident(ident ast.Ident) Var {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.w_error('unknown variable ${ident.name}') }
	}

	match mut obj {
		ast.Var {
			if g.local_vars.len == 0 {
				g.w_error('get_var_from_ident: g.local_vars.len == 0')
			}
			mut c := g.local_vars.len
			for {
				c--
				if g.local_vars[c].name == obj.name {
					return g.local_vars[c]
				}
				if c == 0 {
					break
				}
			}
			g.w_error('get_var_from_ident: unreachable, variable not found')
		}
		ast.ConstField {
			if gbl := g.global_vars[obj.name] {
				return gbl.v
			}
			gbl := g.new_global(obj.name, obj.typ, obj.expr)
			g.global_vars[obj.name] = gbl
			return gbl.v
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}

fn (mut g Gen) get_var_from_expr(node ast.Expr) ?Var {
	match node {
		ast.Ident {
			return g.get_var_from_ident(node)
		}
		ast.ParExpr {
			return g.get_var_from_expr(node.expr)
		}
		ast.SelectorExpr {
			addr := g.get_var_from_expr(node.expr) or {
				g.field_offset(node.expr_type, node.field_name)
				return none
			}

			offset := g.get_field_offset(node.expr_type, node.field_name)
			return g.offset(addr, node.typ, offset)
		}
		/* ast.SelectorExpr {
			
		}
		ast.IndexExpr {

		} */
		/* ast.PrefixExpr {
			// *p = expr
			v := g.get_var_from_expr(node.expr) or {
				return none
			}
			v.ref()
			return none
		} */
		else {
			g.w_error('get_var_from_expr: unexpected `${node.type_name()}`')
		}
	}
}

fn (mut g Gen) bp() wasm.LocalIndex {
	if g.bp_idx == -1 {
		g.bp_idx = g.func.new_local(.i32_t)
	}
	return g.bp_idx
}

fn (mut g Gen) sp() wasm.GlobalIndex {
	if sp := g.sp_global {
		return sp
	}
	// (64KiB - 1KiB) of stack space, grows downwards.
	// Memory addresses from 0 to 1024 are forbidden.
	g.sp_global = g.mod.new_global(none, .i32_t, true, wasm.constexpr_value(0))
	return g.sp()
}

fn (mut g Gen) new_local(name string, typ_ ast.Type) Var {
	mut typ := typ_
	ts := g.table.sym(typ)

	match ts.info {
		ast.Enum {
			typ = ts.info.typ
		}
		ast.Alias {
			typ = ts.info.parent_type
		}
		else {}
	}
	
	is_pointer := typ.nr_muls() == 0 && !g.is_pure_type(typ)
	wtyp := g.get_wasm_type(typ)

	mut v := Var{
		name: name
		typ: typ
		is_pointer: is_pointer
	}

	if !is_pointer {
		v.idx = g.func.new_local(wtyp)
		g.local_vars << v
		return v
	}
	v.idx = g.bp()

	// allocate memory, then assign an offset
	// 
	match ts.info {
		ast.Struct, ast.ArrayFixed {
			size, align := g.pool.type_size(typ)
			padding := calc_padding(g.stack_frame, align)
			address := g.stack_frame
			g.stack_frame += size + padding

			v.offset = address
		}
		else {
			g.w_error('new_local: type `${*ts}` (${ts.info.type_name()}) is not a supported local type')
		}
	}
	g.local_vars << v
	return v
}

fn (mut g Gen) literal_to_constant_expression(typ_ ast.Type, init ast.Expr) ?wasm.ConstExpression {
	typ := ast.mktyp(typ_)
	match init {
		ast.BoolLiteral {
			return wasm.constexpr_value(int(init.val))
		}
		ast.CharLiteral {
			return wasm.constexpr_value(int(init.val.runes()[0]))
		}
		ast.FloatLiteral {
			if typ == ast.f32_type {
				return wasm.constexpr_value(init.val.f32())
			} else if typ == ast.f64_type {
				return wasm.constexpr_value(init.val.f64())
			}
		}
		ast.IntegerLiteral {
			if !typ.is_pure_int() {
				return none
			}
			t := g.get_wasm_type(typ)
			match t {
				.i32_t { return wasm.constexpr_value(init.val.int()) }
				.i64_t { return wasm.constexpr_value(init.val.i64()) }
				else {}
			}
		}
		else {}
	}
	return none
}

fn (mut g Gen) new_global(name string, typ_ ast.Type, init ast.Expr) Global {
	mut typ := typ_
	ts := g.table.sym(typ)

	match ts.info {
		ast.Enum {
			typ = ts.info.typ
		}
		ast.Alias {
			typ = ts.info.parent_type
		}
		else {}
	}

	mut is_mut := false
	is_pointer := typ.nr_muls() == 0 && !g.is_pure_type(typ)
	mut init_expr := ?ast.Expr(none)
	mut data_seg_pos := ?int(none)
	cexpr := if cexpr_v := g.literal_to_constant_expression(typ, init) {
		cexpr_v
	} else {
		// Isn't a literal ...
		if is_pointer {
			// ... allocate memory and append
			pos, is_init := g.pool.append(init, typ)
			if !is_init {
				// ... AND wait for init in `_vinit`
				init_expr = init
			}
			data_seg_pos = pos
			wasm.constexpr_value(0)
		} else {
			// ... wait for init in `_vinit`
			init_expr = init
			is_mut = true

			t := g.get_wasm_type(typ)
			wasm.constexpr_value_zero(t)
		}
	}

	mut glbl := Global{
		init: init_expr
		data_seg: data_seg_pos
		v: Var{
			name: name
			typ: typ
			is_pointer: is_pointer
			is_global: true
			g_idx: g.mod.new_global(none, g.get_wasm_type(typ), is_mut, cexpr)
		}
	}

	return glbl
}

// is_pure_type(voidptr) == true
// is_pure_type(&Struct) == false
fn (g Gen) is_pure_type(typ ast.Type) bool {
	if typ.is_pure_int() || typ.is_pure_float() || typ == ast.char_type_idx || typ.is_pointer()
		|| typ.is_bool() {
		return true
	}
	ts := g.table.sym(typ)
	if ts.info is ast.Alias {
		return g.is_pure_type(ts.info.parent_type)
	}
	return false
}

fn log2(size int) int {
	return match size {
		1 { 0 }
		2 { 1 }
		4 { 2 }
		8 { 3 }
		else { panic("unreachable") }
	}
}

fn (mut g Gen) load(typ ast.Type, offset int) {
	size, align := g.table.type_size(typ)
	wtyp := g.as_numtype(g.get_wasm_type(typ))

	match size {
		1 { g.func.load8(wtyp, typ.is_signed(), log2(align), offset) }
		2 { g.func.load16(wtyp, typ.is_signed(), log2(align), offset) }
		else { g.func.load(wtyp, log2(align), offset) }
	}
}

fn (mut g Gen) store(typ ast.Type, offset int) {
	size, align := g.table.type_size(typ)
	wtyp := g.as_numtype(g.get_wasm_type(typ))

	match size {
		1 { g.func.store8(wtyp, log2(align), offset) }
		2 { g.func.store16(wtyp, log2(align), offset) }
		else { g.func.store(wtyp, log2(align), offset) }
	}
}

fn (mut g Gen) get(v Var) {
	if v.is_global {
		g.func.global_get(v.g_idx)
	} else {
		g.func.local_get(v.idx)
	}

	if v.is_pointer && g.is_pure_type(v.typ) {
		g.load(v.typ, v.offset)
	} else if v.is_pointer && v.offset != 0 {
		g.func.i32_const(v.offset)
		g.func.add(.i32_t)
	}
}

/* fn (mut g Gen) copy(to Var, v Var) {
	if v.is_pointer && !g.is_pure_type(v.typ) {

		return
	}

	g.get(v)
	g.set(to)
} */

// set structures with pointer, memcpy
// set pointers with value, get local, store value
// set value, set local
fn (mut g Gen) set(v Var) {
	if !v.is_pointer {
		if v.is_global {
			g.func.global_set(v.g_idx)
		} else {
			g.func.local_set(v.idx)
		}
		return
	}

	if g.is_pure_type(v.typ) {
		if v.is_global {
			g.func.global_get(v.g_idx)
		} else {
			g.func.local_get(v.idx)
		}

		g.store(v.typ, v.offset)
		return
	}

	size, _ := g.pool.type_size(v.typ)

	l := g.func.new_local(.i32_t)
	g.func.local_set(l)
	
	if size > 8 {
		g.ref(v)
		g.func.local_get(l)
		g.func.i32_const(size)
		g.func.memory_copy()
		return
	}

	mut sz := size
	mut oz := 0
	for sz > 0 {
		g.ref_ignore_offset(v)
		g.func.local_get(l)
		if sz - 8 >= 0 {
			g.load(ast.u64_type_idx, oz)
			g.store(ast.u64_type_idx, v.offset + oz)
			sz -= 8
			oz += 8
		} else if sz - 4 >= 0 {
			g.load(ast.u32_type_idx, oz)
			g.store(ast.u32_type_idx, v.offset + oz)
			sz -= 4
			oz += 4
		} else if sz - 2 >= 0 {
			g.load(ast.u16_type_idx, oz)
			g.store(ast.u16_type_idx, v.offset + oz)
			sz -= 2
			oz += 2
		} else if sz - 1 >= 0 {
			g.load(ast.u8_type_idx, oz)
			g.store(ast.u8_type_idx, v.offset + oz)
			sz -= 1
			oz += 1
		}
	}
}

fn (mut g Gen) ref(v Var) {
	g.ref_ignore_offset(v)

	if v.offset != 0 {
		g.func.i32_const(v.offset)
		g.func.add(.i32_t)
	}
}

fn (mut g Gen) ref_ignore_offset(v Var) {
	if !v.is_pointer {
		panic('unreachable')
	}

	if v.is_global {
		g.func.global_get(v.g_idx)
	} else {
		g.func.local_get(v.idx)
	}
}

// creates a new pointer variable with the offset `offset` and type `typ`
fn (mut g Gen) offset(v Var, typ ast.Type, offset int) Var {
	if !v.is_pointer {
		panic('unreachable')
	}
	
	nv := Var {
		...v
		typ: typ
		offset: v.offset + offset
	}

	return nv
}

fn (mut g Gen) zero_fill(v Var, size int) {
	assert size > 0

	// TODO: support coalescing `zero_fill` calls together.
	//       maybe with some kind of context?
	//
	// ```v
	// struct AA {
	//     a bool
	//     b int = 20
	//     c int
	//     d int
	// }
	// ```
	//
	// ```wast
	// (i32.store8
	//  (local.get $0)
	//  (i32.const 0)
	// )
	// (i32.store offset=4
	//  (i32.const 20)
	//  (local.get $0)
	// )                    ;; /- join these together.
	// (i32.store offset=8  ;;-\
	//  (local.get $0)      ;; | 
	//  (i32.const 0)       ;; |
	// )                    ;; |
	// (i32.store offset=12 ;; |
	//  (local.get $0)      ;; |
	//  (i32.const 0)       ;; |
	// )                    ;;-/
	// ```

	if size > 16 {
		g.ref(v)
		g.func.i32_const(0)
		g.func.i32_const(size)
		g.func.memory_fill()
		return
	}

	mut sz := size
	mut oz := 0
	for sz > 0 {
		g.ref_ignore_offset(v)
		if sz - 8 >= 0 {
			g.func.i64_const(0)
			g.store(ast.u64_type_idx, v.offset + oz)
			sz -= 8
			oz += 8
		} else if sz - 4 >= 0 {
			g.func.i32_const(0)
			g.store(ast.u32_type_idx, v.offset + oz)
			sz -= 4
			oz += 4
		} else if sz - 2 >= 0 {
			g.func.i32_const(0)
			g.store(ast.u16_type_idx, v.offset + oz)
			sz -= 2
			oz += 2
		} else if sz - 1 >= 0 {
			g.func.i32_const(0)
			g.store(ast.u8_type_idx, v.offset + oz)
			sz -= 1
			oz += 1
		}
	}
}

fn (mut g Gen) set_with_expr(init ast.Expr, v Var) {
	match init {
		ast.StructInit {
			size, _ := g.pool.type_size(v.typ)
			ts := g.table.sym(v.typ)
			ts_info := ts.info as ast.Struct
			si := g.pool.type_struct_info(v.typ) or { panic("unreachable") }
			
			if init.fields.len == 0 && !(ts_info.fields.any(it.has_default_expr)) {
				// Struct definition contains no default initialisers
				// AND struct init contains no set values.
				g.zero_fill(v, size)
				return
			}

			for i, f in ts_info.fields {
				field_to_be_set := init.fields.map(it.name).contains(f.name)

				if !field_to_be_set {
					offset := si.offsets[i]
					offset_var := g.offset(v, f.typ, offset)

					fsize, _ := g.pool.type_size(f.typ)
					
					if f.has_default_expr {
						g.expr(f.default_expr, f.typ)
						g.set(offset_var)
					} else {
						g.zero_fill(offset_var, fsize)
					}						
				}
			}

			for f in init.fields {
				field := ts.find_field(f.name) or {
					g.w_error('could not find field `${f.name}` on init')
				}
				
				offset := si.offsets[field.i]
				offset_var := g.offset(v, f.expected_type, offset)

				g.expr(f.expr, f.expected_type)
				g.set(offset_var)
			}
		}
		ast.CallExpr {
			// `set_with_expr` is never called with a multireturn call expression
			is_pt := g.is_param_type(v.typ)

			g.call_expr(init, v.typ, if is_pt { [v] } else { []Var{} })
			if !is_pt {
				g.set(v)
			}
		}
		else {
			g.expr(init, v.typ)
			g.set(v)
		}
	}
}

fn calc_padding(value int, alignment int) int {
	if alignment == 0 {
		return value
	}
	return (alignment - value % alignment) % alignment
}


fn calc_align(value int, alignment int) int {
	if alignment == 0 {
		return value
	}
	return (value + alignment - 1) / alignment * alignment
}

fn (mut g Gen) make_vinit() {
	g.func = g.mod.new_function('_vinit', [], [])
	func_start := g.func.patch_pos()
	{
		for mod_name in g.table.modules {
			if mod_name == 'v.reflection' {
				g.w_error('the wasm backend does not implement `v.reflection` yet')
			}
			init_fn_name := if mod_name != 'builtin' { '${mod_name}.init' } else { 'init' }
			if _ := g.table.find_fn(init_fn_name) {
				g.func.call(init_fn_name)
			}
			cleanup_fn_name := if mod_name != 'builtin' { '${mod_name}.cleanup' } else { 'cleanup' }
			if _ := g.table.find_fn(cleanup_fn_name) {
				g.func.call(cleanup_fn_name)
			}

			for _, gv in g.global_vars {
				if init := gv.init {
					g.expr(init, gv.v.typ)
					g.set(gv.v)
				}
			}
		}
		g.bare_function_frame(func_start)
	}
	g.mod.commit(g.func, true)
	g.bare_function_end()
}

fn (mut g Gen) housekeeping() {
	g.make_vinit()
	
	stack_top := 1024 + (16 * 1024)
	data_base := calc_align(stack_top + 1, g.pool.highest_alignment)
	heap_base := calc_align(data_base + g.pool.buf.len, 16) // 16?
	page_boundary := calc_align(data_base + g.pool.buf.len, 64 * 1024)
	preallocated_pages := page_boundary / (64 * 1024)

	if g.pref.is_verbose {
		eprintln('housekeeping(): acceptable addresses are > 1024')
		eprintln('housekeeping(): stack top: ${stack_top}, data_base: ${data_base} (size: ${g.pool.buf.len}), heap_base: ${heap_base}')
		eprintln('housekeeping(): preallocated pages: ${preallocated_pages}')
	}

	if sp := g.sp_global {
		g.mod.assign_global_init(sp, wasm.constexpr_value(stack_top))
	}
	if g.sp_global != none || g.pool.buf.len > 0 {
		g.mod.assign_memory('memory', true, u32(preallocated_pages), none)
		if g.pool.buf.len > 0 {
			g.mod.new_data_segment(data_base, g.pool.buf)
		}
	}
	for _, gv in g.global_vars {
		if data_seg := gv.data_seg {
			g.mod.assign_global_init(gv.v.g_idx, wasm.constexpr_value(data_base + data_seg))
		}
	}
	if hp := g.heap_base {
		g.mod.assign_global_init(hp, wasm.constexpr_value(heap_base))
	}

	if g.pref.os == .wasi {
		mut fn_start := g.mod.new_function('_start', [], [])
		{
			fn_start.call('_vinit')
			fn_start.call('main.main')
		}
		g.mod.commit(fn_start, true)
	} else {
		g.mod.assign_start('_vinit')
	}
}

// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module wasm

import v.ast
import v.pref
import v.util
import v.token
import v.errors
import v.eval
import v.serialise
import wasm
import os

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	file_path     string // current ast.File path
	warnings      []errors.Warning
	errors        []errors.Error
	table         &ast.Table = unsafe { nil }
	eval          eval.Eval
	enum_vals     map[string]Enum
	//
	mod wasm.Module
	pool serialise.Pool
	func              wasm.Function
	local_vars        []Var
	global_vars       map[string]Global
	return_vars       []Var
	ret_types []ast.Type
	ret_br wasm.LabelIndex
	bp_idx            wasm.LocalIndex = -1 // Base pointer temporary's index for function, if needed (-1 for none)
	sp_global         ?wasm.GlobalIndex
	heap_base         ?wasm.GlobalIndex
	stack_frame       int // Size of the current stack frame, if needed
	is_leaf_function  bool = true
	loop_breakpoint_stack []LoopBreakpoint
	stack_top int // position in linear memory
	data_base int // position in linear memory
	needs_address bool
}

struct Global {
mut:
	init     ?ast.Expr
	v    Var
}

struct LoopBreakpoint {
	c_continue wasm.LabelIndex
	c_break    wasm.LabelIndex
	name       string
}

pub fn (mut g Gen) v_error(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('error:', pos: pos, file_path: g.file_path, message: s)
		exit(1)
	} else {
		g.errors << errors.Error{
			file_path: g.file_path
			pos: pos
			reporter: .gen
			message: s
		}
	}
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('warning:', pos: pos, file_path: g.file_path, message: s)
	} else {
		g.warnings << errors.Warning{
			file_path: g.file_path
			pos: pos
			reporter: .gen
			message: s
		}
	}
}

[noreturn]
pub fn (mut g Gen) w_error(s string) {
	if g.pref.is_verbose {
		print_backtrace()
	}
	util.verror('wasm error', s)
}

fn (g Gen) unpack_type(typ ast.Type) []ast.Type {
	ts := g.table.sym(typ)
	return match ts.info {
		ast.MultiReturn {
			ts.info.types
		}
		else {
			[typ]
		}
	}
}

fn (g Gen) is_param_type(typ ast.Type) bool {
	return !typ.is_ptr() && !g.is_pure_type(typ)
}

fn (mut g Gen) dbg_type_name(name string, typ ast.Type) string {
	return "${name}<${`&`.repeat(typ.nr_muls())}${*g.table.sym(typ)}>"
}

fn (g &Gen) get_ns_plus_name(default_name string, attrs []ast.Attr) (string, string) {
	mut name := default_name
	mut namespace := 'env'

	if cattr := attrs.find_first('wasm_import_namespace') {
		namespace = cattr.arg
	}
	if cattr := attrs.find_first('wasm_import_name') {
		name = cattr.arg
	}

	return namespace, name
}

fn (mut g Gen) fn_external_import(node ast.FnDecl) {
	if !node.no_body || node.is_method {
		g.v_error('interop functions cannot have bodies', node.body_pos)
	}
	if node.language == .js && g.pref.os == .wasi {
		g.v_error('javascript interop functions are not allowed in a `wasi` build', node.pos)
	}
	if node.return_type.has_flag(.option) || node.return_type.has_flag(.result) {
		g.v_error('interop functions must not return option or result', node.pos)
	}

	mut paraml := []wasm.ValType{cap: node.params.len}
	mut retl := []wasm.ValType{cap: 1}
	for arg in node.params {
		if !g.is_pure_type(arg.typ) {
			g.v_error('interop functions do not support complex arguments',
				arg.type_pos)
		}
		paraml << g.get_wasm_type(arg.typ)
	}

	is_ret := node.return_type != ast.void_type

	if is_ret && !g.is_pure_type(node.return_type) {
		g.v_error('interop functions do not support complex returns', node.return_type_pos)
	}
	if is_ret {
		retl << g.get_wasm_type(node.return_type)
	}

	namespace, name := g.get_ns_plus_name(node.short_name, node.attrs)
	g.mod.new_function_import(namespace, name, paraml, retl)
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.language in [.js, .wasm] {
		g.fn_external_import(node)
		return
	}

	name := if node.is_method {
		'${g.table.get_type_name(node.receiver.typ)}.${node.name}'
	} else {
		node.name
	}

	util.timing_start('${@METHOD}: ${name}')
	defer {
		util.timing_measure('${@METHOD}: ${name}')
	}

	if node.no_body {
		return
	}
	if g.pref.is_verbose {
		// println(term.green('\n${name}:'))
	}
	if node.is_deprecated {
		g.warning('fn_decl: ${name} is deprecated', node.pos)
	}

	mut paramdbg := []?string{cap: node.params.len}
	mut paraml := []wasm.ValType{cap: node.params.len}
	mut retl := []wasm.ValType{cap: 1}

	// fn ()!       | fn () &IError
	// fn () ?(...) | fn () (..., bool)
	// fn () !(...) | fn () (..., &IError)
	// 
	// fn (...) struct      | fn (_ &struct, ...)
	// fn (...) !struct     | fn (_ &struct, ...) &IError
	// fn (...) (...struct) | fn (...&struct, ...)

	// TODO: PRESERVE `pub` modifer and `export` attrs
	// TODO: READ THROUGH EVERYTHING AND SUPPORT EVERYTHING

	rt := node.return_type
	rts := g.table.sym(rt)
	match rts.info {
		ast.MultiReturn {
			for t in rts.info.types {
				wtyp := g.get_wasm_type(t)
				if g.is_param_type(t) {
					paramdbg << g.dbg_type_name('__rval${g.return_vars.len}', t)
					paraml << wtyp
					g.return_vars << Var{
						typ: t
						idx: g.return_vars.len
						is_address: true
					}
				} else {
					retl << wtyp
				}
				g.ret_types << t
			}
			if rt.has_flag(.option) {
				g.v_error('option types are not implemented', node.return_type_pos)
				retl << .i32_t // bool
			}
		}
		else {
			if rt.idx() != ast.void_type_idx {
				wtyp := g.get_wasm_type(rt)
				if g.is_param_type(rt) {
					paramdbg << g.dbg_type_name('__rval', rt)
					paraml << wtyp
					g.return_vars << Var{
						typ: rt
						is_address: true
					}
				} else {
					retl << wtyp
				}
				g.ret_types << rt
			} else if rt.has_flag(.option) {
				g.v_error('returning a void option is forbidden', node.return_type_pos)
			}
		}
	}
	if rt.has_flag(.result) {
		g.v_error('result types are not implemented', node.return_type_pos)
		retl << .i32_t // &IError
	}

	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		g.local_vars << Var{
			name: p.name
			typ: p.typ
			idx: g.local_vars.len + g.return_vars.len
			is_address: !g.is_pure_type(p.typ)
		}
		paramdbg << g.dbg_type_name(p.name, p.typ)
		paraml << typ
	}

	mut should_export := g.pref.os == .browser && node.is_pub && node.mod == 'main'
	
	g.func = g.mod.new_debug_function(name, wasm.FuncType{paraml, retl, none}, paramdbg)
	func_start := g.func.patch_pos()
	if node.stmts.len > 0 {
		g.ret_br = g.func.c_block([], retl)
		{
			g.expr_stmts(node.stmts, ast.void_type)
		}
		g.func.c_end(g.ret_br)
		g.bare_function_frame(func_start)
	}
	if cattr := node.attrs.find_first('export') {
		g.func.export_name(cattr.arg)
		should_export = true
	}
	g.mod.commit(g.func, should_export)
	g.bare_function_end()

	// printfn is not implemented!
}

fn (mut g Gen) bare_function_frame(func_start wasm.PatchPos) {
	// Setup stack frame.
	// If the function does not call other functions, 
	// a leaf function, the omission of setting the 
	// stack pointer is perfectly acceptable.
	//
	if g.stack_frame != 0 {
		prolouge := g.func.patch_pos()
		{
			g.func.global_get(g.sp())
			g.func.i32_const(g.stack_frame)
			g.func.sub(.i32_t)
			if !g.is_leaf_function {
				g.func.local_tee(g.bp())
				g.func.global_set(g.sp())
			} else {
				g.func.local_set(g.bp())					
			}
		}
		g.func.patch(func_start, prolouge)
		if !g.is_leaf_function {
			g.func.global_get(g.sp())
			g.func.i32_const(g.stack_frame)
			g.func.add(.i32_t)
			g.func.global_set(g.sp())
		}
	}
}

fn (mut g Gen) bare_function_end() {
	g.local_vars.clear()
	g.return_vars.clear()
	g.ret_types.clear()
	g.bp_idx = -1
	g.stack_frame = 0
	g.is_leaf_function = true
	assert g.loop_breakpoint_stack.len == 0
}

fn (mut g Gen) literalint(val i64, expected ast.Type) {
	match g.get_wasm_type(expected) {
		.i32_t { g.func.i32_const(val) }
		.i64_t { g.func.i64_const(val) }
		.f32_t { g.func.f32_const(f32(val)) }
		.f64_t { g.func.f64_const(f64(val)) }
		else { g.w_error('literalint: bad type `${expected}`') }
	}
}

fn (mut g Gen) literal(val string, expected ast.Type) {
	match g.get_wasm_type(expected) {
		.i32_t { g.func.i32_const(val.int()) }
		.i64_t { g.func.i64_const(val.i64()) }
		.f32_t { g.func.f32_const(val.f32()) }
		.f64_t { g.func.f64_const(val.f64()) }
		else { g.w_error('literal: bad type `${expected}`') }
	}
}

fn (mut g Gen) cast(typ ast.Type, expected_type ast.Type) {
	wtyp := g.as_numtype(g.get_wasm_type(ast.mktyp(typ)))
	expected_wtype := g.as_numtype(g.get_wasm_type(expected_type))

	g.func.cast(wtyp, typ.is_signed(), expected_wtype)
}

fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) {
	if expr is ast.IntegerLiteral {
		g.literal(expr.val, expected_type)
		return
	} else if expr is ast.FloatLiteral {
		g.literal(expr.val, expected_type)
		return
	}

	got_type := ast.mktyp(got_type_raw)
	got_wtype := g.as_numtype(g.get_wasm_type(got_type))
	expected_wtype := g.as_numtype(g.get_wasm_type(expected_type))

	g.expr(expr, got_type)
	g.func.cast(got_wtype, got_type.is_signed(), expected_wtype)
}

fn (mut g Gen) handle_ptr_arithmetic(typ ast.Type) {
	if typ.is_ptr() {
		size, _ := g.pool.type_size(typ)
		g.func.i32_const(size)
		g.func.mul(.i32_t)
	}
}

fn (mut g Gen) infix_expr(node ast.InfixExpr, expected ast.Type) {
	if node.op in [.logical_or, .and] {
		temp := g.func.new_local_named(.i32_t, '__tmp<bool>')
		{
			g.expr(node.left, ast.bool_type)
			g.func.local_set(temp)
		}

		g.func.local_get(temp)
		if node.op == .logical_or {
			g.func.eqz(.i32_t)
		}

		blk := g.func.c_if([], [.i32_t])
		{
			g.expr(node.right, ast.bool_type)
		}
		g.func.c_else(blk)
		{
			g.func.local_get(temp)
		}
		g.func.c_end(blk)
		return
	}

	{
		g.expr(node.left, node.left_type)
	}
	{
		g.expr_with_cast(node.right, node.right_type, node.left_type)
		if node.op in [.plus, .minus] && node.left_type.is_ptr() {
			g.handle_ptr_arithmetic(node.left_type.deref())
		}
	}
	g.infix_from_typ(node.left_type, node.op)

	res_typ := if node.op in [.eq, .ne, .gt, .lt, .ge, .le] {
		ast.bool_type
	} else {
		node.left_type
	}
	g.func.cast(g.as_numtype(g.get_wasm_type(res_typ)), res_typ.is_signed(), g.as_numtype(g.get_wasm_type(expected)))
}

const wasm_builtins = ['__memory_grow', '__memory_fill', '__memory_copy', '__memory_size',
	'__heap_base']

fn (mut g Gen) wasm_builtin(name string, node ast.CallExpr) {
	for idx, arg in node.args {
		g.expr(arg.expr, node.expected_arg_types[idx])
	}

	match name {
		'__memory_grow' {
			g.func.memory_grow()
		}
		'__memory_fill' {
			g.func.memory_fill()
		}
		'__memory_copy' {
			g.func.memory_copy()
		}
		'__memory_size' {
			g.func.memory_size()
		}
		'__heap_base' {
			if hp := g.heap_base {
				g.func.global_get(hp)
			}
			hp := g.mod.new_global('__heap_base', false, .i32_t, false, wasm.constexpr_value(0))
			g.func.global_get(hp)
			g.heap_base = hp
		}
		else {
			panic('unreachable')
		}
	}
}

fn (mut g Gen) prefix_expr(node ast.PrefixExpr, expected ast.Type) {
	match node.op {
		.minus {
			if node.right_type.is_pure_float() {
				g.expr(node.right, node.right_type)
				if node.right_type == ast.f32_type_idx {
					g.func.neg(.f32_t)
				} else {
					g.func.neg(.f64_t)
				}
			} else {
				// -val == 0 - val

				vt := g.get_wasm_type(node.right_type)

				g.literalint(0, node.right_type)
				g.expr(node.right, node.right_type)
				g.func.sub(g.as_numtype(vt))
			}
		}
		.not {
			g.expr(node.right, node.right_type)
			g.func.eqz(.i32_t) // !expr
		}
		.bit_not {
			// ~val == val ^ -1

			vt := g.get_wasm_type(node.right_type)

			g.expr(node.right, node.right_type)
			g.literalint(-1, node.right_type)
			g.func.b_xor(g.as_numtype(vt))
		}
		.amp {
			if v := g.get_var_from_expr(node.right) {
				if !v.is_address {
					g.v_error('cannot take the address of a value that doesn\'t live on the stack', node.pos)
				}
				g.ref(v)
			} else {
				g.needs_address = true
				{
					g.expr(node.right, node.right_type)
				}
				g.needs_address = false
			}
		}
		.mul {
			g.expr(node.right, node.right_type)
			if g.is_pure_type(expected) {
				// in a RHS context, not lvalue
				g.load(expected, 0)
			}
		}
		else {
			// impl deref (.mul), and impl address of (.amp)
			g.w_error('`${node.op}val` prefix expression not implemented')
		}
	}
}

/* 

fn (mut g Gen) new_for_label(node_label string) string {
	g.lbl++
	label := if node_label != '' {
		node_label
	} else {
		g.lbl.str()
	}
	g.for_labels << label

	return label
}

fn (mut g Gen) pop_for_label() {
	g.for_labels.pop()
}

struct BlockPatch {
mut:
	idx   int
	block binaryen.Expression
} */

fn (mut g Gen) if_branch(ifexpr ast.IfExpr, expected ast.Type, idx int) {
	curr := ifexpr.branches[idx]
	params := if expected == ast.void_type {
		[]wasm.ValType{}
	} else {
		g.unpack_type(expected).map(g.get_wasm_type(it))
	}

	g.expr(curr.cond, ast.bool_type)
	blk := g.func.c_if([], params)
	{
		g.expr_stmts(curr.stmts, expected)
	}
	{
		if ifexpr.has_else && idx + 2 >= ifexpr.branches.len {
			g.func.c_else(blk)
			g.expr_stmts(ifexpr.branches[idx + 1].stmts, expected)
		} else if !(idx + 1 >= ifexpr.branches.len) {
			g.func.c_else(blk)
			g.if_branch(ifexpr, expected, idx + 1)
		}
	}
	g.func.c_end(blk)
}

fn (mut g Gen) if_expr(ifexpr ast.IfExpr, expected ast.Type) {
	g.if_branch(ifexpr, expected, 0)
}

fn (mut g Gen) call_expr(node ast.CallExpr, expected ast.Type, existing_rvars []Var) {
	mut wasm_ns := ?string(none)
	mut name := node.name

	is_print := name in ['panic', 'println', 'print', 'eprintln', 'eprint']

	if name in wasm_builtins {
		g.wasm_builtin(node.name, node)
		return
	}

	if node.is_method {
		name = '${g.table.get_type_name(node.receiver_type)}.${node.name}'
	}

	if node.language in [.js, .wasm] {
		cfn_attrs := g.table.fns[node.name].attrs

		short_name := if node.language == .js {
			node.name.all_after_last("JS.")
		} else {
			node.name.all_after_last("WASM.")
		}

		// setting a `?string` in a multireturn causes UNDEFINED BEHAVIOR AND STACK CORRUPTION
		// best to use a workaround till that is fixed

		mut wasm_ns_storage := ''
		wasm_ns_storage, name = g.get_ns_plus_name(short_name, cfn_attrs)
		wasm_ns = wasm_ns_storage
	}

	// callconv: {return structs} {method self} {arguments}
	
	// {return structs}
	//
	mut rvars := existing_rvars.clone()
	rts := g.unpack_type(node.return_type)
	if rvars.len == 0 && node.return_type != ast.void_type {
		for rt in rts {
			if g.is_param_type(rt) {
				v := g.new_local('', rt)
				rvars << v
			}
		}
	}
	for v in rvars {
		g.ref(v)
	}

	// {method self}
	//
	if node.is_method {
		expr := if !node.left_type.is_ptr() && node.receiver_type.is_ptr() {
			ast.Expr(ast.PrefixExpr{
				op: .amp
				right: node.left
			})
		} else {
			node.left
		}
		g.expr(expr, node.receiver_type)
	}
	
	// {arguments}
	//
	for idx, arg in node.args {
		mut expr := arg.expr
		
		typ := arg.typ
		if is_print && typ != ast.string_type {
			has_str, _, _ := g.table.sym(typ).str_method_info()
			if typ != ast.string_type && !has_str {
				g.v_error('cannot implicitly convert as argument does not have a .str() function',
					arg.pos)
			}

			expr = ast.CallExpr{
				name: 'str'
				left: expr
				left_type: typ
				receiver_type: typ
				return_type: ast.string_type
				is_method: true
			}
		}

		g.expr(expr, node.expected_arg_types[idx])
	}

	if namespace := wasm_ns {
		// import calls won't touch `__vsp` !

		g.func.call_import(namespace, name)
	} else {
		// other calls may...
		g.is_leaf_function = false

		g.func.call(name)
	}

	if expected == ast.void_type && node.return_type != ast.void_type {
		for rt in rts { // order doesn't matter
			if !g.is_param_type(rt) {
				g.func.drop()
			}
		}
	} else if rvars.len > 0 && existing_rvars.len == 0 {
		mut rr_vars := []Var{cap: rts.len}
		mut r := rvars.len

		for rt in rts.reverse() {
			if !g.is_param_type(rt) {
				v := g.new_local('', rt)
				rr_vars << v
				g.set(v)
			} else {
				r--
				rr_vars << rvars[r]
			}
		}

		for v in rr_vars.reverse() {
			g.get(v)
		}
	}
	if node.is_noreturn {
		g.func.unreachable()
	}
}

fn (mut g Gen) get_field_offset(typ ast.Type, name string) int {
	ts := g.table.sym(typ)
	field := ts.find_field(name) or { g.w_error('could not find field `${name}` on init') }
	si := g.pool.type_struct_info(typ) or { panic("unreachable") }
	return si.offsets[field.i]
}

fn (mut g Gen) field_offset(typ ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	if offset != 0 {
		g.func.i32_const(offset)
		g.func.add(.i32_t)
	}
}

fn (mut g Gen) load_field(typ ast.Type, ftyp ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	g.load(ftyp, offset)
}

fn (mut g Gen) store_field(typ ast.Type, ftyp ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	g.store(ftyp, offset)
}

fn (mut g Gen) expr(node ast.Expr, expected ast.Type) {
	match node {
		ast.ParExpr, ast.UnsafeExpr {
			g.expr(node.expr, expected)
		}
		ast.ArrayInit {
			v := g.new_local('', node.typ)
			g.set_with_expr(node, v)
			g.get(v)
		}
		ast.GoExpr {
			g.w_error('wasm backend does not support threads')
		}
		ast.IndexExpr {
			// TODO: IMPLICIT BOUNDS CHECKING
			/*
			if !node.is_direct {
				g.w_error('implicit bounds checks are not implemented, create one manually')
			}*/
			
			// ptr + index * size
			mut typ := node.left_type

			g.expr(node.left, node.left_type)
			if node.left_type == ast.string_type {
				// be pedantic...
				g.load_field(ast.string_type, ast.voidptr_type, 'str')
				typ = ast.u8_type
			} else {
				if typ.is_ptr() {
					typ = typ.deref()
				}
			}
			
			size, _ := g.pool.type_size(typ)

			g.expr(node.index, ast.int_type)
			if size > 1 {
				g.literalint(size, ast.int_type)
				g.func.mul(.i32_t)
			}
			
			g.func.add(.i32_t)

			if !g.needs_address {
				// ptr
				g.load(typ, 0)
			}
			g.cast(typ, expected)
		}
		ast.StructInit {
			v := g.new_local('', node.typ)
			g.set_with_expr(node, v)
			g.get(v)
		}
		ast.SelectorExpr {
			if v := g.get_var_from_expr(node) {
				if g.needs_address {
					if !v.is_address {
						g.v_error('cannot take the address of a value that doesn\'t live on the stack', node.pos)
					}

					g.ref(v)
				} else {
					g.get(v)
				}
			} else {
				g.expr(node, node.typ)
				g.field_offset(node.expr_type, node.field_name)
				if g.is_pure_type(node.typ) && !g.needs_address {
					// expected to be a pointer
					g.load(node.typ, 0)
				}
			}
			g.cast(node.typ, expected)
		}
		ast.MatchExpr {
			g.w_error('wasm backend does not support match expressions yet')
		}
		ast.EnumVal {
			type_name := g.table.get_type_name(node.typ)
			ts_type := (g.table.sym(node.typ).info as ast.Enum).typ
			g.literalint(g.enum_vals[type_name].fields[node.val], ts_type)
		}
		ast.OffsetOf {
			styp := g.table.sym(node.struct_type)
			if styp.kind != .struct_ {
				g.v_error('__offsetof expects a struct Type as first argument', node.pos)
			}
			off := g.get_field_offset(node.struct_type, node.field)
			g.literalint(off, ast.u32_type)
		}
		ast.SizeOf {
			if !g.table.known_type_idx(node.typ) {
				g.v_error('unknown type `${*g.table.sym(node.typ)}`', node.pos)
			}
			size, _ := g.table.type_size(node.typ)
			g.literalint(size, ast.u32_type)
		}
		ast.BoolLiteral {
			g.func.i32_const(i32(node.val))
		}
		ast.StringLiteral {
			if expected != ast.string_type {
				val := serialise.eval_escape_codes(node) or { panic('unreachable') }
				str_pos := g.pool.append_string(val)

				// c'str'
				g.literalint(g.data_base + str_pos, ast.voidptr_type)
				return
			}

			v := g.new_local('', ast.string_type)
			g.set_with_expr(node, v)
			g.get(v)
		}
		ast.InfixExpr {
			g.infix_expr(node, expected)
		}
		ast.PrefixExpr {
			g.prefix_expr(node, expected)
		}
		ast.PostfixExpr {
			kind := if node.op == .inc { token.Kind.plus } else { token.Kind.minus }
			v := g.get_var_or_make_from_expr(node.expr, node.typ)

			g.set_prepare(v)
			{
				g.get(v)
				g.literalint(1, node.typ)
				g.handle_ptr_arithmetic(node.typ)
				g.infix_from_typ(node.typ, kind)
			}
			g.set_set(v)
		}
		ast.CharLiteral {
			rns := serialise.eval_escape_codes_raw(node.val) or { panic('unreachable') }.runes()[0]
			g.func.i32_const(rns)
		}
		ast.Ident {
			v := g.get_var_from_ident(node)
			g.get(v)
			g.cast(v.typ, expected)
		}
		ast.IntegerLiteral, ast.FloatLiteral {
			g.literal(node.val, expected)
		}
		ast.Nil {
			g.func.i32_const(0)
		}
		ast.IfExpr {
			g.if_expr(node, expected)
		}
		ast.CastExpr {
			g.expr(node.expr, node.expr_type)
			g.func.cast(g.as_numtype(g.get_wasm_type(node.expr_type)), node.expr_type.is_signed(), g.as_numtype(g.get_wasm_type(node.typ)))
		}
		ast.CallExpr {
			g.call_expr(node, expected, [])
		}
		ast.ConcatExpr {
			types := g.unpack_type(expected)

			for idx, expr in node.vals {
				g.expr(expr, types[idx])
			}
		}
		else {
			g.w_error('wasm.expr(): unhandled node: ' + node.type_name())
		}
	}
}

fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) {
	match node {
		ast.Block {
			g.expr_stmts(node.stmts, expected)
		}
		ast.Return {
			if node.exprs.len == 1 && node.exprs[0] is ast.CallExpr {
				g.call_expr(node.exprs[0] as ast.CallExpr, 0, g.return_vars)
				g.func.c_br(g.ret_br)
				return
			}

			mut r := 0
			for idx, expr in node.exprs {
				typ := g.ret_types[idx] // node.types LIES
				if g.is_param_type(typ) {
					if rhs := g.get_var_from_expr(expr) {
						g.mov(g.return_vars[r], rhs)
					} else {
						g.set_with_expr(expr, g.return_vars[r])
					}					
					r++
				} else {
					g.expr(expr, typ)
				}
			}
			g.func.c_br(g.ret_br)
		}
		ast.ExprStmt {
			g.expr(node.expr, expected)
		}
		ast.ForStmt {
			block := g.func.c_block([], [])
			{
				loop := g.func.c_loop([], [])
				{
					g.loop_breakpoint_stack << LoopBreakpoint{
						c_continue: loop
						c_break: block
						name: node.label
					}

					if !node.is_inf {
						g.expr(node.cond, ast.bool_type)
						g.func.eqz(.i32_t)
						g.func.c_br_if(block) // !cond, goto end
					}
					g.expr_stmts(node.stmts, ast.void_type)
					g.func.c_br(loop) // goto loop

					g.loop_breakpoint_stack.pop()
				}
				g.func.c_end(loop)
			}
			g.func.c_end(block)
		}
		ast.ForCStmt {
			block := g.func.c_block([], [])
			{
				if node.has_init {
					g.expr_stmt(node.init, ast.void_type)
				}

				loop := g.func.c_loop([], [])
				{
					g.loop_breakpoint_stack << LoopBreakpoint{
						c_continue: loop
						c_break: block
						name: node.label
					}

					if node.has_cond {
						g.expr(node.cond, ast.bool_type)
						g.func.eqz(.i32_t)
						g.func.c_br_if(block) // !cond, goto end
					}

					g.expr_stmts(node.stmts, ast.void_type)

					if node.has_inc {
						g.expr_stmt(node.inc, ast.void_type)
					}

					g.func.c_br(loop)
					g.loop_breakpoint_stack.pop()
				}
				g.func.c_end(loop)
			}
			g.func.c_end(block)
		}
		/* ast.ForCStmt {
			mut for_stmt := []binaryen.Expression{}
			if node.has_init {
				for_stmt << g.expr_stmt(node.init, ast.void_type)
			}

			lbl := g.new_for_label(node.label)
			lpp_name := 'L${lbl}'
			blk_name := 'B${lbl}'

			mut loop_exprs := []binaryen.Expression{}
			if node.has_cond {
				condexpr := binaryen.unary(g.mod, binaryen.eqzint32(), g.expr(node.cond,
					ast.bool_type))
				loop_exprs << binaryen.br(g.mod, blk_name.str, condexpr, unsafe { nil })
			}
			loop_exprs << g.expr_stmts(node.stmts, ast.void_type)

			if node.has_inc {
				loop_exprs << g.expr_stmt(node.inc, ast.void_type)
			}
			loop_exprs << binaryen.br(g.mod, lpp_name.str, unsafe { nil }, unsafe { nil })
			loop := binaryen.loop(g.mod, lpp_name.str, g.mkblock(loop_exprs))

			for_stmt << binaryen.block(g.mod, blk_name.str, &loop, 1, type_none)
			g.pop_for_label()
			g.mkblock(for_stmt)
		} */
		ast.BranchStmt {
			mut bp := g.loop_breakpoint_stack.last()
			if node.label != '' {
				for i := g.loop_breakpoint_stack.len ; i > 0 ; {
					i--
					if g.loop_breakpoint_stack[i].name == node.label {
						bp = g.loop_breakpoint_stack[i]
					}
				}
			}

			if node.kind == .key_break {
				g.func.c_br(bp.c_break)
			} else {
				g.func.c_br(bp.c_continue)
			}
		}
		ast.AssignStmt {
			if node.has_cross_var {
				g.w_error('complex assign statements are not implemented')
				// `a, b = b, a`
			} else {
				// `a := 1` | `a, b := 1, 2`

				is_multireturn := node.left.len > 1 && node.right.len == 1
				// `a, b := foo()`
				// `a, b := if cond { 1, 2 } else { 3, 4 }`

				if is_multireturn {
					g.expr(node.right[0], 0)

					for i := node.left.len ; i > 0 ; {
						i--
						left := node.left[i]
						typ := node.left_types[i]

						if left is ast.Ident {
							// `_ = expr`
							if left.kind == .blank_ident {
								// expression still may have side effect
								g.func.drop()
								continue
							}
							if node.op == .decl_assign {
								g.new_local(left.name, typ)
							}
						}

						v := g.get_var_or_make_from_expr(left, typ)
						g.set(v)
					}
					return
				}

				for i, left in node.left {
					right := node.right[i]
					typ := node.left_types[i]

					// `_ = expr` must be evaluated even if the value is being dropped!
					// The optimiser would remove expressions without side effects.

					// a    =  expr
					// b    *= expr
					// _    =  expr
					// a.b  =  expr
					// *a   =  expr
					// a[b] =  expr

					if left is ast.Ident {
						// `_ = expr`
						if left.kind == .blank_ident {
							// expression still may have side effect
							g.expr(right, typ)
							g.func.drop()
							continue
						}
						if node.op == .decl_assign {
							g.new_local(left.name, typ)
						}
					}

					lhs := g.get_var_or_make_from_expr(left, typ)
					if node.op !in [.decl_assign, .assign] {
						rop := token.assign_op_to_infix_op(node.op)
						if !g.is_pure_type(lhs.typ) {
							// main.struct.+
							name := '${g.table.get_type_name(lhs.typ)}.${rop}'

							g.ref(lhs)
							g.ref(lhs)
							g.expr(right, lhs.typ)
							g.func.call(name)
							return
						}


						g.set_prepare(lhs)
						{
							g.get(lhs)
							g.expr(right, lhs.typ)
							g.infix_from_typ(lhs.typ, rop)
						}
						g.set_set(lhs)
						return
					}
					if rhs := g.get_var_from_expr(right) {
						g.mov(lhs, rhs)
					} else {
						g.set_with_expr(right, lhs)
					}
				}
			}
		}
		else {
			g.w_error('wasm.expr_stmt(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) expr_stmts(stmts []ast.Stmt, expected ast.Type) {
	for idx, stmt in stmts {
		rtyp := if idx + 1 == stmts.len {
			expected
		} else {
			ast.void_type
		}
		g.expr_stmt(stmt, rtyp)
	}
}

fn (mut g Gen) toplevel_stmt(node ast.Stmt) {
	match node {
		ast.FnDecl {
			g.fn_decl(node)
		}
		ast.Module {}
		ast.GlobalDecl {}
		ast.ConstDecl {}
		ast.Import {}
		ast.StructDecl {}
		ast.EnumDecl {}
		ast.TypeDecl {}
		else {
			g.w_error('wasm.toplevel_stmt(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) toplevel_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.toplevel_stmt(stmt)
	}
}

struct Enum {
mut:
	fields map[string]i64
}

pub fn (mut g Gen) calculate_enum_fields() {
	// `enum Enum as u64` is supported
	for name, decl in g.table.enum_decls {
		mut enum_vals := Enum{}
		mut value := if decl.is_flag { i64(1) } else { 0 }
		for field in decl.fields {
			if field.has_expr {
				value = g.eval.expr(field.expr, decl.typ).int_val()
			}
			enum_vals.fields[field.name] = value
			if decl.is_flag {
				value <<= 1
			} else {
				value++
			}
		}
		g.enum_vals[name] = enum_vals
	}
}

pub fn gen(files []&ast.File, table &ast.Table, out_name string, w_pref &pref.Preferences) {
	stack_top := 1024 + (16 * 1024)
	mut g := &Gen{
		table: table
		pref: w_pref
		files: files
		eval: eval.new_eval(table, w_pref)
		pool: serialise.new_pool(table, store_relocs: true, null_terminated: false)
		stack_top: stack_top
		data_base: calc_align(stack_top + 1, 16)
	}
	g.table.pointer_size = 4
	g.mod.assign_memory("memory", true, 1, none)

	if g.pref.is_debug {
		g.mod.enable_debug(none)
	}

	g.calculate_enum_fields()
	for file in g.files {
		g.file_path = file.path
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	g.housekeeping()

	mod := g.mod.compile()

	if out_name == '-' {
		eprintln('<stderr> stdout pretty printing is not implemented for the time being')
		print(mod.bytestr())
	}

	if out_name != '-' {
		os.write_file_array(out_name, mod) or { panic(err) }
		if g.pref.wasm_validate {
			if rt := os.find_abs_path_of_executable($if windows { 'wasm-validate.exe' } $else { 'wasm-validate' }) {
				mut p := os.new_process(rt)
				p.set_args([out_name])
				p.set_redirect_stdio()
				p.run()
				err := p.stderr_slurp()
				p.wait()
				if p.code != 0 {
					eprintln(err)
					g.w_error('validation failed, this should not happen. report an issue with the above messages, the webassembly generated, and appropriate code.')
				}
			} else {
				g.w_error('validation failed, this should not happen. report an issue with the above messages, the webassembly generated, and appropriate code.')
			}
		}
	} else if g.pref.wasm_validate {
		eprintln('stdout output, will not validate wasm')
	}
}

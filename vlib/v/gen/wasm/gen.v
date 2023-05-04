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
	stack_frame       int // Size of the current stack frame, if needed
	is_leaf_function  bool = true
	is_return_call bool
	module_import_namespace string
	loop_breakpoint_stack []LoopBreakpoint
}

struct Global {
mut:
	init ?ast.Expr
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

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.language in [.js, .wasm] {
		g.w_error('fn_decl: extern not implemented')
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
					paraml << wtyp
					g.return_vars << Var{
						typ: t
						idx: g.return_vars.len
						is_pointer: true
					}
				} else {
					retl << wtyp
				}
				g.ret_types << t
			}
			if rt.has_flag(.option) {
				retl << .i32_t // bool
			}
		}
		else {
			if rt.idx() != ast.void_type_idx {
				wtyp := g.get_wasm_type(rt)
				if g.is_param_type(rt) {
					paraml << wtyp
					g.return_vars << Var{
						name: '__rval0'
						typ: rt
						is_pointer: true
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
		retl << .i32_t // &IError
	}

	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		g.local_vars << Var{
			name: p.name
			typ: p.typ
			idx: g.local_vars.len + g.return_vars.len
			is_pointer: p.typ.is_ptr() || !g.is_pure_type(p.typ)
		}
		paraml << typ
	}
	
	g.func = g.mod.new_function(name, paraml, retl)
	func_start := g.func.patch_pos()
	if node.stmts.len > 0 {
		g.ret_br = g.func.c_block([], retl)
		{
			g.expr_stmts(node.stmts, ast.void_type)
		}
		g.func.c_end(g.ret_br)
		g.bare_function_frame(func_start)
	}
	g.mod.commit(g.func, true)
	g.bare_function_end()
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
			g.func.local_get(g.bp())
			g.func.global_set(g.sp())
		}
	}
}

fn (mut g Gen) bare_function_end() {
	g.local_vars.clear()
	g.return_vars.clear()
	g.ret_types.clear()
	g.bp_idx = -1
	g.sp_global = none
	g.stack_frame = 0
	g.is_leaf_function = true
	assert g.loop_breakpoint_stack.len == 0
}

fn (mut g Gen) literalint(val i64, expected ast.Type) {
	match g.get_wasm_type(expected) {
		.i32_t { g.func.i32_const(val) }
		.i64_t { g.func.i64_const(val) }
		else { g.w_error('literal: bad type `${expected}`') }
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
		temp := g.func.new_local(.i32_t)
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
	}

	{
		g.expr(node.left, node.left_type)
	}
	{
		g.expr_with_cast(node.right, node.right_type, node.left_type)
		g.handle_ptr_arithmetic(node.left_type)
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
			panic('unimplemented')
			// g.func.global_get()
			// return binaryen.globalget(g.mod, c'__heap_base', type_i32)
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
			v := g.get_var_from_expr(node.right) or { panic('unreachable') }
			g.ref(v)
		}
		.mul {
			g.expr(node.right, node.right_type)
			if g.is_pure_type(expected) {
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
	g.is_leaf_function = false
	mut name := node.name

	is_print := name in ['panic', 'println', 'print', 'eprintln', 'eprint']

	if name in wasm_builtins {
		g.wasm_builtin(node.name, node)
		return
	}

	if node.is_method {
		name = '${g.table.get_type_name(node.receiver_type)}.${node.name}'
	}

	// callconv: {return structs} {method self} {arguments}
	
	// {return structs}
	//
	mut rvars := existing_rvars.clone()
	rts := g.unpack_type(node.return_type)
	if rvars.len == 0 {
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

	g.func.call(name)

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

fn (mut g Gen) expr(node ast.Expr, expected ast.Type) {
	match node {
		ast.ParExpr, ast.UnsafeExpr {
			g.expr(node.expr, expected)
		}
		/* ast.ArrayInit {
			pos := g.allocate_local_var('_anonarray', node.typ)
			expr := g.assign_expr_to_var(Var(Stack{ address: pos, ast_typ: node.typ }),
				node, node.typ)
			g.mknblock('EXPR(ARRAYINIT)', [expr, g.lea_address(pos)])
		} */
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
			size, _ := g.pool.type_size(expected)

			g.expr(node.left, node.left_type)
			if node.left_type == ast.string_type {
				// be pedantic...
				g.load_field(ast.string_type, ast.voidptr_type, 'str')
			}
			
			g.expr(node.index, ast.int_type)
			g.literalint(size, ast.int_type)
			g.func.mul(.i32_t)
			
			g.func.add(.i32_t)

			// ptr
			g.load(expected, 0)
		}
		ast.StructInit {
			v := g.new_local('', node.typ)
			g.set_with_expr(node, v)
			g.get(v)
		}
		ast.SelectorExpr {
			g.expr(node.expr, node.expr_type)
			g.field_offset(node.expr_type, node.field_name)
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
		/* ast.StringLiteral {
			if g.table.sym(expected).info !is ast.Struct {
				offset, _ := g.allocate_string(node)
				return g.literalint(offset, ast.int_type)
			}

			pos := g.allocate_local_var('_anonstring', ast.string_type)

			expr := g.assign_expr_to_var(Var(Stack{ address: pos, ast_typ: ast.string_type }),
				node, ast.string_type)
			g.mknblock('EXPR(STRINGINIT)', [expr, g.lea_address(pos)])
		} */
		ast.InfixExpr {
			g.infix_expr(node, expected)
		}
		ast.PrefixExpr {
			g.prefix_expr(node, expected)
		}
		ast.PostfixExpr {
			kind := if node.op == .inc { token.Kind.plus } else { token.Kind.minus }
			v := g.get_var_from_expr(node.expr) or { panic("unreachable") }

			g.get(v)
			g.literal('1', node.typ)
			g.handle_ptr_arithmetic(node.typ)
			g.infix_from_typ(node.typ, kind)
			g.set(v)
		}
		ast.CharLiteral {
			rns := node.val.runes()[0]
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
					g.set_with_expr(expr, g.return_vars[r])
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
						c_continue: block
						c_break: loop
						name: node.label
					}

					if !node.is_inf {
						g.expr(node.cond, ast.bool_type)
						g.func.eqz(.i32_t)
						g.func.c_br_if(block) // !cond, goto end
						g.expr_stmts(node.stmts, ast.void_type)
						g.func.c_br(loop) // goto loop
					} else {
						g.expr_stmts(node.stmts, ast.void_type)
						g.func.c_br(loop)
					}

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
		}
		ast.BranchStmt {
			mut blabel := if node.label != '' {
				node.label
			} else {
				g.for_labels[g.for_labels.len - 1]
			}

			if node.kind == .key_break {
				blabel = 'B${blabel}'
			} else {
				blabel = 'L${blabel}'
			}
			binaryen.br(g.mod, blabel.str, unsafe { nil }, unsafe { nil })
		} */
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

						if v := g.get_var_from_expr(left) {
							g.set(v)
						} else {
							panic("unimplemented")
						}
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

					if v := g.get_var_from_expr(left) {
						if node.op !in [.decl_assign, .assign] {
							rop := token.assign_op_to_infix_op(node.op)
							// name := '${g.table.get_type_name(expected)}.${rop}'

							g.get(v)
							g.expr(right, v.typ)
							g.infix_from_typ(v.typ, rop)
							g.set(v)
							return
						}	
						g.set_with_expr(right, v)
					} else {
						panic("unimplemented")
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
		ast.Module {
			if ns := node.attrs.find_first('wasm_import_namespace') {
				g.module_import_namespace = ns.arg
			} else {
				g.module_import_namespace = 'env'
			}
		}
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
	mut g := &Gen{
		table: table
		pref: w_pref
		files: files
		eval: eval.new_eval(table, w_pref)
		pool: serialise.new_pool(table)
	}
	g.table.pointer_size = 4
	g.mod.assign_memory("memory", true, 1, none)

	if g.pref.os == .browser {
		eprintln('`-os browser` is experimental and will not live up to expectations...')
	}

	g.calculate_enum_fields()
	for file in g.files {
		g.file_path = file.path
		if g.pref.is_debug {
			// g.file_path_idx = binaryen.moduleadddebuginfofilename(g.mod, g.file_path.str)
		}
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	g.housekeeping()

	/* mut valid := binaryen.modulevalidate(g.mod)
	if valid {
		binaryen.setdebuginfo(w_pref.is_debug)
		if w_pref.is_prod {
			eprintln('`-prod` is not implemented')
		}
	} */

	mod := g.mod.compile()

	if out_name == '-' {
		eprintln('pretty printing is not implemented')
		print(mod.bytestr())
		/* if g.pref.is_verbose {
			binaryen.moduleprint(g.mod)
		} else {
			binaryen.moduleprintstackir(g.mod, w_pref.is_prod)
		} */
	}

	/* if !valid {
		g.w_error('validation failed, this should not happen. report an issue with the above messages')
	} */

	if out_name != '-' {
		os.write_file_array(out_name, mod) or { panic(err) }
	}
}

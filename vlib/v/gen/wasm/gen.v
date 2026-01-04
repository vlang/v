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
import v.gen.wasm.serialise
import wasm
import os

@[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	file_path string // current ast.File path
	warnings  []errors.Warning
	errors    []errors.Error
	table     &ast.Table = unsafe { nil }
	eval      eval.Eval
	enum_vals map[string]Enum

	mod                    wasm.Module
	pool                   serialise.Pool
	func                   wasm.Function
	local_vars             []Var
	global_vars            map[string]Global
	ret_rvars              []Var
	ret                    ast.Type
	ret_types              []ast.Type
	ret_br                 wasm.LabelIndex
	bp_idx                 wasm.LocalIndex = -1 // Base pointer temporary's index for function, if needed (-1 for none)
	sp_global              ?wasm.GlobalIndex
	heap_base              ?wasm.GlobalIndex
	fn_local_idx_end       int
	fn_name                string
	stack_frame            int // Size of the current stack frame, if needed
	is_leaf_function       bool = true
	loop_breakpoint_stack  []LoopBreakpoint
	stack_top              int // position in linear memory
	data_base              int // position in linear memory
	needs_address          bool
	defer_vars             []Var
	is_direct_array_access bool // inside a `[direct_array_access]` function
}

struct Global {
mut:
	init ?ast.Expr
	v    Var
}

pub struct LoopBreakpoint {
	c_continue wasm.LabelIndex
	c_break    wasm.LabelIndex
	name       string
}

@[noreturn]
pub fn (mut g Gen) v_error(s string, pos token.Pos) {
	util.show_compiler_message('error:', pos: pos, file_path: g.file_path, message: s)
	exit(1)
	/*
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
	*/
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('warning:', pos: pos, file_path: g.file_path, message: s)
	} else {
		g.warnings << errors.Warning{
			file_path: g.file_path
			pos:       pos
			reporter:  .gen
			message:   s
		}
	}
}

@[noreturn]
pub fn (mut g Gen) w_error(s string) {
	if g.pref.is_verbose {
		print_backtrace()
	}
	util.verror('wasm error', s)
}

pub fn (g &Gen) unpack_type(typ ast.Type) []ast.Type {
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

pub fn (g &Gen) is_param_type(typ ast.Type) bool {
	return !typ.is_ptr() && !g.is_pure_type(typ)
}

pub fn (mut g Gen) dbg_type_name(name string, typ ast.Type) string {
	return '${name}<${`&`.repeat(typ.nr_muls())}${*g.table.sym(typ)}>'
}

pub fn unpack_literal_int(typ ast.Type) ast.Type {
	return if typ == ast.int_literal_type { ast.i64_type } else { typ }
}

pub fn (g &Gen) get_ns_plus_name(default_name string, attrs []ast.Attr) (string, string) {
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

pub fn (mut g Gen) fn_external_import(node ast.FnDecl) {
	if !node.no_body || node.is_method {
		g.v_error('interop functions cannot have bodies', node.body_pos)
	}
	if node.language == .js && g.pref.os == .wasi {
		g.v_error('javascript interop functions are not allowed in a `wasi` build', node.pos)
	}
	if node.return_type.has_option_or_result() {
		g.v_error('interop functions must not return option or result', node.pos)
	}

	mut paraml := []wasm.ValType{cap: node.params.len}
	mut retl := []wasm.ValType{cap: 1}
	for arg in node.params {
		if !g.is_pure_type(arg.typ) {
			g.v_error('interop functions do not support complex arguments', arg.type_pos)
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

pub fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.language in [.js, .wasm] {
		g.fn_external_import(node)
		return
	}

	if node.attrs.contains('flag_enum_fn') {
		// TODO: remove, when support for fn results is done
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

	g.ret_rvars = []Var{}
	rt := node.return_type
	rts := g.table.sym(rt)
	g.ret = rt
	match rts.info {
		ast.MultiReturn {
			for t in rts.info.types {
				wtyp := g.get_wasm_type(t)
				if g.is_param_type(t) {
					paramdbg << g.dbg_type_name('__rval(${g.ret_rvars.len})', t)
					paraml << wtyp
					g.ret_rvars << Var{
						typ:        t
						idx:        g.ret_rvars.len
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
					paramdbg << g.dbg_type_name('__rval(0)', rt)
					paraml << wtyp
					g.ret_rvars << Var{
						typ:        rt
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
		typ := g.get_wasm_type_int_literal(p.typ)
		ntyp := unpack_literal_int(p.typ)
		g.local_vars << Var{
			name:       p.name
			typ:        ntyp
			idx:        g.local_vars.len + g.ret_rvars.len
			is_address: !g.is_pure_type(p.typ)
		}
		paramdbg << g.dbg_type_name(p.name, p.typ)
		paraml << typ
	}

	// bottom scope

	g.is_direct_array_access = node.is_direct_arr || g.pref.no_bounds_checking
	g.fn_local_idx_end = (g.local_vars.len + g.ret_rvars.len)
	g.fn_name = name

	mut should_export := g.pref.os in [.browser, .wasi] && node.is_pub && node.mod == 'main'

	g.func = g.mod.new_debug_function(name, wasm.FuncType{paraml, retl, none}, paramdbg)
	func_start := g.func.patch_pos()
	if node.stmts.len > 0 {
		g.ret_br = g.func.c_block([], retl)
		{
			g.expr_stmts(node.stmts, ast.void_type)
		}
		{
			for idx, defer_stmt in node.defer_stmts {
				g.get(g.defer_vars[idx])
				lbl := g.func.c_if([], [])
				{
					g.expr_stmts(defer_stmt.stmts, ast.void_type)
				}
				g.func.c_end(lbl)
			}
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

pub fn (mut g Gen) bare_function_frame(func_start wasm.PatchPos) {
	// Setup stack frame.
	// If the function does not call other functions,
	// a leaf function, the omission of setting the
	// stack pointer is perfectly acceptable.
	//
	if g.stack_frame != 0 {
		prologue := g.func.patch_pos()
		{
			g.func.global_get(g.sp())
			g.func.i32_const(i32(g.stack_frame))
			g.func.sub(.i32_t)
			if !g.is_leaf_function {
				g.func.local_tee(g.bp())
				g.func.global_set(g.sp())
			} else {
				g.func.local_set(g.bp())
			}
		}
		g.func.patch(func_start, prologue)
		if !g.is_leaf_function {
			g.func.global_get(g.sp())
			g.func.i32_const(i32(g.stack_frame))
			g.func.add(.i32_t)
			g.func.global_set(g.sp())
		}
	}
}

pub fn (mut g Gen) bare_function_end() {
	g.local_vars.clear()
	g.ret_rvars.clear()
	g.ret_types.clear()
	g.defer_vars.clear()
	g.bp_idx = -1
	g.stack_frame = 0
	g.is_leaf_function = true
	g.is_direct_array_access = false
	assert g.loop_breakpoint_stack.len == 0
}

pub fn (mut g Gen) literalint(val i64, expected ast.Type) {
	match g.get_wasm_type(expected) {
		.i32_t { g.func.i32_const(i32(val)) }
		.i64_t { g.func.i64_const(val) }
		.f32_t { g.func.f32_const(f32(val)) }
		.f64_t { g.func.f64_const(f64(val)) }
		else { g.w_error('literalint: bad type `${expected}`') }
	}
}

pub fn (mut g Gen) literal(val string, expected ast.Type) {
	match g.get_wasm_type(expected) {
		.i32_t { g.func.i32_const(i32(val.int())) }
		.i64_t { g.func.i64_const(val.i64()) }
		.f32_t { g.func.f32_const(val.f32()) }
		.f64_t { g.func.f64_const(val.f64()) }
		else { g.w_error('literal: bad type `${expected}`') }
	}
}

pub fn (mut g Gen) cast(typ ast.Type, expected_type ast.Type) {
	wtyp := g.as_numtype(g.get_wasm_type_int_literal(typ))
	expected_wtype := g.as_numtype(g.get_wasm_type_int_literal(expected_type))

	g.func.cast(wtyp, typ.is_signed(), expected_wtype)
}

pub fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) {
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

pub fn (mut g Gen) handle_ptr_arithmetic(typ ast.Type) {
	if typ.is_ptr() {
		size, _ := g.pool.type_size(typ)
		g.func.i32_const(i32(size))
		g.func.mul(.i32_t)
	}
}

fn (mut g Gen) handle_string_operation(op token.Kind) {
	left_tmp := g.func.new_local_named(.i32_t, '__tmp<string>.left')
	right_tmp := g.func.new_local_named(.i32_t, '__tmp<string>.right')
	g.func.local_set(right_tmp)
	g.func.local_set(left_tmp)

	match op {
		.plus {
			ret_var := g.new_local('', ast.string_type)
			g.ref(ret_var)
			g.func.local_get(left_tmp)
			g.func.local_get(right_tmp)
			g.func.call('string.+')
			g.get(ret_var)
		}
		.eq {
			g.func.local_get(left_tmp)
			g.func.local_get(right_tmp)
			g.func.call('string.==')
		}
		.ne {
			g.func.local_get(left_tmp)
			g.func.local_get(right_tmp)
			g.func.call('string.==')
			g.func.eqz(.i32_t)
		}
		.lt {
			g.func.local_get(left_tmp)
			g.func.local_get(right_tmp)
			g.func.call('string.<')
		}
		.gt {
			g.func.local_get(right_tmp)
			g.func.local_get(left_tmp)
			g.func.call('string.<')
		}
		.le {
			g.func.local_get(right_tmp)
			g.func.local_get(left_tmp)
			g.func.call('string.<')
			g.func.eqz(.i32_t)
		}
		.ge {
			g.func.local_get(left_tmp)
			g.func.local_get(right_tmp)
			g.func.call('string.<')
			g.func.eqz(.i32_t)
		}
		else {
			g.w_error('unsupported string operation: `${op}`')
		}
	}
}

pub fn (mut g Gen) infix_expr(node ast.InfixExpr, expected ast.Type) {
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

pub fn (mut g Gen) prefix_expr(node ast.PrefixExpr, expected ast.Type) {
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
					g.v_error("cannot take the address of a value that doesn't live on the stack",
						node.pos)
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
			if g.is_pure_type(expected) && !g.needs_address {
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

pub fn (mut g Gen) if_branch(ifexpr ast.IfExpr, expected ast.Type, unpacked_params []wasm.ValType, idx int,
	existing_rvars []Var) {
	curr := ifexpr.branches[idx]

	g.expr(curr.cond, ast.bool_type)
	blk := g.func.c_if([], unpacked_params)
	{
		g.rvar_expr_stmts(curr.stmts, expected, existing_rvars)
	}
	{
		if ifexpr.has_else && idx + 2 >= ifexpr.branches.len {
			g.func.c_else(blk)
			g.rvar_expr_stmts(ifexpr.branches[idx + 1].stmts, expected, existing_rvars)
		} else if !(idx + 1 >= ifexpr.branches.len) {
			g.func.c_else(blk)
			g.if_branch(ifexpr, expected, unpacked_params, idx + 1, existing_rvars)
		}
	}
	g.func.c_end(blk)
}

pub fn (mut g Gen) if_expr(ifexpr ast.IfExpr, expected ast.Type, existing_rvars []Var) {
	if ifexpr.is_comptime {
		g.comptime_if_expr(ifexpr, expected, existing_rvars)
		return
	}

	params := if expected == ast.void_type {
		[]wasm.ValType{}
	} else if existing_rvars.len == 0 {
		g.unpack_type(expected).map(g.get_wasm_type(it))
	} else {
		g.unpack_type(expected).filter(!g.is_param_type(it)).map(g.get_wasm_type(it))
	}
	g.if_branch(ifexpr, expected, params, 0, existing_rvars)
}

pub fn (mut g Gen) match_expr(node ast.MatchExpr, expected ast.Type, existing_rvars []Var) {
	results := if expected == ast.void_type {
		[]wasm.ValType{}
	} else if existing_rvars.len == 0 {
		g.unpack_type(expected).map(g.get_wasm_type(it))
	} else {
		g.unpack_type(expected).filter(!g.is_param_type(it)).map(g.get_wasm_type(it))
	}
	g.match_branch(node, expected, results, 0, existing_rvars)
}

fn (mut g Gen) match_branch(node ast.MatchExpr, expected ast.Type, unpacked_params []wasm.ValType, branch_idx int, existing_rvars []Var) {
	if branch_idx >= node.branches.len {
		return
	}

	branch := node.branches[branch_idx]
	mut is_last_branch := branch_idx + 1 >= node.branches.len
	mut has_else := branch.is_else

	if has_else {
		if branch.stmts.len > 0 {
			g.rvar_expr_stmts(branch.stmts, expected, existing_rvars)
		}
		return
	}

	if branch.exprs.len > 0 {
		g.match_branch_exprs(node, expected, unpacked_params, branch_idx, 0, existing_rvars,
			branch)
	} else {
		if branch.stmts.len > 0 {
			g.rvar_expr_stmts(branch.stmts, expected, existing_rvars)
		}
		if !is_last_branch {
			g.match_branch(node, expected, unpacked_params, branch_idx + 1, existing_rvars)
		}
	}
}

fn (mut g Gen) match_branch_exprs(node ast.MatchExpr, expected ast.Type, unpacked_params []wasm.ValType, branch_idx int, expr_idx int, existing_rvars []Var, branch ast.MatchBranch) {
	if expr_idx >= branch.exprs.len {
		return
	}

	mut is_last_branch := branch_idx + 1 >= node.branches.len
	mut is_last_expr := expr_idx + 1 >= branch.exprs.len

	expr := branch.exprs[expr_idx]

	if expr is ast.RangeExpr {
		wasm_type := g.as_numtype(g.get_wasm_type(node.cond_type))
		is_signed := node.cond_type.is_signed()

		g.expr(node.cond, node.cond_type)
		g.expr(expr.high, node.cond_type)
		g.func.le(wasm_type, is_signed)
	} else {
		if g.is_param_type(node.cond_type) {
			// Param types -> strings etc
			g.expr(node.cond, node.cond_type)
			g.expr(expr, node.cond_type)
			g.infix_from_typ(node.cond_type, .eq)
		} else {
			// Numeric types -> direct comparison
			wasm_type := g.as_numtype(g.get_wasm_type(node.cond_type))
			g.expr(node.cond, node.cond_type)
			g.expr(expr, node.cond_type)
			g.func.eq(wasm_type)
		}
	}

	blk := g.func.c_if([], unpacked_params)
	{
		if branch.stmts.len > 0 {
			g.rvar_expr_stmts(branch.stmts, expected, existing_rvars)
		}
	}
	{
		g.func.c_else(blk)
		if is_last_expr {
			if !is_last_branch {
				g.match_branch(node, expected, unpacked_params, branch_idx + 1, existing_rvars)
			}
		} else {
			g.match_branch_exprs(node, expected, unpacked_params, branch_idx, expr_idx + 1,
				existing_rvars, branch)
		}
	}
	g.func.c_end(blk)
}

pub fn (mut g Gen) call_expr(node ast.CallExpr, expected ast.Type, existing_rvars []Var) {
	mut wasm_ns := ?string(none)
	mut name := node.name

	is_print := name in ['panic', 'println', 'print', 'eprintln', 'eprint']

	if node.is_method {
		name = '${g.table.get_type_name(node.receiver_type)}.${node.name}'
	}

	if node.language in [.js, .wasm] {
		cfn_attrs := unsafe { g.table.fns[node.name].attrs }

		short_name := if node.language == .js {
			node.name.all_after_last('JS.')
		} else {
			node.name.all_after_last('WASM.')
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
				op:    .amp
				right: node.left
			})
		} else {
			node.left
		}
		// hack alert!
		if node.receiver_type == ast.int_literal_type && expr is ast.IntegerLiteral {
			g.literal(expr.val, ast.i64_type)
		} else {
			g.expr(expr, node.receiver_type)
		}
	}

	// {arguments}
	//
	for idx, arg in node.args {
		mut expr := arg.expr

		mut typ := arg.typ
		if is_print && typ != ast.string_type {
			has_str, _, _ := g.table.sym(typ).str_method_info()
			if typ != ast.string_type && !has_str {
				g.v_error('cannot implicitly convert as argument does not have a .str() function',
					arg.pos)
			}

			expr = ast.CallExpr{
				name:           'str'
				left:           expr
				left_type:      typ
				receiver_type:  typ
				return_type:    ast.string_type
				is_method:      true
				is_return_used: true
			}
		}

		// another hack alert!
		if node.expected_arg_types[idx] == ast.int_literal_type && mut expr is ast.IntegerLiteral {
			g.literal(expr.val, ast.i64_type)
		} else {
			g.expr(expr, node.expected_arg_types[idx])
		}
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

pub fn (mut g Gen) get_field_offset(typ ast.Type, name string) int {
	ts := g.table.sym(typ)
	field := ts.find_field(name) or { g.w_error('could not find field `${name}` on init') }
	si := g.pool.type_struct_info(typ) or { panic('unreachable') }
	return si.offsets[field.i]
}

pub fn (mut g Gen) field_offset(typ ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	if offset != 0 {
		g.func.i32_const(i32(offset))
		g.func.add(.i32_t)
	}
}

pub fn (mut g Gen) load_field(typ ast.Type, ftyp ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	g.load(ftyp, offset)
}

pub fn (mut g Gen) store_field(typ ast.Type, ftyp ast.Type, name string) {
	offset := g.get_field_offset(typ, name)
	g.store(ftyp, offset)
}

pub fn (mut g Gen) expr(node ast.Expr, expected ast.Type) {
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
			mut direct_array_access := g.is_direct_array_access || node.is_direct
			mut tmp_voidptr_var := wasm.LocalIndex(-1)

			// ptr + index * size
			mut typ := node.left_type
			ts := g.table.sym(typ)

			g.expr(node.left, node.left_type)
			if node.left_type == ast.string_type {
				if !direct_array_access {
					tmp_voidptr_var = g.func.new_local_named(.i32_t, '__tmp<voidptr>')
					g.func.local_tee(tmp_voidptr_var)
				}

				// be pedantic...
				g.load_field(ast.string_type, ast.voidptr_type, 'str')
				typ = ast.u8_type
			} else if typ.is_ptr() {
				typ = typ.deref()
				direct_array_access = true
			} else {
				match ts.info {
					ast.Array {
						g.w_error('wasm backend does not support dynamic arrays')
					}
					ast.ArrayFixed {
						typ = ts.info.elem_type
						if node.index.is_pure_literal() {
							// checker would have gotten this by now
							direct_array_access = true
						}
					}
					else {
						g.w_error('ast.IndexExpr: unreachable')
					}
				}
			}

			size, _ := g.pool.type_size(typ)

			g.expr(node.index, ast.int_type)

			if !direct_array_access {
				g.is_leaf_function = false // calls panic()

				idx_temp := g.func.new_local_named(.i32_t, '__tmp<int>')
				g.func.local_tee(idx_temp)

				// .len
				if node.left_type == ast.string_type {
					g.func.local_get(tmp_voidptr_var)
					g.load_field(ast.string_type, ast.int_type, 'len')
				} else if ts.info is ast.ArrayFixed {
					g.func.i32_const(i32(ts.info.size))
				} else {
					panic('unreachable')
				}

				g.func.ge(.i32_t, false)
				// is_signed: false, negative numbers will be reinterpreted as > 2^31 and will also trigger false
				blk := g.func.c_if([], [])
				{
					g.expr(ast.StringLiteral{ val: '${g.file_pos(node.pos)}: ${ast.Expr(node)}' },
						ast.string_type)
					g.func.call('eprintln')
					g.expr(ast.StringLiteral{ val: 'index out of range' }, ast.string_type)
					g.func.call('panic')
				}
				g.func.c_end(blk)

				g.func.local_get(idx_temp)
			}

			if size > 1 {
				g.literalint(size, ast.int_type)
				g.func.mul(.i32_t)
			}

			g.func.add(.i32_t)

			if !g.is_pure_type(typ) {
				return
			}

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
						g.v_error("cannot take the address of a value that doesn't live on the stack. this is a current limitation.",
							node.pos)
					}
					g.ref(v)
				} else {
					g.get(v)
				}
			} else {
				g.needs_address = true
				{
					g.expr(node.expr, node.typ)
				}
				g.needs_address = false
				g.field_offset(node.expr_type, node.field_name)
				if g.is_pure_type(node.typ) && !g.needs_address {
					// expected to be a pointer
					g.load(node.typ, 0)
				}
			}
			g.cast(node.typ, expected)
		}
		ast.MatchExpr {
			g.match_expr(node, expected, [])
		}
		ast.EnumVal {
			type_name := g.table.get_type_name(node.typ)
			ts_type := (g.table.sym(node.typ).info as ast.Enum).typ
			g.literalint(g.enum_vals[type_name].fields[node.val], ts_type)
		}
		ast.OffsetOf {
			sym := g.table.sym(node.struct_type)
			if sym.kind != .struct {
				g.v_error('__offsetof expects a struct Type as first argument', node.pos)
			}
			off := g.get_field_offset(node.struct_type, node.field)
			g.literalint(off, ast.u32_type)
		}
		ast.SizeOf {
			if !g.table.known_type_idx(node.typ) {
				g.v_error('unknown type `${*g.table.sym(node.typ)}`', node.pos)
			}
			size, _ := g.pool.type_size(node.typ)
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
			g.func.i32_const(i32(rns))
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
			g.if_expr(node, expected, [])
		}
		ast.CastExpr {
			// don't want to handle ast.int_literal_type
			if node.expr is ast.IntegerLiteral || node.expr is ast.FloatLiteral {
				g.expr(node.expr, node.typ)
				return
			}

			g.expr(node.expr, node.expr_type)

			// TODO: unbelievable colossal hack
			mut typ := node.expr_type
			if node.expr is ast.Ident {
				v := g.get_var_from_ident(node.expr)
				if g.is_param(v) && node.expr_type == ast.int_literal_type {
					typ = ast.i64_type
				}
			}

			g.func.cast(g.as_numtype(g.get_wasm_type(typ)), typ.is_signed(), g.as_numtype(g.get_wasm_type(node.typ)))
		}
		ast.CallExpr {
			g.call_expr(node, expected, [])
		}
		else {
			g.w_error('wasm.expr(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) for_in_stmt(node ast.ForInStmt) {
	loop_var_type := unpack_literal_int(node.val_type)
	block := g.func.c_block([], [])
	{
		mut loop_var := Var{}
		loop_var = g.new_local(node.val_var, loop_var_type)

		g.expr(node.cond, loop_var_type)
		g.set(loop_var)

		loop := g.func.c_loop([], [])
		{
			g.loop_breakpoint_stack << LoopBreakpoint{
				c_continue: loop
				c_break:    block
				name:       node.label
			}

			g.get(loop_var)
			g.expr(node.high, loop_var_type)
			wtyp := g.as_numtype(g.get_wasm_type(loop_var_type))
			g.func.lt(wtyp, loop_var_type.is_signed())
			g.func.eqz(.i32_t)
			g.func.c_br_if(block)

			g.expr_stmts(node.stmts, ast.void_type)

			g.set_prepare(loop_var)
			{
				g.get(loop_var)
				g.literalint(1, loop_var_type)
				g.func.add(wtyp)
			}
			g.set(loop_var)

			g.func.c_br(loop)
			g.loop_breakpoint_stack.pop()
		}
		g.func.c_end(loop)
	}
	g.func.c_end(block)
}

pub fn (g &Gen) file_pos(pos token.Pos) string {
	return '${g.file_path}:${pos.line_nr + 1}:${pos.col + 1}'
}

pub fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) {
	match node {
		ast.Block {
			g.expr_stmts(node.stmts, expected)
		}
		ast.Return {
			if node.exprs.len > 1 {
				g.set_with_multi_expr(ast.ConcatExpr{ vals: node.exprs }, g.ret, g.ret_rvars)
			} else if node.exprs.len == 1 {
				g.set_with_multi_expr(node.exprs[0], g.ret, g.ret_rvars)
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
						c_break:    block
						name:       node.label
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
					continue_block := g.func.c_block([], [])
					{
						g.loop_breakpoint_stack << LoopBreakpoint{
							c_continue: continue_block
							c_break:    block
							name:       node.label
						}

						if node.has_cond {
							g.expr(node.cond, ast.bool_type)
							g.func.eqz(.i32_t)
							g.func.c_br_if(block) // !cond, goto end
						}

						g.expr_stmts(node.stmts, ast.void_type)

						g.loop_breakpoint_stack.pop()
					}
					g.func.c_end(continue_block)

					if node.has_inc {
						g.expr_stmt(node.inc, ast.void_type)
					}

					g.func.c_br(loop)
				}
				g.func.c_end(loop)
			}
			g.func.c_end(block)
		}
		ast.ForInStmt {
			g.for_in_stmt(node)
		}
		ast.BranchStmt {
			mut bp := g.loop_breakpoint_stack.last()
			if node.label != '' {
				for i := g.loop_breakpoint_stack.len; i > 0; {
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
		ast.DeferStmt {
			v := g.new_local('__defer(${node.idx_in_fn})', ast.bool_type)
			g.func.i32_const(1)
			g.set(v)
			g.defer_vars << v
		}
		ast.AssertStmt {
			if !node.is_used {
				return
			}

			// calls builtin functions, don't want to corrupt stack frame!
			g.is_leaf_function = false

			g.expr(node.expr, ast.bool_type)
			g.func.eqz(.i32_t) // !expr
			lbl := g.func.c_if([], [])
			{
				// main.main: ${msg}
				// V panic: Assertion failed...

				mut msg := '${g.file_pos(node.pos)}: fn ${g.fn_name}: ${ast.Stmt(node)}'
				if node.extra is ast.StringLiteral {
					msg += ", '${node.extra.val}'"
				}

				g.expr(ast.StringLiteral{ val: msg }, ast.string_type)
				g.func.call('eprintln')
				g.expr(ast.StringLiteral{ val: 'Assertion failed...' }, ast.string_type)
				g.func.call('panic')
			}
			g.func.c_end(lbl)
		}
		ast.AssignStmt {
			if node.has_cross_var {
				g.w_error('complex assign statements are not implemented')
				// `a, b = b, a`
			} else {
				// `a := 1` | `a, b := 1, 2`
				// `a, b := foo()`
				// `a, b := if cond { 1, 2 } else { 3, 4 }`

				is_expr_assign := node.op !in [.decl_assign, .assign]

				// similar code from `call_expr()`
				// create variables or obtain them for use as rvals
				mut rvars := []Var{cap: node.left_types.len}
				for idx, rt in node.left_types {
					left := node.left[idx]

					mut var := Var{}
					mut passed := false

					if left is ast.Ident {
						if left.kind == .blank_ident {
							var = g.new_local('_', rt)
							passed = true
						} else if node.op == .decl_assign {
							var = g.new_local(left.name, rt)
							passed = true
						}
					}

					if !passed && node.op == .assign {
						if v := g.get_var_from_expr(left) {
							var = v
						}
					}

					if g.is_param_type(rt) {
						rvars << var
					}
				}

				mut set := false
				if node.right.len == 1 {
					right := node.right[0]
					match right {
						ast.IfExpr {
							params := node.left_types.filter(!g.is_param_type(it)).map(g.get_wasm_type(it))
							g.if_branch(right, right.typ, params, 0, rvars)
							set = true
						}
						ast.CallExpr {
							g.call_expr(right, 0, rvars)
							set = true
						}
						else {
							// : set = false
							// execute below instead
						}
					}
				}

				// will never be a multi expr
				// assume len == 1 for left and right
				if is_expr_assign {
					left, right, typ := node.left[0], node.right[0], node.left_types[0]

					rop := token.assign_op_to_infix_op(node.op)
					lhs := g.get_var_or_make_from_expr(left, typ)

					if !g.is_pure_type(lhs.typ) {
						// main.struct.+
						name := '${g.table.get_type_name(lhs.typ)}.${rop}'
						g.ref(lhs)
						g.ref(lhs)
						g.expr(right, lhs.typ)
						g.func.call(name)
					} else {
						g.set_prepare(lhs)
						{
							g.get(lhs)
							g.expr(right, lhs.typ)
							g.infix_from_typ(lhs.typ, rop)
						}
						g.set_set(lhs)
					}

					return
				}

				// prepare variables using expr()
				// if is an rvar, set it and ignore following
				if !set {
					assert node.left.len == node.right.len
					mut ridx := 0
					for idx, right in node.right {
						typ := node.left_types[idx]
						if g.is_param_type(typ) {
							g.set_with_expr(right, rvars[ridx])
							ridx++
						} else {
							g.expr(right, typ)
						}
					}
				}

				for i := node.left.len; i > 0; {
					i--
					left := node.left[i]
					typ := node.left_types[i]

					if g.is_param_type(typ) {
						// is already set
						continue
					}

					if left is ast.Ident {
						// `_ = expr`
						if left.kind == .blank_ident {
							// expression still may have side effect
							g.func.drop()
							continue
						}
					}

					v := g.get_var_or_make_from_expr(left, typ)
					g.set(v)
				}
			}
		}
		ast.AsmStmt {
			// assumed expected == void
			g.asm_stmt(node)
		}
		else {
			g.w_error('wasm.expr_stmt(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) expr_stmts(stmts []ast.Stmt, expected ast.Type) {
	g.rvar_expr_stmts(stmts, expected, [])
}

pub fn (mut g Gen) rvar_expr_stmts(stmts []ast.Stmt, expected ast.Type, existing_rvars []Var) {
	for idx, stmt in stmts {
		if idx + 1 >= stmts.len {
			if stmt is ast.ExprStmt {
				g.set_with_multi_expr(stmt.expr, expected, existing_rvars)
			} else {
				g.expr_stmt(stmt, expected)
			}
		} else {
			g.expr_stmt(stmt, ast.void_type)
		}
	}
}

pub fn (mut g Gen) toplevel_stmt(node ast.Stmt) {
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

pub fn gen(files []&ast.File, mut table ast.Table, out_name string, w_pref &pref.Preferences) {
	stack_top := w_pref.wasm_stack_top
	mut g := &Gen{
		table:     table
		pref:      w_pref
		files:     files
		eval:      eval.new_eval(table, w_pref)
		pool:      serialise.new_pool(table, store_relocs: true, null_terminated: false)
		stack_top: stack_top
		data_base: calc_align(stack_top + 1, 16)
	}
	g.table.pointer_size = 4
	g.mod.assign_memory('memory', true, 1, none)

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
		if os.is_atty(1) == 1 {
			eprintln('pretty printing to stdout is not implemented for the time being')
			eprintln('raw bytes can mess up your terminal, are you piping into a file?')
			exit(1)
		} else {
			print(mod.bytestr())
		}
	}

	if out_name != '-' {
		os.write_file_array(out_name, mod) or { panic(err) }
		if g.pref.wasm_validate {
			exe := $if windows { 'wasm-validate.exe' } $else { 'wasm-validate' }
			if rt := os.find_abs_path_of_executable(exe) {
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
				g.w_error('${exe} not found! Try installing WABT (WebAssembly Binary Toolkit). Run `./cmd/tools/install_wabt.vsh`, to download a prebuilt executable for your platform.')
			}
		}
		if g.pref.is_prod {
			exe := $if windows { 'wasm-opt.exe' } $else { 'wasm-opt' }
			if rt := os.find_abs_path_of_executable(exe) {
				// -lmu: low memory unused, very important optimisation
				res := os.execute('${os.quoted_path(rt)} -all -lmu -c -O4 ${os.quoted_path(out_name)} -o ${os.quoted_path(out_name)}')
				if res.exit_code != 0 {
					eprintln(res.output)
					g.w_error('${rt} failed, this should not happen. Report an issue with the above messages, the webassembly generated, and appropriate code.')
				}
			} else {
				g.w_error('${exe} not found! Try installing Binaryen.
				|    Run `./cmd/tools/install_binaryen.vsh`, to download a prebuilt executable for your platform.
				|    After that, either copy or symlink thirdparty/binaryen/bin/wasm-opt to a folder on your PATH,
				|    or add thirdparty/binaryen/bin to your PATH.
				|    Use `wasm-opt --version` to verify that it can be found.
				'.strip_margin())
			}
		}
	} else if g.pref.wasm_validate || g.pref.is_prod {
		eprintln('stdout output, cannot validate or optimise wasm')
	}
}

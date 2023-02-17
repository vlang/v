module wasm

import v.ast
import v.pref
import v.util
import v.token
import v.errors
import v.eval
import binaryen as wa
import os

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	warnings  []errors.Warning
	errors    []errors.Error
	table     &ast.Table = unsafe { nil }
	eval      eval.Eval
	enum_vals map[string]Enum
	//
	bp_idx            int                     // Base pointer temporary's index for function, if needed (-1 for none)
	stack_frame       int                     // Size of the current stack frame, if needed
	mod               wa.Module               // Current Binaryen WebAssembly module
	curr_ret          []ast.Type              // Current return value, multi returns will be split into an array
	local_temporaries []Temporary             // Local WebAssembly temporaries, referenced with an index
	local_addresses   map[string]Stack        // Local stack structures relative to `bp_idx`
	structs           map[ast.Type]StructInfo // Cached struct field offsets
	//
	lbl           int
	for_labels    []string     // A stack of strings containing the names of blocks/loops to break/continue to
	stack_patches []BlockPatch // See `Gen.stack_deinit_push_patch()`
	needs_stack   bool // If true, will import `__vmem` and `__vsp`
	constant_data []ConstantData
	constant_data_offset int
	module_import_namespace string // `[wasm_import_namespace: 'wasi_snapshot_preview1']` else `env`
}

struct StructInfo {
mut:
	offsets []int
}

pub fn (mut g Gen) v_error(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('error:', pos: pos, file_path: g.pref.path, message: s)
		exit(1)
	} else {
		g.errors << errors.Error{
			file_path: g.pref.path
			pos: pos
			reporter: .gen
			message: s
		}
	}
}

pub fn (mut g Gen) warning(s string, pos token.Pos) {
	if g.pref.output_mode == .stdout {
		util.show_compiler_message('warning:', pos: pos, file_path: g.pref.path, message: s)
	} else {
		g.warnings << errors.Warning{
			file_path: g.pref.path
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

fn (mut g Gen) vsp_leave() wa.Expression {
	return wa.globalset(g.mod, c'__vsp', wa.localget(g.mod, g.bp_idx, type_i32))
}

fn (mut g Gen) setup_stack_frame(body wa.Expression) wa.Expression {
	// The V WASM stack grows upwards. This is a choice that came
	// to me after considering the following.
	//
	// 1. Store operator offsets cannot be negative.
	// 2. The size allocated for the stack is unknown until
	//    the end of the function's generation.
	//    This means that stack deallocation code when returning
	//    early from function's do not know how much to free.
	// 3. I came up with an alternative, a single exit point
	//    inside a function, with values "falling through" to the
	//    end of a function and being returned.
	//    This would fix problem 2. It did not work...
	//      https://github.com/WebAssembly/binaryen/issues/5490
	//
	// Any other option would cause a large amount of boilerplate
	// WASM code being duplicated at every return statement.
	//
	// stack_enter:
	//     global.get $__vsp
	//     local.tee $bp_idx
	//     i32.const {stack_frame}
	//     i32.add
	//     global.set $__vsp
	// stack_leave:
	//     local.get $bp_idx
	//     global.set $__vsp

	// No stack allocations needed!
	if g.stack_frame == 0 {
		return body
	}
	g.needs_stack = true

	// vfmt off
	stack_enter := 
		wa.globalset(g.mod, c'__vsp', 
			wa.binary(g.mod, wa.addint32(),
				wa.constant(g.mod, wa.literalint32(g.stack_frame)),
				wa.localtee(g.mod, g.bp_idx,
					wa.globalget(g.mod, c'__vsp', type_i32), type_i32)))
	// vfmt on
	mut n_body := [stack_enter, body]

	if g.curr_ret[0] == ast.void_type {
		n_body << g.vsp_leave()
	}

	for bp in g.stack_patches {
		// Insert stack leave on all return calls
		wa.blockinsertchildat(bp.block, bp.idx, g.vsp_leave())
	}
	g.stack_patches.clear()

	return g.mkblock(n_body)
}

fn (mut g Gen) function_return_wasm_type(typ ast.Type) wa.Type {
	if typ == ast.void_type {
		return type_none
	}
	types := g.unpack_type(typ).filter(g.table.sym(it).kind != .struct_).map(g.get_wasm_type(it))
	if types.len == 0 {
		return type_none
	}
	return wa.typecreate(types.data, types.len)
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

fn (mut g Gen) fn_external_import(node ast.FnDecl) {
	if !node.no_body || node.is_method {
		g.v_error('interop functions cannot have bodies', node.body_pos)
	}
	if node.language == .js && g.pref.os == .wasi {
		g.v_error('javascript interop functions are not allowed in a `wasi` build', node.pos)
	}

	mut paraml := []wa.Type{cap: node.params.len}
	for arg in node.params {
		if !g.is_pure_type(arg.typ) {
			g.v_error('arguments to interop functions must be numbers, pointers or booleans',
				arg.type_pos)
		}
		paraml << g.get_wasm_type(arg.typ)
	}
	if !(node.return_type == ast.void_type || g.is_pure_type(node.return_type)) {
		g.v_error('interop functions must return numbers, pointers or booleans', node.return_type_pos)
	}
	return_type := g.get_wasm_type(node.return_type)

	// internal name: `JS.setpixel`
	// external name: `setpixel`
	wa.addfunctionimport(g.mod, node.name.str, g.module_import_namespace.str, node.short_name.str, wa.typecreate(paraml.data,
		paraml.len), return_type)
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

	// The first parameter is an address of returned struct,
	// regardless if the struct contains one field.
	//   (this should change and is currently a TODO to simplify generation)
	//
	// All structs are passed by reference regardless if the struct contains one field.
	//   (todo again...)
	//
	// Multi returns are implemented with a binaryen tuple type, not a struct reference.

	return_type := g.function_return_wasm_type(node.return_type)

	mut paraml := []wa.Type{cap: node.params.len + 1}
	g.bp_idx = -1
	g.stack_frame = 0

	g.curr_ret = g.unpack_type(node.return_type)

	for idx, typ in g.curr_ret {
		sym := g.table.sym(typ)
		if sym.info is ast.Struct {
			g.local_temporaries << Temporary{
				name: '__return${idx}'
				typ: type_i32 // pointer
				ast_typ: typ
				idx: g.local_temporaries.len
			}
			paraml << ast.voidptr_type
		}
	}

	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		/*
		if g.table.sym(p.typ).info is ast.Struct {
			println("INIT: ${g.structs}, ${g.table.sym(p.typ)}, ${g.table.sym(p.typ).idx}, ${p.typ}, ${p.typ.idx()}")
		}*/
		g.local_temporaries << Temporary{
			name: p.name
			typ: typ
			ast_typ: p.typ
			idx: g.local_temporaries.len
		}
		paraml << typ
	}
	params_type := wa.typecreate(paraml.data, paraml.len)

	g.bp_idx = g.new_local_temporary_anon(ast.int_type)
	mut wasm_expr := g.expr_stmts(node.stmts, ast.void_type)
	wasm_expr = g.setup_stack_frame(wasm_expr)

	mut temporaries := []wa.Type{cap: g.local_temporaries.len - paraml.len}
	for idx := paraml.len; idx < g.local_temporaries.len; idx++ {
		temporaries << g.local_temporaries[idx].typ
	}
	// func :=
	wa.addfunction(g.mod, name.str, params_type, return_type, temporaries.data, temporaries.len,
		wasm_expr)

	// WTF?? map values are not resetting???
	//   g.local_addresses.clear()
	g.local_temporaries.clear()
	g.local_addresses = map[string]Stack{}
	assert g.for_labels.len == 0
}

fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) wa.Expression {
	if expr is ast.IntegerLiteral {
		return g.literal(expr.val, expected_type)
	} else if expr is ast.FloatLiteral {
		return g.literal(expr.val, expected_type)
	}

	got_type := ast.mktyp(got_type_raw)
	return g.cast_t(g.expr(expr, got_type), got_type, expected_type)
}

fn (mut g Gen) literalint(val i64, expected ast.Type) wa.Expression {
	match g.get_wasm_type(expected) {
		type_i32 { return wa.constant(g.mod, wa.literalint32(int(val))) }
		type_i64 { return wa.constant(g.mod, wa.literalint64(val)) }
		else {}
	}
	g.w_error('literalint: bad type `${expected}`')
}

fn (mut g Gen) literal(val string, expected ast.Type) wa.Expression {
	match g.get_wasm_type(expected) {
		type_i32 { return wa.constant(g.mod, wa.literalint32(val.int())) }
		type_i64 { return wa.constant(g.mod, wa.literalint64(val.i64())) }
		type_f32 { return wa.constant(g.mod, wa.literalfloat32(val.f32())) }
		type_f64 { return wa.constant(g.mod, wa.literalfloat64(val.f64())) }
		else {}
	}
	g.w_error('literal: bad type `${expected}`')
}

fn (mut g Gen) postfix_expr(node ast.PostfixExpr) wa.Expression {
	if node.expr !is ast.Ident {
		g.w_error('postfix_expr: not ast.Ident')
	}

	kind := if node.op == .inc { token.Kind.plus } else { token.Kind.minus }

	idx, typ := g.get_local_temporary_from_ident(node.expr as ast.Ident)
	atyp := g.local_temporaries[idx].ast_typ

	op := g.infix_from_typ(atyp, kind)

	return wa.localset(g.mod, idx, wa.binary(g.mod, op, wa.localget(g.mod, idx, typ),
		g.literal('1', atyp)))
}

fn (mut g Gen) infix_expr(node ast.InfixExpr, expected ast.Type) wa.Expression {
	if node.op in [.logical_or, .and] {
		mut exprs := []wa.Expression{cap: 2}

		left := g.expr(node.left, node.left_type)

		temporary := g.new_local_temporary_anon(ast.bool_type)
		exprs << wa.localset(g.mod, temporary, left)

		cmp := if node.op == .logical_or {
			wa.unary(g.mod, wa.eqzint32(), wa.localget(g.mod, temporary, type_i32))
		} else {
			wa.localget(g.mod, temporary, type_i32)
		}
		exprs << wa.bif(g.mod, cmp, g.expr(node.right, node.right_type), wa.localget(g.mod,
			temporary, type_i32))

		return g.mkblock(exprs)
	}

	op := g.infix_from_typ(node.left_type, node.op)

	infix := wa.binary(g.mod, op, g.expr(node.left, node.left_type), g.expr_with_cast(node.right,
		node.right_type, node.left_type))

	res_typ := if infix_kind_return_bool(node.op) {
		ast.bool_type
	} else {
		node.left_type
	}
	return g.cast_t(infix, res_typ, expected)
}

fn (mut g Gen) prefix_expr(node ast.PrefixExpr) wa.Expression {
	expr := g.expr(node.right, node.right_type)

	return match node.op {
		.minus {
			if node.right_type.is_pure_float() {
				if node.right_type == ast.f32_type_idx {
					wa.unary(g.mod, wa.negfloat32(), expr)
				} else {
					wa.unary(g.mod, wa.negfloat64(), expr)
				}
			} else {
				// -val == 0 - val

				if g.get_wasm_type(node.right_type) == type_i32 {
					wa.binary(g.mod, wa.subint32(), wa.constant(g.mod, wa.literalint32(0)),
						expr)
				} else {
					wa.binary(g.mod, wa.subint64(), wa.constant(g.mod, wa.literalint64(0)),
						expr)
				}
			}
		}
		.not {
			assert node.right_type == ast.bool_type
			wa.unary(g.mod, wa.eqzint32(), expr)
		}
		.bit_not {
			// ~val == val ^ -1

			if g.get_wasm_type(node.right_type) == type_i32 {
				wa.binary(g.mod, wa.xorint32(), expr, wa.constant(g.mod, wa.literalint32(-1)))
			} else {
				wa.binary(g.mod, wa.xorint64(), expr, wa.constant(g.mod, wa.literalint64(-1)))
			}
		}
		.amp {
			var := g.get_var_from_expr(node.right)
			match var {
				Stack {
					g.lea_address(var.address)
				}
				else {
					// TODO: function argument structs will just as pointers, handle them later
					g.w_error('Gen.prefix_expr: &val not implemented for non struct values')
				}
			}
		}
		else {
			// impl deref (.mul), and impl address of (.amp)
			g.w_error('`${node.op}val` prefix expression not implemented')
		}
	}
}

fn (mut g Gen) mknblock(name string, nodes []wa.Expression) wa.Expression {
	g.lbl++
	return wa.block(g.mod, '${name}${g.lbl}'.str, nodes.data, nodes.len, type_auto)
}

fn (mut g Gen) mkblock(nodes []wa.Expression) wa.Expression {
	g.lbl++
	return wa.block(g.mod, 'BLK${g.lbl}'.str, nodes.data, nodes.len, type_auto)
}

fn (mut g Gen) if_branch(ifexpr ast.IfExpr, idx int) wa.Expression {
	curr := ifexpr.branches[idx]

	next := if ifexpr.has_else && idx + 2 >= ifexpr.branches.len {
		g.expr_stmts(ifexpr.branches[idx + 1].stmts, ifexpr.typ)
	} else if idx + 1 >= ifexpr.branches.len {
		unsafe { nil }
	} else {
		g.if_branch(ifexpr, idx + 1)
	}
	return wa.bif(g.mod, g.expr(curr.cond, ast.bool_type), g.expr_stmts(curr.stmts, ifexpr.typ),
		next)
}

fn (mut g Gen) if_expr(ifexpr ast.IfExpr) wa.Expression {
	return g.if_branch(ifexpr, 0)
}

fn (mut g Gen) get_ident(node ast.Ident, expected ast.Type) (wa.Expression, int) {
	idx, typ := g.get_local_temporary_from_ident(node)
	expr := wa.localget(g.mod, idx, typ)

	return g.cast(expr, typ, g.is_signed(g.local_temporaries[idx].ast_typ), g.get_wasm_type(expected)), idx
}

const wasm_builtins = ['__memory_grow', '__memory_fill', '__memory_copy']

fn (mut g Gen) wasm_builtin(name string, node ast.CallExpr) wa.Expression {
	mut args := []wa.Expression{cap: node.args.len}
	for idx, arg in node.args {
		args << g.expr(arg.expr, node.expected_arg_types[idx])
	}
	
	match name {
		'__memory_grow' {
			return wa.memorygrow(g.mod, args[0], c'memory', false)
		}
		'__memory_fill' {
			return wa.memoryfill(g.mod, args[0], args[1], args[2], c'memory')
		}
		'__memory_copy' {
			return wa.memorycopy(g.mod, args[0], args[1], args[2], c'memory', c'memory')
		}
		else { panic('unreachable') }
	}
}

fn (mut g Gen) expr(node ast.Expr, expected ast.Type) wa.Expression {
	return match node {
		ast.ParExpr, ast.UnsafeExpr {
			g.expr(node.expr, expected)
		}
		ast.ArrayInit {
			g.w_error('wasm backend does not support arrays yet')
		}
		ast.GoExpr {
			g.w_error('wasm backend does not support threads')
		}
		ast.SelectorExpr {
			g.cast_t(g.path_expr_t(node, node.typ), node.typ, expected)
		}
		ast.StructInit {
			pos := g.allocate_struct('_anonstruct', node.typ)
			expr := g.init_struct(Stack{ address: pos, ast_typ: node.typ }, node)
			g.mknblock('EXPR(STRUCTINIT)', [expr, g.lea_address(pos)])
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
			size, _ := g.table.type_size(node.typ)
			g.literalint(size, ast.u32_type)
		}
		ast.BoolLiteral {
			val := if node.val { wa.literalint32(1) } else { wa.literalint32(0) }
			wa.constant(g.mod, val)
		}
		ast.StringLiteral {
			pos := g.allocate_struct('_anonstring', ast.string_type)
			
			offset, len := g.allocate_string(node)
			g.mknblock('EXPR(STRINGINIT)', [
				g.set_var(Stack{ address: pos, ast_typ: ast.charptr_type }, g.literalint(offset, ast.u32_type), offset: 0),
				g.set_var(Stack{ address: pos, ast_typ: ast.int_type }, g.literalint(len, ast.int_type), offset: g.table.pointer_size)
				g.lea_address(pos)
			])
		}
		ast.InfixExpr {
			g.infix_expr(node, expected)
		}
		ast.PrefixExpr {
			g.prefix_expr(node)
		}
		ast.PostfixExpr {
			g.postfix_expr(node)
		}
		ast.Ident {
			// TODO: only supports local identifiers, no path.expressions or global names
			g.get_var_t(node, expected)
		}
		ast.IntegerLiteral, ast.FloatLiteral {
			g.literal(node.val, expected)
		}
		ast.IfExpr {
			if node.branches.len == 2 && node.is_expr {
				left := g.expr_stmts(node.branches[0].stmts, expected)
				right := g.expr_stmts(node.branches[1].stmts, expected)
				wa.bselect(g.mod, g.expr(node.branches[0].cond, ast.bool_type_idx), left,
					right, g.get_wasm_type(expected))
			} else {
				g.if_expr(node)
			}
		}
		ast.CastExpr {
			expr := g.expr(node.expr, node.expr_type)

			if node.typ == ast.bool_type {
				// WebAssembly booleans use the `i32` type
				//   = 0 | is false
				//   > 0 | is true
				//
				// It's a checker error to cast to bool anyway...

				bexpr := g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type),
					type_i32)
				wa.bselect(g.mod, bexpr, wa.constant(g.mod, wa.literalint32(1)), wa.constant(g.mod,
					wa.literalint32(0)), type_i32)
			} else {
				g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type),
					g.get_wasm_type(node.typ))
			}
		}
		ast.CallExpr {
			mut name := node.name
			mut arguments := []wa.Expression{cap: node.args.len + 1}

			if name in ['panic', 'println', 'print', 'eprintln', 'eprint'] {
				arg := node.args[0]
				if arg.expr !is ast.StringLiteral {
					g.v_error('builtin function `${name}` must be called with a string literal',
						arg.pos)
				}
			} else if name in wasm_builtins {
				return g.wasm_builtin(node.name, node)
			}

			if node.is_method {
				name = '${g.table.get_type_name(node.receiver_type)}.${node.name}'
			}

			ret_types := g.unpack_type(node.return_type)
			structs := ret_types.filter(g.table.sym(it).info is ast.Struct)
			mut structs_addrs := []int{cap: structs.len}

			// ABI: {return structs} {method `self`}, then {arguments}
			for typ in structs {
				pos := g.allocate_struct('_anonstruct', typ)
				structs_addrs << pos
				arguments << g.lea_address(pos)
			}
			if node.is_method {
				expr := if !node.left_type.is_ptr() && node.receiver_type.is_ptr() {
					ast.Expr(ast.PrefixExpr{
						op: .amp
						right: node.left
					})
				} else {
					node.left
				}
				arguments << g.expr(expr, node.receiver_type)
			}
			for idx, arg in node.args {
				arguments << g.expr(arg.expr, node.expected_arg_types[idx])
			}

			fret := g.function_return_wasm_type(node.return_type)
			mut call := wa.call(g.mod, name.str, arguments.data, arguments.len, fret)
			if structs.len != 0 {
				mut temporary := 0
				// The function's return values contains structs and must be reordered

				if ret_types.len - structs.len != 0 {
					temporary = g.new_local_temporary_anon_wtyp(fret)
					call = wa.localset(g.mod, temporary, call)
				}
				mut exprs := []wa.Expression{}

				mut sidx := 0
				mut tidx := 0
				for typ in ret_types {
					ts := g.table.sym(typ)

					if ts.info is ast.Struct {
						exprs << g.lea_address(structs_addrs[tidx])
						tidx++
					} else {
						exprs << wa.tupleextract(g.mod, wa.localget(g.mod, temporary,
							fret), sidx)
						sidx++
					}
				}

				vexpr := if exprs.len != 1 {
					wa.tuplemake(g.mod, exprs.data, exprs.len)
				} else {
					exprs[0]
				}

				call = g.mkblock([call, vexpr])
			}
			if expected == ast.void_type && node.return_type != ast.void_type {
				call = wa.drop(g.mod, call)
			} else if node.is_noreturn {
				// `[noreturn]` functions cannot return values
				call = g.mkblock([call, wa.unreachable(g.mod)])
			}
			call
		}
		else {
			g.w_error('wasm.expr(): unhandled node: ' + node.type_name())
		}
	}
}

fn (mut g Gen) multi_assign_stmt(node ast.AssignStmt) wa.Expression {
	if node.has_cross_var {
		g.w_error('complex assign statements are not implemented')
	}

	//
	// Expected to be a `a, b := multi_return()`.
	//

	mut exprs := []wa.Expression{cap: node.left.len + 1}

	ret := (node.right[0] as ast.CallExpr).return_type
	wret := g.get_wasm_type(ret)
	temporary := g.new_local_temporary_anon(ret)

	// Set multi return function to temporary, then use `tuple.extract`.
	exprs << wa.localset(g.mod, temporary, g.expr(node.right[0], 0))

	for i := 0; i < node.left.len; i++ {
		left := node.left[i]
		typ := node.left_types[i]
		// rtyp := node.right_types[i]

		if left is ast.Ident {
			// `_ = expr`
			if left.kind == .blank_ident {
				continue
			}
			if node.op == .decl_assign {
				g.new_local(left, typ)
			}
		}
		var := g.get_var_from_expr(left)
		popexpr := wa.tupleextract(g.mod, wa.localget(g.mod, temporary, wret), i)
		exprs << g.set_var(var, popexpr)
	}

	return g.mkblock(exprs)
}

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
	block wa.Expression
}

// Adds another `BlockPatch` to the current function to "patch" in
// a `g.vsp_leave()` when the function actually requires stack memory
fn (mut g Gen) stack_deinit_push_patch(val BlockPatch) {
	g.stack_patches
}

fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) wa.Expression {
	return match node {
		ast.Return {
			mut leave_expr_list := []wa.Expression{cap: node.exprs.len}
			mut exprs := []wa.Expression{cap: node.exprs.len}
			for idx, expr in node.exprs {
				if g.table.sym(g.curr_ret[idx]).info is ast.Struct {
					// Could be adapted to use random pointers?
					/*
					if expr is ast.StructInit {
						var := g.local_temporaries[g.get_local_temporary('__return${idx}')]
						leave_expr_list << g.init_struct(var, expr)
					}*/
					var := g.local_temporaries[g.get_local_temporary('__return${idx}')]
					address := g.expr(expr, g.curr_ret[idx])

					leave_expr_list << g.blit(address, g.curr_ret[idx], wa.localget(g.mod,
						var.idx, var.typ))
				} else {
					exprs << g.expr(expr, g.curr_ret[idx])
				}
			}

			mut patch := BlockPatch{
				idx: leave_expr_list.len
			}
			// leave_expr_list << g.vsp_leave()

			ret_expr := if exprs.len == 1 {
				exprs[0]
			} else if exprs.len == 0 {
				unsafe { nil }
			} else {
				wa.tuplemake(g.mod, exprs.data, exprs.len)
			}
			leave_expr_list << wa.ret(g.mod, ret_expr)

			patch.block = g.mkblock(leave_expr_list)
			g.stack_patches << patch

			patch.block
		}
		ast.ExprStmt {
			g.expr(node.expr, expected)
		}
		ast.ForStmt {
			lbl := g.new_for_label(node.label)

			lpp_name := 'L${lbl}'
			blk_name := 'B${lbl}'
			expr := if !node.is_inf {
				// wa.bif(g.mod, g.expr(node.cond, ast.bool_type))

				body := g.expr_stmts(node.stmts, ast.void_type)
				lbody := [
					// If !condition, leave.
					wa.br(g.mod, blk_name.str, wa.unary(g.mod, wa.eqzint32(), g.expr(node.cond,
						ast.bool_type)), unsafe { nil }),
					// Body.
					body,
					// Unconditional loop back to top.
					wa.br(g.mod, lpp_name.str, unsafe { nil }, unsafe { nil }),
				]
				loop := wa.loop(g.mod, lpp_name.str, g.mkblock(lbody))

				wa.block(g.mod, blk_name.str, &loop, 1, type_none)
			} else {
				loop_top := wa.br(g.mod, lpp_name.str, unsafe { nil }, unsafe { nil })

				loop := wa.loop(g.mod, lpp_name.str, g.mkblock([
					g.expr_stmts(node.stmts, ast.void_type),
					loop_top,
				]))

				wa.block(g.mod, blk_name.str, &loop, 1, type_none)
			}
			g.pop_for_label()
			expr
		}
		ast.ForCStmt {
			mut for_stmt := []wa.Expression{}
			if node.has_init {
				for_stmt << g.expr_stmt(node.init, ast.void_type)
			}

			lbl := g.new_for_label(node.label)
			lpp_name := 'L${lbl}'
			blk_name := 'B${lbl}'

			mut loop_exprs := []wa.Expression{}
			if node.has_cond {
				condexpr := wa.unary(g.mod, wa.eqzint32(), g.expr(node.cond, ast.bool_type))
				loop_exprs << wa.br(g.mod, blk_name.str, condexpr, unsafe { nil })
			}
			loop_exprs << g.expr_stmts(node.stmts, ast.void_type)

			if node.has_inc {
				loop_exprs << g.expr_stmt(node.inc, ast.void_type)
			}
			loop_exprs << wa.br(g.mod, lpp_name.str, unsafe { nil }, unsafe { nil })
			loop := wa.loop(g.mod, lpp_name.str, g.mkblock(loop_exprs))

			for_stmt << wa.block(g.mod, blk_name.str, &loop, 1, type_none)
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
			wa.br(g.mod, blabel.str, unsafe { nil }, unsafe { nil })
		}
		ast.AssignStmt {
			if (node.left.len > 1 && node.right.len == 1) || node.has_cross_var {
				// `a, b := foo()`
				// `a, b := if cond { 1, 2 } else { 3, 4 }`
				// `a, b = b, a`

				g.multi_assign_stmt(node)
			} else {
				// `a := 1` | `a,b := 1,2`

				mut exprs := []wa.Expression{cap: node.left.len}
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
							exprs << wa.drop(g.mod, g.expr(right, typ))
							continue
						}
						if node.op == .decl_assign {
							g.new_local(left, typ)
						}
					}

					var := g.get_var_from_expr(left)

					expr := if node.op !in [.decl_assign, .assign] {
						match var {
							Temporary {
								op := g.infix_from_typ(typ, token.assign_op_to_infix_op(node.op))
								infix := wa.binary(g.mod, op, wa.localget(g.mod, var.idx,
									var.typ), g.expr(right, typ))

								infix
							}
							Stack {
								g.w_error('unimplemented')
							}
							else {
								panic('unreachable')
							}
						}
					} else {
						if right is ast.StructInit {
							exprs << g.init_struct(var, right)
							continue
						}
						g.expr(right, typ)
					}
					exprs << g.set_var(var, expr)
				}

				if exprs.len == 1 {
					exprs[0]
				} else if exprs.len != 0 {
					g.mkblock(exprs)
				} else {
					wa.nop(g.mod)
				}
			}
		}
		else {
			g.w_error('wasm.expr_stmt(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) expr_stmts(stmts []ast.Stmt, expected ast.Type) wa.Expression {
	if stmts.len == 0 {
		return wa.nop(g.mod)
	}
	if stmts.len == 1 {
		return g.expr_stmt(stmts[0], expected)
	}
	mut exprl := []wa.Expression{cap: stmts.len}
	for idx, stmt in stmts {
		rtyp := if idx + 1 == stmts.len {
			expected
		} else {
			ast.void_type
		}
		exprl << g.expr_stmt(stmt, rtyp)
	}
	return g.mkblock(exprl)
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

fn (mut g Gen) housekeeping() {
	if g.needs_stack || g.constant_data.len != 0 {
		data := g.constant_data.map(it.data.data)
		data_len := g.constant_data.map(it.data.len)
		data_offsets := g.constant_data.map(wa.constant(g.mod, wa.literalint32(it.offset)))
		passive := []bool{len: g.constant_data.len, init: false}

		wa.setmemory(g.mod, 1, 1, c'memory', 
			data.data,
			passive.data,
			data_offsets.data,
			data_len.data,
			data.len,
			false,
			false,
			c'memory')
	}
	if g.needs_stack {
		// `g.constant_data_offset` rounded up to a multiple of 1024
		offset := g.constant_data_offset + (1024 - g.constant_data_offset % 1024) % 1024
		
		wa.addglobal(g.mod, c'__vsp', type_i32, true, g.literalint(offset, ast.int_type))
	}
	if g.pref.os == .wasi {
		// `_vinit` should be used to initialise the WASM module,
		// then `main.main` can be called safely.

		main_expr := wa.call(g.mod, c'main.main', unsafe { nil }, 0, type_none)
		wa.addfunction(g.mod, c'_start', type_none, type_none, unsafe { nil }, 0, main_expr)
		wa.addfunctionexport(g.mod, c'_start', c'_start')
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
		mod: wa.modulecreate()
	}
	g.table.pointer_size = 4
	wa.modulesetfeatures(g.mod, wa.featureall())

	if g.pref.os == .browser {
		eprintln("`-os browser` is experimental and will not live up to expectations...")
	}

	g.calculate_enum_fields()
	for file in g.files {
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	if g.structs.len != 0 {
		g.needs_stack = true
	}
	g.housekeeping()
	if wa.modulevalidate(g.mod) {
		wa.setdebuginfo(w_pref.is_debug)
		if w_pref.is_prod {
			wa.setoptimizelevel(3)
			wa.moduleoptimize(g.mod)
		}
		if out_name == '-' {
			wa.moduleprintstackir(g.mod, w_pref.is_prod)
		} else {
			bytes := wa.moduleallocateandwrite(g.mod, unsafe { nil })
			str := unsafe { (&char(bytes.binary)).vstring_with_len(int(bytes.binaryBytes)) }
			os.write_file(out_name, str) or { panic(err) }
		}
	} else {
		wa.moduleprint(g.mod)
		wa.moduledispose(g.mod)
		g.w_error('validation failed, this should not happen. report an issue with the above messages')
	}
	wa.moduledispose(g.mod)
}

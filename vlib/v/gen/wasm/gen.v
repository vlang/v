module wasm

import v.ast
import v.pref
import v.util
import v.token
import v.errors
import binaryen as wa
import os

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	warnings []errors.Warning
	errors   []errors.Error
	table    &ast.Table = unsafe { nil }
	//
	bp_idx            int                     // Base pointer temporary's index for function, if needed (-1 for none)
	stack_frame       int                     // Size of the current stack frame, if needed
	mod               wa.Module               // Current Binaryen WebAssembly module
	curr_ret          []ast.Type              // Current return value, multi returns will be split into an array
	//ret               Temporary               // Current return variable
	local_temporaries []Temporary             // Local WebAssembly temporaries, referenced with an index
	local_addresses   map[string]Stack        // Local stack structures relative to `bp_idx`
	structs           map[ast.Type]StructInfo // Cached struct field offsets
	lbl               int
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

fn (mut g Gen) setup_stack_frame(body wa.Expression) wa.Expression {
	// stack_enter:
	//     global.get $__vsp
	//     i32.const {stack_frame}
	//     i32.sub
	//     local.tee $bp_idx
	//     global.set $__vsp
	// stack_enter:
	//     local.get $bp_idx
	//     i32.const {stack_frame}
	//     i32.add
	//     global.set $__vsp

	// vfmt off
	stack_enter := 
		wa.globalset(g.mod, c'__vsp', 
			wa.localtee(g.mod, g.bp_idx, 
				wa.binary(g.mod, wa.subint32(),
					wa.globalget(g.mod, c'__vsp', type_i32), 
					wa.constant(g.mod, wa.literalint32(g.stack_frame))), type_i32))
	stack_leave :=
		wa.globalset(g.mod, c'__vsp', 
			wa.binary(g.mod, wa.addint32(),
				wa.localget(g.mod, g.bp_idx, type_i32), 
				wa.constant(g.mod, wa.literalint32(g.stack_frame))))
	// vfmt on

	mut n_body := [stack_enter, body, stack_leave]

	return g.mkblock(n_body)
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
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
	if node.is_builtin {
		g.warning('fn_decl: ${name} is builtin', node.pos)
	}

	// The first parameter is an address of returned struct,
	// regardless if the struct contains one field.
	//   (this should change and is currently a TODO to simplify generation)
	//
	// All structs are passed by reference regardless if the struct contains one field.
	//   (todo again...)
	//
	// Multi returns are implemented with a binaryen tuple type, not a struct reference.

	ts := g.table.sym(node.return_type)
	return_type := g.get_wasm_type(node.return_type)

	mut paraml := []wa.Type{cap: node.params.len + 1}
	g.bp_idx = -1
	g.stack_frame = 0

	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		idx := g.local_temporaries.len
		g.local_temporaries << Temporary{
			name: p.name
			typ: typ
			ast_typ: p.typ
			idx: idx
		}
		paraml << typ
	}
	params_type := wa.typecreate(paraml.data, paraml.len)

	g.curr_ret = if ts.kind == .multi_return {
		(ts.info as ast.MultiReturn).types
	} else {
		[node.return_type]
	}

	body := g.expr_stmts(node.stmts, ast.void_type)
	mut wasm_expr := wa.block(g.mod, c'__body', &body, 1, type_auto)
	
	if g.bp_idx != -1 {
		wasm_expr = g.setup_stack_frame(wasm_expr)
	}

	mut temporaries := []wa.Type{cap: g.local_temporaries.len - paraml.len}
	for idx := paraml.len; idx < g.local_temporaries.len; idx++ {
		temporaries << g.local_temporaries[idx].typ
	}
	// func :=
	wa.addfunction(g.mod, name.str, params_type, return_type, temporaries.data, temporaries.len,
		wasm_expr)
	if (node.is_pub && node.mod == 'main') || node.name == 'main.main' {
		wa.addfunctionexport(g.mod, name.str, name.str)

		// `_vinit` should be used to initialise the WASM module,
		// then `main.main` can be called safely.
		//
		// wa.setstart(g.mod, func)
	}

	g.local_temporaries.clear()
	g.local_addresses.clear()
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
			mut expr := g.expr(node.expr, type_i32)

			offset := g.get_field_offset(node.expr_type, node.field_name)
			expr = wa.binary(g.mod, wa.addint32(), expr, wa.constant(g.mod, wa.literalint32(offset)))

			g.cast_t(g.deref(expr, node.typ), node.typ, expected)
		}
		ast.StructInit {
			pos := g.allocate_struct('_anonstruct', node.typ)
			expr := g.init_struct(Stack{ address: pos, ast_typ: node.typ }, node)
			return g.mknblock('EXPR(STRUCTINIT)', [expr, g.lea_address(pos)])
		}
		ast.MatchExpr {
			g.w_error('wasm backend does not support match expressions yet')
		}
		ast.EnumVal {
			g.w_error('wasm backend does not support enums yet, however it would be dead simple to implement them')
		}
		ast.BoolLiteral {
			val := if node.val { wa.literalint32(1) } else { wa.literalint32(0) }
			wa.constant(g.mod, val)
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
			/*
			if node.name in ['println'] {
				// TODO: will only print `int` values using `console.log`

				expr := node.args[0].expr
				typ := node.args[0].typ
				
				if !typ.is_pure_int() {
					g.w_error("the `println` builtin function can only print integers")
				}
				
				nexpr := g.cast_t(g.expr(expr, typ), typ, ast.int_type)
			
				wa.call(g.mod, c"__vlog", &nexpr, 1, type_none)
			} else {*/
			mut arguments := []wa.Expression{cap: node.args.len}
			for idx, arg in node.args {
				arguments << g.expr(arg.expr, node.expected_arg_types[idx])
			}

			call := wa.call(g.mod, node.name.str, arguments.data, arguments.len, g.get_wasm_type(node.return_type))
			if node.is_noreturn {
				g.mkblock([call, wa.unreachable(g.mod)])
			} else {
				call
			}
			//}
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
		rtyp := node.right_types[i]

		if left is ast.Ident {
			// `_ = expr`
			if left.kind == .blank_ident {
				continue
			}
			if node.op == .decl_assign {
				g.new_local_temporary(left.name, typ)
			}
		}

		wartyp := g.get_wasm_type(rtyp)
		mut popexpr := wa.tupleextract(g.mod, wa.localget(g.mod, temporary, wret), i)
		popexpr = g.cast(popexpr, wartyp, g.is_signed(rtyp), g.get_wasm_type(typ))

		// TODO: only supports local identifiers, no path.expressions or global names
		idx, _ := g.get_local_temporary_from_ident(left as ast.Ident)
		exprs << wa.localset(g.mod, idx, popexpr)
	}

	return g.mkblock(exprs)
}

fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) wa.Expression {
	return match node {
		ast.Return {
			expr := if node.exprs.len == 1 {
				g.expr(node.exprs[0], g.curr_ret[0])
			} else if node.exprs.len == 0 {
				unsafe { nil }
			} else {
				mut exprs := []wa.Expression{cap: node.exprs.len}
				for idx in 0 .. node.exprs.len {
					exprs << g.expr(node.exprs[idx], g.curr_ret[idx])
				}
				wa.tuplemake(g.mod, exprs.data, exprs.len)
			}
			wa.br(g.mod, c'__body', unsafe { nil }, expr)
		}
		ast.ExprStmt {
			g.expr(node.expr, expected)
		}
		ast.ForStmt {
			if node.label != '' {
				g.w_error('wasm.expr(): `label: for` is unimplemented')
			}

			g.lbl++
			lpp_name := 'L${g.lbl}'
			if !node.is_inf {
				blk_name := 'B${g.lbl}'
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
				wa.loop(g.mod, lpp_name.str, wa.br(g.mod, lpp_name.str, unsafe { nil },
					unsafe { nil }))
			}
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
		ast.Module {}
		ast.Import {}
		ast.StructDecl {}
		ast.EnumDecl {}
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

fn (mut g Gen) vsp_leave() wa.Expression {
	frame := wa.constant(g.mod, wa.literalint32(g.stack_frame))

	return wa.call(g.mod, c'__vsp_leave', &frame, 1, type_none)
}

fn (mut g Gen) housekeeping() {
	wa.addglobalimport(g.mod, c'__vsp', c'env', c'__vsp', type_i32, true)
	wa.addmemoryimport(g.mod, c'__vmem', c'env', c'__vmem', 0)

	// vfmt off
	wa.addfunction(g.mod, c'__vsp_leave', type_i32, type_none, unsafe { nil }, 0, 
		wa.globalset(g.mod, c'__vsp', 
			wa.binary(g.mod, wa.addint32(),
				wa.localget(g.mod, 0, type_i32), 
				wa.globalget(g.mod, c'__vsp', type_i32))))
	// vfmt on
}

pub fn gen(files []&ast.File, table &ast.Table, out_name string, w_pref &pref.Preferences) {
	mut g := &Gen{
		table: table
		out_name: out_name
		pref: w_pref
		files: files
		mod: wa.modulecreate()
	}
	wa.modulesetfeatures(g.mod, wa.featureall())

	g.housekeeping()

	for file in g.files {
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	if wa.modulevalidate(g.mod) {
		if w_pref.is_prod {
			wa.moduleoptimize(g.mod)
		}
		if w_pref.out_name_c.ends_with('/-') || w_pref.out_name_c.ends_with(r'\-') {
			wa.moduleprintstackir(g.mod, w_pref.is_prod)
		} else {
			bytes := wa.moduleallocateandwrite(g.mod, unsafe { nil })
			str := unsafe { (&char(bytes.binary)).vstring_with_len(int(bytes.binaryBytes)) }
			os.write_file(w_pref.out_name, str) or { panic(err) }
		}
	} else {
		wa.moduleprint(g.mod)
		wa.moduledispose(g.mod)
		g.w_error('validation failed, this should not happen. report an issue with the above messages')
	}
	wa.moduledispose(g.mod)
}

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
	mod      wa.Module
	warnings []errors.Warning
	errors   []errors.Error
	table    &ast.Table = unsafe { nil }
	//
	// wasmtypes map[ast.Type]wa.Type
	curr_ret    ast.Type
	local_stack []FuncIdent
	lbl         int
}

struct FuncIdent {
	name string
	typ  wa.Type
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
	util.verror('wasm error', s)
}

fn (mut g Gen) get_local(name string) int {
	if g.local_stack.len == 0 {
		g.w_error('get_local: g.local_stack.len == 0')
	}
	mut c := g.local_stack.len
	for {
		c--
		if g.local_stack[c].name == name {
			return c
		}
		if c == 0 {
			break
		}
	}
	g.w_error("get_local: cannot get '${name}'")
}

fn (mut g Gen) get_local_from_ident(ident ast.Ident) (int, wa.Type) {
	mut obj := ident.obj
	if obj !in [ast.Var, ast.ConstField, ast.GlobalField, ast.AsmRegister] {
		obj = ident.scope.find(ident.name) or { g.w_error('unknown variable ${ident.name}') }
	}
	match mut obj {
		ast.Var {
			idx := g.get_local(obj.name)
			return idx, g.local_stack[idx].typ
		}
		else {
			g.w_error('unsupported variable type type:${obj} name:${ident.name}')
		}
	}
}

fn (mut g Gen) new_local(name string, typ ast.Type) {
	g.local_stack << FuncIdent{
		name: name
		typ: g.get_wasm_type(typ)
	}
}

const (
	type_none = wa.typenone()
	type_auto = wa.typeauto()
	type_i32  = wa.typeint32()
	type_i64  = wa.typeint64()
	type_f32  = wa.typefloat32()
	type_f64  = wa.typefloat64()
)

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

	return_type := if ts.kind == .struct_ {
		g.w_error('structs are not implemented')
	} else if ts.kind == .multi_return {
		g.w_error('multi returns are not implemented')
	} else {
		g.get_wasm_type(node.return_type)
	}

	mut paraml := []wa.Type{cap: node.params.len + 1}
	defer {
		unsafe { paraml.free() }
	}
	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		g.local_stack << FuncIdent{
			name: p.name
			typ: typ
		}
		paraml << typ
	}
	params_type := wa.typecreate(paraml.data, paraml.len)

	g.curr_ret = node.return_type
	wasm_expr := g.expr_stmts(node.stmts, node.return_type)

	mut temporaries := []wa.Type{cap: g.local_stack.len - paraml.len}
	for idx := paraml.len; idx < g.local_stack.len; idx++ {
		temporaries << g.local_stack[idx].typ
	}
	wa.addfunction(g.mod, name.str, params_type, return_type, temporaries.data, temporaries.len,
		wasm_expr)
	if node.is_pub && node.mod == 'main' {
		wa.addfunctionexport(g.mod, name.str, name.str)
	}

	g.local_stack.clear()
}

// println("${g.table.sym(expected)} ${node.val}")

fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) wa.Expression {
	if expr is ast.IntegerLiteral {
		return g.literal(expr.val, expected_type)
	} else if expr is ast.FloatLiteral {
		return g.literal(expr.val, expected_type)
	}

	got_type := ast.mktyp(got_type_raw)
	return g.cast(g.expr(expr, got_type), g.get_wasm_type(got_type), g.is_signed(got_type),
		g.get_wasm_type(expected_type))
}

fn (mut g Gen) literal(val string, expected ast.Type) wa.Expression {
	match g.get_wasm_type(expected) {
		wasm.type_i32 { return wa.constant(g.mod, wa.literalint32(val.int())) }
		wasm.type_i64 { return wa.constant(g.mod, wa.literalint64(val.i64())) }
		wasm.type_f32 { return wa.constant(g.mod, wa.literalfloat32(val.f32())) }
		wasm.type_f64 { return wa.constant(g.mod, wa.literalfloat64(val.f64())) }
		else {}
	}
	g.w_error('literal: bad type `${expected}`')
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
	return g.cast(infix, g.get_wasm_type(res_typ), g.is_signed(res_typ), g.get_wasm_type(expected))
}

fn (mut g Gen) mkblock(nodes []wa.Expression) wa.Expression {
	g.lbl++
	return wa.block(g.mod, 'BLK${g.lbl}'.str, nodes.data, nodes.len, wasm.type_auto)
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

fn (mut g Gen) if_stmt(ifexpr ast.IfExpr) wa.Expression {
	assert !ifexpr.is_expr, '`is_expr` not implemented'

	return g.if_branch(ifexpr, 0)
}

/*
fn (mut g Gen) if_stmt(ifexpr ast.IfExpr) wa.Expression {
	assert !ifexpr.is_expr, '`is_expr` not implemented'

	mut rl := wa.reloopercreate(g.mod)

	mut nodes := []wa.RelooperBlock{cap: ifexpr.branches.len}
	for branch in ifexpr.branches {
		nodes << wa.relooperaddblock(rl, g.expr_stmts(branch.stmts, ifexpr.typ))
	}

	start := wa.relooperaddblock(rl, wa.nop(g.mod))
	end := wa.relooperaddblock(rl, wa.nop(g.mod))

	mut curr := start
	for idx, node in nodes {
		cond := ifexpr.branches[idx].cond

		interp := if idx + 1 < nodes.len {
			wa.relooperaddblock(rl, wa.nop(g.mod))
		} else {
			end
		}

		if ifexpr.has_else && idx + 1 >= nodes.len {
			wa.relooperaddbranch(curr, node, unsafe { nil }, unsafe { nil })
			wa.relooperaddbranch(node, end, unsafe { nil }, unsafe { nil })
			break
		} else {
			wa.relooperaddbranch(curr, node, g.expr(cond, ast.bool_type), unsafe { nil })
		}
		wa.relooperaddbranch(curr, interp, unsafe { nil }, unsafe { nil })
		wa.relooperaddbranch(node, end, unsafe { nil }, unsafe { nil })
		curr = interp
	}

	stmt := wa.relooperrenderanddispose(rl, start, 0)
	return stmt
}*/

fn (mut g Gen) expr(node ast.Expr, expected ast.Type) wa.Expression {
	return match node {
		ast.ParExpr {
			g.expr(node.expr, expected)
		}
		ast.BoolLiteral {
			val := if node.val { wa.literalint32(1) } else { wa.literalint32(0) }
			wa.constant(g.mod, val)
		}
		ast.InfixExpr {
			g.infix_expr(node, expected)
		}
		ast.Ident {
			// TODO: only supports local identifiers, no path.expressions or global names
			idx, typ := g.get_local_from_ident(node)
			wa.localget(g.mod, idx, typ)
		}
		ast.IntegerLiteral, ast.FloatLiteral {
			g.literal(node.val, expected)
		}
		ast.IfExpr {
			if node.branches.len == 2 && node.is_expr {
				wa.bselect(g.mod, g.expr(node.branches[0].cond, ast.bool_type_idx), g.expr_stmts(node.branches[0].stmts,
					node.typ), g.expr_stmts(node.branches[1].stmts, node.typ), g.get_wasm_type(node.typ))
			} else {
				assert !node.is_expr
				g.if_stmt(node)
			}
			// wa.bif(g.mod, g.expr())
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
					wasm.type_i32)
				wa.bselect(g.mod, bexpr, wa.constant(g.mod, wa.literalint32(1)), wa.constant(g.mod,
					wa.literalint32(0)), wasm.type_i32)
			} else {
				g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type),
					g.get_wasm_type(node.typ))
			}
		}
		ast.CallExpr {
			if node.language != .v {
				g.w_error('functions with bodies outside of V are not implemented')
			}
			
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
		}
		else {
			eprintln('wasm.expr(): unhandled node: ' + node.type_name())
			wa.nop(g.mod)
		}
	}
}

fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) wa.Expression {
	return match node {
		ast.Return {
			if node.exprs.len == 1 {
				expr := g.expr_with_cast(node.exprs[0], node.types[0], g.curr_ret)
				wa.ret(g.mod, expr)
			} else {
				g.w_error('multi returns are not implemented')
			}
		}
		ast.ExprStmt {
			if node.typ == ast.void_type {
				g.expr(node.expr, ast.void_type)
			} else {
				g.expr_with_cast(node.expr, node.typ, expected)
			}
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
				
				wa.block(g.mod, blk_name.str, &loop, 1, wasm.type_none)
			} else {
				wa.loop(g.mod, lpp_name.str, wa.br(g.mod, lpp_name.str, unsafe { nil }, unsafe { nil }))
			}
		}
		ast.AssignStmt {
			if (node.left.len > 1 && node.right.len == 1) || node.has_cross_var {
				// Gen[Native].assign_stmt()
				g.w_error('complex assign statements are not implemented')
			}

			mut exprs := []wa.Expression{cap: node.left.len}
			for i, left in node.left {
				right := node.right[i]
				typ := node.left_types[i]

				if left is ast.Ident && node.op == .decl_assign {
					g.new_local(left.name, typ)
				}

				// TODO: only supports local identifiers, no path.expressions or global names
				idx, var_typ := g.get_local_from_ident(left as ast.Ident)
				expr := if node.op !in [.decl_assign, .assign] {
					op := g.infix_from_typ(typ, token.assign_op_to_infix_op(node.op))
					infix := wa.binary(g.mod, op, wa.localget(g.mod, idx, var_typ), g.expr(right,
						typ))

					infix
				} else {
					g.expr(right, typ)
				}
				exprs << wa.localset(g.mod, idx, expr)
			}

			if exprs.len == 1 {
				exprs[0]
			} else {
				g.mkblock(exprs)
			}
		}
		else {
			eprintln('wasm.expr_stmt(): unhandled node: ' + node.type_name())
			wa.nop(g.mod)
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
	for stmt in stmts {
		exprl << g.expr_stmt(stmt, expected)
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
		else {
			eprintln('wasm.toplevel_stmt(): unhandled node: ' + node.type_name())
		}
	}
}

pub fn (mut g Gen) toplevel_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.toplevel_stmt(stmt)
	}
}

pub fn gen(files []&ast.File, table &ast.Table, out_name string, pref &pref.Preferences) {
	// println(pref.should_output_to_stdout())
	mut g := &Gen{
		table: table
		out_name: out_name
		pref: pref
		files: files
		mod: wa.modulecreate()
	}
	wa.modulesetfeatures(g.mod, wa.featuremultivalue())

	for file in g.files {
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	if wa.modulevalidate(g.mod) {
		if pref.is_prod {
			wa.moduleoptimize(g.mod)
		}
		if pref.out_name_c.ends_with('/-') || pref.out_name_c.ends_with(r'\-') {
			wa.moduleprintstackir(g.mod, pref.is_prod)
		} else {
			bytes := wa.moduleallocateandwrite(g.mod, unsafe { nil })
			str := unsafe { (&char(bytes.binary)).vstring_with_len(int(bytes.binaryBytes)) }
			os.write_file(pref.out_name, str) or { panic(err) }
		}
	} else {
		wa.moduleprint(g.mod)
		wa.moduledispose(g.mod)
		g.w_error('validation failed, this should not happen. report an issue with the above messages')
	}
	wa.moduledispose(g.mod)
}

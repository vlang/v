module wasm

import v.ast
import v.pref
import v.util
import v.token
import v.errors
import binaryen as wasm

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	mod      wasm.Module
	warnings []errors.Warning
	errors   []errors.Error
	table    &ast.Table = unsafe { nil }
	//
	// wasmtypes map[ast.Type]wasm.Type
	local_stack []FuncIdent
}

struct FuncIdent {
	name string
	typ  wasm.Type
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

fn (mut g Gen) get_wasm_type(typ ast.Type) wasm.Type {
	if typ == ast.void_type_idx {
		return type_none
	}
	if typ.is_real_pointer() {
		return type_i32
	}
	if typ in ast.number_type_idxs {
		return match typ {
			ast.isize_type_idx, ast.usize_type_idx, ast.i8_type_idx, ast.u8_type_idx, ast.char_type_idx, ast.rune_type_idx, ast.i16_type_idx, ast.u16_type_idx, ast.int_type_idx, ast.u32_type_idx { type_i32 }
			ast.i64_type_idx, ast.u64_type_idx, ast.int_literal_type_idx { type_i64 }
			ast.f32_type_idx { type_f32 }
			ast.f64_type_idx, ast.float_literal_type_idx { type_f64 }
			else { type_i32 }
		}
	}
	if typ == ast.bool_type_idx {
		return type_i32
	}
	g.w_error("get_wasm_type: unreachable type '${typ}'")
}

fn (mut g Gen) get_local(name string) int {
	if g.local_stack.len == 0 {
		g.w_error("get_local: g.local_stack.len == 0")
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

fn (mut g Gen) get_local_from_ident(ident ast.Ident) (int, wasm.Type) {
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
	g.local_stack << FuncIdent{name:name, typ: g.get_wasm_type(typ)}
}

const (
	type_none = wasm.typenone()
	type_auto = wasm.typeauto()
	type_i32 = wasm.typeint32()
	type_i64 = wasm.typeint64()
	type_f32 = wasm.typefloat32()
	type_f64 = wasm.typefloat64()
)

const (
	cast_signed = {
		type_i32: {
			type_i64: wasm.extendsint32()
			type_f32: wasm.convertsint32tofloat32()
			type_f64: wasm.convertsint32tofloat64()
		}
		type_i64: {
			type_i32: wasm.wrapint64()
			type_f32: wasm.convertsint64tofloat32()
			type_f64: wasm.convertsint64tofloat64()
		}
		type_f32: {
			type_i32: wasm.truncsatsfloat32toint32()
			type_i64: wasm.truncsatsfloat32toint64()
			type_f64: wasm.promotefloat32()
		}
		type_f64: {
			type_i32: wasm.truncsatsfloat64toint32()
			type_i64: wasm.truncsatsfloat64toint64()
			type_f32: wasm.demotefloat64()
		}
	}
	cast_unsigned = {
		type_i32: {
			type_i64: wasm.extenduint32()
			type_f32: wasm.convertuint32tofloat32()
			type_f64: wasm.convertuint32tofloat64()
		}
		type_i64: {
			type_i32: wasm.wrapint64()
			type_f32: wasm.convertuint64tofloat32()
			type_f64: wasm.convertuint64tofloat64()
		}
		type_f32: {
			type_i32: wasm.truncsatufloat32toint32()
			type_i64: wasm.truncsatufloat32toint64()
		}
		type_f64: {
			type_i32: wasm.truncsatufloat64toint32()
			type_i64: wasm.truncsatufloat64toint64()
		}
	}
)

fn (mut g Gen) is_signed(typ ast.Type) bool {
	if typ.is_pure_float() {
		return true
	}
	return typ.is_signed()
}

fn (mut g Gen) cast(expr wasm.Expression, from wasm.Type, is_signed bool, to wasm.Type) wasm.Expression {
	if from == to {
		return expr
	}
	
	// In the official spec, integers are represented in twos complement.
	// WebAssembly does not keep signedness information in it's types
	// and uses instructions with variants for signed or unsigned values.
	// 
	// You only need to know if the original type is signed or not to
	// perform casting.

	// A large match statement performs better than hardcoded map values.
	// However, it is not as readable. Spy says they stay.
	
	val := if is_signed {
		cast_signed[from][to]
	} else {
		cast_unsigned[from][to]
	}
	if isnil(val) {
		g.w_error("bad cast: from ${from} (is signed: ${is_signed}) to ${to}")
	}
	return wasm.unary(g.mod, val, expr)
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	name := if node.is_method {
		'${g.table.get_type_name(node.receiver.typ)}.${node.name}'
	} else {
		node.name
	}

	util.timing_start("${@METHOD}: ${name}")
	defer { util.timing_measure("${@METHOD}: ${name}") }

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

	mut paraml := []wasm.Type{cap: node.params.len + 1}
	defer {
		unsafe { paraml.free() }
	}
	for p in node.params {
		typ := g.get_wasm_type(p.typ)
		g.local_stack << FuncIdent {name: p.name typ: typ}
		paraml << typ
	}
	params_type := wasm.typecreate(paraml.data, paraml.len)

	wasm_expr := g.expr_stmts(node.stmts, node.return_type)
	
	mut temporaries := []wasm.Type{cap: g.local_stack.len - paraml.len}
	for idx := paraml.len ; idx < g.local_stack.len ; idx++ {
		temporaries << g.local_stack[idx].typ
	}
	wasm.addfunction(g.mod, name.str, params_type, return_type, temporaries.data, temporaries.len, wasm_expr)
	if node.is_pub {
		wasm.addfunctionexport(g.mod, name.str, name.str)
	}

	g.local_stack.clear()
}

fn (mut g Gen) expr_with_cast(expr ast.Expr, got_type_raw ast.Type, expected_type ast.Type) wasm.Expression {
	got_type := ast.mktyp(got_type_raw)
	return g.cast(g.expr(expr, got_type), g.get_wasm_type(got_type), g.is_signed(got_type), g.get_wasm_type(expected_type))
}

fn (mut g Gen) infix_from_typ(typ ast.Type, op token.Kind) wasm.Op {
	wasm_typ := g.get_wasm_type(typ)

	match wasm_typ {
		type_i32 {
			match op {
				.plus {
					return wasm.addint32()
				}
				.minus {
					return wasm.subint32()
				}
				.mul {
					return wasm.mulint32()
				}
				.mod {
					if typ.is_signed() {
						return wasm.remsint32()
					} else {
						return wasm.remuint32()
					}
				}
				.div {
					if typ.is_signed() {
						return wasm.divsint32()
					} else {
						return wasm.divuint32()
					}
				}
				else {}
			}
		}
		type_i64 {
			match op {
				.plus {
					return wasm.addint64()
				}
				.minus {
					return wasm.subint64()
				}
				.mul {
					return wasm.mulint64()
				}
				.mod {
					if typ.is_signed() {
						return wasm.remsint64()
					} else {
						return wasm.remuint64()
					}
				}
				.div {
					if typ.is_signed() {
						return wasm.divsint64()
					} else {
						return wasm.divuint64()
					}
				}
				else {}
			}
		}
		type_f32 {
			match op {
				.plus  { return wasm.addfloat32() }
				.minus { return wasm.subfloat32() }
				.mul   { return wasm.mulfloat32() }
				.div   { return wasm.divfloat32() }
				else {}
			}
		}
		type_f64 {
			match op {
				.plus  { return wasm.addfloat64() }
				.minus { return wasm.subfloat64() }
				.mul   { return wasm.mulfloat64() }
				.div   { return wasm.divfloat64() }
				else {}
			}
		}
		else {}
	}
	g.w_error("bad infix: op `${op}`")
}

fn (mut g Gen) literal(val string, expected ast.Type) wasm.Expression {
	match g.get_wasm_type(expected) {
		type_i32 { return wasm.constant(g.mod, wasm.literalint32(val.int()))   }
		type_i64 { return wasm.constant(g.mod, wasm.literalint64(val.i64()))   }
		type_f32 { return wasm.constant(g.mod, wasm.literalfloat32(val.f32())) }
		type_f64 { return wasm.constant(g.mod, wasm.literalfloat64(val.f64())) }
		else {}
	}
	g.w_error("literal: bad type `${expected}`")
}

fn (mut g Gen) infix_expr(node ast.InfixExpr, expected ast.Type) wasm.Expression {
	op := g.infix_from_typ(node.left_type, node.op)

	infix := wasm.binary(g.mod, op, g.expr(node.left, node.left_type), g.expr_with_cast(node.right, node.right_type, node.left_type))
	return g.cast(infix, g.get_wasm_type(node.left_type), g.is_signed(node.left_type), g.get_wasm_type(expected))
}

fn (mut g Gen) mkblock(nodes []wasm.Expression) wasm.Expression {
	return wasm.block(g.mod, c'blk', nodes.data, nodes.len, type_auto)
}

fn (mut g Gen) lint(v int) wasm.Expression {
	return wasm.constant(g.mod, wasm.literalint32(v))
}

fn (mut g Gen) call(v int) wasm.Expression {
	ops := [g.lint(v)]
	return wasm.call(g.mod, c"check", ops.data, ops.len, wasm.typenone())
}


fn (mut g Gen) if_branches(ifexpr ast.IfExpr) wasm.Expression {
	assert !ifexpr.is_expr, '`is_expr` not implemented'

	mut rl := wasm.reloopercreate(g.mod)

	mut nodes := []wasm.RelooperBlock{cap: ifexpr.branches.len}
	for branch in ifexpr.branches {
		nodes << wasm.relooperaddblock(rl, g.expr_stmts(branch.stmts, ifexpr.typ))
	}

	start := wasm.relooperaddblock(rl, wasm.nop(g.mod))
	end := wasm.relooperaddblock(rl, wasm.nop(g.mod))
	
	mut curr := start
	for idx, node in nodes {
		cond := ifexpr.branches[idx].cond

		interp := if idx + 1 < nodes.len {
			wasm.relooperaddblock(rl, wasm.nop(g.mod))
		} else {
			end
		}

		if idx + 1 >= nodes.len {
			wasm.relooperaddbranch(curr, node, unsafe { nil }, unsafe { nil })
			wasm.relooperaddbranch(node, end, unsafe { nil }, unsafe { nil })
			break
		} else {
			wasm.relooperaddbranch(curr, node, g.expr(cond, ast.bool_type), unsafe { nil })
		}
		wasm.relooperaddbranch(curr, interp, unsafe { nil }, unsafe { nil })
		wasm.relooperaddbranch(node, end, unsafe { nil }, unsafe { nil })
		curr = interp
	}

	return wasm.relooperrenderanddispose(rl, start, 0)
}

fn (mut g Gen) expr(node ast.Expr, expected ast.Type) wasm.Expression {
	return match node {
		ast.ParExpr {
			g.expr(node.expr, expected)
		}
		ast.BoolLiteral {
			val := if node.val { wasm.literalint32(1) } else { wasm.literalint32(0) }
			wasm.constant(g.mod, val)
		}  
		ast.InfixExpr {
			g.infix_expr(node, expected)
		}
		ast.Ident {
			// TODO: only supports local identifiers, no path.expressions or global names
			idx, typ := g.get_local_from_ident(node)
			wasm.localget(g.mod, idx, typ)
		}
		ast.IntegerLiteral, ast.FloatLiteral {
			g.literal(node.val, expected)
		}
		ast.IfExpr {
			if node.branches.len == 2 && node.is_expr {
				wasm.bselect(g.mod, g.expr(node.branches[0].cond, ast.bool_type_idx), g.expr_stmts(node.branches[0].stmts, node.typ), g.expr_stmts(node.branches[1].stmts, node.typ), g.get_wasm_type(node.typ))
			} else {
				assert !node.is_expr
				g.if_branches(node)
			}
			// wasm.bif(g.mod, g.expr())
			
		}
		ast.CastExpr {
			expr := g.expr(node.expr, node.expr_type)
			
			if node.typ == ast.bool_type {
				// WebAssembly booleans use the `i32` type
				//   = 0 | is false
				//   > 0 | is true
				// 
				// It's a checker error to cast to bool anyway...
				
				bexpr := g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type), type_i32)
				wasm.bselect(g.mod, bexpr, wasm.constant(g.mod, wasm.literalint32(1)), wasm.constant(g.mod, wasm.literalint32(0)), type_i32)
			} else {
				g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type), g.get_wasm_type(node.typ))
			}
		}
		ast.EmptyExpr {
			g.w_error('wasm.expr(): called with ast.EmptyExpr')
		}
		else {
			eprintln('wasm.expr(): unhandled node: ' + node.type_name())
			wasm.nop(g.mod)
		}
	}
}

fn (mut g Gen) expr_stmt(node ast.Stmt, expected ast.Type) wasm.Expression {
	return match node {
		ast.Return {
			if node.exprs.len == 1 {
				g.expr_with_cast(node.exprs[0], node.types[0], expected)
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
		ast.AssignStmt {
			if (node.left.len > 1 && node.right.len == 1) || node.has_cross_var {
				// Gen[Native].assign_stmt()
				g.w_error('complex assign statements are not implemented')
			}

			mut exprs := []wasm.Expression{cap: node.left.len}
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
					infix := wasm.binary(g.mod, op, wasm.localget(g.mod, idx, var_typ), g.expr(right, typ))

					infix
				} else {
					g.expr(right, typ)
				}
				exprs << wasm.localset(g.mod, idx, expr)
			}

			if exprs.len == 1 {
				exprs[0]
			} else {
				g.mkblock(exprs)
			}
		}
		else {
			eprintln('wasm.expr_stmt(): unhandled node: ' + node.type_name())
			wasm.nop(g.mod)
		}
	}
}

pub fn (mut g Gen) expr_stmts(stmts []ast.Stmt, expected ast.Type) wasm.Expression {
	if stmts.len == 0 {
		return wasm.nop(g.mod)
	}
	if stmts.len == 1 {
		return g.expr_stmt(stmts[0], expected)
	}
	mut exprl := []wasm.Expression{cap: stmts.len}
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

pub fn gen(files []&ast.File, table &ast.Table, out_name string, pref &pref.Preferences) (int, int) {
	mut g := &Gen{
		table: table
		out_name: out_name
		pref: pref
		files: files
		mod: wasm.modulecreate()
	}
	wasm.modulesetfeatures(g.mod, wasm.featuremultivalue())

	for file in g.files {
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.toplevel_stmts(file.stmts)
	}
	if wasm.modulevalidate(g.mod) {
		wasm.moduleoptimize(g.mod)
		wasm.moduleprintstackir(g.mod, true)
		// wasm.moduleprint(g.mod)
	} else {
		wasm.moduleprint(g.mod)
	}
	wasm.moduledispose(g.mod)

	return 0, 0
}

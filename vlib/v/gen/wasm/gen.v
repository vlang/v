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

const (
	type_none = wasm.typenone()
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
	wasm.addfunction(g.mod, name.str, params_type, return_type, unsafe { nil }, 0, wasm_expr)

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
			op := g.infix_from_typ(node.left_type, node.op)

			infix := wasm.binary(g.mod, op, g.expr(node.left, node.left_type), g.expr_with_cast(node.right, node.right_type, node.left_type))
			g.cast(infix, g.get_wasm_type(node.left_type), g.is_signed(node.left_type), g.get_wasm_type(expected))
		}
		ast.Ident {
			idx, typ := g.get_local_from_ident(node)
			wasm.localget(g.mod, idx, typ)
		}
		ast.IntegerLiteral, ast.FloatLiteral {
			g.literal(node.val, expected)
		}
		ast.IfExpr {
			if node.branches.len == 2 && node.has_else {
				wasm.bselect(g.mod, g.expr(node.branches[0].cond, ast.bool_type_idx), g.expr_stmts(node.branches[0].stmts, node.typ), g.expr_stmts(node.branches[1].stmts, node.typ), g.get_wasm_type(node.typ))
			} else {
				g.w_error('complex if expressions are not implemented')
			}
			// wasm.bif(g.mod, g.expr())
		}
		ast.CastExpr {
			expr := g.expr(node.expr, node.typ)
			
			if node.expr_type == ast.bool_type {
				// WebAssembly booleans use the `i32` type
				//   = 0 | is false
				//   > 0 | is true
				
				g.cast(expr, g.get_wasm_type(node.expr_type), g.is_signed(node.expr_type), type_i32)
			} else {
				g.expr(node.expr, node.typ)
			}
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
			g.expr_with_cast(node.expr, node.typ, expected)
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
	return wasm.block(g.mod, c'blk', exprl.data, exprl.len, wasm.typeauto())
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
		wasm.moduleprintstackir(g.mod, true)
	} else {
		wasm.moduleprint(g.mod)
	}
	wasm.moduledispose(g.mod)

	return 0, 0
}

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
	wasmtypes map[ast.Type]wasm.Type
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
		return wasm.typenone()
	}
	if typ.is_real_pointer() {
		return wasm.typeint32()
	}
	if typ in ast.number_type_idxs {
		return match typ {
			ast.isize_type_idx, ast.usize_type_idx, ast.i8_type_idx, ast.u8_type_idx, ast.char_type_idx, ast.rune_type_idx, ast.i16_type_idx, ast.u16_type_idx, ast.int_type_idx, ast.u32_type_idx { wasm.typeint32() }
			ast.i64_type_idx, ast.u64_type_idx, ast.int_literal_type_idx { wasm.typeint64() }
			ast.f32_type_idx { wasm.typefloat32() }
			ast.f64_type_idx, ast.float_literal_type_idx { wasm.typefloat64() }
			else { wasm.typeint64() }
		}
	}
	if typ == ast.bool_type_idx {
		return wasm.typeint32()
	}
	panic('unreachable type ${typ}')
	// g.w_error('non concrete types or multi returns are not implemented')
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	name := if node.is_method {
		'${g.table.get_type_name(node.receiver.typ)}.${node.name}'
	} else {
		node.name
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

	println(node.name)
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
		paraml << g.get_wasm_type(p.typ)
	}
	params_type := wasm.typecreate(paraml.data, paraml.len)

	wasm_expr := g.expr_stmts(node.stmts)

	wasm.addfunction(g.mod, name.str, params_type, return_type, unsafe { nil }, 0, wasm_expr)
}

fn (mut g Gen) expr(node ast.Expr) wasm.Expression {
	return match node {
		ast.ParExpr {
			g.expr(node.expr)
		}
		ast.BoolLiteral {
			wasm.constant(g.mod, if node.val { wasm.literalint32(1) } else { wasm.literalint32(0) })
		}
		ast.InfixExpr {
			if node.left_type != node.right_type {
				panic("eeeeeeeeeeeeeeeeeee")
			}
			if !node.left_type.is_signed() || g.get_wasm_type(node.left_type) != wasm.typeint32() {
				panic("unimplemented")
			}

			// TODO: handle type casting if necessary
			// TODO: implement more ops
			// TODO: handle signed and unsigned integer types

			op := match node.op {
				.plus  { wasm.addint32()  }
				.minus { wasm.subint32()  }
				.mod   { wasm.remsint32() }
				.mul   { wasm.mulint32()  }
				.div   { wasm.divsint32() }
				else { panic("unimplemented infix ${node.op}") }
			}

			wasm.binary(g.mod, op, g.expr(node.left), g.expr(node.right))
		}
		ast.Ident {
			// TODO: handle ast.Ident
			println(node)
			wasm.nop(g.mod)
		}
		/* ast.FloatLiteral {
			// follow native impl
		} */
		else {
			eprintln('wasm.expr(): unhandled node: ' + node.type_name())
			wasm.nop(g.mod)
		}
	}
}

fn (mut g Gen) expr_stmt(node ast.Stmt) wasm.Expression {
	return match node {
		ast.Return {
			if node.exprs.len == 1 {
				g.expr(node.exprs[0])
			} else {
				g.w_error('multi returns are not implemented')
			}
		}
		else {
			eprintln('wasm.expr_stmt(): unhandled node: ' + node.type_name())
			wasm.nop(g.mod)
		}
	}
}

pub fn (mut g Gen) expr_stmts(stmts []ast.Stmt) wasm.Expression {
	if stmts.len == 0 {
		return wasm.nop(g.mod)
	}
	if stmts.len == 1 {
		return g.expr_stmt(stmts[0])
	}
	mut exprl := []wasm.Expression{cap: stmts.len}
	for stmt in stmts {
		exprl << g.expr_stmt(stmt)
	}
	return wasm.block(g.mod, c'blk', exprl.data, exprl.len, wasm.typeauto())
}

fn (mut g Gen) toplevel_stmt(node ast.Stmt) {
	match node {
		ast.FnDecl {
			g.fn_decl(node)
		}
		/* ast.Return {
			if node.exprs.len == 1 {
				// wasm.g.expr(node.exprs[0])
			} else {
				g.w_error('multi returns are not implemented')
			}
		} */
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
	wasm.moduleprint(g.mod)
	println(wasm.modulevalidate(g.mod))

	return 0, 0
}

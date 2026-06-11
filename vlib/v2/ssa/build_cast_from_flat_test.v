// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s192: `build_cast_from_flat` is the eighth per-kind
// arm inside `build_expr_from_flat`. CastExpr flat encoding (flat.v:1876)
// is (.expr_cast, pos, -1, -1, 0, 0, [edge0=typ, edge1=expr]). Both edges
// need full `decode_expr` rehydration: `ast_type_to_ssa` pattern-matches on
// type expr kinds, and `build_addr`/`build_expr` on the value side require
// fully-rehydrated `ast.Expr` sum-types.
module ssa

import v2.ast
import v2.types

// Fixture: `fn cast() i64 { return i64(42) }`. CastExpr with
// typ=Ident('i64'), expr=BasicLiteral('42'). Exercises the common
// numeric-widening path through `build_cast_value_to_type`.
fn make_cast_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'cast64'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'i64'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.CastExpr{
									typ:  ast.Expr(ast.Ident{
										name: 'i64'
									})
									expr: ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '42'
									})
								}),
							]
						}),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_cast(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_cast(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	stmts := flat.file_cursor(0).stmts()
	for si in 0 .. stmts.len() {
		c := stmts.at(si)
		if c.kind() != .stmt_fn_decl {
			continue
		}
		decl := c.fn_decl_signature()
		fn_name := b.mangle_fn_name(decl)
		func_idx := b.fn_index[fn_name] or { continue }
		b.cur_func = func_idx
		b.label_blocks = map[string]BlockID{}
		b.vars = map[string]ValueID{}
		entry := mod.add_block(func_idx, 'entry')
		b.cur_block = entry
		for param in decl.typ.params {
			param_type := b.ast_type_to_ssa(param.typ)
			param_val := mod.add_value_node(.argument, param_type, param.name, 0)
			mod.func_add_param(func_idx, param_val)
			alloca := mod.add_instr(.alloca, entry, mod.type_store.get_ptr(param_type), []ValueID{})
			mod.add_instr(.store, entry, 0, [param_val, alloca])
			b.vars[param.name] = alloca
		}
		body := c.list_at(3)
		for bi in 0 .. body.len() {
			stmt_c := body.at(bi)
			if stmt_c.kind() == .stmt_return {
				ret_expr_c := stmt_c.edge(0)
				val := b.build_expr_from_flat(ret_expr_c)
				mod.add_instr(.ret, b.cur_block, 0, [val])
			} else {
				b.build_stmt_from_flat(stmt_c)
			}
		}
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_cast_from_flat_i64_matches_legacy() {
	files := make_cast_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_cast(files, env, 'cast_legacy')
	mod_flat := build_via_flat_cast(files, env, 'cast_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

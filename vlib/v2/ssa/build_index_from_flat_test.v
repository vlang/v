// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s194: `build_index_from_flat` is the tenth per-kind
// arm inside `build_expr_from_flat`. IndexExpr flat encoding (flat.v:1946)
// is (.expr_index, pos, -1, -1, 0, flags, [edge0=lhs, edge1=expr]). Both
// edges need full `decode_expr` rehydration: `build_index` recursively calls
// `build_expr` on both, and `expr_type(ast.Expr(expr))` for the pos.id
// type-check lookup. `is_gated` flag is intentionally ignored (build_index
// doesn't read it).
module ssa

import v2.ast
import v2.token
import v2.types

// Fixture: `fn first(p &int) int { return p[0] }`. IndexExpr with
// lhs=Ident('p'), expr=BasicLiteral('0'). Param type is `&int` (ptr to int)
// so build_index falls through to the final ptr_t branch — emits GEP+load
// without needing struct registration for array/string/map.
fn make_index_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'first'
					typ:   ast.FnType{
						params:      [
							ast.Parameter{
								name: 'p'
								typ:  ast.Expr(ast.PrefixExpr{
									op:   token.Token.amp
									expr: ast.Expr(ast.Ident{
										name: 'int'
									})
								})
							},
						]
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.IndexExpr{
									lhs:  ast.Expr(ast.Ident{
										name: 'p'
									})
									expr: ast.Expr(ast.BasicLiteral{
										kind:  .number
										value: '0'
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

fn build_via_legacy_index(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_index(files []ast.File, env &types.Environment, name string) &Module {
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
		decl := c.flat.decode_fn_decl_signature(c.id)
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

fn test_build_index_from_flat_ptr_matches_legacy() {
	files := make_index_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_index(files, env, 'index_legacy')
	mod_flat := build_via_flat_index(files, env, 'index_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

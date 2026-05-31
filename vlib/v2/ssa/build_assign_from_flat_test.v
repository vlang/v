// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for `build_assign_from_flat`. AssignStmt flat encoding
// stores lhs/rhs expression edges split at `c.extra_int()` and the assignment
// operator in `c.aux()`. The cursor port must match legacy assign lowering
// without rehydrating lhs/rhs expression arrays.
module ssa

import v2.ast
import v2.token
import v2.types

fn assign_pin_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn assign_pin_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn assign_pin_stmt(lhs []ast.Expr, rhs []ast.Expr, op token.Token) ast.Stmt {
	return ast.Stmt(ast.AssignStmt{
		lhs: lhs
		rhs: rhs
		op:  op
	})
}

fn assign_pin_decl(name string, rhs ast.Expr) ast.Stmt {
	return assign_pin_stmt([assign_pin_ident(name)], [rhs], .decl_assign)
}

fn assign_pin_selector(lhs ast.Expr, rhs string) ast.Expr {
	return ast.Expr(ast.SelectorExpr{
		lhs: lhs
		rhs: ast.Ident{
			name: rhs
		}
	})
}

fn assign_pin_index(lhs ast.Expr, idx ast.Expr) ast.Expr {
	return ast.Expr(ast.IndexExpr{
		lhs:  lhs
		expr: idx
	})
}

fn assign_pin_prefix(op token.Token, expr ast.Expr) ast.Expr {
	return ast.Expr(ast.PrefixExpr{
		op:   op
		expr: expr
	})
}

fn assign_pin_fixed_array() ast.Expr {
	return ast.Expr(ast.ArrayInitExpr{
		exprs: [assign_pin_num('1'), assign_pin_num('2')]
		len:   ast.Expr(ast.PostfixExpr{
			op:   .not
			expr: ast.empty_expr
		})
	})
}

fn assign_pin_point_init() ast.Expr {
	return ast.Expr(ast.InitExpr{
		typ:    assign_pin_ident('Point')
		fields: [
			ast.FieldInit{
				name:  'x'
				value: assign_pin_num('1')
			},
		]
	})
}

// Fixture: simple declarations, rebinding, compound ops, multi-assign, fixed
// array index stores, struct field stores, and pointer dereference stores.
fn make_assign_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.StructDecl{
					name:   'Point'
					fields: [
						ast.FieldDecl{
							name: 'x'
							typ:  assign_pin_ident('int')
						},
					]
				}),
				ast.Stmt(ast.FnDecl{
					name:  'do_assign'
					typ:   ast.FnType{}
					stmts: [
						assign_pin_decl('x', assign_pin_num('1')),
						assign_pin_decl('y', assign_pin_ident('x')),
						assign_pin_stmt([assign_pin_ident('x')], [
							assign_pin_num('7')], .assign),
						assign_pin_stmt([assign_pin_ident('x')], [
							assign_pin_num('2')], .plus_assign),
						assign_pin_stmt([assign_pin_ident('x'),
							assign_pin_ident('y')], [assign_pin_ident('y'),
							assign_pin_ident('x')], .assign),
						assign_pin_decl('arr', assign_pin_fixed_array()),
						assign_pin_stmt([
							assign_pin_index(assign_pin_ident('arr'), assign_pin_num('0')),
						], [
							assign_pin_ident('x'),
						], .assign),
						assign_pin_decl('p', assign_pin_point_init()),
						assign_pin_stmt([
							assign_pin_selector(assign_pin_ident('p'), 'x'),
						], [
							assign_pin_index(assign_pin_ident('arr'), assign_pin_num('0')),
						], .assign),
						assign_pin_stmt([
							assign_pin_selector(assign_pin_ident('p'), 'x'),
						], [
							assign_pin_num('1'),
						], .plus_assign),
						assign_pin_decl('ptr', assign_pin_prefix(.amp, assign_pin_ident('x'))),
						assign_pin_stmt([
							assign_pin_prefix(.mul, assign_pin_ident('ptr')),
						], [
							assign_pin_selector(assign_pin_ident('p'), 'x'),
						], .assign),
					]
				}),
			]
		},
	]
}

fn build_via_legacy_assign(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_types_pass1(files[0])
	b.register_types_pass2(files[0])
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_assign(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	fc := flat.file_cursor(0)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_types_pass1_from_flat(fc)
	b.register_types_pass2_from_flat(fc)
	b.register_fn_signatures_from_flat(fc)
	stmts := fc.stmts()
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
		body := c.list_at(3)
		b.build_stmts_from_flat(body)
		if !b.block_has_terminator(b.cur_block) {
			mod.add_instr(.ret, b.cur_block, 0, []ValueID{})
		}
	}
	return mod
}

fn test_build_assign_from_flat_matches_legacy() {
	files := make_assign_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_assign(files, env, 'assign_legacy')
	mod_flat := build_via_flat_assign(files, env, 'assign_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 1
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}

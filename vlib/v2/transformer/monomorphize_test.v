// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: !windows
module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

fn mono_test_transformer() &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:                        &vpref.Preferences{}
		env:                         unsafe { env }
		needed_clone_fns:            map[string]string{}
		needed_array_contains_fns:   map[string]ArrayMethodInfo{}
		needed_array_index_fns:      map[string]ArrayMethodInfo{}
		needed_array_last_index_fns: map[string]ArrayMethodInfo{}
	}
}

fn test_worker_clone_sees_sumtype_decl_variant_cache() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'foo/box.v'
			code: '
module foo

pub struct Box[T] {
pub:
	value T
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import foo as f

type ImportedValue = f.Box[int] | f.Box[string]
'
		},
	])
	mut transformer := checked.trans
	transformer.pre_pass(checked.files)
	mut worker := transformer.new_worker_clone(1)
	expected := ['foo__Box_T_int', 'foo__Box_T_string']

	assert transformer.get_sum_type_variants('ImportedValue') == expected
	assert worker.get_sum_type_variants('ImportedValue') == expected
	worker.sum_type_decl_variant_names['WorkerOnly'] = ['Nope']
	assert 'WorkerOnly' !in transformer.sum_type_decl_variant_names

	flat := ast.flatten_files(checked.files)
	mut flat_transformer := Transformer.new_with_pref(checked.trans.env, checked.trans.pref)
	flat_transformer.pre_pass_from_flat(&flat)
	mut flat_worker := flat_transformer.new_worker_clone(2)

	assert flat_transformer.get_sum_type_variants('ImportedValue') == expected
	assert flat_worker.get_sum_type_variants('ImportedValue') == expected
	flat_worker.sum_type_decl_variant_names['WorkerOnly'] = ['Nope']
	assert 'WorkerOnly' !in flat_transformer.sum_type_decl_variant_names
}

fn test_get_expr_type_prefers_cloned_init_expr_type_over_stale_position_type() {
	mut env := types.Environment.new()
	pos := token.Pos{
		id: 7
	}
	env.set_expr_type(pos.id, types.Type(types.Struct{
		name: 'gitly__GitHubRepoInfo'
	}))
	mut t := mono_test_transformer()
	t.env = env
	t.cur_module = 'json2'
	mut config_scope := types.new_scope(unsafe { nil })
	config_scope.insert_type('Config', types.Type(types.Struct{
		name: 'config__Config'
	}))
	t.cached_scopes['config'] = config_scope
	expr := ast.Expr(ast.InitExpr{
		typ: ast.Expr(ast.SelectorExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'config'
			})
			rhs: ast.Ident{
				name: 'Config'
			}
		})
		pos: pos
	})
	typ := t.get_expr_type(expr) or { panic('expected cloned init expression type') }
	assert typ.name() == 'config__Config'
}

// substitute_type: primitives

fn test_substitute_type_returns_same_type_when_bindings_empty() {
	original := types.Type(types.NamedType('T'))
	out := substitute_type(original, map[string]types.Type{})
	assert out.name() == 'T'
}

fn test_substitute_type_replaces_placeholder_named_type() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('T')), bindings)
	assert out.name() == 'int'
}

fn test_generic_bindings_from_method_call_args_skip_receiver_param() {
	mut t := mono_test_transformer()
	t.local_decl_types['file'] = types.Type(types.Struct{
		name: 'File'
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.Struct{
				name: 'Walker'
			}),
			types.Type(types.NamedType('T')),
		]
		generic_param_names_by_param:   ['', 'T']
		generic_param_indexes_by_param: [-1, 0]
		generic_params:                 ['T']
	}
	bindings := t.generic_bindings_from_call_args(info, [
		ast.Expr(ast.Ident{
			name: 'file'
		}),
	]) or { panic('expected binding') }
	concrete := bindings['T'] or { panic('missing T binding') }
	assert concrete.name() == 'File'
}

fn test_generic_bindings_from_call_args_use_ast_direct_generic_metadata() {
	mut t := mono_test_transformer()
	t.local_decl_types['file'] = types.Type(types.Struct{
		name: 'File'
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.void_),
		]
		generic_param_names_by_param:   ['T']
		generic_param_indexes_by_param: [0]
		generic_params:                 ['T']
	}
	bindings := t.generic_bindings_from_call_args(info, [
		ast.Expr(ast.Ident{
			name: 'file'
		}),
	]) or { panic('expected binding from direct generic parameter metadata') }
	concrete := bindings['T'] or { panic('missing T binding') }
	assert concrete.name() == 'File'
}

fn test_generic_call_info_lookup_does_not_match_unrelated_short_name() {
	mut t := mono_test_transformer()
	t.generic_fn_decl_index['write'] = ast.FnDecl{
		name: 'write'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	if _ := t.generic_fn_decl_for_call_info('v__Gen__write') {
		assert false, 'qualified non-generic methods must not resolve to unrelated generic functions'
	}
	if decl := t.generic_fn_decl_for_call_info('write') {
		assert decl.name == 'write'
	} else {
		assert false, 'expected exact generic function lookup to work'
	}
}

fn test_resolve_monomorphize_decl_key_prefers_module_suffix_over_short_name() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'uniq'
	}
	decl_node := {
		'uniq':         decl
		'arrays__uniq': decl
	}
	resolved := t.resolve_monomorphize_decl_key('http__arrays__uniq', decl_node) or {
		panic('missing resolved key')
	}
	assert resolved == 'arrays__uniq'
}

fn test_resolve_monomorphize_decl_key_rejects_unrelated_short_name() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'encode'
	}
	decl_node := {
		'encode':        decl
		'json2__encode': decl
	}
	if _ := t.resolve_monomorphize_decl_key('base64__encode', decl_node) {
		assert false, 'qualified generic lookup must not fall back to unrelated short names'
	}
	if _ := t.resolve_monomorphize_decl_key('base64.encode', decl_node) {
		assert false, 'dotted generic lookup must not fall back to unrelated short names'
	}
}

fn test_generic_call_info_bare_lookup_stays_in_current_module() {
	mut t := mono_test_transformer()
	t.cur_module = 'cbor'
	json_decl := ast.FnDecl{
		name: 'encode'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'JsonT'
				}),
			]
		}
	}
	cbor_decl := ast.FnDecl{
		name: 'encode'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'CborT'
				}),
			]
		}
	}
	t.generic_fn_decl_index['encode'] = json_decl
	t.generic_fn_decl_index['json2__encode'] = json_decl
	t.generic_fn_decl_index['cbor__encode'] = cbor_decl
	decl := t.generic_fn_decl_for_call_info('encode') or { panic('missing current module decl') }
	gp := decl.typ.generic_params[0] as ast.Ident
	assert gp.name == 'CborT'
}

fn test_transformer_pointer_guards_accept_low_canonical_pointers() {
	assert sumtype_payload_word_is_valid(1, 0x10000)
	assert transformer_data_ptr_has_valid_address(0x10000)
	assert !sumtype_payload_word_is_valid(1, 0)
	assert !sumtype_payload_word_is_valid(1, 3)
}

fn mono_transform_dijkstra_shape_for_test() []ast.File {
	source_path := os.join_path(os.temp_dir(), 'v2_mono_dijkstra_shape_${os.getpid()}.v')
	os.write_file(source_path, '
module main

struct NODE {
mut:
	data int
	priority int
}

fn push_pq[T](mut prior_queue []T, data int, priority int) {
	_ = prior_queue
	_ = data
	_ = priority
}

fn updating_priority[T](mut prior_queue []T, search_data int, new_priority int) {
	_ = prior_queue
	_ = search_data
	_ = new_priority
}

fn departure_priority[T](mut prior_queue []T) int {
	_ = prior_queue
	return 0
}

fn all_adjacents[T](g [][]T, v int) []int {
	mut temp := []int{}
	for i in 0 .. g.len {
		if g[v][i] > 0 {
			temp << i
		}
	}
	return temp
}

fn print_solution[T](dist []T) {
	_ = dist
}

fn print_paths_dist[T](path []T, dist []T) {
	_ = path
	_ = dist
}

fn dijkstra(g [][]int, s int) {
	mut pq_queue := []NODE{}
	push_pq(mut pq_queue, s, 0)
	mut n := g.len
	mut dist := []int{len: n, init: -1}
	mut path := []int{len: n, init: -1}
	dist[s] = 0
	for pq_queue.len != 0 {
		mut v := departure_priority(mut pq_queue)
		mut adjs_of_v := all_adjacents(g, v)
		mut new_dist := 0
		for w in adjs_of_v {
			new_dist = dist[v] + g[v][w]
			if dist[w] == -1 {
				dist[w] = new_dist
				push_pq(mut pq_queue, w, dist[w])
				path[w] = v
			}
			if dist[w] > new_dist {
				dist[w] = new_dist
				updating_priority(mut pq_queue, w, dist[w])
				path[w] = v
			}
		}
	}
	print_solution(dist)
	print_paths_dist(path, dist)
}
') or {
		panic('failed to write temp source')
	}
	defer {
		os.rm(source_path) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([source_path], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := Transformer.new_with_pref(env, prefs)
	return trans.transform_files(files)
}

fn mono_collect_call_names_from_expr(expr ast.Expr, mut names []string) {
	match expr {
		ast.ArrayInitExpr {
			for nested in expr.exprs {
				mono_collect_call_names_from_expr(nested, mut names)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.Ident {
				names << expr.lhs.name
			}
			mono_collect_call_names_from_expr(expr.lhs, mut names)
			for arg in expr.args {
				mono_collect_call_names_from_expr(arg, mut names)
			}
		}
		ast.IfExpr {
			mono_collect_call_names_from_expr(expr.cond, mut names)
			for stmt in expr.stmts {
				mono_collect_call_names_from_stmt(stmt, mut names)
			}
			mono_collect_call_names_from_expr(expr.else_expr, mut names)
		}
		ast.InfixExpr {
			mono_collect_call_names_from_expr(expr.lhs, mut names)
			mono_collect_call_names_from_expr(expr.rhs, mut names)
		}
		ast.MatchExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
			for branch in expr.branches {
				for cond in branch.cond {
					mono_collect_call_names_from_expr(cond, mut names)
				}
				for stmt in branch.stmts {
					mono_collect_call_names_from_stmt(stmt, mut names)
				}
			}
		}
		ast.ModifierExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.OrExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
			for stmt in expr.stmts {
				mono_collect_call_names_from_stmt(stmt, mut names)
			}
		}
		ast.ParenExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.PrefixExpr {
			mono_collect_call_names_from_expr(expr.expr, mut names)
		}
		ast.SelectorExpr {
			mono_collect_call_names_from_expr(expr.lhs, mut names)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				mono_collect_call_names_from_expr(inter.expr, mut names)
				mono_collect_call_names_from_expr(inter.format_expr, mut names)
			}
		}
		else {}
	}
}

fn mono_collect_string_literals_from_expr(expr ast.Expr, mut values []string) {
	match expr {
		ast.ArrayInitExpr {
			for nested in expr.exprs {
				mono_collect_string_literals_from_expr(nested, mut values)
			}
		}
		ast.CallExpr {
			mono_collect_string_literals_from_expr(expr.lhs, mut values)
			for arg in expr.args {
				mono_collect_string_literals_from_expr(arg, mut values)
			}
		}
		ast.IfExpr {
			mono_collect_string_literals_from_expr(expr.cond, mut values)
			for stmt in expr.stmts {
				mono_collect_string_literals_from_stmt(stmt, mut values)
			}
			mono_collect_string_literals_from_expr(expr.else_expr, mut values)
		}
		ast.InfixExpr {
			mono_collect_string_literals_from_expr(expr.lhs, mut values)
			mono_collect_string_literals_from_expr(expr.rhs, mut values)
		}
		ast.ModifierExpr {
			mono_collect_string_literals_from_expr(expr.expr, mut values)
		}
		ast.OrExpr {
			mono_collect_string_literals_from_expr(expr.expr, mut values)
			for stmt in expr.stmts {
				mono_collect_string_literals_from_stmt(stmt, mut values)
			}
		}
		ast.ParenExpr {
			mono_collect_string_literals_from_expr(expr.expr, mut values)
		}
		ast.PrefixExpr {
			mono_collect_string_literals_from_expr(expr.expr, mut values)
		}
		ast.SelectorExpr {
			mono_collect_string_literals_from_expr(expr.lhs, mut values)
		}
		ast.StringInterLiteral {
			values << expr.values
			for inter in expr.inters {
				mono_collect_string_literals_from_expr(inter.expr, mut values)
				mono_collect_string_literals_from_expr(inter.format_expr, mut values)
			}
		}
		ast.StringLiteral {
			values << expr.value
		}
		else {}
	}
}

fn mono_collect_string_literals_from_stmt(stmt ast.Stmt, mut values []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.lhs {
				mono_collect_string_literals_from_expr(expr, mut values)
			}
			for expr in stmt.rhs {
				mono_collect_string_literals_from_expr(expr, mut values)
			}
		}
		ast.ExprStmt {
			mono_collect_string_literals_from_expr(stmt.expr, mut values)
		}
		ast.ForStmt {
			mono_collect_string_literals_from_expr(stmt.cond, mut values)
			for nested in stmt.stmts {
				mono_collect_string_literals_from_stmt(nested, mut values)
			}
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_string_literals_from_expr(expr, mut values)
			}
		}
		else {}
	}
}

fn mono_collect_call_names_from_stmt(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				mono_collect_call_names_from_expr(expr, mut names)
			}
		}
		ast.ExprStmt {
			mono_collect_call_names_from_expr(stmt.expr, mut names)
		}
		ast.ForStmt {
			mono_collect_call_names_from_expr(stmt.cond, mut names)
			for nested in stmt.stmts {
				mono_collect_call_names_from_stmt(nested, mut names)
			}
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_call_names_from_expr(expr, mut names)
			}
		}
		else {}
	}
}

fn mono_call_names_for_fn(files []ast.File, fn_name string) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					mono_collect_call_names_from_stmt(nested, mut names)
				}
			}
		}
	}
	return names
}

fn mono_assign_rhs_for_var(files []ast.File, fn_name string, var_name string) ?ast.Expr {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					if nested is ast.AssignStmt {
						for i, lhs in nested.lhs {
							if lhs is ast.Ident && lhs.name == var_name && i < nested.rhs.len {
								return nested.rhs[i]
							}
						}
					}
				}
			}
		}
	}
	return none
}

fn mono_init_field(init ast.InitExpr, field_name string) ?ast.Expr {
	for field in init.fields {
		if field.name == field_name {
			return field.value
		}
	}
	return none
}

fn mono_assert_init_ident_name(init ast.InitExpr, expected string) {
	assert init.typ is ast.Ident, 'expected InitExpr typ ${expected}, got ${init.typ}'
	assert (init.typ as ast.Ident).name == expected
}

fn mono_assert_basic_number_expr(expr ast.Expr, expected string) {
	assert expr is ast.BasicLiteral, 'expected number literal ${expected}, got ${expr}'
	lit := expr as ast.BasicLiteral
	assert lit.kind == token.Token.number
	assert lit.value == expected
}

fn mono_assert_sumtype_payload_memdup_expr(expr ast.Expr, expected_variant string) {
	assert expr is ast.CastExpr, 'expected voidptr payload cast for ${expected_variant}, got ${expr}'
	voidptr_cast := expr as ast.CastExpr
	assert voidptr_cast.typ is ast.Ident
	assert (voidptr_cast.typ as ast.Ident).name == 'voidptr'
	assert voidptr_cast.expr is ast.CallExpr, 'expected memdup payload call, got ${voidptr_cast.expr}'
	memdup_call := voidptr_cast.expr as ast.CallExpr
	assert memdup_call.lhs is ast.Ident
	assert (memdup_call.lhs as ast.Ident).name == 'memdup'
	assert memdup_call.args.len == 2, 'expected memdup payload and sizeof args, got ${memdup_call.args}'
	payload_ref := memdup_call.args[0]
	assert payload_ref is ast.PrefixExpr, 'expected memdup payload address, got ${payload_ref}'
	payload_prefix := payload_ref as ast.PrefixExpr
	assert payload_prefix.op == token.Token.amp
	assert payload_prefix.expr is ast.InitExpr, 'expected concrete variant init payload, got ${payload_prefix.expr}'
	payload_init := payload_prefix.expr as ast.InitExpr
	mono_assert_init_ident_name(payload_init, expected_variant)
	size_arg := memdup_call.args[1]
	assert size_arg is ast.KeywordOperator, 'expected sizeof payload arg, got ${size_arg}'
	size_op := size_arg as ast.KeywordOperator
	assert size_op.op == token.Token.key_sizeof
	assert size_op.exprs.len == 1
	assert size_op.exprs[0] is ast.Ident
	assert (size_op.exprs[0] as ast.Ident).name == expected_variant
}

fn mono_collect_tag_compare_rhs_from_expr(expr ast.Expr, mut values []ast.Expr) {
	match expr {
		ast.IfExpr {
			mono_collect_tag_compare_rhs_from_expr(expr.cond, mut values)
			for stmt in expr.stmts {
				mono_collect_tag_compare_rhs_from_stmt(stmt, mut values)
			}
			mono_collect_tag_compare_rhs_from_expr(expr.else_expr, mut values)
		}
		ast.InfixExpr {
			if expr.op == token.Token.eq && expr.lhs is ast.SelectorExpr {
				lhs := expr.lhs as ast.SelectorExpr
				if lhs.rhs.name == '_tag' {
					values << expr.rhs
				}
			}
			mono_collect_tag_compare_rhs_from_expr(expr.lhs, mut values)
			mono_collect_tag_compare_rhs_from_expr(expr.rhs, mut values)
		}
		ast.ParenExpr {
			mono_collect_tag_compare_rhs_from_expr(expr.expr, mut values)
		}
		else {}
	}
}

fn mono_collect_tag_compare_rhs_from_stmt(stmt ast.Stmt, mut values []ast.Expr) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				mono_collect_tag_compare_rhs_from_expr(expr, mut values)
			}
		}
		ast.ExprStmt {
			mono_collect_tag_compare_rhs_from_expr(stmt.expr, mut values)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_tag_compare_rhs_from_expr(expr, mut values)
			}
		}
		else {}
	}
}

fn mono_collect_return_exprs_from_stmt(stmt ast.Stmt, mut exprs []ast.Expr) {
	match stmt {
		ast.BlockStmt {
			for nested in stmt.stmts {
				mono_collect_return_exprs_from_stmt(nested, mut exprs)
			}
		}
		ast.ExprStmt {
			mono_collect_return_exprs_from_expr(stmt.expr, mut exprs)
		}
		ast.ForStmt {
			for nested in stmt.stmts {
				mono_collect_return_exprs_from_stmt(nested, mut exprs)
			}
			mono_collect_return_exprs_from_stmt(stmt.init, mut exprs)
			mono_collect_return_exprs_from_stmt(stmt.post, mut exprs)
		}
		ast.ReturnStmt {
			exprs << stmt.exprs
		}
		else {}
	}
}

fn mono_collect_return_exprs_from_expr(expr ast.Expr, mut exprs []ast.Expr) {
	match expr {
		ast.IfExpr {
			for stmt in expr.stmts {
				mono_collect_return_exprs_from_stmt(stmt, mut exprs)
			}
			mono_collect_return_exprs_from_expr(expr.else_expr, mut exprs)
		}
		else {}
	}
}

fn mono_return_exprs_for_decl(decl ast.FnDecl) []ast.Expr {
	mut exprs := []ast.Expr{}
	for stmt in decl.stmts {
		mono_collect_return_exprs_from_stmt(stmt, mut exprs)
	}
	return exprs
}

fn mono_tag_compare_rhs_for_fn(files []ast.File, fn_name string) []ast.Expr {
	mut values := []ast.Expr{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				for nested in stmt.stmts {
					mono_collect_tag_compare_rhs_from_stmt(nested, mut values)
				}
			}
		}
	}
	return values
}

fn mono_tag_compare_rhs_for_decl(decl ast.FnDecl) []ast.Expr {
	mut values := []ast.Expr{}
	for stmt in decl.stmts {
		mono_collect_tag_compare_rhs_from_stmt(stmt, mut values)
	}
	return values
}

fn mono_has_fn(files []ast.File, fn_name string) bool {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				return true
			}
		}
	}
	return false
}

fn mono_fn_names(files []ast.File) []string {
	mut names := []string{}
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				names << stmt.name
			}
		}
	}
	return names
}

fn mono_method_decl_by_receiver(files []ast.File, receiver_name string, method_name string) ?ast.FnDecl {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.is_method && stmt.name == method_name {
				receiver := stmt.receiver.typ
				if receiver is ast.Ident && receiver.name == receiver_name {
					return stmt
				}
			}
		}
	}
	return none
}

fn mono_fn_decl_by_name(files []ast.File, fn_name string) ?ast.FnDecl {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == fn_name {
				return stmt
			}
		}
	}
	return none
}

fn mono_assert_method_clone_by_receiver(files []ast.File, receiver_name string, method_name string) ast.FnDecl {
	fn_names := mono_fn_names(files)
	decl := mono_method_decl_by_receiver(files, receiver_name, method_name) or {
		assert false, 'missing concrete method clone ${receiver_name}.${method_name}, got ${fn_names}'
		return ast.FnDecl{}
	}
	assert decl.is_method, '${receiver_name}.${method_name} must stay a method clone'
	assert decl.name == method_name
	assert decl.stmts.len > 0, '${receiver_name}.${method_name} must keep its cloned method body'
	return decl
}

fn mono_call_names_for_decl(decl ast.FnDecl) []string {
	mut names := []string{}
	for stmt in decl.stmts {
		mono_collect_call_names_from_stmt(stmt, mut names)
	}
	return names
}

fn mono_call_lhs_shape(expr ast.Expr) string {
	match expr {
		ast.GenericArgs {
			return 'GenericArgs(${mono_call_lhs_shape(expr.lhs)})'
		}
		ast.GenericArgOrIndexExpr {
			return 'GenericArgOrIndexExpr(${mono_call_lhs_shape(expr.lhs)})'
		}
		ast.Ident {
			return 'Ident(${expr.name})'
		}
		ast.IndexExpr {
			return 'IndexExpr(${mono_call_lhs_shape(expr.lhs)})'
		}
		ast.SelectorExpr {
			return 'Selector(${mono_call_lhs_shape(expr.lhs)}.${expr.rhs.name})'
		}
		else {
			return typeof(expr).name
		}
	}
}

fn mono_collect_call_lhs_shapes_from_expr(expr ast.Expr, mut shapes []string) {
	match expr {
		ast.CallExpr {
			shapes << mono_call_lhs_shape(expr.lhs)
			mono_collect_call_lhs_shapes_from_expr(expr.lhs, mut shapes)
			for arg in expr.args {
				mono_collect_call_lhs_shapes_from_expr(arg, mut shapes)
			}
		}
		ast.IfExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.cond, mut shapes)
			for stmt in expr.stmts {
				mono_collect_call_lhs_shapes_from_stmt(stmt, mut shapes)
			}
			mono_collect_call_lhs_shapes_from_expr(expr.else_expr, mut shapes)
		}
		ast.InfixExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.lhs, mut shapes)
			mono_collect_call_lhs_shapes_from_expr(expr.rhs, mut shapes)
		}
		ast.MatchExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.expr, mut shapes)
			for branch in expr.branches {
				for cond in branch.cond {
					mono_collect_call_lhs_shapes_from_expr(cond, mut shapes)
				}
				for stmt in branch.stmts {
					mono_collect_call_lhs_shapes_from_stmt(stmt, mut shapes)
				}
			}
		}
		ast.ParenExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.expr, mut shapes)
		}
		ast.PrefixExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.expr, mut shapes)
		}
		ast.SelectorExpr {
			mono_collect_call_lhs_shapes_from_expr(expr.lhs, mut shapes)
		}
		else {}
	}
}

fn mono_collect_call_lhs_shapes_from_stmt(stmt ast.Stmt, mut shapes []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				mono_collect_call_lhs_shapes_from_expr(expr, mut shapes)
			}
		}
		ast.ExprStmt {
			mono_collect_call_lhs_shapes_from_expr(stmt.expr, mut shapes)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_call_lhs_shapes_from_expr(expr, mut shapes)
			}
		}
		else {}
	}
}

fn mono_call_lhs_shapes_for_decl(decl ast.FnDecl) []string {
	mut shapes := []string{}
	for stmt in decl.stmts {
		mono_collect_call_lhs_shapes_from_stmt(stmt, mut shapes)
	}
	return shapes
}

fn mono_raw_call_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			return expr.rhs.name
		}
		else {
			return mono_call_lhs_shape(expr)
		}
	}
}

fn mono_collect_raw_call_names_from_expr(expr ast.Expr, mut names []string) {
	match expr {
		ast.CallExpr {
			names << mono_raw_call_name(expr.lhs)
			mono_collect_raw_call_names_from_expr(expr.lhs, mut names)
			for arg in expr.args {
				mono_collect_raw_call_names_from_expr(arg, mut names)
			}
		}
		ast.IfExpr {
			mono_collect_raw_call_names_from_expr(expr.cond, mut names)
			for stmt in expr.stmts {
				mono_collect_raw_call_names_from_stmt(stmt, mut names)
			}
			mono_collect_raw_call_names_from_expr(expr.else_expr, mut names)
		}
		ast.InfixExpr {
			mono_collect_raw_call_names_from_expr(expr.lhs, mut names)
			mono_collect_raw_call_names_from_expr(expr.rhs, mut names)
		}
		ast.MatchExpr {
			mono_collect_raw_call_names_from_expr(expr.expr, mut names)
			for branch in expr.branches {
				for cond in branch.cond {
					mono_collect_raw_call_names_from_expr(cond, mut names)
				}
				for stmt in branch.stmts {
					mono_collect_raw_call_names_from_stmt(stmt, mut names)
				}
			}
		}
		ast.ParenExpr {
			mono_collect_raw_call_names_from_expr(expr.expr, mut names)
		}
		ast.PrefixExpr {
			mono_collect_raw_call_names_from_expr(expr.expr, mut names)
		}
		ast.SelectorExpr {
			mono_collect_raw_call_names_from_expr(expr.lhs, mut names)
		}
		else {}
	}
}

fn mono_collect_raw_call_names_from_stmt(stmt ast.Stmt, mut names []string) {
	match stmt {
		ast.AssignStmt {
			for expr in stmt.rhs {
				mono_collect_raw_call_names_from_expr(expr, mut names)
			}
		}
		ast.ExprStmt {
			mono_collect_raw_call_names_from_expr(stmt.expr, mut names)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				mono_collect_raw_call_names_from_expr(expr, mut names)
			}
		}
		else {}
	}
}

fn mono_raw_call_names_for_decl(decl ast.FnDecl) []string {
	mut names := []string{}
	for stmt in decl.stmts {
		mono_collect_raw_call_names_from_stmt(stmt, mut names)
	}
	return names
}

fn mono_string_literals_for_decl(decl ast.FnDecl) []string {
	mut values := []string{}
	for stmt in decl.stmts {
		mono_collect_string_literals_from_stmt(stmt, mut values)
	}
	return values
}

fn mono_specs_summary(t &Transformer) string {
	mut keys := t.env.generic_types.keys()
	keys.sort()
	mut parts := []string{}
	for key in keys {
		bindings_list := t.env.generic_types[key] or { continue }
		mut sigs := []string{cap: bindings_list.len}
		for bindings in bindings_list {
			sigs << generic_bindings_signature(bindings)
		}
		sigs.sort()
		parts << '${key}=[${sigs.join(',')}]'
	}
	return parts.join(';')
}

fn mono_has_spec(t &Transformer, key string, sig string) bool {
	bindings_list := t.env.generic_types[key] or { return false }
	for bindings in bindings_list {
		if generic_bindings_signature(bindings) == sig {
			return true
		}
	}
	return false
}

fn mono_decl_label(decl ast.FnDecl) string {
	if decl.is_method && decl.receiver.typ is ast.Ident {
		return '${(decl.receiver.typ as ast.Ident).name}__${decl.name}'
	}
	return decl.name
}

fn mono_last_clone_names(t &Transformer) []string {
	mut names := []string{}
	for _, stmts in t.last_mono_clones {
		for stmt in stmts {
			if stmt is ast.FnDecl {
				names << mono_decl_label(stmt)
			}
		}
	}
	names.sort()
	return names
}

fn mono_last_clone_decl(t &Transformer, name string) ?ast.FnDecl {
	for _, stmts in t.last_mono_clones {
		for stmt in stmts {
			if stmt is ast.FnDecl && (stmt.name == name || mono_decl_label(stmt) == name) {
				return stmt
			}
		}
	}
	return none
}

fn mono_binding_lookup_summary(t &Transformer, keys []string) string {
	mut parts := []string{cap: keys.len}
	for key in keys {
		if bindings := t.lookup_monomorphized_fn_bindings('main', key) {
			parts << '${key}=${generic_bindings_signature(bindings)}'
		} else {
			parts << '${key}=none'
		}
	}
	return parts.join(';')
}

fn mono_assert_no_qualified_queue_array_string_fn_decl(fn_names []string, method_name string) {
	qualified := 'datatypes__Queue_T_Array_string__${method_name}'
	assert qualified !in fn_names, 'qualified method FnDecl should not be emitted for canonical clone: ${fn_names}'
}

fn mono_assert_no_open_queue_method_names(fn_names []string, call_names []string, method_name string) {
	short_open := 'Queue__${method_name}'
	qualified_open := 'datatypes__Queue__${method_name}'
	assert short_open !in fn_names, 'open Queue method FnDecl leaked into ${fn_names}'
	assert qualified_open !in fn_names, 'open datatypes Queue method FnDecl leaked into ${fn_names}'
	assert short_open !in call_names, 'open Queue method call leaked into ${call_names}'
	assert qualified_open !in call_names, 'open datatypes Queue method call leaked into ${call_names}'
}

fn mono_assert_no_open_queue_is_empty_names(fn_names []string, call_names []string) {
	mono_assert_no_open_queue_method_names(fn_names, call_names, 'is_empty')
}

fn mono_assert_specialized_inner_calls(call_names []string, expected string, open_names []string) {
	assert expected in call_names, 'expected specialized internal call ${expected}, got ${call_names}'
	for open_name in open_names {
		assert open_name !in call_names, 'open internal call ${open_name} leaked into ${call_names}'
	}
}

fn mono_assert_no_open_str_calls(call_names []string, open_names []string) {
	for open_name in open_names {
		assert open_name !in call_names, 'open str call ${open_name} leaked into ${call_names}'
	}
}

fn mono_assert_no_unresolved_generic_receiver_names(names []string, label string) {
	for name in names {
		assert !name.contains('unknown__'), '${label} leaked unresolved call ${name} in ${names}'
		assert !name.contains('_T_datatypes_T'), '${label} kept unresolved receiver generic ${name} in ${names}'
	}
}

struct MonoSource {
	rel  string
	code string
}

struct MonoCheckedSources {
	files []ast.File
	trans &Transformer
}

fn mono_nested_min_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) min[T]() T {
	return match tree {
		Empty { T(1e9) }
		Node[T] { tree.value }
	}
}

fn (tree Tree[T]) take_min[T]() T {
	return match tree {
		Empty { T(0) }
		Node[T] { tree.right.min() }
	}
}

fn use() f64 {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	return tree.take_min()
}
'
}

fn mono_insert_size_source() string {
	return '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn (tree Tree[T]) insert[T](x T) Tree[T] {
	return match tree {
		Empty { Node[T]{x, tree, tree} }
		Node[T] {
			if x == tree.value {
				tree
			} else if x < tree.value {
				Node[T]{
					...tree
					left: tree.left.insert(x)
				}
			} else {
				Node[T]{
					...tree
					right: tree.right.insert(x)
				}
			}
		}
	}
}

fn use() int {
	mut tree := Tree[f64](Empty{})
	tree = tree.insert(0.2)
	tree = tree.insert(0.5)
	return tree.size()
}
'
}

fn mono_transform_sources_for_test(sources []MonoSource) []ast.File {
	checked := mono_check_sources_for_test(sources)
	mut trans := checked.trans
	return trans.transform_files(checked.files)
}

fn mono_check_sources_for_test(sources []MonoSource) MonoCheckedSources {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_mono_sources_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for source in sources {
		path := os.join_path(tmp_dir, source.rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source.code) or { panic('failed to write ${path}') }
		paths << path
	}
	prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := Transformer.new_with_pref(env, prefs)
	return MonoCheckedSources{
		files: files
		trans: trans
	}
}

fn mono_transform_sources_flat_direct_for_test(sources []MonoSource) []ast.File {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_mono_sources_flat_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for source in sources {
		path := os.join_path(tmp_dir, source.rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source.code) or { panic('failed to write ${path}') }
		paths << path
	}
	prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	flat := ast.flatten_files(files)
	mut trans := Transformer.new_with_pref(env, prefs)
	return trans.transform_flat_to_flat_direct(&flat, []).to_files()
}

fn test_imported_generic_struct_init_receiver_method_call_is_monomorphized() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'datatypes/queue.v'
			code: '
module datatypes

pub struct Queue[T] {
mut:
	elements []T
}

pub fn (mut queue Queue[T]) push(value T) {
	queue.elements << value
}

pub fn (queue Queue[T]) is_empty() bool {
	return queue.elements.len == 0
}

pub fn (mut queue Queue[T]) pop() !T {
	return queue.elements[0]
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use(value []string) bool {
	mut queue := datatypes.Queue[[]string]{}
	queue.push(value)
	if queue.is_empty() {
		return false
	}
	popped := queue.pop() or { return false }
	return popped.len == 1
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'push')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'is_empty')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'pop')
	mono_assert_no_qualified_queue_array_string_fn_decl(fn_names, 'push')
	mono_assert_no_qualified_queue_array_string_fn_decl(fn_names, 'is_empty')
	mono_assert_no_qualified_queue_array_string_fn_decl(fn_names, 'pop')
	assert call_names == [
		'datatypes__Queue_T_Array_string__push',
		'datatypes__Queue_T_Array_string__is_empty',
		'datatypes__Queue_T_Array_string__pop',
	], 'expected exact imported Queue[[]string] method calls, got ${call_names}'
	mono_assert_no_open_queue_method_names(fn_names, call_names, 'push')
	mono_assert_no_open_queue_is_empty_names(fn_names, call_names)
	mono_assert_no_open_queue_method_names(fn_names, call_names, 'pop')
}

fn test_imported_generic_method_clone_body_rewrites_nested_receiver_calls() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'datatypes/inner.v'
			code: '
module datatypes

pub struct Inner[T] {
mut:
	values []T
}

pub fn (inner Inner[T]) is_empty() bool {
	return inner.values.len == 0
}

pub fn (mut inner Inner[T]) shift() !T {
	return inner.values[0]
}
'
		},
		MonoSource{
			rel:  'datatypes/outer.v'
			code: '
module datatypes

pub struct Outer[T] {
mut:
	inner Inner[T]
}

pub fn (outer Outer[T]) is_empty() bool {
	return outer.inner.is_empty()
}

pub fn (mut outer Outer[T]) pop() !T {
	return outer.inner.shift()
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use() bool {
	mut outer := datatypes.Outer[[]string]{}
	if outer.is_empty() {
		return false
	}
	popped := outer.pop() or { return false }
	return popped.len == 1
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	outer_is_empty := mono_assert_method_clone_by_receiver(files, 'Outer_T_Array_string',
		'is_empty')
	outer_pop := mono_assert_method_clone_by_receiver(files, 'Outer_T_Array_string', 'pop')
	mono_assert_method_clone_by_receiver(files, 'Inner_T_Array_string', 'is_empty')
	mono_assert_method_clone_by_receiver(files, 'Inner_T_Array_string', 'shift')
	assert call_names == [
		'datatypes__Outer_T_Array_string__is_empty',
		'datatypes__Outer_T_Array_string__pop',
	], 'expected exact imported Outer[[]string] method calls, got ${call_names}'
	assert 'datatypes__Outer__is_empty' !in call_names
	assert 'datatypes__Outer__pop' !in call_names
	assert 'Outer__is_empty' !in call_names
	assert 'Outer__pop' !in call_names
	assert 'datatypes__Outer_T_Array_string__is_empty' !in fn_names
	assert 'datatypes__Outer_T_Array_string__pop' !in fn_names
	is_empty_body_calls := mono_call_names_for_decl(outer_is_empty)
	pop_body_calls := mono_call_names_for_decl(outer_pop)
	mono_assert_specialized_inner_calls(is_empty_body_calls,
		'datatypes__Inner_T_Array_string__is_empty', [
		'datatypes__Inner__is_empty',
		'Inner__is_empty',
	])
	mono_assert_specialized_inner_calls(pop_body_calls, 'datatypes__Inner_T_Array_string__shift', [
		'datatypes__Inner__shift',
		'Inner__shift',
	])
}

fn test_local_generic_struct_init_receiver_method_call_is_monomorphized() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Queue[T] {
mut:
	elements []T
}

fn (queue Queue[T]) is_empty() bool {
	return queue.elements.len == 0
}

fn use() bool {
	mut queue := Queue[[]string]{}
	return queue.is_empty()
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'is_empty')
	assert call_names == ['Queue_T_Array_string__is_empty'], 'expected exact local Queue[[]string].is_empty call, got ${call_names}'
	mono_assert_no_open_queue_is_empty_names(fn_names, call_names)
}

fn test_generic_sumtype_cast_init_does_not_lower_to_open_call() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn use() {
	tree := Tree[f64](Empty{})
	_ = tree
}
'
		},
	])
	call_names := mono_call_names_for_fn(files, 'use')
	tree_rhs := mono_assign_rhs_for_var(files, 'use', 'tree') or {
		assert false, 'missing transformed tree assignment'
		return
	}
	assert tree_rhs is ast.InitExpr, 'generic sumtype cast should lower to InitExpr, got ${typeof(tree_rhs).name}'
	tree_init := tree_rhs as ast.InitExpr
	mono_assert_init_ident_name(tree_init, 'Tree_T_f64')
	tag_value := mono_init_field(tree_init, '_tag') or {
		assert false, 'missing Tree_T_f64 _tag field in ${tree_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '0')
	assert call_names == [], 'generic sumtype cast should lower to a wrapper, got calls ${call_names}'
	assert 'Tree_T_f64' !in call_names
}

fn test_generic_sumtype_concrete_variant_wrap_does_not_lower_to_open_call() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn use() {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	_ = tree
}
'
		},
	])
	call_names := mono_call_names_for_fn(files, 'use')
	tree_rhs := mono_assign_rhs_for_var(files, 'use', 'tree') or {
		assert false, 'missing transformed tree assignment'
		return
	}
	assert tree_rhs is ast.InitExpr, 'generic sumtype variant cast should lower to InitExpr, got ${typeof(tree_rhs).name}'
	tree_init := tree_rhs as ast.InitExpr
	mono_assert_init_ident_name(tree_init, 'Tree_T_f64')
	tag_value := mono_init_field(tree_init, '_tag') or {
		assert false, 'missing Tree_T_f64 _tag field in ${tree_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '1')
	payload_value := mono_init_field(tree_init, '_data._Node_T_f64') or {
		assert false, 'missing Tree_T_f64 Node_T_f64 payload field in ${tree_init.fields}'
		return
	}
	mono_assert_sumtype_payload_memdup_expr(payload_value, 'Node_T_f64')
	assert 'Tree_T_f64' !in call_names, 'generic sumtype cast leaked callable Tree_T_f64 in ${call_names}'
	assert 'Tree' !in call_names, 'generic sumtype cast leaked callable Tree in ${call_names}'
	for name in call_names {
		assert !name.contains('Tree_T_T'), 'generic sumtype cast leaked open generic name ${name} in ${call_names}'
	}
}

fn test_non_generic_concrete_sumtype_return_wraps_variant() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn make() Tree[int] {
	return Node[int]{value: 7}
}
'
		},
	])
	make_decl := mono_fn_decl_by_name(files, 'make') or {
		assert false, 'missing transformed make function'
		return
	}
	returns := mono_return_exprs_for_decl(make_decl)
	assert returns.len == 1, 'expected one make return, got ${returns}'
	ret_expr := returns[0]
	assert ret_expr is ast.InitExpr, 'Tree[int] return should wrap Node[int], got ${ret_expr}'
	ret_init := ret_expr as ast.InitExpr
	mono_assert_init_ident_name(ret_init, 'Tree_T_int')
	tag_value := mono_init_field(ret_init, '_tag') or {
		assert false, 'missing Tree_T_int _tag field in ${ret_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '1')
	payload_value := mono_init_field(ret_init, '_data._Node_T_int') or {
		assert false, 'missing Tree_T_int Node_T_int payload field in ${ret_init.fields}'
		return
	}
	mono_assert_sumtype_payload_memdup_expr(payload_value, 'Node_T_int')
}

fn test_nested_module_concrete_generic_sumtype_base_does_not_fall_back_to_local_tree() {
	nested_tree_type := types.Type(types.SumType{
		name:           'foo.bar__Tree'
		generic_params: ['T']
		variants:       [
			types.Type(types.Struct{
				name: 'foo.bar__Empty'
			}),
			types.Type(types.Struct{
				name:           'foo.bar__Node'
				generic_params: ['T']
			}),
		]
	})
	local_tree_type := types.Type(types.SumType{
		name:           'main__Tree'
		generic_params: ['T']
		variants:       [
			types.Type(types.Struct{
				name: 'main__Wrong'
			}),
		]
	})
	foo_tree_type := types.Type(types.SumType{
		name:           'foo__Tree'
		generic_params: ['T']
		variants:       [
			types.Type(types.Struct{
				name: 'foo__Wrong'
			}),
		]
	})
	mut nested_scope := types.new_scope(unsafe { nil })
	nested_scope.insert_type('Tree', nested_tree_type)
	mut foo_scope := types.new_scope(unsafe { nil })
	foo_scope.insert_type('Tree', foo_tree_type)
	mut main_scope := types.new_scope(unsafe { nil })
	main_scope.insert_type('Tree', local_tree_type)
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.cached_scopes = {
		'foo.bar': nested_scope
		'foo':     foo_scope
		'main':    main_scope
	}
	t.cur_monomorphized_fn_bindings = {
		'T': types.Type(types.int_)
	}

	base := t.concrete_generic_sumtype_base_type('foo__bar__Tree_T_int') or {
		assert false, 'missing nested foo.bar Tree base'
		return
	}
	assert base is types.SumType
	assert (base as types.SumType).name == 'foo.bar__Tree'
	info := t.sumtype_wrap_info_for_name('foo__bar__Tree_T_int') or {
		assert false, 'missing nested foo.bar Tree wrap info'
		return
	}
	assert info.name == 'foo__bar__Tree_T_int'
	assert info.variants == ['foo__bar__Empty', 'foo__bar__Node_T_int']
	assert 'foo__Wrong_T_int' !in info.variants
	assert 'main__Wrong_T_int' !in info.variants
}

fn test_leaf_module_concrete_generic_sumtype_base_fallback_beats_parent_module_tree() {
	bar_tree_type := types.Type(types.SumType{
		name:           'bar__Tree'
		generic_params: ['T']
		variants:       [
			types.Type(types.Struct{
				name: 'bar__Empty'
			}),
			types.Type(types.Struct{
				name:           'bar__Node'
				generic_params: ['T']
			}),
		]
	})
	foo_tree_type := types.Type(types.SumType{
		name:           'foo__Tree'
		generic_params: ['T']
		variants:       [
			types.Type(types.Struct{
				name: 'foo__Wrong'
			}),
		]
	})
	mut bar_scope := types.new_scope(unsafe { nil })
	bar_scope.insert_type('Tree', bar_tree_type)
	mut foo_scope := types.new_scope(unsafe { nil })
	foo_scope.insert_type('Tree', foo_tree_type)
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.cached_scopes = {
		'bar': bar_scope
		'foo': foo_scope
	}
	t.cur_monomorphized_fn_bindings = {
		'T': types.Type(types.int_)
	}

	base := t.concrete_generic_sumtype_base_type('foo__bar__Tree_T_int') or {
		assert false, 'missing leaf module bar Tree base'
		return
	}
	assert base is types.SumType
	assert (base as types.SumType).name == 'bar__Tree'
	info := t.sumtype_wrap_info_for_name('foo__bar__Tree_T_int') or {
		assert false, 'missing leaf module bar Tree wrap info'
		return
	}
	assert info.name == 'foo__bar__Tree_T_int'
	assert info.variants == ['bar__Empty', 'bar__Node_T_int']
	assert 'foo__Wrong_T_int' !in info.variants
}

fn test_option_concrete_sumtype_return_wraps_variant() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn make() ?Tree[int] {
	return Node[int]{value: 7}
}
'
		},
	])
	make_decl := mono_fn_decl_by_name(files, 'make') or {
		assert false, 'missing transformed make function'
		return
	}
	returns := mono_return_exprs_for_decl(make_decl)
	assert returns.len == 1, 'expected one make return, got ${returns}'
	ret_expr := returns[0]
	assert ret_expr is ast.InitExpr, '?Tree[int] return should wrap Node[int], got ${ret_expr}'
	ret_init := ret_expr as ast.InitExpr
	mono_assert_init_ident_name(ret_init, 'Tree_T_int')
	tag_value := mono_init_field(ret_init, '_tag') or {
		assert false, 'missing Tree_T_int _tag field in ${ret_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '1')
	payload_value := mono_init_field(ret_init, '_data._Node_T_int') or {
		assert false, 'missing Tree_T_int Node_T_int payload field in ${ret_init.fields}'
		return
	}
	mono_assert_sumtype_payload_memdup_expr(payload_value, 'Node_T_int')
}

fn test_result_concrete_sumtype_return_wraps_variant() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn make() !Tree[int] {
	return Node[int]{value: 7}
}
'
		},
	])
	make_decl := mono_fn_decl_by_name(files, 'make') or {
		assert false, 'missing transformed make function'
		return
	}
	returns := mono_return_exprs_for_decl(make_decl)
	assert returns.len == 1, 'expected one make return, got ${returns}'
	ret_expr := returns[0]
	assert ret_expr is ast.InitExpr, '!Tree[int] return should wrap Node[int], got ${ret_expr}'
	ret_init := ret_expr as ast.InitExpr
	mono_assert_init_ident_name(ret_init, 'Tree_T_int')
	tag_value := mono_init_field(ret_init, '_tag') or {
		assert false, 'missing Tree_T_int _tag field in ${ret_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '1')
	payload_value := mono_init_field(ret_init, '_data._Node_T_int') or {
		assert false, 'missing Tree_T_int Node_T_int payload field in ${ret_init.fields}'
		return
	}
	mono_assert_sumtype_payload_memdup_expr(payload_value, 'Node_T_int')
}

fn test_pointer_generic_sumtype_variant_uses_concrete_variant_name() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = &Node[T] | Empty

fn make() Tree[int] {
	return &Node[int]{value: 7}
}
'
		},
	])
	mut trans := checked.trans
	main_scope := trans.env.get_scope('main') or { panic('missing main scope') }
	tree_type := main_scope.lookup_type_parent('Tree', 0) or { panic('missing Tree type') }
	assert tree_type is types.SumType, 'expected Tree sum type, got ${tree_type}'
	tree := tree_type as types.SumType
	mut variants := []string{cap: tree.variants.len}
	bindings := {
		'T': types.Type(types.int_)
	}
	for variant in tree.variants {
		concrete_variant := trans.instantiate_generic_sumtype_variant(variant, bindings)
		variants << trans.type_to_c_name(concrete_variant)
	}
	assert variants == ['Node_T_intptr', 'Empty']
}

fn test_generic_sumtype_declared_param_order_is_preserved_for_crossed_variants() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Left[T] {
	value T
}

struct Right[T] {
	value T
}

type Either[K, V] = Left[V] | Right[K]

fn make() Either[string, int] {
	return Left[int]{value: 1}
}
'
		},
	])
	mut trans := checked.trans
	main_scope := trans.env.get_scope('main') or { panic('missing main scope') }
	either_type := main_scope.lookup_type_parent('Either', 0) or { panic('missing Either type') }
	params := generic_template_type_param_names_from_type(either_type)
	assert params == ['K', 'V'], 'Either generic param order should stay K,V, got ${params}'
	files := trans.transform_files(checked.files)
	make_decl := mono_fn_decl_by_name(files, 'make') or {
		assert false, 'missing transformed make function'
		return
	}
	returns := mono_return_exprs_for_decl(make_decl)
	assert returns.len == 1, 'expected one make return, got ${returns}'
	ret_expr := returns[0]
	assert ret_expr is ast.InitExpr, 'Either[string, int] return should wrap Left[int], got ${ret_expr}'
	ret_init := ret_expr as ast.InitExpr
	mono_assert_init_ident_name(ret_init, 'Either_T_string_int')
	tag_value := mono_init_field(ret_init, '_tag') or {
		assert false, 'missing Either_T_string_int _tag field in ${ret_init.fields}'
		return
	}
	mono_assert_basic_number_expr(tag_value, '0')
	payload_value := mono_init_field(ret_init, '_data._Left_T_int') or {
		assert false, 'missing Either_T_string_int Left_T_int payload field in ${ret_init.fields}'
		return
	}
	mono_assert_sumtype_payload_memdup_expr(payload_value, 'Left_T_int')
}

fn test_generic_template_type_param_names_handles_recursive_generic_struct_fields() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'datatypes/linked_list.v'
			code: '
module datatypes

pub struct ListNode[T] {
mut:
	data T
	next &ListNode[T] = unsafe { nil }
}

pub struct LinkedList[T] {
mut:
	head &ListNode[T] = unsafe { nil }
}
'
		},
		MonoSource{
			rel:  'datatypes/queue.v'
			code: '
module datatypes

pub struct Queue[T] {
mut:
	elements LinkedList[T]
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use() {
	_ := datatypes.Queue[[]string]{}
}
'
		},
	])
	mut trans := checked.trans
	datatypes_scope := trans.env.get_scope('datatypes') or { panic('missing datatypes scope') }
	queue_type := datatypes_scope.lookup_type_parent('Queue', 0) or { panic('missing Queue type') }
	params := generic_template_type_param_names_from_type(queue_type)
	assert params == ['T'], 'recursive generic Queue params should be [T], got ${params}'
}

fn test_generic_template_type_param_names_collects_embedded_generic_params() {
	wrapper_type := types.Type(types.Struct{
		name:           'Wrapper'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'value'
				typ:  types.Type(types.NamedType('T'))
			},
		]
		embedded:       [
			types.Struct{
				name:           'Embedded'
				generic_params: ['U']
				fields:         [types.Field{
					name: 'inner'
					typ:  types.Type(types.NamedType('U'))
				}]
			},
		]
	})
	params := generic_template_type_param_names_from_type(wrapper_type)
	assert params == ['T', 'U'], 'embedded generic params should be collected, got ${params}'
}

fn test_generic_template_type_param_names_keeps_same_name_different_generic_params_separate() {
	root_type := types.Type(types.Struct{
		name:   'Root'
		fields: [
			types.Field{
				name: 'left'
				typ:  types.Type(types.Struct{
					name:           'Box'
					generic_params: ['T']
				})
			},
			types.Field{
				name: 'right'
				typ:  types.Type(types.Struct{
					name:           'Box'
					generic_params: ['U']
				})
			},
		]
	})
	params := generic_template_type_param_names_from_type(root_type)
	assert params == ['T', 'U'], 'same-name structs with different params should stay distinct, got ${params}'
}

fn test_generic_template_type_param_names_does_not_guard_empty_struct_names() {
	root_type := types.Type(types.Struct{
		fields: [
			types.Field{
				name: 'left'
				typ:  types.Type(types.Struct{
					generic_params: ['T']
				})
			},
			types.Field{
				name: 'right'
				typ:  types.Type(types.Struct{
					generic_params: ['U']
				})
			},
		]
	})
	params := generic_template_type_param_names_from_type(root_type)
	assert params == ['T', 'U'], 'empty-name structs should not share one structural guard, got ${params}'
}

fn test_generic_sumtype_match_lowers_generic_branch_to_numeric_tag() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn tag(tree Tree[f64]) int {
	return match tree {
		Empty { 0 }
		Node[f64] { 1 }
	}
}
'
		},
	])
	rhs_values := mono_tag_compare_rhs_for_fn(files, 'tag')
	mut saw_zero := false
	mut saw_one := false
	for rhs in rhs_values {
		assert rhs !is ast.IndexExpr, 'generic sumtype match left index rhs ${rhs}'
		assert rhs !is ast.GenericArgs, 'generic sumtype match left generic args rhs ${rhs}'
		assert rhs !is ast.GenericArgOrIndexExpr, 'generic sumtype match left generic arg/index rhs ${rhs}'
		if rhs is ast.BasicLiteral && rhs.kind == token.Token.number {
			if rhs.value == '0' {
				saw_zero = true
			}
			if rhs.value == '1' {
				saw_one = true
			}
		}
	}
	assert saw_zero, 'generic sumtype match missing Empty tag 0 in ${rhs_values}'
	assert saw_one, 'generic sumtype match missing Node[f64] tag 1 in ${rhs_values}'
}

fn test_sumtype_decl_variant_cache_records_concrete_generic_variants_only() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'foo/box.v'
			code: '
module foo

pub struct Box[T] {
pub:
	value T
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import foo as f

struct Empty {}

struct Node[T] {
	value T
}

struct Box[T] {
	value T
}

type Tree[T] = Empty | Node[T]
type Value = Box[int] | Box[string]
type ImportedValue = f.Box[int] | f.Box[string]
'
		},
	])
	mut trans := checked.trans
	trans.pre_pass(checked.files)
	assert trans.get_sum_type_variants('Value') == ['Box_T_int', 'Box_T_string']
	assert trans.get_sum_type_variants('main__Value') == ['Box_T_int', 'Box_T_string']
	assert trans.get_sum_type_variants('ImportedValue') == ['foo__Box_T_int', 'foo__Box_T_string']
	assert trans.get_sum_type_variants('main__ImportedValue') == ['foo__Box_T_int',
		'foo__Box_T_string']
	assert 'Tree' !in trans.sum_type_decl_variant_names
	assert 'main__Tree' !in trans.sum_type_decl_variant_names

	flat := ast.flatten_files(checked.files)
	mut flat_trans := Transformer.new_with_pref(checked.trans.env, checked.trans.pref)
	flat_trans.pre_pass_from_flat(&flat)
	assert flat_trans.get_sum_type_variants('Value') == ['Box_T_int', 'Box_T_string']
	assert flat_trans.get_sum_type_variants('main__Value') == ['Box_T_int', 'Box_T_string']
	assert flat_trans.get_sum_type_variants('ImportedValue') == ['foo__Box_T_int',
		'foo__Box_T_string']
	assert flat_trans.get_sum_type_variants('main__ImportedValue') == [
		'foo__Box_T_int',
		'foo__Box_T_string',
	]
	assert 'Tree' !in flat_trans.sum_type_decl_variant_names
	assert 'main__Tree' !in flat_trans.sum_type_decl_variant_names
}

fn test_generic_sumtype_match_uses_full_specialization_when_generic_base_repeats() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Box[T] {
	value T
}

type Value = Box[int] | Box[string]

fn tag(value Value) int {
	return match value {
		Box[string] { 20 }
		else { 0 }
	}
}
'
		},
	])
	assert_generic_sumtype_repeated_base_string_tag(files, 'legacy')
}

fn test_flat_direct_generic_sumtype_match_uses_full_specialization_when_generic_base_repeats() {
	files := mono_transform_sources_flat_direct_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Box[T] {
	value T
}

type Value = Box[int] | Box[string]

fn tag(value Value) int {
	return match value {
		Box[string] { 20 }
		else { 0 }
	}
}
'
		},
	])
	assert_generic_sumtype_repeated_base_string_tag(files, 'flat direct')
}

fn test_generic_sumtype_match_smartcast_scans_full_specialization_body() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: generic_sumtype_repeated_base_smartcast_source()
		},
	])
	assert_generic_sumtype_repeated_base_smartcast_echo(files, 'legacy')
}

fn test_flat_direct_generic_sumtype_match_smartcast_scans_full_specialization_body() {
	files := mono_transform_sources_flat_direct_for_test([
		MonoSource{
			rel:  'main.v'
			code: generic_sumtype_repeated_base_smartcast_source()
		},
	])
	assert_generic_sumtype_repeated_base_smartcast_echo(files, 'flat direct')
}

fn generic_sumtype_repeated_base_smartcast_source() string {
	return '
module main

struct Box[T] {
	value T
}

type Value = Box[int] | Box[string]

fn echo[T](value T) T {
	return value
}

fn use(value Value) string {
	return match value {
		Box[string] { echo(value.value) }
		else { "" }
	}
}
'
}

fn assert_generic_sumtype_repeated_base_string_tag(files []ast.File, label string) {
	rhs_values := mono_tag_compare_rhs_for_fn(files, 'tag')
	mut saw_string_tag := false
	for rhs in rhs_values {
		assert rhs !is ast.IndexExpr, '${label}: generic sumtype repeated base left index rhs ${rhs}'
		assert rhs !is ast.GenericArgs, '${label}: generic sumtype repeated base left generic args rhs ${rhs}'
		assert rhs !is ast.GenericArgOrIndexExpr, '${label}: generic sumtype repeated base left generic arg/index rhs ${rhs}'
		if rhs is ast.BasicLiteral && rhs.kind == token.Token.number {
			assert rhs.value != '0', '${label}: Box[string] branch matched first Box[int] tag in ${rhs_values}'
			if rhs.value == '1' {
				saw_string_tag = true
			}
		}
	}
	assert saw_string_tag, '${label}: Box[string] branch should match the second generic specialization tag, got ${rhs_values}'
}

fn assert_generic_sumtype_repeated_base_smartcast_echo(files []ast.File, label string) {
	fn_names := mono_fn_names(files)
	assert 'echo_T_string' in fn_names, '${label}: smartcasted Box[string] branch did not collect echo[string], got ${fn_names}'
	assert 'echo_T_int' !in fn_names, '${label}: Box[string] branch collected wrong echo[int] specialization, got ${fn_names}'
	call_names := mono_call_names_for_fn(files, 'use')
	assert 'echo_T_string' in call_names, '${label}: use() should call echo_T_string from Box[string] branch, got ${call_names}'
	assert 'echo' !in call_names, '${label}: open echo call leaked in ${call_names}'
}

fn test_generic_sumtype_receiver_no_arg_method_call_is_monomorphized() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return 0
}

fn use() int {
	tree := Tree[f64](Empty{})
	return tree.size()
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'size_T_f64')
	assert call_names == ['Tree_T_f64__size_T_f64'], 'expected concrete Tree[f64].size call, got ${call_names}'
	assert 'Tree__size' !in fn_names
	assert 'Tree__size' !in call_names
	assert 'Tree_T_f64' !in call_names
}

fn test_generic_sumtype_receiver_recursive_size_calls_are_monomorphized() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 + tree.left.size() + tree.right.size() }
	}
}

fn use() string {
	tree := Tree[f64](Empty{})
	return "size \${tree.size()}"
}
'
		},
	])
	fn_names := mono_fn_names(files)
	use_call_names := mono_call_names_for_fn(files, 'use')
	size_decl := mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'size_T_f64')
	size_call_names := mono_call_names_for_decl(size_decl)
	assert 'Tree_T_f64__size_T_f64' in use_call_names, 'interpolation should call concrete Tree[f64].size, got ${use_call_names}'
	assert size_call_names == [
		'Tree_T_f64__size_T_f64',
		'Tree_T_f64__size_T_f64',
	], 'recursive Tree[f64].size body should call concrete size twice, got ${size_call_names}'
	assert 'Tree__size' !in fn_names
	assert 'Tree__size' !in use_call_names
	assert 'Tree__size' !in size_call_names
	assert 'Tree_T_T__size_T' !in fn_names
	assert 'Tree_T_T__size_T' !in use_call_names
	assert 'Tree_T_T__size_T' !in size_call_names
	assert 'Tree_T_f64' !in use_call_names
}

fn test_generic_sumtype_receiver_match_lowers_generic_branch_to_numeric_tag() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Empty {}

struct Node[T] {
	value T
	left  Tree[T]
	right Tree[T]
}

type Tree[T] = Empty | Node[T]

fn (tree Tree[T]) size[T]() int {
	return match tree {
		Empty { 0 }
		Node[T] { 1 }
	}
}

fn use() int {
	empty := Tree[f64](Empty{})
	tree := Tree[f64](Node[f64]{1.0, empty, empty})
	return tree.size()
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	size_decl := mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'size_T_f64')
	assert call_names == ['Tree_T_f64__size_T_f64'], 'expected concrete Tree[f64].size call, got ${call_names}'
	assert 'Tree_T_T__size_T' !in fn_names
	assert 'Tree_T_T__size_T' !in call_names
	rhs_values := mono_tag_compare_rhs_for_decl(size_decl)
	mut saw_zero := false
	mut saw_one := false
	for rhs in rhs_values {
		assert rhs !is ast.IndexExpr, 'generic receiver match left index rhs ${rhs}'
		assert rhs !is ast.GenericArgs, 'generic receiver match left generic args rhs ${rhs}'
		assert rhs !is ast.GenericArgOrIndexExpr, 'generic receiver match left generic arg/index rhs ${rhs}'
		if rhs is ast.BasicLiteral && rhs.kind == token.Token.number {
			if rhs.value == '0' {
				saw_zero = true
			}
			if rhs.value == '1' {
				saw_one = true
			}
		}
	}
	assert saw_zero, 'generic receiver match missing Empty tag 0 in ${rhs_values}'
	assert saw_one, 'generic receiver match missing Node[T] tag 1 in ${rhs_values}'
}

fn test_generic_sumtype_receiver_nested_min_call_is_monomorphized() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: mono_nested_min_source()
		},
	])
	fn_names := mono_fn_names(files)
	use_call_names := mono_call_names_for_fn(files, 'use')
	take_min_decl := mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'take_min_T_f64')
	take_min_call_names := mono_call_names_for_decl(take_min_decl)
	assert use_call_names == ['Tree_T_f64__take_min_T_f64'], 'expected use to call only concrete take_min, got ${use_call_names}'
	assert 'Tree_T_f64__min_T_f64' in take_min_call_names, 'take_min min-call classification: calls=${take_min_call_names}; has_concrete=${'Tree_T_f64__min_T_f64' in take_min_call_names}; has_open_tree=${'Tree__min' in take_min_call_names}; has_open_generic=${'Tree_T_T__min_T' in take_min_call_names}; has_raw_min=${'min' in take_min_call_names}; has_any_min=${take_min_call_names.any(it.contains('min'))}'
	if _ := mono_method_decl_by_receiver(files, 'Tree_T_f64', 'min_T_f64') {
	} else {
		assert false, 'missing concrete method clone Tree_T_f64.min_T_f64; take_min_call_names=${take_min_call_names}; fn_names=${fn_names}; has_concrete_call=${'Tree_T_f64__min_T_f64' in take_min_call_names}; has_open_tree=${'Tree__min' in take_min_call_names}; has_open_generic=${'Tree_T_T__min_T' in take_min_call_names}; has_raw_min=${'min' in take_min_call_names}; has_any_min=${take_min_call_names.any(it.contains('min'))}'
	}
	assert 'Tree_T_f64__min_T_f64' in take_min_call_names, 'concrete take_min should call concrete min, got ${take_min_call_names}'
	for names in [fn_names, use_call_names, take_min_call_names] {
		assert 'Tree__min' !in names
		assert 'Tree_T_T__min_T' !in names
		assert 'Node_T' !in names
		assert 'Node_T_T' !in names
		assert 'unknown__min' !in names
		for name in names {
			assert !name.contains('unknown__'), 'unresolved generic receiver call leaked in ${names}'
			assert !name.contains('Tree_T_T__min_T'), 'open generic min call leaked in ${names}'
			assert !name.contains('Node_T_T'), 'open generic Node leaked in ${names}'
		}
	}
}

fn test_generic_sumtype_receiver_nested_min_collects_smartcast_callee() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: mono_nested_min_source()
		},
	])
	mut trans := checked.trans
	files := checked.files
	trans.env.generic_types = map[string][]map[string]types.Type{}
	trans.pre_pass(files)
	trans.collect_declared_method_fns(files)
	trans.collect_struct_field_generic_decl_types(files)
	trans.collect_generic_call_specs(files)
	sig := 'T:f64'
	before_has_take_min := mono_has_spec(trans, 'Tree__take_min', sig)
	before_has_min := mono_has_spec(trans, 'Tree__min', sig)
	before_specs := mono_specs_summary(trans)
	prepared := trans.monomorphize_pass(files)
	clone_names := mono_last_clone_names(trans)
	mut take_min_call_names := []string{}
	mut take_min_call_shapes := []string{}
	if take_min_decl := mono_last_clone_decl(trans, 'Tree_T_f64__take_min_T_f64') {
		take_min_call_names = mono_raw_call_names_for_decl(take_min_decl)
		take_min_call_shapes = mono_call_lhs_shapes_for_decl(take_min_decl)
	}
	binding_summary := mono_binding_lookup_summary(trans, [
		'Tree_T_f64__take_min_T_f64',
		'take_min_T_f64',
		'Tree_T_f64__take_min',
	])
	trans.collect_generic_call_specs_in_new_clones(prepared)
	after_has_min := mono_has_spec(trans, 'Tree__min', sig)
	after_specs := mono_specs_summary(trans)
	assert before_has_take_min, 'expected initial take_min spec; before_specs=${before_specs}'
	assert !before_has_min, 'min spec should be discovered from the first take_min clone, before_specs=${before_specs}'
	assert 'Tree_T_f64__take_min_T_f64' in clone_names, 'missing first take_min clone; clones=${clone_names}'
	assert take_min_call_names == ['min'], 'first take_min clone should still contain raw selector call before collect2; calls=${take_min_call_names}; shapes=${take_min_call_shapes}'
	assert take_min_call_shapes == ['Selector(Selector(Ident(tree).right).min)'], 'unexpected take_min clone call shape before collect2: ${take_min_call_shapes}'
	assert binding_summary.contains('Tree_T_f64__take_min_T_f64=T:f64'), 'missing take_min clone bindings: ${binding_summary}'
	assert after_has_min, 'collect2 should discover Tree__min ${sig}; after_specs=${after_specs}'
	assert after_specs.contains('Tree__min=[${sig}]'), 'missing concrete min spec after collect2; after_specs=${after_specs}'
	assert !after_specs.contains('Tree_T_T__min_T'), 'open generic min spec leaked after collect2: ${after_specs}'
	assert !after_specs.contains('unknown__min'), 'unknown min spec leaked after collect2: ${after_specs}'
}

fn test_generic_sumtype_receiver_insert_return_wraps_concrete_variant() {
	checked := mono_check_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: mono_insert_size_source()
		},
	])
	mut trans := checked.trans
	files := trans.transform_files(checked.files)
	fn_names := mono_fn_names(files)
	use_call_names := mono_call_names_for_fn(files, 'use')
	insert_decl := mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'insert_T_f64')
	binding_summary := mono_binding_lookup_summary(trans, [
		'Tree_T_f64__insert_T_f64',
		'insert_T_f64',
		'Tree_T_f64__insert',
	])
	mono_assert_method_clone_by_receiver(files, 'Tree_T_f64', 'size_T_f64')
	insert_returns := mono_return_exprs_for_decl(insert_decl)
	mut assoc_type_summary := []string{}
	for ret_expr in insert_returns {
		if ret_expr is ast.Ident && ret_expr.name.starts_with('_assoc_t') {
			assoc_type_summary << if typ := trans.lookup_var_type(ret_expr.name) {
				'${ret_expr.name}:${trans.type_to_c_name(typ)}'
			} else {
				'${ret_expr.name}:<none>'
			}
		}
	}
	mut node_wrap_count := 0
	mut saw_node_wrap := false
	for ret_expr in insert_returns {
		assert ret_expr !is ast.GenericArgs, 'insert returned open GenericArgs ${ret_expr}'
		assert ret_expr !is ast.GenericArgOrIndexExpr, 'insert returned open GenericArgOrIndexExpr ${ret_expr}'
		assert ret_expr !is ast.Ident || (ret_expr as ast.Ident).name != 'Node_T_f64', 'insert returned raw Node_T_f64 variant instead of Tree_T_f64 wrapper'

		if ret_expr is ast.InitExpr {
			if ret_expr.typ is ast.Ident {
				assert ret_expr.typ.name != 'Node_T_f64', 'insert returned raw Node_T_f64 init instead of Tree_T_f64 wrapper: ${insert_returns}'
			}
		}

		if ret_expr is ast.InitExpr {
			if ret_expr.typ is ast.Ident && ret_expr.typ.name == 'Tree_T_f64' {
				tag_value := mono_init_field(ret_expr, '_tag') or { continue }
				if tag_value is ast.BasicLiteral && tag_value.value == '1' {
					node_wrap_count++
					payload_value := mono_init_field(ret_expr, '_data._Node_T_f64') or { continue }
					if payload_value is ast.CastExpr && payload_value.expr is ast.CallExpr {
						memdup_call := payload_value.expr as ast.CallExpr
						if memdup_call.args.len == 2 && memdup_call.args[0] is ast.PrefixExpr {
							payload_ref := memdup_call.args[0] as ast.PrefixExpr
							if payload_ref.expr is ast.InitExpr {
								mono_assert_sumtype_payload_memdup_expr(payload_value, 'Node_T_f64')
								saw_node_wrap = true
							}
						}
					}
				}
			}
		}
	}
	assert saw_node_wrap, 'Tree_T_f64.insert_T_f64 should wrap Node_T_f64 return with tag 1; return_type=${insert_decl.typ.return_type}; bindings=${binding_summary}; returns=${insert_returns}; fn_names=${fn_names}; use_calls=${use_call_names}'
	assert node_wrap_count >= 3, 'Tree_T_f64.insert_T_f64 should wrap Empty plus both recursive Node branches; got ${node_wrap_count}; assoc_types=${assoc_type_summary}; returns=${insert_returns}'
	for names in [fn_names, use_call_names, mono_call_names_for_decl(insert_decl)] {
		assert 'Tree_T_T__insert_T' !in names
		assert 'Node_T' !in names
		assert 'Node_T_T' !in names
		for name in names {
			assert !name.contains('Tree_T_T'), 'open Tree generic leaked in ${names}'
			assert !name.contains('Node_T_T'), 'open Node generic leaked in ${names}'
		}
	}
}

fn test_shadowed_local_receiver_names_keep_distinct_generic_bindings() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Queue[T] {
mut:
	elements []T
}

fn (queue Queue[T]) is_empty() bool {
	return queue.elements.len == 0
}

fn use_strings() bool {
	mut queue := Queue[[]string]{}
	return queue.is_empty()
}

fn use_ints() bool {
	mut queue := Queue[int]{}
	return queue.is_empty()
}
'
		},
	])
	fn_names := mono_fn_names(files)
	string_call_names := mono_call_names_for_fn(files, 'use_strings')
	int_call_names := mono_call_names_for_fn(files, 'use_ints')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'is_empty')
	mono_assert_method_clone_by_receiver(files, 'Queue_T_int', 'is_empty')
	assert string_call_names == ['Queue_T_Array_string__is_empty'], 'expected shadowed string queue call, got ${string_call_names}'
	assert int_call_names == ['Queue_T_int__is_empty'], 'expected shadowed int queue call, got ${int_call_names}'
	mono_assert_no_open_queue_is_empty_names(fn_names, string_call_names)
	mono_assert_no_open_queue_is_empty_names(fn_names, int_call_names)
}

fn test_explicit_generic_str_interpolation_uses_specialized_method_not_auto_str() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Box[T] {
	value T
}

fn (box Box[T]) str() string {
	return "explicit-box"
}

fn use(value int) string {
	box := Box[int]{value: value}
	return "\${box}"
}
'
		},
	])
	fn_names := mono_fn_names(files)
	box_str := mono_assert_method_clone_by_receiver(files, 'Box_T_int', 'str')
	assert 'Box_T_int__str' !in fn_names, 'auto-str/full-symbol FnDecl should not replace explicit generic method: ${fn_names}'
	assert 'Box__str' !in fn_names
	literals := mono_string_literals_for_decl(box_str)
	assert '"explicit-box"' in literals, 'explicit generic str body was not cloned, got literals ${literals}'
	call_names := mono_call_names_for_fn(files, 'use')
	assert call_names == ['Box_T_int__str'], 'interpolation should call specialized explicit str, got ${call_names}'
	mono_assert_no_open_str_calls(call_names, [
		'Box__str',
		'main__Box__str',
	])
}

fn test_flat_direct_explicit_generic_str_interpolation_uses_specialized_method_not_auto_str() {
	files := mono_transform_sources_flat_direct_for_test([
		MonoSource{
			rel:  'main.v'
			code: '
module main

struct Box[T] {
	value T
}

fn (box Box[T]) str() string {
	return "explicit-box"
}

fn use(value int) string {
	box := Box[int]{value: value}
	return "\${box}"
}
'
		},
	])
	fn_names := mono_fn_names(files)
	box_str := mono_assert_method_clone_by_receiver(files, 'Box_T_int', 'str')
	assert 'Box_T_int__str' !in fn_names, 'auto-str/full-symbol FnDecl should not replace explicit generic method: ${fn_names}'
	assert 'Box__str' !in fn_names
	literals := mono_string_literals_for_decl(box_str)
	assert '"explicit-box"' in literals, 'flat direct explicit generic str body was not cloned, got literals ${literals}'
	call_names := mono_call_names_for_fn(files, 'use')
	assert call_names == ['Box_T_int__str'], 'flat direct interpolation should call specialized explicit str, got ${call_names}'
	mono_assert_no_open_str_calls(call_names, [
		'Box__str',
		'main__Box__str',
	])
}

fn test_nested_generic_explicit_str_rewrites_internal_str_calls() {
	files := mono_transform_sources_for_test([
		MonoSource{
			rel:  'datatypes/node.v'
			code: '
module datatypes

pub struct ListNode[T] {
	value T
}

pub fn (node ListNode[T]) str() string {
	return "node"
}
'
		},
		MonoSource{
			rel:  'datatypes/linked_list.v'
			code: '
module datatypes

pub struct LinkedList[T] {
	head ListNode[T]
}

pub fn (list LinkedList[T]) str() string {
	return "\${list.head}"
}
'
		},
		MonoSource{
			rel:  'datatypes/queue.v'
			code: '
module datatypes

pub struct Queue[T] {
	list LinkedList[T]
}

pub fn (queue Queue[T]) str() string {
	return "\${queue.list}"
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use() string {
	queue := datatypes.Queue[[]string]{}
	return "\${queue}"
}
'
		},
	])
	queue_str := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'str')
	linked_str := mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'str')
	mono_assert_method_clone_by_receiver(files, 'ListNode_T_Array_string', 'str')
	call_names := mono_call_names_for_fn(files, 'use')
	assert call_names == ['datatypes__Queue_T_Array_string__str'], 'interpolation should call specialized Queue str, got ${call_names}'
	mono_assert_no_open_str_calls(call_names, [
		'datatypes__Queue__str',
		'Queue__str',
	])
	queue_body_calls := mono_call_names_for_decl(queue_str)
	linked_body_calls := mono_call_names_for_decl(linked_str)
	assert 'datatypes__LinkedList_T_Array_string__str' in queue_body_calls, 'Queue str body should call specialized LinkedList str, got ${queue_body_calls}'
	mono_assert_no_open_str_calls(queue_body_calls, [
		'datatypes__LinkedList__str',
		'LinkedList__str',
	])
	assert 'datatypes__ListNode_T_Array_string__str' in linked_body_calls, 'LinkedList str body should call specialized ListNode str, got ${linked_body_calls}'
	mono_assert_no_open_str_calls(linked_body_calls, [
		'datatypes__ListNode__str',
		'ListNode__str',
	])
}

fn test_flat_direct_queue_nested_array_str_interpolation_forces_str_cascade() {
	files := mono_transform_sources_flat_direct_for_test([
		MonoSource{
			rel:  'datatypes/linked_list.v'
			code: '
module datatypes

pub struct LinkedList[T] {
mut:
	values []T
}

pub fn (mut list LinkedList[T]) push(value T) {
	list.values << value
}

pub fn (list LinkedList[T]) array() []T {
	return list.values
}

pub fn (list LinkedList[T]) str() string {
	return list.array().str()
}
'
		},
		MonoSource{
			rel:  'datatypes/queue.v'
			code: '
module datatypes

pub struct Queue[T] {
mut:
	elements LinkedList[T]
}

pub fn (mut queue Queue[T]) push(value T) {
	queue.elements.push(value)
}

pub fn (queue Queue[T]) str() string {
	return queue.elements.str()
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use(value []string) string {
	mut queue := datatypes.Queue[[]string]{}
	queue.push(value)
	return "\${queue}"
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	queue_push := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'push')
	queue_str := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'str')
	linked_push := mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'push')
	linked_str := mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'str')
	mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'array')
	assert 'datatypes__Queue_T_Array_string__str' !in fn_names, 'auto-str/full-symbol FnDecl should not replace explicit Queue.str: ${fn_names}'
	assert call_names == [
		'datatypes__Queue_T_Array_string__push',
		'datatypes__Queue_T_Array_string__str',
	], 'flat direct Queue interpolation should call specialized explicit str without a prior queue.str(), got ${call_names}'
	queue_push_calls := mono_call_names_for_decl(queue_push)
	queue_str_calls := mono_call_names_for_decl(queue_str)
	linked_push_calls := mono_call_names_for_decl(linked_push)
	linked_str_calls := mono_call_names_for_decl(linked_str)
	assert 'datatypes__LinkedList_T_Array_string__push' in queue_push_calls, 'Queue.push body should specialize LinkedList.push, got ${queue_push_calls}'
	assert 'datatypes__LinkedList_T_Array_string__str' in queue_str_calls, 'Queue.str body should specialize LinkedList.str, got ${queue_str_calls}'
	assert 'datatypes__LinkedList_T_Array_string__array' in linked_str_calls, 'LinkedList.str body should specialize LinkedList.array, got ${linked_str_calls}'
	assert 'Array_Array_string_str' in linked_str_calls, 'LinkedList.str body should call nested array str helper, got ${linked_str_calls}'
	mono_assert_no_unresolved_generic_receiver_names(fn_names, 'flat direct Queue str fn decls')
	mono_assert_no_unresolved_generic_receiver_names(call_names,
		'flat direct Queue interpolation body')
	mono_assert_no_unresolved_generic_receiver_names(queue_push_calls,
		'flat direct Queue.push body')
	mono_assert_no_unresolved_generic_receiver_names(queue_str_calls, 'flat direct Queue.str body')
	mono_assert_no_unresolved_generic_receiver_names(linked_push_calls,
		'flat direct LinkedList.push body')
	mono_assert_no_unresolved_generic_receiver_names(linked_str_calls,
		'flat direct LinkedList.str body')
	mono_assert_no_open_str_calls(call_names, [
		'datatypes__Queue__str',
		'Queue__str',
	])
	mono_assert_no_open_str_calls(queue_str_calls, [
		'datatypes__LinkedList__str',
		'LinkedList__str',
	])
}

fn test_flat_direct_imported_generic_receiver_clone_body_keeps_bindings() {
	files := mono_transform_sources_flat_direct_for_test([
		MonoSource{
			rel:  'datatypes/linked_list.v'
			code: '
module datatypes

pub struct LinkedList[T] {
mut:
	values []T
}

pub fn (mut list LinkedList[T]) push(value T) {
	list.values << value
}

pub fn (list LinkedList[T]) is_empty() bool {
	return list.values.len == 0
}

pub fn (mut list LinkedList[T]) shift() !T {
	return list.values[0]
}
'
		},
		MonoSource{
			rel:  'datatypes/queue.v'
			code: '
module datatypes

pub struct Queue[T] {
mut:
	elements LinkedList[T]
}

pub fn (mut queue Queue[T]) push(value T) {
	queue.elements.push(value)
}

pub fn (queue Queue[T]) is_empty() bool {
	return queue.elements.is_empty()
}

pub fn (mut queue Queue[T]) pop() !T {
	return queue.elements.shift()
}
'
		},
		MonoSource{
			rel:  'main.v'
			code: '
module main

import datatypes

fn use(value []string) bool {
	mut queue := datatypes.Queue[[]string]{}
	queue.push(value)
	if queue.is_empty() {
		return false
	}
	popped := queue.pop() or { return false }
	return popped.len == 1
}
'
		},
	])
	fn_names := mono_fn_names(files)
	call_names := mono_call_names_for_fn(files, 'use')
	queue_push := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'push')
	queue_is_empty := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string',
		'is_empty')
	queue_pop := mono_assert_method_clone_by_receiver(files, 'Queue_T_Array_string', 'pop')
	mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'push')
	mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'is_empty')
	mono_assert_method_clone_by_receiver(files, 'LinkedList_T_Array_string', 'shift')
	assert call_names == [
		'datatypes__Queue_T_Array_string__push',
		'datatypes__Queue_T_Array_string__is_empty',
		'datatypes__Queue_T_Array_string__pop',
	], 'flat direct should specialize imported Queue[[]string] calls, got ${call_names}'
	push_body_calls := mono_call_names_for_decl(queue_push)
	is_empty_body_calls := mono_call_names_for_decl(queue_is_empty)
	pop_body_calls := mono_call_names_for_decl(queue_pop)
	assert 'datatypes__LinkedList_T_Array_string__push' in push_body_calls, 'Queue.push body should specialize LinkedList.push, got ${push_body_calls}'
	assert 'datatypes__LinkedList_T_Array_string__is_empty' in is_empty_body_calls, 'Queue.is_empty body should specialize LinkedList.is_empty, got ${is_empty_body_calls}'
	assert 'datatypes__LinkedList_T_Array_string__shift' in pop_body_calls, 'Queue.pop body should specialize LinkedList.shift, got ${pop_body_calls}'
	mono_assert_no_unresolved_generic_receiver_names(fn_names, 'flat direct fn decls')
	mono_assert_no_unresolved_generic_receiver_names(call_names, 'flat direct use body')
	mono_assert_no_unresolved_generic_receiver_names(push_body_calls, 'flat direct Queue.push body')
	mono_assert_no_unresolved_generic_receiver_names(is_empty_body_calls,
		'flat direct Queue.is_empty body')
	mono_assert_no_unresolved_generic_receiver_names(pop_body_calls, 'flat direct Queue.pop body')
}

fn test_transform_dijkstra_shape_rewrites_inferred_generic_nested_array_call() {
	files := mono_transform_dijkstra_shape_for_test()
	call_names := mono_call_names_for_fn(files, 'dijkstra')
	assert 'all_adjacents_T_int' in call_names
	assert 'all_adjacents' !in call_names
	assert mono_has_fn(files, 'all_adjacents_T_int')
}

fn test_inferred_generic_call_name_for_nested_array_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
				ast.Parameter{
					name: 'v'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
			]
		}
	}
	t.local_decl_types['g'] = types.Type(types.Array{
		elem_type: types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
	})
	info := CallFnInfo{
		param_types:                    [
			types.Type(types.Array{
				elem_type: types.Type(types.Array{
					elem_type: types.Type(types.NamedType('T'))
				})
			}),
			types.Type(types.int_),
		]
		generic_param_names_by_param:   ['T', '']
		generic_param_indexes_by_param: [0, -1]
		generic_params:                 ['T']
	}
	call_name := t.inferred_generic_call_name('all_adjacents', info, [
		ast.Expr(ast.Ident{
			name: 'g'
		}),
		ast.Expr(ast.Ident{
			name: 'v'
		}),
	]) or { panic('missing inferred generic call name') }
	assert call_name == 'all_adjacents_T_int'
}

fn test_generic_call_info_preserves_nested_array_generic_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
			]
		}
	}
	info := t.generic_call_info_for_decl('all_adjacents') or { panic('missing generic call info') }
	assert info.param_types.len == 1
	outer := info.param_types[0] as types.Array
	inner := outer.elem_type as types.Array
	assert inner.elem_type.name() == 'T'
}

fn test_transform_rewrites_inferred_generic_call_for_nested_array_param() {
	mut t := mono_test_transformer()
	t.cur_module = 'main'
	t.generic_fn_decl_index['all_adjacents'] = ast.FnDecl{
		name: 'all_adjacents'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'g'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}))
					}))
				},
				ast.Parameter{
					name: 'v'
					typ:  ast.Expr(ast.Ident{
						name: 'int'
					})
				},
			]
		}
	}
	t.local_decl_types['g'] = types.Type(types.Array{
		elem_type: types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
	})
	t.local_decl_types['v'] = types.Type(types.int_)
	out := t.transform_call_expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'all_adjacents'
		}
		args: [
			ast.Expr(ast.Ident{
				name: 'g'
			}),
			ast.Expr(ast.Ident{
				name: 'v'
			}),
		]
	})
	assert out is ast.CallExpr
	call := out as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'all_adjacents_T_int'
}

fn test_clone_fn_decl_substitutes_fn_literal_signature() {
	mut t := mono_test_transformer()
	decl := ast.FnDecl{
		name:  'make'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.FnLiteral{
						typ:   ast.FnType{
							params:      [
								ast.Parameter{
									name: 'x'
									typ:  ast.Expr(ast.Ident{
										name: 'T'
									})
								},
							]
							return_type: ast.Expr(ast.Ident{
								name: 'T'
							})
						}
						stmts: [
							ast.Stmt(ast.ReturnStmt{
								exprs: [
									ast.Expr(ast.Ident{
										name: 'x'
									}),
								]
							}),
						]
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.int_)
	}, 'make_T_int', 'main', 'main')
	ret := cloned.stmts[0] as ast.ReturnStmt
	lit := ret.exprs[0] as ast.FnLiteral
	param_typ := lit.typ.params[0].typ as ast.Ident
	return_typ := lit.typ.return_type as ast.Ident

	assert param_typ.name == 'int'
	assert return_typ.name == 'int'
}

fn test_monomorphize_pass_keeps_imported_clone_in_declaring_file() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	bindings := {
		'T': types.Type(types.string_)
	}
	t.env.generic_types['m__wrap'] = [bindings]
	t.generic_spec_owner_file[generic_spec_owner_key('m__wrap', bindings)] = 1
	helper_decl := ast.FnDecl{
		name: 'helper'
		typ:  ast.FnType{
			return_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}
	}
	decl := ast.FnDecl{
		name:  'wrap'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'int'
			})
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Expr(ast.Ident{
							name: 'helper'
						})
					}),
				]
			}),
		]
	}
	files := [
		ast.File{
			mod:   'm'
			stmts: [ast.Stmt(helper_decl), ast.Stmt(decl)]
		},
		ast.File{
			mod: 'main'
		},
	]
	out := t.monomorphize_pass(files)
	assert out[0].stmts.len == 3
	assert out[1].stmts.len == 0
	assert out[0].stmts[2] is ast.FnDecl
	cloned := out[0].stmts[2] as ast.FnDecl
	assert cloned.name == 'm__wrap_T_string'
	ret := cloned.stmts[0] as ast.ReturnStmt
	call := ret.exprs[0] as ast.CallExpr
	assert call.lhs is ast.Ident
	assert (call.lhs as ast.Ident).name == 'helper'
}

fn test_register_generic_bindings_keeps_first_owner_for_duplicate_spec() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	bindings := {
		'T': types.Type(types.string_)
	}
	t.cur_generic_call_file_idx = 0
	t.register_generic_bindings('json2__Encoder__encode_value', bindings)
	t.cur_generic_call_file_idx = 1
	t.register_generic_bindings('json2__Encoder__encode_value', bindings)
	owner_key := generic_spec_owner_key('json2__Encoder__encode_value', bindings)
	owner := t.generic_spec_owner_file[owner_key] or { -1 }
	assert owner == 0
}

fn test_generic_call_concrete_return_type_prefers_substitution_over_stale_pos_type() {
	mut env := types.Environment.new()
	env.set_expr_type(77, types.Type(types.int_))
	mut t := mono_test_transformer()
	t.env = env
	t.generic_fn_decl_index['identity'] = ast.FnDecl{
		name: 'identity'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'identity'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0.0'
			}),
		]
		pos:  token.Pos{
			id: 77
		}
	})
	ret := t.get_expr_type(call) or { panic('missing generic call return type') }
	assert ret is types.Primitive
	assert (ret as types.Primitive).props.has(types.Properties.float)
}

fn test_generic_call_concrete_return_type_finds_current_module_bare_call() {
	mut t := mono_test_transformer()
	t.cur_module = 'math'
	t.generic_fn_decl_index['math__identity'] = ast.FnDecl{
		name: 'identity'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	call := ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'identity'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0.0'
			}),
		]
	})
	ret := t.generic_call_concrete_return_type(call) or {
		panic('missing current-module generic return type')
	}
	assert ret is types.Primitive
	assert (ret as types.Primitive).props.has(types.Properties.float)
}

fn test_get_expr_type_prefers_local_decl_cache_over_stale_scope_type() {
	mut t := mono_test_transformer()
	mut scope := types.new_scope(unsafe { nil })
	scope.insert('ret', types.Type(types.int_))
	t.scope = scope
	t.local_decl_types['ret'] = types.Type(types.f64_)
	typ := t.get_expr_type(ast.Expr(ast.Ident{
		name: 'ret'
	})) or { panic('missing ident type') }
	assert typ.name() == 'f64'
}

fn test_clone_generic_type_name_selector_to_string_literal() {
	mut t := mono_test_transformer()
	expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'T'
		}
		rhs: ast.Ident{
			name: 'name'
		}
		pos: token.Pos{
			id: 11
		}
	})
	out := t.clone_expr_with_bindings_and_fields(expr, {
		'T': types.Type(types.i64_)
	}, []CloneComptimeFieldCtx{})
	assert out is ast.StringLiteral
	assert (out as ast.StringLiteral).value == 'i64'
}

fn test_substitute_type_leaves_non_matching_named_type_alone() {
	bindings := {
		'T': types.Type(types.int_)
	}
	out := substitute_type(types.Type(types.NamedType('U')), bindings)
	assert out.name() == 'U'
}

// substitute_type: compound recursion

fn test_substitute_type_recurses_into_pointer_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Pointer{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Pointer {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected Pointer, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_array_elem() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		assert out.elem_type.name() == 'int'
	} else {
		assert false, 'expected Array, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_map_key_and_value() {
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	original := types.Type(types.Map{
		key_type:   types.Type(types.NamedType('K'))
		value_type: types.Type(types.NamedType('V'))
	})
	out := substitute_type(original, bindings)
	if out is types.Map {
		assert out.key_type.name() == 'string'
		assert out.value_type.name() == 'int'
	} else {
		assert false, 'expected Map, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_option_base() {
	bindings := {
		'T': types.Type(types.int_)
	}
	original := types.Type(types.OptionType{
		base_type: types.Type(types.NamedType('T'))
	})
	out := substitute_type(original, bindings)
	if out is types.OptionType {
		assert out.base_type.name() == 'int'
	} else {
		assert false, 'expected OptionType, got ${out.type_name()}'
	}
}

fn test_substitute_type_recurses_into_nested_array_pointer() {
	bindings := {
		'T': types.Type(types.f64_)
	}
	original := types.Type(types.Array{
		elem_type: types.Type(types.Pointer{
			base_type: types.Type(types.NamedType('T'))
		})
	})
	out := substitute_type(original, bindings)
	if out is types.Array {
		elem := out.elem_type
		if elem is types.Pointer {
			assert elem.base_type.name() == 'f64'
		} else {
			assert false, 'expected nested Pointer, got ${elem.type_name()}'
		}
	} else {
		assert false, 'expected outer Array, got ${out.type_name()}'
	}
}

// substitute_type_in_expr: AST-level

fn test_substitute_type_in_expr_replaces_placeholder_ident() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Ident{
		name: 'T'
	})
	out := t.substitute_type_in_expr(expr, bindings)
	if out is ast.Ident {
		assert out.name == 'int'
	} else {
		assert false, 'expected Ident, got ${out.type_name()}'
	}
}

fn test_substitute_type_in_expr_recurses_into_array_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'T'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	at := out_ty as ast.ArrayType
	elem := at.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_substitute_type_in_expr_recurses_into_map_type() {
	mut t := mono_test_transformer()
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	expr := ast.Expr(ast.Type(ast.MapType{
		key_type:   ast.Expr(ast.Ident{
			name: 'K'
		})
		value_type: ast.Expr(ast.Ident{
			name: 'V'
		})
	}))
	out := t.substitute_type_in_expr(expr, bindings)
	out_ty := out as ast.Type
	mt := out_ty as ast.MapType
	key := mt.key_type as ast.Ident
	val := mt.value_type as ast.Ident
	assert key.name == 'string'
	assert val.name == 'int'
}

fn test_substitute_type_in_expr_recurses_into_generic_index_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	expr := ast.Expr(ast.GenericArgOrIndexExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'AtomicVal'
		})
		expr: ast.Expr(ast.Ident{
			name: 'T'
		})
	})
	out := t.substitute_type_in_expr(expr, bindings)
	gen := out as ast.GenericArgOrIndexExpr
	arg := gen.expr as ast.Ident
	assert arg.name == 'int'
}

// specialized_fn_name: matches cleanc's naming

fn test_specialized_fn_name_uses_all_placeholders_suffix_when_concrete_is_self() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.NamedType('T'))
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T'
}

fn test_specialized_fn_name_uses_concrete_token_suffix() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'foo'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
	}
	bindings := {
		'T': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'foo_T_int'
}

fn test_specialized_fn_name_handles_multiple_params() {
	t := mono_test_transformer()
	decl := ast.FnDecl{
		name: 'pair'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'K'
				}),
				ast.Expr(ast.Ident{
					name: 'V'
				}),
			]
		}
	}
	bindings := {
		'K': types.Type(types.string_)
		'V': types.Type(types.int_)
	}
	assert t.specialized_fn_name(decl, bindings) == 'pair_T_string_int'
}

fn test_clone_fn_decl_tracks_local_decl_types_for_interpolation_repair() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	node_type := types.Type(types.Struct{
		name:   'json2__Node'
		fields: [
			types.Field{
				name: 'value'
				typ:  value_info_type
			},
		]
	})
	decoder_type := types.Type(types.Struct{
		name:   'json2__Decoder'
		fields: [
			types.Field{
				name: 'current_node'
				typ:  types.Type(types.Pointer{
					base_type: node_type
				})
			},
		]
	})
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	t.cur_module = 'json2'
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('Decoder', decoder_type)
	json2_scope.insert_type('Node', node_type)
	json2_scope.insert_type('ValueInfo', value_info_type)
	t.cached_scopes['json2'] = json2_scope
	t.needed_str_fns = map[string]string{}
	t.needed_enum_str_fns = map[string]types.Enum{}
	value_kind_expr := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'string_info'
		})
		rhs: ast.Ident{
			name: 'value_kind'
		}
	})
	stale_arg := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.Ident{
				name: 'string__str'
			})
			args: [value_kind_expr]
		})
		rhs: ast.Ident{
			name: 'str'
		}
	})
	decl := ast.FnDecl{
		name:      'decode_string'
		is_method: true
		receiver:  ast.Parameter{
			name: 'decoder'
			typ:  ast.Expr(ast.Ident{
				name: 'Decoder'
			})
		}
		typ:       ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts:     [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'string_info'
					}),
				]
				rhs: [
					ast.Expr(ast.SelectorExpr{
						lhs: ast.Expr(ast.SelectorExpr{
							lhs: ast.Expr(ast.Ident{
								name: 'decoder'
							})
							rhs: ast.Ident{
								name: 'current_node'
							}
						})
						rhs: ast.Ident{
							name: 'value'
						}
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.StringInterLiteral{
					values: [
						"'Expected string, but got ",
						"'",
					]
					inters: [
						ast.StringInter{
							expr: stale_arg
						},
					]
				})
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'decode_string_T_string', 'json2', 'main')
	_ = t.env.get_fn_scope('main', 'Decoder__decode_string_T_string') or {
		panic('missing target module cloned function scope')
	}
	expr_stmt := cloned.stmts[1] as ast.ExprStmt
	lit := expr_stmt.expr as ast.StringInterLiteral
	repaired := lit.inters[0].expr
	assert repaired is ast.SelectorExpr
	repaired_call := (repaired as ast.SelectorExpr).lhs
	assert repaired_call is ast.CallExpr
	repaired_lhs := (repaired_call as ast.CallExpr).lhs
	assert repaired_lhs is ast.Ident
	assert (repaired_lhs as ast.Ident).name == 'json2__ValueKind__str'
}

fn test_moved_clone_qualifies_source_module_type_positions() {
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('DecoderOptions', types.Type(types.Struct{
		name: 'json2__DecoderOptions'
	}))
	json2_scope.insert_type('Decoder', types.Type(types.Struct{
		name: 'json2__Decoder'
	}))
	json2_scope.insert_type('StructFieldInfo', types.Type(types.Struct{
		name: 'json2__StructFieldInfo'
	}))
	t.cached_scopes['json2'] = json2_scope
	decl := ast.FnDecl{
		name:  'decode'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'val'
					typ:  ast.Expr(ast.Ident{
						name: 'string'
					})
				},
				ast.Parameter{
					name: 'params'
					typ:  ast.Expr(ast.Ident{
						name: 'DecoderOptions'
					})
				},
			]
			return_type:    ast.Expr(ast.Type(ast.ResultType{
				base_type: ast.Expr(ast.Ident{
					name: 'T'
				})
			}))
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'decoder'
					}),
				]
				rhs: [
					ast.Expr(ast.InitExpr{
						typ: ast.Expr(ast.Ident{
							name: 'Decoder'
						})
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.ArrayInitExpr{
					typ: ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'StructFieldInfo'
						})
					}))
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.Struct{
			name: 'api__ApiBranchCount'
		})
	}, 'json2__decode_T_api_ApiBranchCount', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	options_type := cloned.typ.params[1].typ as ast.Ident
	assert options_type.name == 'json2__DecoderOptions'
	assign := cloned.stmts[0] as ast.AssignStmt
	init := assign.rhs[0] as ast.InitExpr
	init_type := init.typ as ast.Ident
	assert init_type.name == 'json2__Decoder'
	expr_stmt := cloned.stmts[1] as ast.ExprStmt
	array_init := expr_stmt.expr as ast.ArrayInitExpr
	array_type := array_init.typ as ast.Type
	array := array_type as ast.ArrayType
	elem := array.elem_type as ast.Ident
	assert elem.name == 'json2__StructFieldInfo'
}

fn test_moved_clone_qualifies_source_module_generic_function_value() {
	mut t := mono_test_transformer()
	handler_decl := ast.FnDecl{
		name: 'parallel_request_handler'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
	}
	t.generic_fn_decl_index['parallel_request_handler'] = handler_decl
	t.generic_fn_decl_index['json2__parallel_request_handler'] = handler_decl
	decl := ast.FnDecl{
		name:  'run_new'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.InitExpr{
					typ:    ast.Expr(ast.Ident{
						name: 'ServerConfig'
					})
					fields: [
						ast.FieldInit{
							name:  'handler'
							value: ast.Expr(ast.GenericArgs{
								lhs:  ast.Expr(ast.Ident{
									name: 'parallel_request_handler'
								})
								args: [
									ast.Expr(ast.Ident{
										name: 'A'
									}),
									ast.Expr(ast.Ident{
										name: 'X'
									}),
								]
							})
						},
					]
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'A': types.Type(types.Struct{
			name: 'main__App'
		})
		'X': types.Type(types.Struct{
			name: 'main__Context'
		})
	}, 'json2__run_new_T_App_Context', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	expr_stmt := cloned.stmts[0] as ast.ExprStmt
	init := expr_stmt.expr as ast.InitExpr
	handler := init.fields[0].value as ast.Ident
	assert handler.name == 'json2__parallel_request_handler_T_main_App_main_Context'
}

fn test_moved_clone_qualifies_source_module_generic_function_value_index_expr() {
	mut t := mono_test_transformer()
	handler_decl := ast.FnDecl{
		name: 'parallel_request_handler'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
	}
	t.generic_fn_decl_index['parallel_request_handler'] = handler_decl
	t.generic_fn_decl_index['json2__parallel_request_handler'] = handler_decl
	decl := ast.FnDecl{
		name:  'run_new'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'A'
				}),
				ast.Expr(ast.Ident{
					name: 'X'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.InitExpr{
					typ:    ast.Expr(ast.Ident{
						name: 'ServerConfig'
					})
					fields: [
						ast.FieldInit{
							name:  'handler'
							value: ast.Expr(ast.IndexExpr{
								lhs:  ast.Expr(ast.GenericArgOrIndexExpr{
									lhs:  ast.Expr(ast.Ident{
										name: 'parallel_request_handler'
									})
									expr: ast.Expr(ast.Ident{
										name: 'A'
									})
								})
								expr: ast.Expr(ast.Ident{
									name: 'X'
								})
							})
						},
					]
				})
			}),
		]
	}
	mut cloned := t.clone_fn_decl_with_substitutions(decl, {
		'A': types.Type(types.Struct{
			name: 'main__App'
		})
		'X': types.Type(types.Struct{
			name: 'main__Context'
		})
	}, 'json2__run_new_T_App_Context', 'json2', 'main')
	cloned = t.qualify_moved_clone_source_module_types(cloned, 'json2')
	expr_stmt := cloned.stmts[0] as ast.ExprStmt
	init := expr_stmt.expr as ast.InitExpr
	handler := init.fields[0].value as ast.Ident
	assert handler.name == 'json2__parallel_request_handler_T_main_App_main_Context'
}

fn test_collect_generic_struct_field_type_resolves_prefix_pointer_generic_arg() {
	value_kind_type := types.Type(types.Enum{
		name: 'json2__ValueKind'
	})
	value_info_type := types.Type(types.Struct{
		name:   'json2__ValueInfo'
		fields: [
			types.Field{
				name: 'value_kind'
				typ:  value_kind_type
			},
		]
	})
	node_type := types.Type(types.Struct{
		name:           'json2__Node'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'value'
				typ:  types.Type(types.NamedType('T'))
			},
		]
	})
	mut t := mono_test_transformer()
	t.env = types.Environment.new()
	t.cur_module = 'json2'
	mut json2_scope := types.new_scope(unsafe { nil })
	json2_scope.insert_type('Node', node_type)
	json2_scope.insert_type('ValueInfo', value_info_type)
	t.cached_scopes['json2'] = json2_scope
	t.collect_struct_decl_generic_field_types(ast.StructDecl{
		name:   'Decoder'
		fields: [
			ast.FieldDecl{
				name: 'current_node'
				typ:  ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Expr(ast.GenericArgOrIndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'Node'
						})
						expr: ast.Expr(ast.Ident{
							name: 'ValueInfo'
						})
					})
				})
			},
		]
	}, 'json2')
	field_typ := t.lookup_struct_field_generic_decl_type('json2__Decoder', 'current_node') or {
		panic('missing concrete current_node type')
	}
	assert field_typ is types.Pointer
	node_concrete := (field_typ as types.Pointer).base_type
	assert node_concrete is types.Struct
	value_field := struct_field_by_name(node_concrete as types.Struct, 'value') or {
		panic('missing Node.value field')
	}
	assert value_field.typ.name() == 'json2__ValueInfo'
}

// clone_fn_decl_with_substitutions: end-to-end on a trivial generic fn

fn test_clone_fn_decl_renames_and_substitutes_param_and_return_types() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	body_pos := token.Pos{
		id: 1
	}
	decl := ast.FnDecl{
		name:  'identity'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'x'
					typ:  ast.Expr(ast.Ident{
						name: 'T'
					})
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.Ident{
						name: 'x'
						pos:  body_pos
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'identity_T_int', '', '')
	assert cloned.name == 'identity_T_int'
	assert cloned.typ.generic_params.len == 0
	assert cloned.typ.params.len == 1
	param_typ := cloned.typ.params[0].typ as ast.Ident
	assert param_typ.name == 'int'
	ret_typ := cloned.typ.return_type as ast.Ident
	assert ret_typ.name == 'int'
	// Body is preserved structurally (x is a value identifier, not a type — stays 'x')
	assert cloned.stmts.len == 1
	if cloned.stmts[0] is ast.ReturnStmt {
		ret_stmt := cloned.stmts[0] as ast.ReturnStmt
		ret_ident := ret_stmt.exprs[0] as ast.Ident
		assert ret_ident.name == 'x'
	} else {
		assert false, 'expected ReturnStmt'
	}
}

fn test_clone_fn_decl_substitutes_inside_array_param_type() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name: 'first'
		typ:  ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'arr'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
			return_type:    ast.Expr(ast.Ident{
				name: 'T'
			})
		}
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'first_T_int', '', '')
	assert cloned.name == 'first_T_int'
	param_typ := cloned.typ.params[0].typ as ast.Type
	arr_typ := param_typ as ast.ArrayType
	elem := arr_typ.elem_type as ast.Ident
	assert elem.name == 'int'
}

fn test_clone_fn_decl_registers_array_index_decl_type_in_clone_scope() {
	mut t := mono_test_transformer()
	value_pos := token.Pos{
		id: 801
	}
	index_pos := token.Pos{
		id: 802
	}
	first_lhs_pos := token.Pos{
		id: 803
	}
	t.env.set_expr_type(value_pos.id, types.Type(types.Array{
		elem_type: types.Type(types.f64_)
	}))
	t.env.set_expr_type(index_pos.id, types.Type(types.f64_))
	decl := ast.FnDecl{
		name:  'first'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'value'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [
					ast.Expr(ast.Ident{
						name: 'first'
						pos:  first_lhs_pos
					}),
				]
				rhs: [
					ast.Expr(ast.IndexExpr{
						lhs:  ast.Expr(ast.Ident{
							name: 'value'
							pos:  value_pos
						})
						expr: ast.Expr(ast.BasicLiteral{
							kind:  .number
							value: '0'
						})
						pos:  index_pos
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'first_T_string', 'orm', 'main')
	scope := t.env.get_fn_scope('orm', 'first_T_string') or {
		panic('missing cloned function scope')
	}
	first_type := scope.lookup_var_type('first') or { panic('missing first type') }
	assert first_type is types.String
	stmt := cloned.stmts[0] as ast.AssignStmt
	lhs := stmt.lhs[0] as ast.Ident
	lhs_type := t.synth_types[lhs.pos.id] or { panic('missing first synth type') }
	assert lhs_type is types.String
}

fn test_clone_fn_decl_registers_nested_array_index_decl_type_in_clone_scope() {
	mut t := mono_test_transformer()
	value_pos := token.Pos{
		id: 821
	}
	index_pos := token.Pos{
		id: 822
	}
	item_lhs_pos := token.Pos{
		id: 823
	}
	t.env.set_expr_type(value_pos.id, types.Type(types.Array{
		elem_type: types.Type(types.f64_)
	}))
	t.env.set_expr_type(index_pos.id, types.Type(types.f64_))
	decl := ast.FnDecl{
		name:  'first_nested'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
			params:         [
				ast.Parameter{
					name: 'value'
					typ:  ast.Expr(ast.Type(ast.ArrayType{
						elem_type: ast.Expr(ast.Ident{
							name: 'T'
						})
					}))
				},
			]
		}
		stmts: [
			ast.Stmt(ast.ForStmt{
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [
							ast.Expr(ast.Ident{
								name: 'item'
								pos:  item_lhs_pos
							}),
						]
						rhs: [
							ast.Expr(ast.IndexExpr{
								lhs:  ast.Expr(ast.Ident{
									name: 'value'
									pos:  value_pos
								})
								expr: ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '0'
								})
								pos:  index_pos
							}),
						]
					}),
				]
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, {
		'T': types.Type(types.string_)
	}, 'first_nested_T_string', 'orm', 'main')
	scope := t.env.get_fn_scope('orm', 'first_nested_T_string') or {
		panic('missing cloned function scope')
	}
	item_type := scope.lookup_var_type('item') or { panic('missing item type') }
	assert item_type is types.String
	for_stmt := cloned.stmts[0] as ast.ForStmt
	stmt := for_stmt.stmts[0] as ast.AssignStmt
	lhs := stmt.lhs[0] as ast.Ident
	lhs_type := t.synth_types[lhs.pos.id] or { panic('missing item synth type') }
	assert lhs_type is types.String
}

fn test_clone_fn_decl_preserves_unrelated_stmts() {
	mut t := mono_test_transformer()
	bindings := {
		'T': types.Type(types.int_)
	}
	decl := ast.FnDecl{
		name:  'noop'
		typ:   ast.FnType{
			generic_params: [
				ast.Expr(ast.Ident{
					name: 'T'
				}),
			]
		}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '42'
				})
			}),
		]
	}
	cloned := t.clone_fn_decl_with_substitutions(decl, bindings, 'noop_T_int', '', '')
	assert cloned.stmts.len == 1
	es := cloned.stmts[0] as ast.ExprStmt
	lit := es.expr as ast.BasicLiteral
	assert lit.value == '42'
}

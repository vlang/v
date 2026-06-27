module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn transformer_with_x64_target(target_os string) &Transformer {
	env := &types.Environment{}
	prefs := &vpref.Preferences{
		backend:   .x64
		arch:      .x64
		target_os: target_os
	}
	return Transformer.new_with_pref(env, prefs)
}

fn transformer_with_macos_tiny_candidate_graph() &Transformer {
	mut t := transformer_with_x64_target('macos')
	t.enable_macos_tiny_candidate_graph()
	return t
}

fn runtime_init_call_names(stmts []ast.Stmt) []string {
	mut names := []string{}
	for stmt in stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.CallExpr {
				if stmt.expr.lhs is ast.Ident {
					names << stmt.expr.lhs.name
				}
			}
		}
	}
	return names
}

fn runtime_init_call_names_from_flat(mut t Transformer, files []ast.File) []string {
	flat := ast.flatten_files(files)
	return runtime_init_call_names(t.runtime_const_init_main_calls_parts_from_flat(&flat))
}

fn runtime_init_test_files(main_imports []ast.ImportStmt) []ast.File {
	return [
		ast.File{
			mod:     'main'
			imports: main_imports
			stmts:   [
				ast.Stmt(ast.FnDecl{
					name: 'main'
				}),
			]
		},
		ast.File{
			mod:   'os'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'init'
				}),
			]
		},
	]
}

fn runtime_init_test_files_with_main_extra_stmts(extra_stmts []ast.Stmt) []ast.File {
	mut files := runtime_init_test_files([]ast.ImportStmt{})
	mut main_stmts := []ast.Stmt{cap: extra_stmts.len + files[0].stmts.len}
	main_stmts << extra_stmts
	main_stmts << files[0].stmts
	files[0] = ast.File{
		mod:   files[0].mod
		stmts: main_stmts
	}
	return files
}

fn runtime_init_import_stmt(name string) ast.Stmt {
	return ast.Stmt(ast.ImportStmt{
		name: name
	})
}

fn runtime_init_comptime_if_import_stmt(cond_name string, import_name string) ast.Stmt {
	return ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.ComptimeExpr{
			expr: ast.Expr(ast.IfExpr{
				cond:  ast.Expr(ast.Ident{
					name: cond_name
				})
				stmts: [runtime_init_import_stmt(import_name)]
			})
		})
	})
}

fn runtime_init_comptime_else_import_stmt(cond_name string, import_name string) ast.Stmt {
	return ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.ComptimeExpr{
			expr: ast.Expr(ast.IfExpr{
				cond:      ast.Expr(ast.Ident{
					name: cond_name
				})
				else_expr: ast.Expr(ast.IfExpr{
					cond:  ast.empty_expr
					stmts: [runtime_init_import_stmt(import_name)]
				})
			})
		})
	})
}

fn seed_runtime_const_init_names(mut t Transformer) {
	t.runtime_const_modules = ['os', 'main']
	t.runtime_const_init_fn_name['os'] = '__v_init_consts_os'
	t.runtime_const_init_fn_name['main'] = '__v_init_consts_main'
}

fn test_linux_minimal_runtime_filters_unimported_runtime_init_calls() {
	mut t := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t)
	names :=
		runtime_init_call_names(t.runtime_const_init_main_calls_parts(runtime_init_test_files([]ast.ImportStmt{})))
	assert names == ['__v_init_consts_main']
}

fn test_linux_minimal_runtime_keeps_imported_runtime_init_calls() {
	mut t := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names(t.runtime_const_init_main_calls_parts(runtime_init_test_files([
		ast.ImportStmt{
			name: 'os'
		},
	])))
	assert names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
}

fn test_linux_minimal_runtime_filters_unimported_runtime_init_calls_from_flat() {
	mut t := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names_from_flat(mut t, runtime_init_test_files([]ast.ImportStmt{}))
	assert names == ['__v_init_consts_main']
}

fn test_linux_minimal_runtime_keeps_imported_runtime_init_calls_from_flat() {
	mut t := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names_from_flat(mut t, runtime_init_test_files([
		ast.ImportStmt{
			name: 'os'
		},
	]))
	assert names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
}

fn test_macos_tiny_candidate_filters_unimported_runtime_init_calls() {
	mut t := transformer_with_macos_tiny_candidate_graph()
	seed_runtime_const_init_names(mut t)
	names :=
		runtime_init_call_names(t.runtime_const_init_main_calls_parts(runtime_init_test_files([]ast.ImportStmt{})))
	assert names == ['__v_init_consts_main']
}

fn test_macos_tiny_candidate_keeps_imported_runtime_init_calls() {
	mut t := transformer_with_macos_tiny_candidate_graph()
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names(t.runtime_const_init_main_calls_parts(runtime_init_test_files([
		ast.ImportStmt{
			name: 'os'
		},
	])))
	assert names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
}

fn test_linux_minimal_runtime_keeps_active_comptime_imports_from_flat() {
	files := runtime_init_test_files_with_main_extra_stmts([
		runtime_init_comptime_if_import_stmt('linux', 'os'),
	])
	mut t_legacy := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t_legacy)
	mut t_flat := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t_flat)

	legacy_names := runtime_init_call_names(t_legacy.runtime_const_init_main_calls_parts(files))
	flat_names := runtime_init_call_names_from_flat(mut t_flat, files)
	assert legacy_names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
	assert flat_names == legacy_names
}

fn test_linux_minimal_runtime_keeps_comptime_else_imports_from_flat() {
	files := runtime_init_test_files_with_main_extra_stmts([
		runtime_init_comptime_else_import_stmt('windows', 'os'),
	])
	mut t_legacy := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t_legacy)
	mut t_flat := transformer_with_x64_target('linux')
	seed_runtime_const_init_names(mut t_flat)

	legacy_names := runtime_init_call_names(t_legacy.runtime_const_init_main_calls_parts(files))
	flat_names := runtime_init_call_names_from_flat(mut t_flat, files)
	assert legacy_names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
	assert flat_names == legacy_names
}

fn test_macos_tiny_candidate_filters_unimported_runtime_init_calls_from_flat() {
	mut t := transformer_with_macos_tiny_candidate_graph()
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names_from_flat(mut t, runtime_init_test_files([]ast.ImportStmt{}))
	assert names == ['__v_init_consts_main']
}

fn test_macos_tiny_candidate_keeps_imported_runtime_init_calls_from_flat() {
	mut t := transformer_with_macos_tiny_candidate_graph()
	seed_runtime_const_init_names(mut t)
	names := runtime_init_call_names_from_flat(mut t, runtime_init_test_files([
		ast.ImportStmt{
			name: 'os'
		},
	]))
	assert names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
}

fn test_non_minimal_x64_runtime_keeps_existing_init_behavior() {
	mut t := transformer_with_x64_target('macos')
	seed_runtime_const_init_names(mut t)
	names :=
		runtime_init_call_names(t.runtime_const_init_main_calls_parts(runtime_init_test_files([]ast.ImportStmt{})))
	assert names == ['os____v_init_consts_os', 'os__init', '__v_init_consts_main']
}

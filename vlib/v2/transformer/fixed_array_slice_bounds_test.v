module transformer

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.types

fn transform_fixed_array_slice_code_for_test(code string) []ast.File {
	tmp_file := os.join_path(os.temp_dir(), 'v2_transformer_fixed_slice_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut transformer := Transformer.new_with_pref(env, prefs)
	return transformer.transform_files(files)
}

fn count_fixed_slice_call_in_expr(expr ast.Expr, name string) int {
	match expr {
		ast.ArrayInitExpr {
			mut count := count_fixed_slice_call_in_expr(expr.typ, name)
			for item in expr.exprs {
				count += count_fixed_slice_call_in_expr(item, name)
			}
			count += count_fixed_slice_call_in_expr(expr.init, name)
			count += count_fixed_slice_call_in_expr(expr.cap, name)
			count += count_fixed_slice_call_in_expr(expr.len, name)
			count += count_fixed_slice_call_in_expr(expr.update_expr, name)
			return count
		}
		ast.CallExpr {
			mut count := if expr.lhs is ast.Ident && expr.lhs.name == name { 1 } else { 0 }
			count += count_fixed_slice_call_in_expr(expr.lhs, name)
			for arg in expr.args {
				count += count_fixed_slice_call_in_expr(arg, name)
			}
			return count
		}
		ast.CastExpr {
			return count_fixed_slice_call_in_expr(expr.typ, name) +
				count_fixed_slice_call_in_expr(expr.expr, name)
		}
		ast.IndexExpr {
			return count_fixed_slice_call_in_expr(expr.lhs, name) +
				count_fixed_slice_call_in_expr(expr.expr, name)
		}
		ast.InfixExpr {
			return count_fixed_slice_call_in_expr(expr.lhs, name) +
				count_fixed_slice_call_in_expr(expr.rhs, name)
		}
		ast.InitExpr {
			mut count := count_fixed_slice_call_in_expr(expr.typ, name)
			for field in expr.fields {
				count += count_fixed_slice_call_in_expr(field.value, name)
			}
			return count
		}
		ast.KeywordOperator {
			mut count := 0
			for value in expr.exprs {
				count += count_fixed_slice_call_in_expr(value, name)
			}
			return count
		}
		ast.MapInitExpr {
			mut count := count_fixed_slice_call_in_expr(expr.typ, name)
			for key in expr.keys {
				count += count_fixed_slice_call_in_expr(key, name)
			}
			for value in expr.vals {
				count += count_fixed_slice_call_in_expr(value, name)
			}
			return count
		}
		ast.ModifierExpr {
			return count_fixed_slice_call_in_expr(expr.expr, name)
		}
		ast.ParenExpr {
			return count_fixed_slice_call_in_expr(expr.expr, name)
		}
		ast.PrefixExpr {
			return count_fixed_slice_call_in_expr(expr.expr, name)
		}
		ast.RangeExpr {
			return count_fixed_slice_call_in_expr(expr.start, name) +
				count_fixed_slice_call_in_expr(expr.end, name)
		}
		ast.SelectorExpr {
			return count_fixed_slice_call_in_expr(expr.lhs, name)
		}
		ast.Tuple {
			mut count := 0
			for value in expr.exprs {
				count += count_fixed_slice_call_in_expr(value, name)
			}
			return count
		}
		ast.UnsafeExpr {
			return count_fixed_slice_call_in_stmts(expr.stmts, name)
		}
		else {
			return 0
		}
	}
}

fn count_fixed_slice_call_in_stmt(stmt ast.Stmt, name string) int {
	match stmt {
		ast.AssignStmt {
			mut count := 0
			for lhs in stmt.lhs {
				count += count_fixed_slice_call_in_expr(lhs, name)
			}
			for rhs in stmt.rhs {
				count += count_fixed_slice_call_in_expr(rhs, name)
			}
			return count
		}
		ast.ExprStmt {
			return count_fixed_slice_call_in_expr(stmt.expr, name)
		}
		else {
			return 0
		}
	}
}

fn count_fixed_slice_call_in_stmts(stmts []ast.Stmt, name string) int {
	mut count := 0
	for stmt in stmts {
		count += count_fixed_slice_call_in_stmt(stmt, name)
	}
	return count
}

fn transformed_fixed_slice_fn(files []ast.File, name string) ?ast.FnDecl {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == name {
				return stmt
			}
		}
	}
	return none
}

fn test_fixed_array_slice_bounds_are_materialized_once() {
	files := transform_fixed_array_slice_code_for_test('
fn next() int {
	return 1
}

fn stop() int {
	return 3
}

fn main() {
	arr := [4]int{}
	part := arr[next()..stop()]
	_ = part
}
')
	main_decl := transformed_fixed_slice_fn(files, 'main') or {
		assert false, 'missing transformed main'
		return
	}

	assert count_fixed_slice_call_in_stmts(main_decl.stmts, 'next') == 1
	assert count_fixed_slice_call_in_stmts(main_decl.stmts, 'stop') == 1
	assert count_fixed_slice_call_in_stmts(main_decl.stmts, 'new_array_from_c_array') == 1
}

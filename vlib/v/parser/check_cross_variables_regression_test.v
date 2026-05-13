module parser

import v.ast
import v.checker
import v.pref

fn test_check_cross_variables_skips_unresolved_call_arg_stringification() {
	mut p := Parser{}
	left := [ast.Expr(ast.Ident{
		name: 'size'
	}), ast.Expr(ast.Ident{
		name: 'align'
	})]
	right := ast.Expr(ast.CallExpr{
		name: 'type_size'
		args: [
			ast.CallArg{
				expr: ast.ArrayInit{
					elem_type: ast.Type(45)
				}
			},
		]
	})
	assert !p.check_cross_variables(left, right)
}

fn test_script_mode_allows_method_declarations_after_statements() {
	source := '
struct Msg {}

ms := &Msg{}

fn (m &Msg) one() {
	m.two()
}

fn (m &Msg) two() {}

ms.one()
'
	mut table := ast.new_table()
	mut vpref := pref.new_preferences()
	vpref.is_script = true
	mut prog := parse_text(source, 'issue_7280.vsh', mut table, .skip_comments, vpref)
	assert prog.errors.len == 0
	assert prog.stmts.len == 5
	assert prog.stmts[0] is ast.Module
	assert prog.stmts[1] is ast.StructDecl
	assert prog.stmts[2] is ast.FnDecl
	assert prog.stmts[3] is ast.FnDecl
	assert prog.stmts[4] is ast.FnDecl
	first_method := prog.stmts[2] as ast.FnDecl
	second_method := prog.stmts[3] as ast.FnDecl
	main_fn := prog.stmts[4] as ast.FnDecl
	assert first_method.is_method
	assert first_method.short_name == 'one'
	assert second_method.is_method
	assert second_method.short_name == 'two'
	assert main_fn.is_main
	assert main_fn.stmts.len == 2
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
	assert checker_.errors.len == 0
}

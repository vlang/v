module newjs

import strings
import v.ast
import v.token
import v.pref
import v.util
import v.util.version
import v.depgraph

[heap]
struct Expression {
	str string 
	parens bool 
}

fn (e &Expression) str() string {
	return e.str
}

fn (e &Expression) str_with_parens() string {
	if e.parens {
		return '(' + e.str + ')'
	}
	return e.str
}

fn (mut fc FuncContext) translate_expr(expr ast.Expr) &Expression {
	return unsafe { nil }
}
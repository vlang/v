module js

import v.ast
import v.util

fn (mut g JsGen) str_val(node ast.StringInterLiteral, i int) {
	expr := node.exprs[i]
	typ := g.unwrap_generic(node.expr_types[i])
	if typ == ast.string_type {
		g.expr(expr)
	} else if node.fmts[i] == `s` || typ.has_flag(.variadic) {
		g.gen_expr_to_string(expr, typ)
	} else if typ.is_number() || typ.is_pointer() || node.fmts[i] == `d` {
		g.expr(expr)
	} else {
		g.expr(expr)
	}
}

fn (mut g JsGen) string_inter_literal(node ast.StringInterLiteral) {
	g.write('`')
	for i, val in node.vals {
		escaped_val := util.smart_quote(val, false)

		g.write('$escaped_val')
		if i >= node.exprs.len {
			break
		}
		g.write('${')
		g.str_val(node,i)
		g.write('}')
	}
	g.write('`')
}

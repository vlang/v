module c

import v.ast

fn (mut g Gen) new_ctemp_var(expr ast.Expr, expr_type ast.Type) ast.CTempVar {
	return ast.CTempVar{
		name:   g.new_tmp_var()
		typ:    expr_type
		is_ptr: expr_type.is_ptr()
		orig:   expr
	}
}

fn (mut g Gen) new_ctemp_var_then_gen(expr ast.Expr, expr_type ast.Type) ast.CTempVar {
	x := g.new_ctemp_var(expr, expr_type)
	g.gen_ctemp_var(x)
	return x
}

fn (mut g Gen) gen_ctemp_var(tvar ast.CTempVar) {
	styp := g.typ(tvar.typ)
	g.write('${styp} ${tvar.name} = ')
	g.expr(tvar.orig)
	g.writeln(';')
	g.set_current_pos_as_last_stmt_pos()
}

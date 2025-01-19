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
	mut x := g.new_ctemp_var(expr, expr_type)
	g.gen_ctemp_var(mut x)
	return x
}

fn (mut g Gen) gen_ctemp_var(mut tvar ast.CTempVar) {
	styp := g.styp(tvar.typ)
	final_sym := g.table.final_sym(tvar.typ)
	if final_sym.info is ast.ArrayFixed {
		tvar.is_fixed_ret = final_sym.info.is_fn_ret
		g.writeln('${styp} ${tvar.name};')
		if tvar.is_fixed_ret {
			g.write('memcpy(${tvar.name}.ret_arr, ')
			g.expr(tvar.orig)
			g.writeln(' , sizeof(${styp[3..]}));')
		} else {
			g.write('memcpy(&${tvar.name}, ')
			g.expr(tvar.orig)
			g.writeln(' , sizeof(${styp}));')
		}
	} else {
		g.write('${styp} ${tvar.name} = ')
		g.expr(tvar.orig)
		g.writeln(';')
	}
	g.set_current_pos_as_last_stmt_pos()
}

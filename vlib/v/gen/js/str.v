module js

import v.ast

/*
fn (mut g JsGen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	is_shared := etype.has_flag(.shared_f)
	mut typ := etype
	if is_shared {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}

	mut sym := g.table.sym(typ)
	// when type is alias, print the aliased value
	if mut sym.info is ast.Alias {
		parent_sym := g.table.sym(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = parent_sym
		}
	}
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	is_var_mut := expr.is_auto_deref_var()
	if typ.has_flag(.variadic) {
		str_fn_name := g.gen_str_for_type(typ)
		g.write('${str_fn_name}(')
		g.expr(expr)
		g.write(')')
	} else if typ == ast.string_type {
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.write('new string((')
		g.expr(expr)
		g.write(').valueOf()? "true" : "false")')
	} else if sym.kind == .none_ {
		g.write('new string("<none>")')
	} else if sym.kind == .enum_ {
		if expr !is ast.EnumVal {
			str_fn_name := g.gen_str_for_type(typ)
			g.write('${str_fn_name}(')
			g.expr(expr)
			g.write(')')
		} else {
			g.write('new string("')
			g.expr(expr)
			g.write('")')
		}
	} else if sym_has_str_method
		|| sym.kind in [.array, .array_fixed, .map, .struct_, .multi_return, .sum_type, .interface_] {
		is_ptr := typ.is_ptr()
		str_fn_name := g.gen_str_for_type(typ)
		g.write('${str_fn_name}(')
		if str_method_expects_ptr && !is_ptr {
			g.write('new \$ref(')
			g.expr(expr)
			g.write(')')
		} else if (!str_method_expects_ptr && is_ptr && !is_shared) || is_var_mut {
			g.expr(expr)
			g.gen_deref_ptr(etype)
		}

		g.expr(expr)
		g.write(')')
	} else {
		str_fn_name := g.gen_str_for_type(typ)
		g.write('${str_fn_name}(')

		if sym.kind != .function {
			g.expr(expr)
			if expr.is_auto_deref_var() {
				g.write('.val')
			}
		}
		g.write(')')
	}
}*/

fn (mut g JsGen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	is_shared := etype.has_flag(.shared_f)
	mut typ := etype
	if is_shared {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}
	mut sym := g.table.sym(typ)
	// when type is alias, print the aliased value
	if mut sym.info is ast.Alias {
		parent_sym := g.table.sym(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = parent_sym
		}
	}
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	if typ.has_flag(.variadic) {
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')
		g.expr(expr)
		g.write(')')
	} else if typ == ast.string_type {
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.expr(expr)
		g.write('.valueOf()? new string("true") : new string("false")')
	} else if sym.kind == .none_ {
		g.write('new string("<none>")')
	} else if sym.kind == .enum_ {
		if expr !is ast.EnumVal {
			str_fn_name := g.get_str_fn(typ)
			g.write('${str_fn_name}(')
			g.expr(expr)
			g.write(')')
		} else {
			g.write('new string("')
			g.expr(expr)
			g.write('")')
		}
	} else if sym_has_str_method
		|| sym.kind in [.array, .array_fixed, .map, .struct_, .multi_return, .sum_type, .interface_] {
		is_ptr := typ.is_ptr()
		is_var_mut := expr.is_auto_deref_var()
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')
		if str_method_expects_ptr && !is_ptr {
			g.write('new \$ref(')
		}

		g.expr(expr)
		if (!str_method_expects_ptr && is_ptr && !is_shared) || is_var_mut {
			g.write('.val')
		}
		g.write(')')
		if str_method_expects_ptr && !is_ptr {
			g.write(')')
		}
	} else {
		str_fn_name := g.get_str_fn(typ)
		g.write('${str_fn_name}(')

		if sym.kind != .function {
			g.expr(expr)
		}
		if expr.is_auto_deref_var() {
			g.write('.val')
		}
		g.write(')')
	}
}

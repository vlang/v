module js

import v.ast

fn (mut g JsGen) gen_expr_to_string(expr ast.Expr, etype ast.Type) {
	mut typ := etype
	if etype.has_flag(.shared_f) {
		typ = typ.clear_flag(.shared_f).set_nr_muls(0)
	}

	mut sym := g.table.get_type_symbol(typ)

	if mut sym.info is ast.Alias {
		parent_sym := g.table.get_type_symbol(sym.info.parent_type)
		if parent_sym.has_method('str') {
			typ = sym.info.parent_type
			sym = parent_sym
		}
	}

	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	if typ.has_flag(.variadic) {
		// todo(playX): generate str method just like in the C backend
		g.write('new string(')
		g.expr(expr)
		g.write('.valueOf()')
		g.write('.toString())')
	} else if typ == ast.string_type {
		g.expr(expr)
	} else if typ == ast.bool_type {
		g.write('new string(')
		g.expr(expr)
		g.write('.valueOf() ? "true" : "false")')
	} else if sym.kind == .none_ {
		g.write('new string("<none>")')
	} else if sym.kind == .enum_ {
		g.write('new string(')
		if expr !is ast.EnumVal {
			g.expr(expr)
			g.write('.valueOf()')
			g.write('.toString()')
		} else {
			g.write('"')
			g.expr(expr)
			g.write('"')
		}
		g.write(')')
	} else if sym.kind == .interface_ && sym_has_str_method {
		is_ptr := typ.is_ptr()
		g.write(sym.mod.replace_once('${g.ns.name}.', ''))
		g.write('.')
		g.write(sym.name)
		g.write('.prototype.str.call(')
		g.expr(expr)
		if !str_method_expects_ptr && is_ptr {
			g.gen_deref_ptr(typ)
		}
		g.write(')')
	}
	//|| sym.kind in [.array, .array_fixed, .map, .struct_, .multi_return,.sum_type, .interface_]
	else if sym_has_str_method {
		g.write('new string(')
		g.write('Object.getPrototypeOf(/*str exists*/')
		g.expr(expr)
		is_ptr := typ.is_ptr()
		g.gen_deref_ptr(typ)
		g.write(').str.call(')
		g.expr(expr)
		if !str_method_expects_ptr && is_ptr {
			g.gen_deref_ptr(typ)
		}

		g.write('))')
	} else if sym.kind == .struct_ && !sym_has_str_method {
		g.write('new string(')
		g.expr(expr)
		g.gen_deref_ptr(typ)
		g.write('.toString())')
	} else {
		g.write('new string(')
		g.expr(expr)
		g.gen_deref_ptr(typ)
		g.write('.valueOf().toString())')
	}
}

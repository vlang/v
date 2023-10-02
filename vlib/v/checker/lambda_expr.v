module checker

import v.ast

pub fn (mut c Checker) lambda_expr(mut node ast.LambdaExpr, exp_typ ast.Type) ast.Type {
	// defer { eprintln('> line: ${@LINE} | exp_typ: $exp_typ | node: ${voidptr(node)} | node.typ: ${node.typ}') }
	if node.is_checked {
		return node.typ
	}
	if exp_typ == 0 {
		c.error('lambda expressions are allowed only in places expecting function callbacks',
			node.pos)
		return ast.void_type
	}
	exp_sym := c.table.sym(exp_typ)
	if exp_sym.kind != .function {
		c.error('a lambda expression was used, but `${exp_sym.kind}` was expected', node.pos)
		return ast.void_type
	}
	if exp_sym.info is ast.FnType {
		if node.params.len != exp_sym.info.func.params.len {
			c.error('lambda expression has ${node.params.len} params, but the expected fn callback needs ${exp_sym.info.func.params.len} params',
				node.pos)
			return ast.void_type
		}
		mut params := []ast.Param{}
		mut generic_types := map[ast.Type]bool{}
		for idx, mut x in node.params {
			eparam := exp_sym.info.func.params[idx]
			eparam_type := eparam.typ
			eparam_auto_deref := eparam.typ.is_ptr()
			c.lambda_expr_fix_type_of_param(mut node, mut x, eparam_type)
			if eparam_type.has_flag(.generic) {
				generic_types[eparam_type] = true
			}
			params << ast.Param{
				pos: x.pos
				name: x.name
				typ: eparam_type
				type_pos: x.pos
				is_auto_rec: eparam_auto_deref
			}
		}

		is_variadic := false
		return_type := exp_sym.info.func.return_type
		return_type_pos := node.pos
		if return_type.has_flag(.generic) {
			generic_types[return_type] = true
		}

		mut generic_names := []string{}
		for t, _ in generic_types {
			gtnames := c.table.generic_type_names(t)
			for x in gtnames {
				if x !in generic_names {
					generic_names << x
				}
			}
		}
		// dump(generic_types)
		// dump(generic_names)

		mut stmts := []ast.Stmt{}
		mut has_return := false
		if return_type == ast.void_type {
			stmts << ast.ExprStmt{
				pos: node.pos
				expr: node.expr
				is_expr: false
				typ: return_type
			}
		} else {
			stmts << ast.Return{
				pos: node.pos
				exprs: [node.expr]
			}
			has_return = true
		}

		mut func := ast.Fn{
			params: params
			is_variadic: is_variadic
			return_type: return_type
			is_method: false
		}
		name := c.table.get_anon_fn_name(c.file.unique_prefix, func, node.pos.pos)
		func.name = name
		idx := c.table.find_or_register_fn_type(func, true, false)
		typ := ast.new_type(idx)
		node.func = &ast.AnonFn{
			decl: ast.FnDecl{
				name: name
				short_name: ''
				mod: c.file.mod.name
				stmts: stmts
				has_return: has_return
				return_type: return_type
				return_type_pos: return_type_pos
				params: params
				is_variadic: is_variadic
				is_method: false
				is_anon: true
				no_body: false
				pos: node.pos.extend(node.pos_end)
				file: c.file.path
				scope: node.scope.parent
				generic_names: generic_names
			}
			typ: typ
		}
		if node.func.decl.generic_names.len > 0 {
			c.table.register_fn_generic_types(node.func.decl.fkey())
		}
		c.anon_fn(mut node.func)
	}
	node.is_checked = true
	node.typ = exp_typ

	return exp_typ
}

pub fn (mut c Checker) lambda_expr_fix_type_of_param(mut node ast.LambdaExpr, mut pident ast.Ident, ptype ast.Type) {
	if mut v := node.scope.find(pident.name) {
		if mut v is ast.Var {
			v.is_arg = true
			v.typ = ptype
			v.is_auto_deref = ptype.is_ptr()
			v.expr = ast.empty_expr
		}
	}
	c.ident(mut pident)
	pident.obj.typ = ptype
}

pub fn (mut c Checker) support_lambda_expr_in_sort(param_type ast.Type, return_type ast.Type, mut expr ast.LambdaExpr) {
	is_auto_rec := param_type.is_ptr()
	mut expected_fn := ast.Fn{
		params: [
			ast.Param{
				name: 'zza'
				typ: param_type
				is_auto_rec: is_auto_rec
			},
			ast.Param{
				name: 'zzb'
				typ: param_type
				is_auto_rec: is_auto_rec
			},
		]
		return_type: return_type
	}
	expected_fn_type := ast.new_type(c.table.find_or_register_fn_type(expected_fn, true,
		false))
	c.lambda_expr(mut expr, expected_fn_type)
}

pub fn (mut c Checker) support_lambda_expr_one_param(param_type ast.Type, return_type ast.Type, mut expr ast.LambdaExpr) {
	mut expected_fn := ast.Fn{
		params: [
			ast.Param{
				name: 'xx'
				typ: param_type
				is_auto_rec: param_type.is_ptr()
			},
		]
		return_type: return_type
	}
	cb_type := c.table.find_or_register_fn_type(expected_fn, true, false)
	expected_fn_type := ast.new_type(cb_type)
	c.lambda_expr(mut expr, expected_fn_type)
}

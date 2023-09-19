module checker

import v.ast

pub fn (mut c Checker) lambda_expr(mut node ast.LambdaExpr, exp_typ ast.Type) ast.Type {
	// defer { eprintln('> line: ${@LINE} | exp_typ: $exp_typ | node: ${voidptr(node)} | node.typ: ${node.typ}') }
	if node.is_checked {
		return node.typ
	}
	if !c.inside_fn_arg {
		c.error('lambda expressions are allowed only inside function or method callsites',
			node.pos)
		return ast.void_type
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
		for idx, mut x in node.params {
			eparam := exp_sym.info.func.params[idx]
			eparam_type := eparam.typ
			eparam_auto_deref := eparam.typ.is_ptr()
			if mut v := node.scope.find(x.name) {
				if mut v is ast.Var {
					v.is_arg = true
					v.typ = eparam_type
					v.expr = ast.empty_expr
					v.is_auto_deref = eparam_auto_deref
				}
			}
			c.ident(mut x)
			x.obj.typ = eparam_type

			params << ast.Param{
				pos: x.pos
				name: x.name
				typ: eparam_type
				type_pos: x.pos
				is_auto_rec: eparam_auto_deref
			}
		}
		/////
		is_variadic := false
		return_type := exp_sym.info.func.return_type
		return_type_pos := node.pos
		mut stmts := []ast.Stmt{}
		mut return_stmt := ast.Return{
			pos: node.pos
			exprs: [node.expr]
		}
		stmts << return_stmt

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
			}
			typ: typ
		}
		c.anon_fn(mut node.func)
	}
	node.is_checked = true
	node.typ = exp_typ

	return exp_typ
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

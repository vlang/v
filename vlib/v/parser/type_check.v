module parser

import (
	v.ast
	v.types
	v.table
)

pub fn (p &Parser) check_get_type(expr ast.Expr) types.TypeIdent {
	match expr {
		ast.CallExpr {
			func := p.table.find_fn(it.name) or {
				panic('unknown fn $it.name')
			}
			return func.return_ti
		}
		ast.Ident {
			if it.kind == .variable {
				var := p.table.find_var(it.name) or {
					p.error('name expr unknown variable `$it.name`')
					exit(0)
				}
				return var.ti
			}
			return it.ti
			// return types.void_ti
		}
		ast.StructInit {
			return it.ti
		}
		ast.StringLiteral {
			return types.string_ti
		}
		ast.IntegerLiteral {
			// if it.val.contains('.') {
			// 	return types.i64_ti
			// }
			return types.int_ti
		}
		ast.SelectorExpr {
			ti := p.check_get_type(it.expr)
			if !(ti.kind in [.placeholder, .struct_]) {
				println('$ti.name is not a struct')
			}
			struct_ := p.table.types[ti.idx] as types.Struct
			for field in struct_.fields {
				if field.name == it.field {
					return field.ti
				}
			}
			if struct_.parent_idx != 0 {
				parent := p.table.types[struct_.parent_idx] as types.Struct
				for field in parent.fields {
					if field.name == it.field {
						return field.ti
					}
				}
			}
			p.error('unknown field `${ti.name}.$it.field`')
		}
		ast.BinaryExpr {
			println('CHECK TYPE BINARYEXPR')
			ti := p.check_get_type(it.left)
			return ti
		}
		else {
			types.void_ti
		}
	}
	return types.void_ti
}

pub fn (p &Parser) check_call_expr(call_expr ast.CallExpr) {
	fn_name := call_expr.name
	if f := p.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if call_expr.args.len < f.args.len {
			p.error('too few arguments in call to `$fn_name`')
		} else if call_expr.args.len > f.args.len {
			p.error('too many arguments in call to `$fn_name`')
		}
		for i, arg in f.args {
			arg_expr := call_expr.args[i]
			ti := p.check_get_type(arg_expr)
			if !p.table.check(&ti, &arg.ti) {
				p.error('cannot use type `$ti.name` as type `$arg.ti.name` in argument to `$fn_name`')
			}
		}
	} else {
		// p.error('unhknown fn: $fn_name')
		p.warn('unknown function `$fn_name`')
	}
}

pub fn (p &Parser) check_assign_expr(assign_expr ast.AssignExpr) {
	left_ti := p.check_get_type(assign_expr.left)
	val_ti := p.check_get_type(assign_expr.val)
	if !p.table.check(val_ti, left_ti) {
		println('TODO FIX: REMEMBER fn_decl calls register_var')
		for var in p.table.local_vars {
			println(' * $var.name')
		}
		p.error('cannot assign $val_ti.name to $left_ti.name')
	}
}

pub fn (p &Parser) check_struct_init(struct_init ast.StructInit) {
	typ := p.table.find_type(struct_init.ti.name) or {
		p.error('unknown struct: $struct_init.ti.name')
		panic('')
	}
	match typ {
		types.Placeholder {
			p.error('unknown struct: $struct_init.ti.name')
		}
		types.Struct {
			for i, expr in struct_init.exprs {
				field := it.fields[i]
				expr_ti := p.check_get_type(expr)
				if !p.table.check(expr_ti, field.ti) {
					p.error('cannot assign $expr_ti.name as $field.ti.name for field $field.name')
				}
			}
		}
		else {}
	}
}

pub fn (p &Parser) check_method_call(method_call ast.MethodCallExpr) {
	ti := p.check_get_type(method_call.expr)
	if !p.table.has_method(ti.idx, method_call.name) {
		p.error('type `$ti.name` has no method `$method_call.name`')
	}
}

pub fn (p &Parser) check_selector_expr(selector_expr ast.SelectorExpr) {
	ti := p.check_get_type(selector_expr.expr)
	if !ti.kind in [.placeholder, .struct_] {
		p.error('$ti.name is not a struct')
	}
	struct_ := p.table.types[ti.idx] as types.Struct
	if !p.table.struct_has_field(struct_, selector_expr.field) {
		p.error('unknown field `${ti.name}.$selector_expr.field`')
	}
}

pub fn (p &Parser) check_binary_expr(binary_expr ast.BinaryExpr) {
	left_ti := p.check_get_type(binary_expr.left)
	right_ti := p.check_get_type(binary_expr.right)
	if !p.table.check(&right_ti, &left_ti) {
		p.error('binary expr: cannot use $right_ti.name as $left_ti.name')
	}
}

pub fn (p mut Parser) check_types() {
	for ctx in p.type_checks {
		match ctx.expr {
			ast.AssignExpr {
				p.check_assign_expr(it)
			}
			ast.CallExpr {
				p.check_call_expr(it)
			}
			ast.StructInit {
				p.check_struct_init(it)
			}
			ast.MethodCallExpr {
				p.check_method_call(it)
			}
			ast.SelectorExpr {
				p.check_selector_expr(it)
			}
			ast.BinaryExpr {
				p.check_binary_expr(it)
			}
			else { println('UNKNOWN CHECK') }
		}
		match ctx.stmt {
			ast.VarDecl {
				if _ := p.table.find_var(it.name) {
					p.error('redefinition of `$it.name`')
				}
				ti := p.check_get_type(it.expr)
				p.table.register_var(table.Var{
					name: it.name
					ti: ti
					is_mut: it.is_mut
				})
			}
			ast.Return {
				println('RETURN STMT')
				// p.check_struct_init(it)
			}
			else {}
		}
	}
}

pub fn (p mut Parser) add_check_expr(expr ast.Expr) {
	p.type_checks << TypeCheck{
		expr: expr
	}
}

pub fn (p mut Parser) add_check_stmt(stmt ast.Stmt) {
	p.type_checks << TypeCheck{
		stmt: stmt
	}
}

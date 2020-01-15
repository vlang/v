// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.table
	v.types
)

// enum CheckKind {
// 	assign
// }

pub struct Checker {
	table  &table.Table
mut:
	checks []Check
}

struct Check{
	expr ast.Expr
	stmt ast.Stmt
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
	}
}

pub fn (c &Checker) get_expr_ti(expr ast.Expr) types.TypeIdent {
	match expr {
		ast.CallExpr {
			func := c.table.find_fn(it.name) or {
				panic('unknown fn $it.name')
			}
			return func.return_ti
		}
		ast.MethodCallExpr {
			ti := c.get_expr_ti(it.expr)
			func := c.table.find_method(ti.idx, it.name) or {
				panic('unknown method ${ti.name}.$it.name')
			}
			return func.return_ti
		}
		ast.Ident {
			if it.kind == .variable {
				info := it.info as ast.IdentVar
				if info.ti.kind != .unresolved {
					println(' ~~~~~~~~~~ A $info.ti.name')
					return info.ti
				}
				ti := c.get_expr_ti(info.expr)
				println(' ~~~~~~~~~~ B $ti.name')
				return ti
			}
			// return it.ti
			return types.void_ti
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
			ti := c.get_expr_ti(it.expr)
			if !(ti.kind in [.placeholder, .struct_]) {
				println('$ti.name is not a struct')
			}
			struct_ := c.table.types[ti.idx] as types.Struct
			for field in struct_.fields {
				if field.name == it.field {
					return field.ti
				}
			}
			if struct_.parent_idx != 0 {
				parent := c.table.types[struct_.parent_idx] as types.Struct
				for field in parent.fields {
					if field.name == it.field {
						return field.ti
					}
				}
			}
			c.error('unknown field `${ti.name}.$it.field`')
		}
		ast.BinaryExpr {
			println('CHECK TYPE BINARYEXPR')
			ti := c.get_expr_ti(it.left)
			return ti
		}
		else {
			return types.unresolved_ti
		}
	}
	return types.unresolved_ti
}

pub fn (c &Checker) check_struct_init(struct_init ast.StructInit) {
	typ := c.table.find_type(struct_init.ti.name) or {
		c.error('unknown struct: $struct_init.ti.name')
		panic('')
	}
	match typ {
		types.Placeholder {
			c.error('unknown struct: $struct_init.ti.name')
		}
		types.Struct {
			for i, expr in struct_init.exprs {
				field := it.fields[i]
				expr_ti := c.get_expr_ti(expr)
				if !c.check(expr_ti, field.ti) {
					c.error('cannot assign $expr_ti.name as $field.ti.name for field $field.name')
				}
			}
		}
		else {}
	}
}

pub fn (c &Checker) check_binary_expr(binary_expr ast.BinaryExpr) {
	left_ti := c.get_expr_ti(binary_expr.left)
	right_ti := c.get_expr_ti(binary_expr.right)
	if !c.check(&right_ti, &left_ti) {
		c.error('binary expr: cannot use $right_ti.name as $left_ti.name')
	}
}

// TODO: non deferred
pub fn (c &Checker) check_return_stmt(return_stmt ast.Return) {
	mut got_tis := []types.TypeIdent
	for expr in return_stmt.exprs {
		// expr,ti := p.expr(0)
		// exprs << expr
		ti := c.get_expr_ti(expr)
		got_tis << ti
	}
	expected_ti := return_stmt.expected_ti
	mut expected_tis := [expected_ti]
	if expected_ti.kind == .multi_return {
		mr_type := c.table.types[expected_ti.idx] as types.MultiReturn
		expected_tis = mr_type.tis
	}
	if expected_tis.len != got_tis.len {
		c.error('wrong number of return arguments:\n\texpected: $expected_tis.str()\n\tgot: $got_tis.str()')
	}
	for i, exp_ti in expected_tis {
		got_ti := got_tis[i]
		println('checking return $got_ti.name, $exp_ti.name')
		if !c.check(got_ti, exp_ti) {
			c.error('cannot use `$got_ti.name` as type `$exp_ti.name` in return argument')
		}
	}
}

pub fn (c mut Checker) check_deferred() {
	for ctx in c.checks {
		match ctx.expr {
			ast.AssignExpr {
				c.check_assign_expr(it)
			}
			ast.CallExpr {
				c.check_call_expr(it)
			}
			ast.StructInit {
				c.check_struct_init(it)
			}
			ast.MethodCallExpr {
				c.check_method_call_expr(it)
			}
			ast.SelectorExpr {
				c.check_selector_expr(it)
			}
			ast.BinaryExpr {
				c.check_binary_expr(it)
			}
			else { println('UNKNOWN EXPR CHECK') }
		}
		match ctx.stmt {
			ast.Return {
				c.check_return_stmt(it)
			}
			else { println('UNKNOWN STMT CHECK') }
		}
	}
}

pub fn (c mut Checker) add_check_expr(expr ast.Expr) {
	c.checks << Check{
		expr: expr
	}
}

pub fn (c mut Checker) add_check_stmt(stmt ast.Stmt) {
	c.checks << Check{
		stmt: stmt
	}
}

pub fn (c &Checker) check(got, expected &types.TypeIdent) bool {
	println('check: $got.name, $expected.name')
	if expected.kind == .voidptr {
		return true
	}
	//if expected.name == 'array' {
	//	return true
	//}
	if got.idx != expected.idx {
		return false
	}
	return true
}

pub fn (c &Checker) error(s string) {
	print_backtrace()
	eprintln('checker: error: $s')
	exit(1)
}
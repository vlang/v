// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.types
)

pub fn (c &Checker) check_call_expr(call_expr ast.CallExpr) {
	fn_name := call_expr.name
	if f := c.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if call_expr.args.len < f.args.len {
			c.error('too few arguments in call to `$fn_name`')
		} else if call_expr.args.len > f.args.len {
			c.error('too many arguments in call to `$fn_name`')
		}
		for i, arg in f.args {
			arg_expr := call_expr.args[i]
			ti := c.get_expr_ti(arg_expr)
			if !c.check(&ti, &arg.ti) {
				c.error('cannot use type `$ti.name` as type `$arg.ti.name` in argument to `$fn_name`')
			}
		}
	} else {
		c.error('unknown fn: $fn_name')
		// c.warn('unknown function `$fn_name`')
	}
}

pub fn (c mut Checker) check_method_call(method_call ast.MethodCallExpr, left_ti &types.TypeIdent) types.TypeIdent {
	if !(left_ti.kind in [.unresolved, .placeholder]) {
		if method := c.table.find_method(left_ti.idx, method_call.name) {
			return method.return_ti
		} else {
			// c.error('type `$left_ti.name` has no method `$method_call.name`')
			c.add_check_expr(method_call)
		}
	} else {
		c.add_check_expr(method_call)
	}
	return types.unresolved_ti
}

pub fn (c &Checker) check_method_call_expr(method_call ast.MethodCallExpr) {
	ti := c.get_expr_ti(method_call.expr)
	if !c.table.has_method(ti.idx, method_call.name) {
		c.error('type `$ti.name` has no method `$method_call.name`')
	}
}

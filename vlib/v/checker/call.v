// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
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
			ti := c.check_get_type(arg_expr)
			if !c.check(&ti, &arg.ti) {
				c.error('cannot use type `$ti.name` as type `$arg.ti.name` in argument to `$fn_name`')
			}
		}
	} else {
		c.error('unknown fn: $fn_name')
		// c.warn('unknown function `$fn_name`')
	}
}

pub fn (c &Checker) check_method_call(method_call ast.MethodCallExpr) {
	ti := c.check_get_type(method_call.expr)
	if !c.table.has_method(ti.idx, method_call.name) {
		c.error('type `$ti.name` has no method `$method_call.name`')
	}
}

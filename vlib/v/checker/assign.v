// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.types
)

pub fn (c mut Checker) check_assign(assign_expr ast.AssignExpr, left_ti, right_ti &types.TypeIdent) {
	if left_ti.kind in [.void, .unresolved, .placeholder] ||
		right_ti.kind in [.void, .unresolved, .placeholder] {
		println('### Check assign deferred: $left_ti.str() - $right_ti.str()')
		c.add_check_expr(assign_expr)
	} else {
		left_ti2 := c.get_expr_ti(assign_expr.left)
		println('### Checked assign on fly : $left_ti.name - $left_ti2.name')
		c.check_assign_ti(left_ti, right_ti)
	}
}

fn (c &Checker) check_assign_ti(left_ti, right_ti &types.TypeIdent) {
	if !c.check(right_ti, left_ti) {
		c.error('cannot assign $right_ti.name to $left_ti.name')
	}
}

fn (c &Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_ti := c.get_expr_ti(assign_expr.left)
	right_ti := c.get_expr_ti(assign_expr.val)
	c.check_assign_ti(&left_ti, &right_ti)
}

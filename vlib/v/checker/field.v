// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.types
)

pub fn (c mut Checker) check_selector(selector_expr ast.SelectorExpr, ti &types.TypeIdent) types.TypeIdent {
	if ti.kind == .struct_ {
		println('on the fly field check')
		return c.has_field(ti.idx, selector_expr.field)
	} else {
		println('deferred field check')
		c.add_check_expr(selector_expr)
		return types.unresolved_ti
	}
}

pub fn (c &Checker) check_selector_expr(selector_expr ast.SelectorExpr) {
	ti := c.check_get_type(selector_expr.expr)
	c.has_field(ti.idx, selector_expr.field)
}

fn (c &Checker) has_field(ti &types.TypeIdent, field string) types.TypeIdent {
	// struct_ := c.table.types[ti.idx] as types.Struct
	match c.table.types[ti.idx] {
		types.Struct {
			// if !c.table.struct_has_field(it, field) {
			// 	c.error('AAA unknown field `${it.name}.$field`')
			// }
			if f := c.table.struct_find_field(it, field) {
				println('FOUDN FIELD: $f.name - $f.ti.name')
				return f.ti
			}
			c.error('AAA unknown field `${it.name}.$field`')
		}
		else {
			c.error('$ti.name is not a struct')
		}
	}
	return types.unresolved_ti
}

// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v.ast

pub fn (mut t Transformer) array_init(mut node ast.ArrayInit) ast.Expr {
	for mut expr in node.exprs {
		expr = t.expr(mut expr)
	}
	if node.has_len {
		node.len_expr = t.expr(mut node.len_expr)
	}
	if node.has_cap {
		node.cap_expr = t.expr(mut node.cap_expr)
	}
	if node.has_init {
		node.init_expr = t.expr(mut node.init_expr)
	}
	if !t.pref.new_transform || t.skip_array_transform || node.is_fixed || t.inside_in
		|| node.has_len || node.has_cap || node.exprs.len == 0 {
		return node
	}
	// For C and native transform into a function call `builtin__new_array_from_c_array_noscan(...)` etc
	len := node.exprs.len
	len_arg := ast.CallArg{
		expr: ast.IntegerLiteral{
			val: len.str()
		}
		typ:  ast.int_type
	}
	sizeof_arg := ast.CallArg{
		expr: ast.SizeOf{
			is_type: true
			typ:     node.elem_type
		}
		typ:  ast.int_type
	}
	fixed_array_idx := t.table.find_or_register_array_fixed(node.elem_type, len, ast.empty_expr,
		false)
	fixed_array_typ := if node.elem_type.has_flag(.generic) {
		ast.new_type(fixed_array_idx).set_flag(.generic)
	} else {
		ast.new_type(fixed_array_idx)
	}
	fixed_array_arg := ast.CallArg{
		expr: ast.CastExpr{
			expr:      ast.ArrayInit{
				is_fixed:   true
				has_val:    true
				typ:        fixed_array_typ
				elem_type:  node.elem_type
				exprs:      node.exprs
				expr_types: node.exprs.map(it.type())
			}
			typ:       ast.voidptr_type
			typname:   'voidptr'
			expr_type: fixed_array_typ
		}
		typ:  ast.voidptr_type_idx
	}
	mut call_expr := ast.CallExpr{
		name:        'new_array_from_c_array'
		mod:         'builtin'
		scope:       unsafe { nil }
		args:        [len_arg, len_arg, sizeof_arg, fixed_array_arg] //, sizeof(voidptr), _MOV((voidptr[${len}]){')
		return_type: node.typ
	}
	return call_expr
}

pub fn (mut t Transformer) find_new_array_len(node ast.AssignStmt) {
	if !t.pref.is_prod {
		return
	}
	// looking for, array := []type{len:int}
	mut right := node.right[0]
	if mut right is ast.ArrayInit {
		mut left := node.left[0]
		if mut left is ast.Ident {
			// we can not analyse mut array
			if left.is_mut {
				t.index.safe_access(left.name, -2)
				return
			}
			// as we do not need to check any value under the setup len
			if !right.has_len {
				t.index.safe_access(left.name, -1)
				return
			}
			mut len := int(0)
			value := right.len_expr
			if value is ast.IntegerLiteral {
				len = value.val.int() + 1
			}
			t.index.safe_access(left.name, len)
		}
	}
}

// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v.ast

pub fn (mut t Transformer) array_init(mut node ast.ArrayInit) ast.Expr {
	// For JS and Go generate array init using their syntax
	// if t.pref.backend !in [.c, .native] {
	if !t.pref.new_transform || node.is_fixed { // || t.file.mod.name != 'main' {
		for mut expr in node.exprs {
			expr = t.expr(mut expr)
		}
		node.len_expr = t.expr(mut node.len_expr)
		node.cap_expr = t.expr(mut node.cap_expr)
		node.init_expr = t.expr(mut node.init_expr)
		return node
	}
	// For C and native transform into a function call `builtin__new_array_from_c_array_noscan(...)` etc
	// println('transformer array-init ${t.pref.backend}')
	// array_type := t.table.unwrap(node.typ)
	// mut array_styp := ''
	// elem_type := t.table.unwrap(node.elem_type)
	// mut shared_styp := '' // only needed for shared &[]{...}
	// is_shared := false // TODO g.is_shared => t.is_shared
	len := node.exprs.len
	// elem_sym := t.table.sym(t.unwrap_generic(node.elem_type))
	// elem_sym := t.table.sym(t.table.unwrap(node.elem_type))
	// if false { // array_type.unaliased_sym.kind == .array_fixed {
	// g.fixed_array_init(node, array_type, var_name, is_amp)
	// if is_amp {
	// g.write(')')
	//}
	//} else if len == 0 {
	// `[]int{len: 6, cap:10, init:22}`
	// g.array_init_with_fields(node, elem_type, is_amp, shared_styp, var_name)
	//} else {
	// `[1, 2, 3]`
	// elem_styp := g.styp(elem_type.typ)
	noscan := '' // if t.pref.backend != .native { '_noscan' } else { '' } // g.check_noscan(elem_type.typ)
	mut fn_name := 'builtin__new_array_from_c_array'
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
	fixed_array_arg := ast.CallArg{
		expr: ast.ArrayInit{
			is_fixed:  true
			has_val:   true
			typ:       t.table.find_or_register_array_fixed(node.elem_type, len, ast.empty_expr,
				false)
			elem_type: node.elem_type
			exprs:     node.exprs
		}
		typ:  ast.voidptr_type_idx
	}
	// if false { // elem_type.unaliased_sym.kind == .function {
	//} else {
	fn_name = 'new_array_from_c_array' + noscan
	// g.write('builtin__new_array_from_c_array${noscan}(${len}, ${len}, sizeof(${elem_styp}), _MOV((${elem_styp}[${len}]){')
	//}
	call_expr := ast.CallExpr{
		name:        fn_name
		mod:         'builtin'
		scope:       ast.empty_scope // node.scope
		args:        [len_arg, len_arg, sizeof_arg, fixed_array_arg] //, sizeof(voidptr), _MOV((voidptr[${len}]){')
		return_type: node.typ
	}
	// println('call expr')
	// println(call_expr)
	return call_expr
	/*
		if len > 8 {
			g.writeln('')
			g.write('\t\t')
		}
		is_iface_or_sumtype := elem_sym.kind in [.sum_type, .interface]
		for i, expr in node.exprs {
			expr_type := if node.expr_types.len > i { node.expr_types[i] } else { node.elem_type }
			if expr_type == ast.string_type
				&& expr !in [ast.IndexExpr, ast.CallExpr, ast.StringLiteral, ast.StringInterLiteral, ast.InfixExpr] {
				if is_iface_or_sumtype {
					g.expr_with_cast(expr, expr_type, node.elem_type)
				} else {
					g.write('builtin__string_clone(')
					g.expr(expr)
					g.write(')')
				}
			} else {
				if node.elem_type.has_flag(.option) {
					g.expr_with_opt(expr, expr_type, node.elem_type)
				} else if elem_type.unaliased_sym.kind == .array_fixed
					&& expr in [ast.Ident, ast.SelectorExpr] {
					info := elem_type.unaliased_sym.info as ast.ArrayFixed
					g.fixed_array_var_init(g.expr_string(expr), expr.is_auto_deref_var(),
						info.elem_type, info.size)
				} else {
					g.expr_with_cast(expr, expr_type, node.elem_type)
				}
			}
			if i != len - 1 {
				if i > 0 && i & 7 == 0 { // i > 0 && i % 8 == 0
					g.writeln(',')
					g.write('\t\t')
				} else {
					g.write(', ')
				}
			}
		}
		g.write('}))')
		if g.is_shared {
			g.write('}, sizeof(${shared_styp}))')
		} else if is_amp {
			g.write(')')
		}
		*/
	//}
}

pub fn (mut t Transformer) index_expr(mut node ast.IndexExpr) ast.Expr {
	// if t.pref.new_transform {
	// println(t.file.mod)
	//}
	t.check_safe_array(mut node)
	node.left = t.expr(mut node.left)
	node.index = t.expr(mut node.index)
	node.or_expr = t.expr(mut node.or_expr) as ast.OrExpr
	// if true { //!t.pref.new_transform { //|| t.file.mod.name != 'main' {
	if !t.pref.new_transform { //|| t.file.mod.name != 'main' {
		return node
	}
	// Recursively transform the left side (array/map) and the index
	node.left = t.expr(mut node.left)
	node.index = t.expr(mut node.index)
	// Only apply transformation if	it is an array (not a fixed array, map, or option)
	if node.is_array && !node.is_farray && !node.is_option && !node.is_map {
		// Transformation: x[i] => (*(T*)builtin__array_get(x, i))
		mut left_expr := node.left
		mut left_type := node.left_type
		// If the array is a pointer/reference (e.g. &[]int), we must dereference it
		// because builtin__array_get expects the array struct by value.
		if left_type.is_ptr() {
			left_expr = ast.PrefixExpr{
				op:    .mul
				right: left_expr
				pos:   node.pos
			}
			// The type of the argument becomes the value type (remove pointer)
			left_type = left_type.set_nr_muls(0)
		}
		// Create the call: builtin__array_get(x, i)
		call_expr := ast.CallExpr{
			name:        'builtin__array_get'
			mod:         'builtin'
			args:        [
				ast.CallArg{
					expr: left_expr
					typ:  left_type
				},
				ast.CallArg{
					expr: node.index
					typ:  ast.int_type
				},
			]
			return_type: ast.voidptr_type
		}
		// Create the pointer type T* to cast the result (voidptr) to.
		// node.typ is the element type T.
		ptr_typ := node.typ.ref()
		// Create the cast: (T*)builtin__array_get(...)
		cast_expr := ast.CastExpr{
			expr:    call_expr
			typ:     ptr_typ
			typname: t.table.get_type_name(ptr_typ)
		}
		// Dereference the result to get the element: *...
		return ast.PrefixExpr{
			op:    .mul
			right: cast_expr
			pos:   node.pos
		}
	}
	return node
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

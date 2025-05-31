// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

/*
import v.ast

fn (mut g Gen) allocate_raw_array(name string, size i32, items i32) i32 {
	pos := g.code_gen.allocate_var(name, size, items)
	g.stack_var_pos += (size * items)
	return pos
}

fn (mut g Gen) array_init(var Var, node ast.ArrayInit) {
	if var is ast.Ident {
		var_object := g.get_var_from_ident(var)
		g.array_init(match var_object {
			LocalVar {
				var_object as LocalVar
			}
			GlobalVar {
				var_object as GlobalVar
			}
			else {
				g.n_error('array_init() not implemented for ${var_object}')
			}
		}, node)
		return
	}

	array_type := g.unwrap(node.typ)
	ts := g.table.sym(array_type)
	len := node.exprs.len

	elem_type := g.unwrap(node.elem_type)

	if ts.kind == .array_fixed {
		g.array_init_fixed(var)
	} else if len == 0 {
		// `[]i32{len: 6, cap: 10, init: 22}`
		g.array_init_with_fields(var, node, elem_type)
	} else {
		// `[1, 2, 3]`
		g.array_init_from_raw(var)
	}
}

fn (mut g Gen) array_init_fixed(var Var) {
	g.n_error('fixed array initialization not implemented yet')
}

// `[]i32{len: 6, cap: 10, init: 22}`
fn (mut g Gen) array_init_with_fields(var Var, node ast.ArrayInit, elem_type ast.Type) {
	if node.has_index { // `[]i32{len: 5, init: index * index}`
		g.n_error('array initialization with `index` variable not supported')
	}

	elem_ts := g.table.sym(elem_type)

	is_default_array := elem_ts.kind == .array && node.has_default
	is_default_map := elem_ts.kind == .map && node.has_default

	function_addr := g.fn_addr[if is_default_array {
		'__new_array_with_array_default'
	} else if is_default_map {
		'__new_array_with_map_default'
	} else {
		'__new_array_with_default'
	}]

	len := if node.has_len { node.len_expr } else { ast.IntegerLiteral{'0', node.pos} }
	cap := if node.has_cap { node.cap_expr } else { ast.IntegerLiteral{'0', node.pos} }
	elem_size_expr := ast.IntegerLiteral{g.get_type_size(node.elem_type).str(), node.pos}
	default_expr := if node.has_default {
		node.default_expr
	} else {
		ast.IntegerLiteral{'0', node.pos}
	}

	if is_default_array || is_default_map {
		g.n_error('arrays of arrays and arrays of maps not implemented yet')
	}

	//	g.code_gen.call_with_args(function_addr, [len, cap, elem_size_expr, default_expr])
}

fn (mut g Gen) array_init_from_raw(var Var) {
	g.n_error('raw array initialization not implemented yet')
}
*/

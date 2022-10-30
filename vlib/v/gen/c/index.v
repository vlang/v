// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) index_expr(node ast.IndexExpr) {
	if node.index is ast.RangeExpr {
		g.range_expr(node, node.index)
	} else {
		sym := g.table.final_sym(g.unwrap_generic(node.left_type))
		if sym.kind == .array {
			g.index_of_array(node, sym)
		} else if sym.kind == .array_fixed {
			g.index_of_fixed_array(node, sym)
		} else if sym.kind == .map {
			g.index_of_map(node, sym)
		} else if sym.kind == .string && !node.left_type.is_ptr() {
			gen_or := node.or_expr.kind != .absent || node.is_option
			if gen_or {
				tmp_opt := g.new_tmp_var()
				cur_line := g.go_before_stmt(0)
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.typ(ast.u8_type.set_flag(.optional))
				g.write('$opt_elem_type $tmp_opt = string_at_with_check(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				g.writeln(');')
				if !node.is_option {
					g.or_block(tmp_opt, node.or_expr, ast.u8_type)
				}
				g.write('\n$cur_line*(byte*)&${tmp_opt}.data')
			} else {
				is_direct_array_access := g.is_direct_array_access || node.is_direct
				if is_direct_array_access {
					g.expr(node.left)
					g.write('.str[ ')
					g.expr(node.index)
					g.write(']')
				} else {
					g.write('string_at(')
					g.expr(node.left)
					g.write(', ')
					g.expr(node.index)
					g.write(')')
				}
			}
		} else {
			g.expr(node.left)
			g.write('[')
			g.expr(node.index)
			g.write(']')
		}
	}
}

fn (mut g Gen) range_expr(node ast.IndexExpr, range ast.RangeExpr) {
	sym := g.table.final_sym(node.left_type)
	mut tmp_opt := ''
	mut cur_line := ''
	mut gen_or := node.or_expr.kind != .absent || node.is_option
	mut tmp_left := ''

	if sym.kind == .string {
		if node.is_gated {
			g.write('string_substr_ni(')
		} else {
			if gen_or {
				tmp_opt = g.new_tmp_var()
				cur_line = g.go_before_stmt(0)
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.typ(ast.string_type.set_flag(.optional))
				g.write('$opt_elem_type $tmp_opt = string_substr_with_check(')
			} else {
				g.write('string_substr(')
			}
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
	} else if sym.kind == .array {
		if !range.has_high {
			tmp_left = g.new_tmp_var()
			tmp_type := g.typ(node.left_type)
			g.insert_before_stmt('${util.tabs(g.indent)}$tmp_type $tmp_left;')
			// (tmp = expr, array_slice(...))
			g.write('($tmp_left = ')
			g.expr(node.left)
			g.write(', ')
		}
		if node.is_gated {
			g.write('array_slice_ni(')
		} else {
			g.write('array_slice(')
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		if range.has_high {
			g.expr(node.left)
		} else {
			g.write(tmp_left)
		}
	} else if sym.kind == .array_fixed {
		// Convert a fixed array to V array when doing `fixed_arr[start..end]`
		info := sym.info as ast.ArrayFixed
		noscan := g.check_noscan(info.elem_type)
		if node.is_gated {
			g.write('array_slice_ni(')
		} else {
			g.write('array_slice(')
		}
		g.write('new_array_from_c_array${noscan}(')
		ctype := g.typ(info.elem_type)
		g.write('$info.size, $info.size, sizeof($ctype), ')
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
		g.write(')')
	} else {
		g.expr(node.left)
	}
	g.write(', ')
	if range.has_low {
		g.expr(range.low)
	} else {
		g.write('0')
	}
	g.write(', ')
	if range.has_high {
		g.expr(range.high)
	} else if sym.kind == .array_fixed {
		info := sym.info as ast.ArrayFixed
		g.write('$info.size')
	} else if sym.kind == .array {
		if node.left_type.is_ptr() {
			g.write('$tmp_left->')
		} else {
			g.write('${tmp_left}.')
		}
		g.write('len)')
	} else {
		g.write('(')
		g.expr(node.left)
		g.write(').len')
	}
	g.write(')')

	if gen_or {
		if !node.is_option {
			g.or_block(tmp_opt, node.or_expr, ast.string_type)
		}

		g.write('\n$cur_line*(string*)&${tmp_opt}.data')
	}
}

fn (mut g Gen) index_of_array(node ast.IndexExpr, sym ast.TypeSymbol) {
	gen_or := node.or_expr.kind != .absent || node.is_option
	left_is_ptr := node.left_type.is_ptr()
	info := sym.info as ast.Array
	elem_type := info.elem_type
	elem_sym := g.table.sym(elem_type)
	elem_type_str := if elem_sym.kind == .function {
		'voidptr'
	} else {
		g.typ(info.elem_type)
	}
	// `vals[i].field = x` is an exception and requires `array_get`:
	// `(*(Val*)array_get(vals, i)).field = x;`
	if g.is_assign_lhs && node.is_setter {
		is_direct_array_access := g.is_direct_array_access || node.is_direct
		is_op_assign := g.assign_op != .assign && info.elem_type != ast.string_type
		if is_direct_array_access {
			g.write('(($elem_type_str*)')
		} else if is_op_assign {
			g.write('(*($elem_type_str*)array_get(')
			if left_is_ptr && !node.left_type.has_flag(.shared_f) {
				g.write('*')
			}
		} else {
			g.is_arraymap_set = true // special handling of assign_op and closing with '})'
			g.write('array_set(')
			if !left_is_ptr || node.left_type.has_flag(.shared_f) {
				g.write('&')
			}
		}
		g.expr(node.left)
		if node.left_type.has_flag(.shared_f) {
			if left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		if is_direct_array_access {
			if left_is_ptr && !node.left_type.has_flag(.shared_f) {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write('data)[')
			g.expr(node.index)
			g.write(']')
		} else {
			g.write(', ')
			g.expr(node.index)
			if !is_op_assign {
				mut need_wrapper := true
				/*
				match node.right {
								ast.EnumVal, ast.Ident {
									// `&x` is enough for variables and enums
									// `&(Foo[]){ ... }` is only needed for function calls and literals
									need_wrapper = false
								}
								else {}
							}
				*/
				if need_wrapper {
					g.write(', &($elem_type_str[]) { ')
				} else {
					g.write(', &')
				}
			} else {
				// `x[0] *= y`
				g.write('))')
			}
		}
	} else {
		is_direct_array_access := g.is_direct_array_access || node.is_direct
		// do not clone inside `opt_ok(opt_ok(&(string[]) {..})` before returns
		needs_clone := info.elem_type == ast.string_type_idx && g.is_autofree && !(g.inside_return
			&& g.fn_decl != unsafe { nil } && g.fn_decl.return_type.has_flag(.optional))
			&& !g.is_assign_lhs
		is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
		cur_line := if is_gen_or_and_assign_rhs {
			line := g.go_before_stmt(0)
			g.out.write_string(util.tabs(g.indent))
			line
		} else {
			''
		}
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		tmp_opt_ptr := if gen_or { g.new_tmp_var() } else { '' }
		if gen_or {
			g.write('$elem_type_str* $tmp_opt_ptr = ($elem_type_str*)(array_get_with_check(')
			if left_is_ptr && !node.left_type.has_flag(.shared_f) {
				g.write('*')
			}
		} else {
			if needs_clone {
				g.write('/*2*/string_clone(')
			}
			if g.is_fn_index_call {
				if elem_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&elem_sym.info, '')
					g.write(')(*($elem_type_str*)array_get(')
				}
				if left_is_ptr && !node.left_type.has_flag(.shared_f) {
					g.write('*')
				}
			} else if is_direct_array_access {
				g.write('(($elem_type_str*)')
			} else {
				g.write('(*($elem_type_str*)array_get(')
				if left_is_ptr && !node.left_type.has_flag(.shared_f) {
					g.write('*')
				}
			}
		}
		g.expr(node.left)
		// TODO: test direct_array_access when 'shared' is implemented
		if node.left_type.has_flag(.shared_f) {
			if left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		if is_direct_array_access && !gen_or {
			if left_is_ptr && !node.left_type.has_flag(.shared_f) {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write('data)[')
			g.expr(node.index)
			g.write(']')
		} else {
			g.write(', ')
			g.expr(node.index)
			if g.is_fn_index_call {
				g.write(')))')
			} else {
				g.write('))')
			}
		}
		if needs_clone {
			g.write(')')
		}
		if gen_or {
			g.writeln(';')
			opt_elem_type := g.typ(elem_type.set_flag(.optional))
			g.writeln('$opt_elem_type $tmp_opt = {0};')
			g.writeln('if ($tmp_opt_ptr) {')
			g.writeln('\t*(($elem_type_str*)&${tmp_opt}.data) = *(($elem_type_str*)$tmp_opt_ptr);')
			g.writeln('} else {')
			g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = _v_error(_SLIT("array index out of range"));')
			g.writeln('}')
			if !node.is_option {
				g.or_block(tmp_opt, node.or_expr, elem_type)
			}
			g.write('\n$cur_line*($elem_type_str*)${tmp_opt}.data')
		}
	}
}

fn (mut g Gen) index_of_fixed_array(node ast.IndexExpr, sym ast.TypeSymbol) {
	info := sym.info as ast.ArrayFixed
	elem_type := info.elem_type
	elem_sym := g.table.sym(elem_type)
	is_fn_index_call := g.is_fn_index_call && elem_sym.info is ast.FnType

	if node.left is ast.ArrayInit {
		tmp := g.new_tmp_var()
		line := g.go_before_stmt(0).trim_space()
		styp := g.typ(node.left_type)
		g.empty_line = true
		g.write('$styp $tmp = ')
		g.expr(node.left)
		g.writeln(';')
		g.write(line)
		g.write(tmp)
	} else {
		if is_fn_index_call {
			g.write('(*')
		}
		if node.left_type.is_ptr() {
			g.write('(*')
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
	}
	g.write('[')
	if g.is_direct_array_access || g.pref.translated || node.index is ast.IntegerLiteral {
		g.expr(node.index)
	} else {
		// bounds check
		g.write('v_fixed_index(')
		g.expr(node.index)
		g.write(', $info.size)')
	}
	g.write(']')
	if is_fn_index_call {
		g.write(')')
	}
}

fn (mut g Gen) index_of_map(node ast.IndexExpr, sym ast.TypeSymbol) {
	gen_or := node.or_expr.kind != .absent || node.is_option
	left_is_ptr := node.left_type.is_ptr()
	info := sym.info as ast.Map
	key_type_str := g.typ(info.key_type)
	elem_type := info.value_type
	elem_sym := g.table.sym(elem_type)
	elem_type_str := if elem_sym.kind == .function {
		'voidptr'
	} else {
		if g.inside_return {
			g.typ(elem_type)
		} else {
			g.typ(elem_type.clear_flag(.optional).clear_flag(.result))
		}
	}
	get_and_set_types := elem_sym.kind in [.struct_, .map]
	if g.is_assign_lhs && !g.is_arraymap_set && !get_and_set_types {
		if g.assign_op == .assign || info.value_type == ast.string_type {
			g.is_arraymap_set = true
			g.write('map_set(')
		} else {
			if node.is_setter {
				g.write('(*(($elem_type_str*)map_get_and_set((map*)')
			} else {
				g.write('(*(($elem_type_str*)map_get((map*)')
			}
		}
		if !left_is_ptr || node.left_type.has_flag(.shared_f) {
			g.write('&')
		}
		if node.left is ast.IndexExpr {
			g.inside_map_index = true
			g.expr(node.left)
			g.inside_map_index = false
		} else {
			g.expr(node.left)
		}
		if node.left_type.has_flag(.shared_f) {
			g.write('->val')
		}
		g.write(', &($key_type_str[]){')
		old_is_arraymap_set := g.is_arraymap_set
		old_is_assign_lhs := g.is_assign_lhs
		g.is_arraymap_set = false
		g.is_assign_lhs = false
		g.expr(node.index)
		g.is_arraymap_set = old_is_arraymap_set
		g.is_assign_lhs = old_is_assign_lhs
		g.write('}')
		g.arraymap_set_pos = g.out.len
		g.write(', &($elem_type_str[]) { ')
		if g.assign_op != .assign && info.value_type != ast.string_type {
			zero := g.type_default(info.value_type)
			g.write('$zero })))')
		}
	} else if g.inside_map_postfix || g.inside_map_infix || g.inside_map_index
		|| (g.is_assign_lhs && !g.is_arraymap_set && get_and_set_types) {
		zero := g.type_default(info.value_type)
		if node.is_setter {
			g.write('(*($elem_type_str*)map_get_and_set((map*)')
		} else {
			g.write('(*($elem_type_str*)map_get((map*)')
		}
		if !left_is_ptr || node.left_type.has_flag(.shared_f) {
			g.write('&')
		}
		g.expr(node.left)
		if node.left_type.has_flag(.shared_f) {
			g.write('->val')
		}
		g.write(', &($key_type_str[]){')
		g.expr(node.index)
		g.write('}, &($elem_type_str[]){ $zero }))')
	} else {
		zero := g.type_default(info.value_type)
		is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
		cur_line := if is_gen_or_and_assign_rhs {
			line := g.go_before_stmt(0)
			g.out.write_string(util.tabs(g.indent))
			line
		} else {
			''
		}
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		tmp_opt_ptr := if gen_or { g.new_tmp_var() } else { '' }
		if gen_or {
			g.write('$elem_type_str* $tmp_opt_ptr = ($elem_type_str*)(map_get_check(')
		} else {
			if g.is_fn_index_call {
				if elem_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&elem_sym.info, '')
					g.write(')(*(voidptr*)map_get(')
				}
			} else {
				g.write('(*($elem_type_str*)map_get(')
			}
		}
		if !left_is_ptr || node.left_type.has_flag(.shared_f) {
			g.write('ADDR(map, ')
			g.expr(node.left)
		} else {
			g.write('(')
			g.expr(node.left)
		}
		if node.left_type.has_flag(.shared_f) {
			if left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		g.write('), &($key_type_str[]){')
		g.expr(node.index)
		g.write('}')
		if gen_or {
			g.write('))')
		} else if g.is_fn_index_call {
			g.write(', &(voidptr[]){ $zero })))')
		} else {
			g.write(', &($elem_type_str[]){ $zero }))')
		}
		if gen_or {
			g.writeln(';')
			opt_elem_type := g.typ(elem_type.set_flag(.optional))
			g.writeln('$opt_elem_type $tmp_opt = {0};')
			g.writeln('if ($tmp_opt_ptr) {')
			g.writeln('\t*(($elem_type_str*)&${tmp_opt}.data) = *(($elem_type_str*)$tmp_opt_ptr);')
			g.writeln('} else {')
			g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = _v_error(_SLIT("array index out of range"));')
			g.writeln('}')
			if !node.is_option {
				g.or_block(tmp_opt, node.or_expr, elem_type)
			}
			g.write('\n${cur_line}(*($elem_type_str*)${tmp_opt}.data)')
		}
	}
}

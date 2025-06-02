// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) index_expr(node ast.IndexExpr) {
	if node.index is ast.RangeExpr {
		g.index_range_expr(node, node.index)
	} else {
		left_type := g.unwrap_generic(g.type_resolver.get_type_or_default(node.left, node.left_type))
		sym := g.table.final_sym(left_type)
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
				cur_line := g.go_before_last_stmt()
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.styp(ast.u8_type.set_flag(.option))
				g.write('${opt_elem_type} ${tmp_opt} = string_at_with_check(')
				g.expr(node.left)
				g.write(', ')
				g.expr(node.index)
				g.writeln(');')
				if !node.is_option {
					g.or_block(tmp_opt, node.or_expr, ast.u8_type)
				}
				g.write('\n${cur_line}*(byte*)&${tmp_opt}.data')
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
		} else if sym.info is ast.Aggregate
			&& sym.info.types.all(g.table.type_kind(it) in [.array, .array_fixed, .string, .map]) {
			// treating sumtype of array types
			unwrapped_got_type := sym.info.types[g.aggregate_type_idx]
			g.index_expr(ast.IndexExpr{ ...node, left_type: unwrapped_got_type })
		} else {
			g.expr(node.left)
			g.write('[')
			g.expr(node.index)
			g.write(']')
		}
	}
}

fn (mut g Gen) index_range_expr(node ast.IndexExpr, range ast.RangeExpr) {
	unwrapped_left_type := g.unwrap_generic(node.left_type)
	sym := g.table.final_sym(unwrapped_left_type)
	mut tmp_opt := ''
	mut cur_line := ''
	mut gen_or := node.or_expr.kind != .absent || node.is_option
	left_is_shared := unwrapped_left_type.has_flag(.shared_f)
	if sym.kind == .string {
		if node.is_gated {
			g.write('string_substr_ni(')
		} else {
			if gen_or {
				tmp_opt = g.new_tmp_var()
				cur_line = g.go_before_last_stmt()
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.styp(ast.string_type.set_flag(.result))
				g.write('${opt_elem_type} ${tmp_opt} = string_substr_with_check(')
			} else {
				g.write('string_substr(')
			}
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
	} else if sym.kind == .array {
		if node.is_gated {
			g.write('array_slice_ni(')
		} else {
			g.write('array_slice(')
		}
		if left_is_shared {
			g.write('(')
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.left)
		if left_is_shared {
			g.write(').val')
		}
	} else if sym.info is ast.ArrayFixed {
		// Convert a fixed array to V array when doing `fixed_arr[start..end]`
		noscan := g.check_noscan(sym.info.elem_type)
		if node.is_gated {
			g.write('array_slice_ni(')
		} else {
			g.write('array_slice(')
		}
		g.write('new_array_from_c_array${noscan}(')
		ctype := g.styp(sym.info.elem_type)
		g.write('${sym.info.size}, ${sym.info.size}, sizeof(${ctype}), ')
		if left_is_shared {
			g.write('(')
		}
		if node.left_type.is_ptr() {
			g.write('*')
		}
		if node.left is ast.ArrayInit {
			var := g.new_tmp_var()
			line := g.go_before_last_stmt().trim_space()
			styp := g.styp(node.left_type)
			g.empty_line = true
			g.write('${styp} ${var} = ')
			g.expr(node.left)
			g.writeln(';')
			g.write2(line, ' ${var}')
		} else {
			g.expr(node.left)
		}
		if left_is_shared {
			g.write(').val')
		}
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
	} else if sym.info is ast.ArrayFixed {
		g.write('${sym.info.size}')
	} else {
		g.write('2147483647') // max_int
	}
	g.write(')')

	if gen_or {
		if !node.is_option {
			g.or_block(tmp_opt, node.or_expr, ast.string_type.set_flag(.result))
		}

		g.write('\n${cur_line}*(string*)&${tmp_opt}.data')
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
		g.styp(info.elem_type)
	}
	left_is_shared := node.left_type.has_flag(.shared_f)
	// `vals[i].field = x` is an exception and requires `array_get`:
	// `(*(Val*)array_get(vals, i)).field = x;`
	if g.is_assign_lhs && node.is_setter {
		is_direct_array_access := g.is_direct_array_access || node.is_direct
		is_op_assign := g.assign_op != .assign && info.elem_type != ast.string_type
		if is_direct_array_access {
			g.write('((${elem_type_str}*)')
		} else if is_op_assign {
			g.write('(*(${elem_type_str}*)array_get(')
			if left_is_ptr && !left_is_shared {
				g.write('*')
			}
		} else {
			g.cur_indexexpr << node.pos.pos
			g.is_arraymap_set = true // special handling of assign_op and closing with '})'
			g.write('array_set(')
			if !left_is_ptr || left_is_shared {
				g.write('&')
			}
		}
		if node.left is ast.IndexExpr {
			g.inside_array_index = true
			g.expr(node.left)
			g.inside_array_index = false
		} else {
			g.expr(node.left)
		}

		if left_is_shared {
			if node.index !is ast.RangeExpr && left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		if is_direct_array_access {
			if left_is_ptr && !left_is_shared {
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
				if elem_sym.kind != .array_fixed {
					if need_wrapper {
						g.write(', &(${elem_type_str}[]) { ')
					} else {
						g.write(', &')
					}
				}
			} else {
				// `x[0] *= y`
				g.write('))')
			}
		}
	} else {
		is_direct_array_access := g.is_direct_array_access || node.is_direct
		is_fn_index_call := g.is_fn_index_call && elem_sym.info is ast.FnType
		// do not clone inside `opt_ok(opt_ok(&(string[]) {..})` before returns
		needs_clone := info.elem_type == ast.string_type_idx && g.is_autofree && !(g.inside_return
			&& g.fn_decl != unsafe { nil } && g.fn_decl.return_type.has_flag(.option))
			&& !g.is_assign_lhs
		is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
		cur_line := if is_gen_or_and_assign_rhs {
			line := g.go_before_last_stmt()
			g.out.write_string(util.tabs(g.indent))
			line
		} else {
			''
		}
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		tmp_opt_ptr := if gen_or { g.new_tmp_var() } else { '' }
		if gen_or {
			g.write('${elem_type_str}* ${tmp_opt_ptr} = (${elem_type_str}*)(array_get_with_check(')
			if left_is_ptr && !left_is_shared {
				g.write('*')
			}
		} else {
			if needs_clone {
				g.write('string_clone(')
			}
			if is_fn_index_call {
				if elem_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&elem_sym.info, '')
					if is_direct_array_access {
						g.write(')((${elem_type_str}*)')
					} else {
						g.write(')(*(${elem_type_str}*)array_get(')
					}
				}
				if left_is_ptr && !left_is_shared {
					g.write('*')
				}
			} else if is_direct_array_access {
				g.write('((${elem_type_str}*)')
			} else {
				g.write('(*(${elem_type_str}*)array_get(')
				if left_is_ptr && !left_is_shared {
					g.write('*')
				}
			}
		}
		g.expr(node.left)
		// TODO: test direct_array_access when 'shared' is implemented
		if left_is_shared {
			if left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		if is_direct_array_access && !gen_or {
			if left_is_ptr && !left_is_shared {
				g.write('->')
			} else {
				g.write('.')
			}
			g.write('data)[')
			g.expr(node.index)
			g.write(']')
			if is_fn_index_call {
				g.write(')')
			}
		} else {
			g.write(', ')
			g.expr(node.index)
			if is_fn_index_call {
				g.write(')))')
			} else {
				g.write('))')
			}
		}
		if !gen_or && needs_clone {
			g.write(')')
		}
		if gen_or {
			g.writeln(';')
			opt_elem_type := g.styp(elem_type.set_flag(.option))
			g.writeln('${opt_elem_type} ${tmp_opt} = {0};')
			g.writeln('if (${tmp_opt_ptr}) {')
			g.writeln('\t*((${elem_type_str}*)&${tmp_opt}.data) = *((${elem_type_str}*)${tmp_opt_ptr});')
			g.writeln('} else {')
			g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = _v_error(_S("array index out of range"));')
			g.writeln('}')
			if !node.is_option {
				g.or_block(tmp_opt, node.or_expr, elem_type)
			}
			if !g.is_amp {
				if g.inside_opt_or_res && elem_type.has_flag(.option) && g.inside_assign {
					g.write('\n${cur_line}(*(${elem_type_str}*)&${tmp_opt})')
				} else {
					g.write('\n${cur_line}(*(${elem_type_str}*)${tmp_opt}.data)')
				}
			} else {
				g.write('\n${cur_line}*${tmp_opt_ptr}')
			}
		}
	}
}

fn (mut g Gen) index_of_fixed_array(node ast.IndexExpr, sym ast.TypeSymbol) {
	info := sym.info as ast.ArrayFixed
	elem_type := info.elem_type
	elem_sym := g.table.sym(elem_type)
	is_fn_index_call := g.is_fn_index_call && elem_sym.info is ast.FnType

	if node.left is ast.ArrayInit {
		past := g.past_tmp_var_new()
		styp := g.styp(node.left_type)
		g.write('${styp} ${past.tmp_var} = ')
		g.expr(node.left)
		g.writeln(';')
		g.past_tmp_var_done(past)
	} else if node.left is ast.IndexExpr && node.left.is_setter {
		line := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		tmp_var := g.new_tmp_var()
		styp := g.styp(node.left_type)
		g.write('${styp}* ${tmp_var} = &')
		g.expr(node.left)
		g.writeln(';')
		g.write(line)
		g.write('(*')
		g.write(tmp_var)
		g.write(')')
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
		if node.left_type.has_flag(.shared_f) {
			g.write('.val')
		}
	}
	g.write('[')
	if g.is_direct_array_access || g.pref.translated || node.index is ast.IntegerLiteral {
		g.expr(node.index)
	} else {
		// bounds check
		g.write('v_fixed_index(')
		g.expr(node.index)
		g.write(', ${info.size})')
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
	key_type_str := g.styp(info.key_type)
	val_type := info.value_type
	val_sym := g.table.sym(val_type)
	left_is_shared := node.left_type.has_flag(.shared_f)
	val_type_str := if val_sym.kind == .function {
		'voidptr'
	} else {
		if g.inside_return {
			g.styp(val_type)
		} else {
			g.styp(val_type.clear_flag(.result))
		}
	}
	get_and_set_types := val_sym.kind in [.struct, .map, .array, .array_fixed]
	if g.is_assign_lhs && !g.is_arraymap_set && !get_and_set_types {
		if g.assign_op == .assign || info.value_type == ast.string_type {
			g.cur_indexexpr << node.pos.pos
			g.is_arraymap_set = true
			g.write('map_set(')
		} else {
			if node.is_setter {
				g.write('(*((${val_type_str}*)map_get_and_set((map*)')
			} else {
				g.write('(*((${val_type_str}*)map_get((map*)')
			}
		}
		if !left_is_ptr || left_is_shared {
			g.write('&')
		}
		if node.left is ast.IndexExpr {
			g.inside_map_index = true
			g.expr(node.left)
			g.inside_map_index = false
		} else {
			g.expr(node.left)
		}
		if left_is_shared {
			g.write('->val')
		}
		g.write(', &(${key_type_str}[]){')
		old_is_arraymap_set := g.is_arraymap_set
		old_is_assign_lhs := g.is_assign_lhs
		g.is_arraymap_set = false
		g.is_assign_lhs = false
		g.expr(node.index)
		g.is_arraymap_set = old_is_arraymap_set
		g.is_assign_lhs = old_is_assign_lhs
		g.write('}')
		g.arraymap_set_pos = g.out.len
		g.write(', &(${val_type_str}[]) { ')
		if g.assign_op != .assign && info.value_type != ast.string_type {
			zero := g.type_default(info.value_type)
			g.write('${zero} })))')
		}
	} else if !gen_or && (g.inside_map_postfix || g.inside_map_infix
		|| g.inside_map_index || g.inside_array_index || (g.is_assign_lhs && !g.is_arraymap_set
		&& get_and_set_types)) {
		zero := g.type_default(info.value_type)
		if node.is_setter {
			g.write('(*(${val_type_str}*)map_get_and_set((map*)')
		} else {
			g.write('(*(${val_type_str}*)map_get((map*)')
		}
		if !left_is_ptr || left_is_shared {
			g.write('&')
		}
		g.expr(node.left)
		if left_is_shared {
			g.write('->val')
		}
		g.write(', &(${key_type_str}[]){')
		old_is_assign_lhs := g.is_assign_lhs
		g.is_assign_lhs = false
		g.expr(node.index)
		g.is_assign_lhs = old_is_assign_lhs
		g.write('}, &(${val_type_str}[]){ ${zero} }))')
	} else {
		zero := g.type_default(info.value_type)
		is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
		cur_line := if is_gen_or_and_assign_rhs {
			line := g.go_before_last_stmt()
			g.out.write_string(util.tabs(g.indent))
			line
		} else {
			''
		}
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		tmp_opt_ptr := if gen_or { g.new_tmp_var() } else { '' }
		mut is_fn_last_index_call := false
		if gen_or {
			g.write('${val_type_str}* ${tmp_opt_ptr} = (${val_type_str}*)(map_get_check(')
		} else {
			if g.is_fn_index_call {
				if val_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&val_sym.info, '')
					g.write(')(*(voidptr*)map_get(')
					is_fn_last_index_call = true
					g.is_fn_index_call = false
				}
			} else {
				g.write('(*(${val_type_str}*)map_get(')
			}
		}
		if !left_is_ptr || left_is_shared {
			g.write('ADDR(map, ')
			g.expr(node.left)
		} else {
			g.write('(')
			g.expr(node.left)
		}
		if left_is_shared {
			if left_is_ptr {
				g.write('->val')
			} else {
				g.write('.val')
			}
		}
		g.write('), &(${key_type_str}[]){')
		g.expr(node.index)
		g.write('}')
		if gen_or {
			g.write('))')
		} else if is_fn_last_index_call {
			g.write(', &(voidptr[]){ ${zero} })))')
		} else {
			g.write(', &(${val_type_str}[]){ ${zero} }))')
		}
		if gen_or {
			g.writeln(';')
			opt_val_type := g.styp(val_type.set_flag(.option))
			g.writeln('${opt_val_type} ${tmp_opt} = {0};')
			g.writeln('if (${tmp_opt_ptr}) {')
			if val_sym.kind == .array_fixed {
				g.writeln('\tmemcpy((${val_type_str}*)${tmp_opt}.data, (${val_type_str}*)${tmp_opt_ptr}, sizeof(${val_type_str}));')
			} else {
				g.writeln('\t*((${val_type_str}*)&${tmp_opt}.data) = *((${val_type_str}*)${tmp_opt_ptr});')
			}
			g.writeln('} else {')
			g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = _v_error(_S("map key does not exist"));')
			g.writeln('}')
			if !node.is_option {
				g.or_block(tmp_opt, node.or_expr, val_type)
			}
			g.write('\n${cur_line}(*(${val_type_str}*)${tmp_opt}.data)')
		}
	}
}

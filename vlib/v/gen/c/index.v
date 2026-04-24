// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

struct IndexOperatorMethodInfo {
	method        ast.Fn
	name          string
	receiver_type ast.Type
}

enum CWideIndexKind {
	plain
	signed_64
	unsigned_64
}

fn (mut g Gen) c_wide_index_kind(index_type ast.Type) CWideIndexKind {
	if g.pref.backend != .c || index_type == 0 {
		return .plain
	}
	mut internal_index_type := g.table.unaliased_type(index_type.clear_flag(.variadic))
	internal_index_sym := g.table.final_sym(internal_index_type)
	if internal_index_sym.kind == .enum {
		internal_index_type = internal_index_sym.enum_info().typ
	}
	if internal_index_type == ast.int_literal_type || !internal_index_type.is_int() {
		return .plain
	}
	int_size, _ := g.table.type_size(ast.int_type_idx)
	internal_index_size, _ := g.table.type_size(internal_index_type.idx_type())
	if internal_index_type.is_signed() {
		return if internal_index_size <= int_size { .plain } else { .signed_64 }
	}
	return if internal_index_size < int_size { .plain } else { .unsigned_64 }
}

fn (mut g Gen) resolved_index_operator_receiver_type(receiver ast.Expr, receiver_type ast.Type) ast.Type {
	mut resolved_type := g.recheck_concrete_type(g.resolved_expr_type(receiver, receiver_type))
	if resolved_type == 0 {
		resolved_type = receiver_type
	}
	if resolved_type == 0 || resolved_type.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(resolved_type) {
		resolved_type = g.resolved_expr_type(receiver, receiver_type)
	}
	if resolved_type == 0 {
		resolved_type = receiver_type
	}
	return g.unwrap_generic(g.recheck_concrete_type(resolved_type))
}

fn (mut g Gen) index_operator_method_info(receiver ast.Expr, receiver_type ast.Type, op string) ?IndexOperatorMethodInfo {
	resolved_receiver_type := g.resolved_index_operator_receiver_type(receiver, receiver_type)
	recv :=
		g.unwrap(if resolved_receiver_type != 0 { resolved_receiver_type } else { receiver_type })
	mut method := ast.Fn{}
	mut method_name := ''
	if recv.sym.has_method(op) || recv.sym.has_method_with_generic_parent(op) {
		method = recv.sym.find_method_with_generic_parent(op) or {
			recv.sym.find_method(op) or { return none }
		}
		method_name = recv.sym.cname + '_' + util.replace_op(op)
		if recv.sym.is_builtin() {
			method_name = 'builtin__${method_name}'
		}
	} else if recv.unaliased_sym.has_method_with_generic_parent(op) {
		method = recv.unaliased_sym.find_method_with_generic_parent(op) or { return none }
		method_name = recv.unaliased_sym.cname + '_' + util.replace_op(op)
		if recv.unaliased_sym.is_builtin() {
			method_name = 'builtin__${method_name}'
		}
	} else {
		return none
	}
	method_name = g.specialized_method_name_from_receiver(method, recv.typ, method_name)
	return IndexOperatorMethodInfo{
		method:        method
		name:          method_name
		receiver_type: recv.typ
	}
}

fn (mut g Gen) index_operator_call(receiver ast.Expr, receiver_type ast.Type, index ast.Expr, index_type ast.Type, op string, value ast.Expr, value_type ast.Type) {
	info := g.index_operator_method_info(receiver, receiver_type, op) or {
		g.error('missing `${op}` overload for `${g.table.type_to_str(receiver_type)}`',
			receiver.pos())
		return
	}
	g.write(info.name)
	g.write('(')
	g.op_arg(receiver, info.method.params[0].typ, info.receiver_type)
	g.write(', ')
	g.op_arg(index, info.method.params[1].typ, index_type)
	if op == '[]=' {
		g.write(', ')
		g.op_arg(value, info.method.params[2].typ, value_type)
	}
	g.write(')')
}

fn (mut g Gen) index_expr(node ast.IndexExpr) {
	if node.index is ast.RangeExpr {
		g.index_range_expr(node, node.index)
	} else {
		if node.is_index_operator {
			g.index_operator_call(node.left, node.left_type, node.index, node.index_type, '[]',
				ast.empty_expr, ast.void_type)
			return
		}
		mut left_type := ast.Type(0)
		if node.left is ast.Ident {
			resolved_current_type := g.resolve_current_fn_generic_param_type(node.left.name)
			if resolved_current_type != 0 {
				left_type = g.unwrap_generic(g.recheck_concrete_type(resolved_current_type))
			}
		}
		if left_type == 0 {
			left_type = g.unwrap_generic(g.recheck_concrete_type(node.left_type))
		}
		if left_type == 0 || left_type.has_flag(.generic)
			|| g.type_has_unresolved_generic_parts(left_type) {
			left_type = g.unwrap_generic(g.recheck_concrete_type(g.resolved_expr_type(node.left,
				node.left_type)))
		}
		if left_type == 0 {
			left_type = g.unwrap_generic(g.type_resolver.get_type_or_default(node.left,
				node.left_type))
		}
		sym := g.table.final_sym(left_type)
		if sym.kind == .array {
			g.index_of_array(node, sym)
		} else if sym.kind == .array_fixed {
			g.index_of_fixed_array(node, sym)
		} else if sym.kind == .map {
			g.index_of_map(node, sym)
		} else if sym.kind == .string && !node.left_type.is_ptr() {
			gen_or := node.or_expr.kind != .absent || node.is_option
			wide_index_kind := g.c_wide_index_kind(node.index_type)
			string_at_fn := if node.is_gated {
				'builtin__string_at_ni'
			} else {
				match wide_index_kind {
					.signed_64 { 'builtin__string_at_i64' }
					.unsigned_64 { 'builtin__string_at_u64' }
					else { 'builtin__string_at' }
				}
			}
			string_at_with_check_fn := if node.is_gated {
				'builtin__string_at_with_check_ni'
			} else {
				match wide_index_kind {
					.signed_64 { 'builtin__string_at_with_check_i64' }
					.unsigned_64 { 'builtin__string_at_with_check_u64' }
					else { 'builtin__string_at_with_check' }
				}
			}
			if gen_or {
				tmp_opt := g.new_tmp_var()
				cur_line := g.go_before_last_stmt()
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.styp(ast.u8_type.set_flag(.option))
				g.write('${opt_elem_type} ${tmp_opt} = ${string_at_with_check_fn}(')
				g.expr(ast.Expr(node.left))
				g.write(', ')
				g.expr(node.index)
				g.writeln(');')
				if !node.is_option {
					g.or_block(tmp_opt, node.or_expr, ast.u8_type)
				}
				g.write('\n${cur_line}*(byte*)&${tmp_opt}.data')
			} else {
				is_direct_array_access := !node.is_gated && wide_index_kind == .plain
					&& (g.is_direct_array_access || node.is_direct)
				if is_direct_array_access {
					g.expr(ast.Expr(node.left))
					g.write('.str[ ')
					g.expr(node.index)
					g.write(']')
				} else {
					g.write('${string_at_fn}(')
					g.expr(ast.Expr(node.left))
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
			g.expr(ast.Expr(node.left))
			g.write('[')
			g.expr(node.index)
			g.write(']')
		}
	}
}

fn (mut g Gen) index_range_expr(node ast.IndexExpr, range ast.RangeExpr) {
	mut resolved_left_type := ast.Type(0)
	if node.left is ast.Ident {
		resolved_current_type := g.resolve_current_fn_generic_param_type(node.left.name)
		if resolved_current_type != 0 {
			resolved_left_type = resolved_current_type
		}
	}
	if resolved_left_type == 0 {
		resolved_left_type = g.recheck_concrete_type(node.left_type)
	}
	if resolved_left_type == 0 || resolved_left_type.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(resolved_left_type) {
		resolved_left_type = g.resolved_expr_type(node.left, node.left_type)
	}
	if resolved_left_type == 0 {
		resolved_left_type = g.type_resolver.get_type_or_default(node.left, node.left_type)
	}
	unwrapped_left_type := g.unwrap_generic(g.recheck_concrete_type(resolved_left_type))
	sym := g.table.final_sym(unwrapped_left_type)
	mut tmp_opt := ''
	mut cur_line := ''
	mut gen_or := node.or_expr.kind != .absent || node.is_option
	left_is_shared := resolved_left_type.has_flag(.shared_f)
	if sym.kind == .string {
		if node.is_gated {
			g.write('builtin__string_substr_ni(')
		} else {
			if gen_or {
				tmp_opt = g.new_tmp_var()
				cur_line = g.go_before_last_stmt()
				g.out.write_string(util.tabs(g.indent))
				opt_elem_type := g.styp(ast.string_type.set_flag(.result))
				g.write('${opt_elem_type} ${tmp_opt} = builtin__string_substr_with_check(')
			} else {
				g.write('builtin__string_substr(')
			}
		}
		if resolved_left_type.is_ptr() {
			g.write('*')
		}
		g.expr(ast.Expr(node.left))
	} else if sym.kind == .array {
		if node.is_gated {
			g.write('builtin__array_slice_ni(')
		} else {
			g.write('builtin__array_slice(')
		}
		if left_is_shared {
			g.write('(')
		}
		if resolved_left_type.is_ptr() {
			g.write('*')
		}
		g.expr(ast.Expr(node.left))
		if left_is_shared {
			g.write(').val')
		}
	} else if sym.info is ast.ArrayFixed {
		// Convert a fixed array to V array when doing `fixed_arr[start..end]`
		noscan := g.check_noscan(sym.info.elem_type)
		if node.is_gated {
			g.write('builtin__array_slice_ni(')
		} else {
			g.write('builtin__array_slice(')
		}
		g.write('builtin__new_array_from_c_array${noscan}(')
		ctype := g.styp(sym.info.elem_type)
		g.write('${sym.info.size}, ${sym.info.size}, sizeof(${ctype}), ')
		if left_is_shared {
			g.write('(')
		}
		if resolved_left_type.is_ptr() {
			g.write('*')
		}
		if node.left is ast.ArrayInit {
			var := g.new_tmp_var()
			line := g.go_before_last_stmt().trim_space()
			styp := g.styp(node.left_type)
			g.empty_line = true
			g.write('${styp} ${var} = ')
			g.expr(ast.Expr(node.left))
			g.writeln(';')
			g.write2(line, ' ${var}')
		} else {
			g.expr(ast.Expr(node.left))
		}
		if left_is_shared {
			g.write(').val')
		}
		g.write(')')
	} else {
		g.expr(ast.Expr(node.left))
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
		g.write('${max_int}')
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
	resolved_left_type := g.recheck_concrete_type(g.resolved_expr_type(node.left, node.left_type))
	left_type := if resolved_left_type != 0 { resolved_left_type } else { node.left_type }
	left_sym := if left_type != 0 {
		g.table.final_sym(g.unwrap_generic(left_type))
	} else {
		g.table.final_sym(g.unwrap_generic(node.left_type))
	}
	array_left_type := if left_sym.kind == .array { left_type } else { node.left_type }
	info := if left_sym.kind == .array {
		left_sym.info as ast.Array
	} else {
		sym.info as ast.Array
	}
	resolved_elem_type_ := g.recheck_concrete_type(g.resolved_expr_type(node, node.typ))
	resolved_elem_type := if resolved_elem_type_ == ast.int_literal_type {
		ast.int_type
	} else if resolved_elem_type_ == ast.float_literal_type {
		ast.f64_type
	} else {
		resolved_elem_type_
	}
	elem_type := if resolved_elem_type != 0 && resolved_elem_type != ast.void_type {
		resolved_elem_type
	} else if info.elem_type == ast.int_literal_type {
		ast.int_type
	} else if info.elem_type == ast.float_literal_type {
		ast.f64_type
	} else {
		info.elem_type
	}
	elem_sym := g.table.final_sym(elem_type)
	left_is_ptr := array_left_type.is_ptr() || node.left.is_auto_deref_var()
	result_type := match true {
		gen_or && elem_type.has_flag(.option) {
			node.typ.clear_flag(.option)
		}
		gen_or {
			node.typ
		}
		else {
			elem_type
		}
	}

	result_sym := g.table.final_sym(result_type)
	elem_type_str := if elem_sym.kind == .function { 'voidptr' } else { g.styp(elem_type) }
	result_type_str := if result_sym.kind == .function { 'voidptr' } else { g.styp(result_type) }
	left_is_shared := array_left_type.has_flag(.shared_f)
	wide_index_kind := g.c_wide_index_kind(node.index_type)
	array_get_fn := if node.is_gated {
		'builtin__array_get_ni'
	} else {
		match wide_index_kind {
			.signed_64 { 'builtin__array_get_i64' }
			.unsigned_64 { 'builtin__array_get_u64' }
			else { 'builtin__array_get' }
		}
	}
	array_get_with_check_fn := if node.is_gated {
		'builtin__array_get_with_check_ni'
	} else {
		match wide_index_kind {
			.signed_64 { 'builtin__array_get_with_check_i64' }
			.unsigned_64 { 'builtin__array_get_with_check_u64' }
			else { 'builtin__array_get_with_check' }
		}
	}
	array_set_fn := if node.is_gated {
		'builtin__array_set_ni'
	} else {
		match wide_index_kind {
			.signed_64 { 'builtin__array_set_i64' }
			.unsigned_64 { 'builtin__array_set_u64' }
			else { 'builtin__array_set' }
		}
	}
	// `vals[i].field = x` is an exception and requires `array_get`:
	// `(*(Val*)array_get(vals, i)).field = x;`
	if g.is_assign_lhs && node.is_setter {
		is_direct_array_access := !node.is_gated && wide_index_kind == .plain
			&& (g.is_direct_array_access || node.is_direct)
		is_op_assign := g.assign_op != .assign && info.elem_type != ast.string_type
		if is_direct_array_access {
			g.write('((${elem_type_str}*)')
		} else if is_op_assign {
			g.write('(*(${elem_type_str}*)${array_get_fn}(')
			if left_is_ptr && !left_is_shared {
				g.write('*')
			}
		} else {
			g.cur_indexexpr << node.pos.pos
			g.is_arraymap_set = true // special handling of assign_op and closing with '})'
			g.write('${array_set_fn}(')
			if !left_is_ptr || left_is_shared {
				g.write('&')
			}
		}
		if node.left is ast.IndexExpr {
			g.inside_array_index = true
			g.expr(ast.Expr(node.left))
			g.inside_array_index = false
		} else {
			g.expr(ast.Expr(node.left))
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
		is_direct_array_access := !node.is_gated && wide_index_kind == .plain
			&& (g.is_direct_array_access || node.is_direct)
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
			g.write('${elem_type_str}* ${tmp_opt_ptr} = (${elem_type_str}*)(${array_get_with_check_fn}(')
			if left_is_ptr && !left_is_shared {
				g.write('*')
			}
		} else {
			if needs_clone {
				g.write('builtin__string_clone(')
			}
			if is_fn_index_call {
				if elem_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&elem_sym.info, '')
					if is_direct_array_access {
						g.write(')((${elem_type_str}*)')
					} else {
						g.write(')(*(${elem_type_str}*)${array_get_fn}(')
					}
				}
				if left_is_ptr && !left_is_shared && !is_direct_array_access {
					g.write('*')
				}
			} else if is_direct_array_access {
				g.write('((${elem_type_str}*)')
			} else {
				g.write('(*(${elem_type_str}*)${array_get_fn}(')
				if left_is_ptr && !left_is_shared {
					g.write('*')
				}
			}
		}
		g.expr(ast.Expr(node.left))
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
			if elem_type.has_flag(.option) && !g.inside_opt_or_res {
				g.writeln('\tif (${tmp_opt_ptr}->state == 0) {')
				g.writeln('\t\t*((${result_type_str}*)&${tmp_opt}.data) = *((${result_type_str}*)${tmp_opt_ptr}->data);')
				g.writeln('\t} else {')
				g.writeln('\t\t${tmp_opt}.state = ${tmp_opt_ptr}->state;')
				g.writeln('\t\t${tmp_opt}.err = ${tmp_opt_ptr}->err;')
				g.writeln('\t}')
			} else {
				g.writeln('\t*((${elem_type_str}*)&${tmp_opt}.data) = *((${elem_type_str}*)${tmp_opt_ptr});')
			}
			g.writeln('} else {')
			g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = builtin___v_error(_S("array index out of range"));')
			g.writeln('}')
			if !node.is_option {
				g.or_block(tmp_opt, node.or_expr, elem_type)
			}
			if is_gen_or_and_assign_rhs {
				g.set_current_pos_as_last_stmt_pos()
			}
			if !g.is_amp {
				if g.inside_opt_or_res && elem_type.has_flag(.option) && g.inside_assign {
					g.write('\n${cur_line}(*(${elem_type_str}*)&${tmp_opt})')
				} else if elem_type.has_flag(.option) && !g.inside_opt_or_res {
					g.write('\n${cur_line}(*(${result_type_str}*)${tmp_opt}.data)')
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
	wide_index_kind := g.c_wide_index_kind(node.index_type)

	if node.left is ast.ArrayInit {
		past := g.past_tmp_var_new()
		styp := g.styp(node.left_type)
		g.write('${styp} ${past.tmp_var} = ')
		g.expr(ast.Expr(node.left))
		g.writeln(';')
		g.past_tmp_var_done(past)
	} else if node.left is ast.IndexExpr && node.left.is_setter {
		line := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		tmp_var := g.new_tmp_var()
		styp := g.styp(node.left_type)
		g.write('${styp}* ${tmp_var} = &')
		g.expr(ast.Expr(node.left))
		g.writeln(';')
		g.write(line)
		g.write('(*')
		g.write(tmp_var)
		g.write(')')
	} else {
		if is_fn_index_call {
			g.write('(*')
		}
		if node.left_type.is_ptr() || node.left.is_auto_deref_var() {
			g.write('(*')
			g.expr(ast.Expr(node.left))
			g.write(')')
		} else {
			g.expr(ast.Expr(node.left))
		}
		if node.left_type.has_flag(.shared_f) {
			g.write('.val')
		}
	}
	g.write('[')
	if node.is_gated {
		g.write('builtin__v_fixed_index_ni(')
		g.expr(node.index)
		g.write(', ${info.size})')
	} else if wide_index_kind == .signed_64 {
		g.write('builtin__v_fixed_index_i64(')
		g.expr(node.index)
		g.write(', ${info.size})')
	} else if wide_index_kind == .unsigned_64 {
		g.write('builtin__v_fixed_index_u64(')
		g.expr(node.index)
		g.write(', ${info.size})')
	} else if g.is_direct_array_access || g.pref.translated || node.index is ast.IntegerLiteral {
		g.expr(node.index)
	} else {
		// bounds check
		g.write('builtin__v_fixed_index(')
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
	mut map_left_type := g.recheck_concrete_type(node.left_type)
	resolved_left_type := g.recheck_concrete_type(g.resolved_expr_type(node.left, node.left_type))
	if resolved_left_type != 0
		&& (g.cur_concrete_types.len > 0 || map_left_type == 0 || map_left_type.has_flag(.generic)
		|| g.type_has_unresolved_generic_parts(map_left_type)
		|| g.unwrap_generic(resolved_left_type) != g.unwrap_generic(map_left_type)) {
		map_left_type = resolved_left_type
	}
	mut left_is_ptr := map_left_type.is_ptr()
	if !left_is_ptr && g.is_assign_lhs && node.left is ast.Ident
		&& g.resolved_ident_is_auto_heap(node.left) {
		left_is_ptr = true
	}
	left_sym := if map_left_type != 0 {
		*g.table.final_sym(g.unwrap_generic(map_left_type))
	} else {
		sym
	}
	info := if left_sym.kind == .map {
		left_sym.info as ast.Map
	} else {
		sym.info as ast.Map
	}
	mut key_type := g.unwrap_generic(g.recheck_concrete_type(info.key_type))
	mut val_type := g.unwrap_generic(g.recheck_concrete_type(info.value_type))
	if key_type == 0 {
		key_type = info.key_type
	}
	if val_type == 0 {
		val_type = info.value_type
	}
	if node.left is ast.Ident {
		ident_key_type := g.resolved_ident_map_key_type(node.left)
		if ident_key_type != 0 {
			key_type = ident_key_type
		}
		ident_val_type := g.resolved_ident_map_value_type(node.left)
		if ident_val_type != 0 {
			val_type = ident_val_type
		}
	}
	if key_type == ast.usize_type || val_type == ast.usize_type {
		name_key_type, name_val_type := g.resolved_map_types_from_name(left_sym.name)
		if key_type == ast.usize_type && name_key_type != 0 {
			key_type = name_key_type
		}
		if val_type == ast.usize_type && name_val_type != 0 {
			val_type = name_val_type
		}
	}
	if val_type == ast.usize_type && node.typ != 0 {
		candidate_val_type := g.unwrap_generic(g.recheck_concrete_type(node.typ))
		if candidate_val_type != 0 && candidate_val_type != ast.void_type
			&& candidate_val_type !in [ast.int_literal_type, ast.float_literal_type] {
			val_type = candidate_val_type
		}
	}
	val_sym := g.table.final_sym(val_type)
	left_is_shared := map_left_type.has_flag(.shared_f)
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
	use_get_and_set := g.inside_left_shift || (node.is_setter && !g.inside_map_infix)
	if g.is_assign_lhs && !g.is_arraymap_set && !get_and_set_types {
		if g.assign_op == .assign || val_type == ast.string_type {
			g.cur_indexexpr << node.pos.pos
			g.is_arraymap_set = true
			g.write('builtin__map_set(')
		} else {
			if use_get_and_set {
				g.write('(*((${val_type_str}*)builtin__map_get_and_set((map*)')
			} else {
				g.write('(*((${val_type_str}*)builtin__map_get((map*)')
			}
		}
		if !left_is_ptr || left_is_shared {
			g.write('&')
		}
		if node.left is ast.IndexExpr {
			g.inside_map_index = true
			g.expr(ast.Expr(node.left))
			g.inside_map_index = false
		} else {
			g.expr(node.left)
		}
		if left_is_shared {
			g.write('->val')
		}
		g.write(', ')
		old_is_arraymap_set := g.is_arraymap_set
		old_is_assign_lhs := g.is_assign_lhs
		g.is_arraymap_set = false
		g.is_assign_lhs = false
		g.write_map_key_arg(node.index, key_type)
		g.is_arraymap_set = old_is_arraymap_set
		g.is_assign_lhs = old_is_assign_lhs
		g.arraymap_set_pos = g.out.len
		g.write(', &(${val_type_str}[]) { ')
		if g.assign_op != .assign && val_type != ast.string_type {
			zero := g.type_default(val_type)
			g.write('${zero} })))')
		}
	} else if !gen_or && (g.inside_map_postfix || g.inside_map_infix
		|| g.inside_map_index || g.inside_array_index || g.inside_left_shift
		|| (g.is_assign_lhs && !g.is_arraymap_set && get_and_set_types)) {
		zero := g.type_default(val_type)
		if use_get_and_set {
			g.write('(*(${val_type_str}*)builtin__map_get_and_set((map*)')
		} else {
			g.write('(*(${val_type_str}*)builtin__map_get((map*)')
		}
		if !left_is_ptr || left_is_shared {
			g.write('&')
		}
		g.expr(node.left)
		if left_is_shared {
			g.write('->val')
		}
		g.write(', ')
		old_is_assign_lhs := g.is_assign_lhs
		g.is_assign_lhs = false
		g.write_map_key_arg(node.index, key_type)
		g.is_assign_lhs = old_is_assign_lhs
		g.write(', &(${val_type_str}[]){ ${zero} }))')
	} else {
		zero := g.type_default(val_type)
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
			g.write('${val_type_str}* ${tmp_opt_ptr} = (${val_type_str}*)(builtin__map_get_check(')
		} else {
			if g.is_fn_index_call {
				if val_sym.info is ast.FnType {
					g.write('((')
					g.write_fn_ptr_decl(&val_sym.info, '')
					g.write(')(*(voidptr*)builtin__map_get(')
					is_fn_last_index_call = true
					g.is_fn_index_call = false
				}
			} else {
				g.write('(*(${val_type_str}*)builtin__map_get(')
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
		g.write('), ')
		g.write_map_key_arg(node.index, key_type)
		if gen_or {
			g.write('))')
		} else if is_fn_last_index_call {
			g.write(', &(voidptr[]){ ${zero} })))')
		} else {
			g.write(', &(${val_type_str}[]){ ${zero} }))')
		}
		if gen_or {
			g.writeln(';')
			// The surrounding expression may already carry a pointer prefix placeholder.
			direct_ptr_cur_line := if cur_line.ends_with('&') || cur_line.ends_with('H') {
				cur_line[..cur_line.len - 1]
			} else {
				cur_line
			}
			if g.unsafe_level > 0 && !node.is_option && !node.is_setter
				&& node.or_expr.kind == .block && node.or_expr.stmts.len == 1 {
				last_stmt := node.or_expr.stmts[0]
				if last_stmt is ast.ExprStmt && last_stmt.typ.is_any_kind_of_pointer() {
					// handle the case of `p := unsafe{ &m[key] or { nil } }` directly, without an intermediate option + copies etc:
					g.write('if (!${tmp_opt_ptr}) { ${tmp_opt_ptr} = ')
					g.expr(last_stmt.expr)
					g.write('; }')
					if cur_line.ends_with('&') || cur_line.ends_with('H') {
						g.write('\n${direct_ptr_cur_line}${tmp_opt_ptr}')
					} else {
						g.write('\n${cur_line}(*${tmp_opt_ptr})')
					}
					return
				}
			}
			if val_type.has_flag(.option) {
				or_value_type := g.resolved_or_block_value_type(node.or_expr)
				keep_option_result := node.is_option || (node.or_expr.kind == .block
					&& (or_value_type in [ast.none_type, ast.none_type_idx]
					|| or_value_type.has_flag(.option)))
				plain_val_type := val_type.clear_option_and_result()
				plain_val_sym := g.table.final_sym(plain_val_type)
				plain_val_type_str := if plain_val_sym.kind == .function {
					'voidptr'
				} else {
					g.styp(plain_val_type)
				}
				g.writeln('${val_type_str} ${tmp_opt} = {0};')
				g.writeln('if (${tmp_opt_ptr}) {')
				g.writeln('\t${tmp_opt} = *${tmp_opt_ptr};')
				g.writeln('} else {')
				g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = builtin___v_error(_S("map key does not exist"));')
				g.writeln('}')
				if !node.is_option {
					g.or_block_on_value(tmp_opt, node.or_expr, val_type)
				}
				if is_gen_or_and_assign_rhs {
					g.set_current_pos_as_last_stmt_pos()
				}
				if keep_option_result {
					g.write('\n${cur_line}${tmp_opt}')
				} else {
					g.write('\n${cur_line}(*(${plain_val_type_str}*)${tmp_opt}.data)')
				}
			} else {
				opt_val_type := g.styp(val_type.set_flag(.option))
				g.writeln('${opt_val_type} ${tmp_opt} = {0};')
				g.writeln('if (${tmp_opt_ptr}) {')
				if val_sym.kind == .array_fixed {
					g.writeln('\tmemcpy((${val_type_str}*)${tmp_opt}.data, (${val_type_str}*)${tmp_opt_ptr}, sizeof(${val_type_str}));')
				} else {
					g.writeln('\t*((${val_type_str}*)&${tmp_opt}.data) = *((${val_type_str}*)${tmp_opt_ptr});')
				}
				g.writeln('} else {')
				g.writeln('\t${tmp_opt}.state = 2; ${tmp_opt}.err = builtin___v_error(_S("map key does not exist"));')
				g.writeln('}')
				if !node.is_option {
					g.or_block(tmp_opt, node.or_expr, val_type)
				}
				if is_gen_or_and_assign_rhs {
					g.set_current_pos_as_last_stmt_pos()
				}
				g.write('\n${cur_line}(*(${val_type_str}*)${tmp_opt}.data)')
			}
		}
	}
}

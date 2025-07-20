// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn (mut g Gen) for_c_stmt(node ast.ForCStmt) {
	g.loop_depth++
	if node.is_multi {
		g.is_vlines_enabled = false
		g.inside_for_c_stmt = true
		if node.label.len > 0 {
			g.writeln('${node.label}:')
		}
		g.writeln('{')
		g.indent++
		if node.has_init {
			g.stmt(node.init)
			if node.init is ast.ExprStmt {
				g.write('; ')
			}
		}
		g.writeln('bool _is_first = true;')
		g.writeln('while (true) {')
		g.writeln('\tif (_is_first) {')
		g.writeln('\t\t_is_first = false;')
		g.writeln('\t} else {')
		if node.has_inc {
			g.indent++
			g.stmt(node.inc)
			g.writeln(';')
			g.indent--
		}
		g.writeln('}')
		if node.has_cond {
			g.write('if (!(')
			g.expr(node.cond)
			g.writeln(')) break;')
		}
		g.is_vlines_enabled = true
		g.inside_for_c_stmt = false
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.writeln('}')
		g.indent--
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	} else {
		g.is_vlines_enabled = false
		g.inside_for_c_stmt = true
		if node.label.len > 0 {
			g.writeln('${node.label}:')
		}
		g.set_current_pos_as_last_stmt_pos()
		g.skip_stmt_pos = true
		g.write('for (')
		if !node.has_init {
			g.write('; ')
		} else {
			g.stmt(node.init)
			if node.init is ast.ExprStmt {
				g.write('; ')
			}
			// Remove excess return and add space
			if g.out.last_n(1) == '\n' {
				g.go_back(1)
				g.empty_line = false
				g.write(' ')
			}
		}
		if node.has_cond {
			g.expr(node.cond)
		}
		g.write('; ')
		if node.has_inc {
			mut processed := false
			if node.inc is ast.ExprStmt && node.inc.expr is ast.ConcatExpr {
				for inc_expr_idx, inc_expr in node.inc.expr.vals {
					g.expr(inc_expr)
					if inc_expr_idx < node.inc.expr.vals.len - 1 {
						g.write(', ')
					}
				}
				processed = true
			}
			if !processed {
				g.stmt(node.inc)
			}
		}
		g.writeln(') {')
		g.skip_stmt_pos = false
		g.is_vlines_enabled = true
		g.inside_for_c_stmt = false
		g.stmts(node.stmts)
		if node.label.len > 0 {
			g.writeln('${node.label}__continue: {}')
		}
		g.writeln('}')
		if node.label.len > 0 {
			g.writeln('${node.label}__break: {}')
		}
	}
	g.loop_depth--
}

fn (mut g Gen) for_stmt(node ast.ForStmt) {
	g.loop_depth++
	g.is_vlines_enabled = false
	if node.label.len > 0 {
		g.writeln('${node.label}:')
	}
	g.writeln('for (;;) {')
	if !node.is_inf {
		g.indent++
		g.set_current_pos_as_last_stmt_pos()
		g.write('if (!(')
		g.expr(node.cond)
		g.writeln(')) break;')
		g.indent--
	}
	g.is_vlines_enabled = true
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}
	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('${node.label}__break: {}')
	}
	g.loop_depth--
}

fn (mut g Gen) for_in_stmt(node_ ast.ForInStmt) {
	mut node := unsafe { node_ }
	mut is_comptime := false

	if (node.cond is ast.Ident && node.cond.ct_expr) || node.cond is ast.ComptimeSelector {
		mut unwrapped_typ := g.unwrap_generic(node.cond_type)
		ctyp := g.type_resolver.get_type(node.cond)
		if ctyp != ast.void_type {
			unwrapped_typ = g.unwrap_generic(ctyp)
			is_comptime = true
		}

		mut unwrapped_sym := g.table.sym(unwrapped_typ)

		node.cond_type = unwrapped_typ
		node.val_type = g.table.value_type(unwrapped_typ)
		if node.val_is_mut {
			node.val_type = node.val_type.ref()
		}
		node.scope.update_var_type(node.val_var, node.val_type)
		node.kind = unwrapped_sym.kind

		if is_comptime {
			g.type_resolver.update_ct_type(node.val_var, node.val_type)
			node.scope.update_ct_var_kind(node.val_var, .value_var)

			defer {
				g.type_resolver.type_map.delete(node.val_var)
			}
		}

		if node.key_var.len > 0 {
			key_type := match unwrapped_sym.kind {
				.map { unwrapped_sym.map_info().key_type }
				else { ast.int_type }
			}
			node.key_type = key_type
			node.scope.update_var_type(node.key_var, key_type)

			if is_comptime {
				g.type_resolver.update_ct_type(node.key_var, node.key_type)
				node.scope.update_ct_var_kind(node.key_var, .key_var)

				defer {
					g.type_resolver.type_map.delete(node.key_var)
				}
			}
		}
	}

	if node.kind == .any && !is_comptime {
		mut unwrapped_typ := g.unwrap_generic(node.cond_type)
		mut unwrapped_sym := g.table.sym(unwrapped_typ)
		node.kind = unwrapped_sym.kind
		node.cond_type = unwrapped_typ
		if node.key_var.len > 0 {
			key_type := match unwrapped_sym.kind {
				.map { unwrapped_sym.map_info().key_type }
				else { ast.int_type }
			}
			node.key_type = key_type
			node.scope.update_var_type(node.key_var, key_type)
		}
		node.val_type = g.table.value_type(unwrapped_typ)
		node.scope.update_var_type(node.val_var, node.val_type)
	}
	g.loop_depth++
	if node.label.len > 0 {
		g.writeln('\t${node.label}: {}')
	}
	if node.is_range {
		// `for x in 1..10 {`
		i := if node.val_var == '_' { g.new_tmp_var() } else { c_name(node.val_var) }
		val_typ := ast.mktyp(node.val_type)
		g.write('for (${g.styp(val_typ)} ${i} = ')
		g.expr(node.cond)
		g.write('; ${i} < ')
		g.expr(node.high)
		g.writeln('; ++${i}) {')
	} else if node.kind == .array {
		// `for num in nums {`
		// g.writeln('// FOR IN array')
		mut styp := g.styp(node.val_type)
		mut val_sym := g.table.sym(node.val_type)
		op_field := g.dot_or_ptr(node.cond_type)

		mut cond_var := ''
		cond_is_option := node.cond_type.has_flag(.option)
		if (node.cond is ast.Ident && !cond_is_option)
			|| (node.cond is ast.SelectorExpr && node.cond.or_block.kind == .absent) {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			old_inside_opt_or_res := g.inside_opt_or_res
			if cond_is_option {
				g.inside_opt_or_res = true
			}
			g.expr(node.cond)
			g.inside_opt_or_res = old_inside_opt_or_res
			g.writeln(';')
		}
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		g.empty_line = true
		opt_expr := '(*(${g.styp(node.cond_type.clear_flag(.option))}*)${cond_var}${op_field}data)'
		cond_expr := if cond_is_option {
			'${opt_expr}${op_field}len'
		} else {
			'${cond_var}${op_field}len'
		}
		g.writeln('for (int ${i} = 0; ${i} < ${cond_expr}; ++${i}) {')
		if node.val_var != '_' {
			if mut val_sym.info is ast.FnType {
				g.write('\t')
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
				g.writeln(' = ((voidptr*)${cond_var}${op_field}data)[${i}];')
			} else if !node.val_type.has_flag(.option) && val_sym.kind == .array_fixed
				&& !node.val_is_mut {
				right := '((${styp}*)${cond_var}${op_field}data)[${i}]'
				g.writeln('\t${styp} ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*(${styp}*)${c_name(node.val_var)}, (byte*)${right}, sizeof(${styp}));')
			} else {
				needs_memcpy := !node.val_type.is_ptr() && !node.val_type.has_flag(.option)
					&& g.table.final_sym(node.val_type).kind == .array_fixed
				// If val is mutable (pointer behind the scenes), we need to generate
				// `int* val = ((int*)arr.data) + i;`
				// instead of
				// `int* val = ((int**)arr.data)[i];`
				// right := if node.val_is_mut { styp } else { styp + '*' }
				right := if cond_is_option {
					'((${styp}*)${opt_expr}${op_field}data)[${i}]'
				} else if node.val_is_mut || node.val_is_ref {
					'((${styp})${cond_var}${op_field}data) + ${i}'
				} else if val_sym.kind == .array_fixed {
					'((${styp}*)${cond_var}${op_field}data)[${i}]'
				} else {
					'((${styp}*)${cond_var}${op_field}data)[${i}]'
				}
				if !needs_memcpy {
					g.writeln('\t${styp} ${c_name(node.val_var)} = ${right};')
				} else {
					g.writeln('\t${styp} ${c_name(node.val_var)} = {0};')
					g.writeln('\tmemcpy(${c_name(node.val_var)}, ${right}, sizeof(${styp}));')
				}
			}
		}
	} else if node.kind == .array_fixed {
		mut cond_var := ''
		cond_type_is_ptr := node.cond_type.is_ptr()
		cond_is_literal := node.cond is ast.ArrayInit
		if cond_is_literal {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			g.expr(node.cond)
			g.writeln(';')
		} else if cond_type_is_ptr {
			cond_var = g.new_tmp_var()
			cond_var_type := g.styp(node.cond_type).trim('*')
			if !node.cond.is_lvalue() {
				g.write('${cond_var_type} *${cond_var} = ((${cond_var_type})')
			} else {
				g.write('${cond_var_type} *${cond_var} = (')
			}
			g.expr(node.cond)
			g.writeln(');')
		} else {
			cond_var = g.expr_string(node.cond)
		}
		idx := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		cond_sym := g.table.final_sym(node.cond_type)
		info := cond_sym.info as ast.ArrayFixed
		g.writeln('for (int ${idx} = 0; ${idx} != ${info.size}; ++${idx}) {')
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			is_fixed_array := val_sym.kind == .array_fixed && !node.val_is_mut
				&& !node.val_type.has_flag(.option)
			if val_sym.info is ast.FnType {
				g.write('\t')
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
			} else if is_fixed_array {
				styp := g.styp(node.val_type)
				g.writeln('\t${styp} ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*(${styp}*)${c_name(node.val_var)}, (byte*)${cond_var}[${idx}], sizeof(${styp}));')
			} else {
				styp := g.styp(node.val_type)
				g.write('\t${styp} ${c_name(node.val_var)}')
			}
			if !is_fixed_array {
				addr := if node.val_is_mut { '&' } else { '' }
				if cond_type_is_ptr {
					g.writeln(' = ${addr}(*${cond_var})[${idx}];')
				} else if cond_is_literal {
					g.writeln(' = ${addr}${cond_var}[${idx}];')
				} else {
					g.write(' = ${addr}')
					g.expr(node.cond)
					if info.is_fn_ret {
						g.write('.ret_arr')
					}
					g.writeln('[${idx}];')
				}
			}
		}
	} else if node.kind == .map {
		// `for key, val in map {
		// g.writeln('// FOR IN map')
		mut cond_var := ''
		if node.cond is ast.Ident {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write2(g.styp(node.cond_type), ' ${cond_var} = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		dot_or_ptr := g.dot_or_ptr(node.cond_type)
		idx := g.new_tmp_var()
		map_len := g.new_tmp_var()
		g.empty_line = true
		g.writeln('int ${map_len} = ${cond_var}${dot_or_ptr}key_values.len;')
		g.writeln('for (int ${idx} = 0; ${idx} < ${map_len}; ++${idx} ) {')
		// TODO: don't have this check when the map has no deleted elements
		g.indent++
		diff := g.new_tmp_var()
		g.writeln('int ${diff} = ${cond_var}${dot_or_ptr}key_values.len - ${map_len};')
		g.writeln('${map_len} = ${cond_var}${dot_or_ptr}key_values.len;')
		// TODO: optimize this
		g.writeln('if (${diff} < 0) {')
		g.writeln('\t${idx} = -1;')
		g.writeln('\tcontinue;')
		g.writeln('}')
		g.writeln('if (!DenseArray_has_index(&${cond_var}${dot_or_ptr}key_values, ${idx})) {continue;}')
		if node.key_var != '_' {
			key_styp := g.styp(node.key_type)
			key := c_name(node.key_var)
			g.writeln('${key_styp} ${key} = *(${key_styp}*)DenseArray_key(&${cond_var}${dot_or_ptr}key_values, ${idx});')
			// TODO: analyze whether node.key_type has a .clone() method and call .clone() for all types:
			if node.key_type == ast.string_type {
				g.writeln('${key} = string_clone(${key});')
			}
		}
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			if val_sym.info is ast.FnType {
				tcc_bug := c_name(node.val_var)
				g.write_fn_ptr_decl(&val_sym.info, tcc_bug)
				g.write(' = (*(voidptr*)')
				g.writeln('DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}));')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				val_styp := g.styp(node.val_type)
				g.writeln('${val_styp} ${c_name(node.val_var)};')
				g.writeln('memcpy(*(${val_styp}*)${c_name(node.val_var)}, (byte*)DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}), sizeof(${val_styp}));')
			} else {
				val_styp := g.styp(node.val_type)
				if node.val_type.is_ptr() {
					if node.val_is_mut || node.val_is_ref {
						g.write('${val_styp} ${c_name(node.val_var)} = &(*(${val_styp})')
					} else {
						g.write('${val_styp} ${c_name(node.val_var)} = (*(${val_styp}*)')
					}
				} else {
					g.write('${val_styp} ${c_name(node.val_var)} = (*(${val_styp}*)')
				}
				g.writeln('DenseArray_value(&${cond_var}${dot_or_ptr}key_values, ${idx}));')
			}
		}
		g.indent--
	} else if node.kind == .string {
		cond := if node.cond in [ast.StringLiteral, ast.StringInterLiteral] {
			ast.Expr(g.new_ctemp_var_then_gen(node.cond, ast.string_type))
		} else {
			node.cond
		}
		field_accessor := if node.cond_type.is_ptr() { '->' } else { '.' }
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		g.write('for (int ${i} = 0; ${i} < ')
		g.expr(cond)
		g.writeln('${field_accessor}len; ++${i}) {')
		if node.val_var != '_' {
			g.write('\tu8 ${c_name(node.val_var)} = ')
			g.expr(cond)
			g.writeln('${field_accessor}str[${i}];')
		}
	} else if node.kind in [.struct, .interface] {
		cond_type_sym := g.table.sym(node.cond_type)
		mut next_fn := ast.Fn{}
		// use alias `next` method if exists else use parent type `next` method
		if cond_type_sym.kind == .alias {
			next_fn = cond_type_sym.find_method_with_generic_parent('next') or {
				g.table.final_sym(node.cond_type).find_method_with_generic_parent('next') or {
					verror('`next` method not found')
					return
				}
			}
		} else {
			next_fn = cond_type_sym.find_method_with_generic_parent('next') or {
				verror('`next` method not found')
				return
			}
		}
		ret_typ := next_fn.return_type
		t_expr := g.new_tmp_var()
		g.write('${g.styp(node.cond_type)} ${t_expr} = ')
		g.expr(node.cond)
		g.writeln(';')
		if node.key_var in ['', '_'] {
			g.writeln('while (1) {')
		} else {
			g.writeln('for (size_t ${node.key_var} = 0;; ++${node.key_var}) {')
		}
		t_var := g.new_tmp_var()
		receiver_typ := g.unwrap_generic(next_fn.params[0].typ)
		receiver_styp := g.cc_type(receiver_typ, false)
		mut fn_name := receiver_styp.replace_each(['*', '', '.', '__']) + '_next'
		receiver_sym := g.table.sym(receiver_typ)
		if receiver_sym.info is ast.Struct {
			if receiver_sym.info.concrete_types.len > 0 {
				fn_name = g.generic_fn_name(receiver_sym.info.concrete_types, fn_name)
			}
		} else if receiver_sym.info is ast.Interface {
			left_cc_type := g.cc_type(g.table.unaliased_type(node.cond_type), false)
			left_type_name := util.no_dots(left_cc_type)
			fn_name = '${c_name(left_type_name)}_name_table[${t_expr}._typ]._method_next'
		}
		g.write('\t${g.styp(ret_typ)} ${t_var} = ${fn_name}(')
		if !node.cond_type.is_ptr() && receiver_typ.is_ptr() {
			g.write('&')
		}
		if node.kind == .interface {
			g.writeln('${t_expr}._object);')
		} else {
			g.writeln('${t_expr});')
		}
		g.writeln('\tif (${t_var}.state != 0) break;')
		val := if node.val_var in ['', '_'] { g.new_tmp_var() } else { node.val_var }
		val_styp := g.styp(ret_typ.clear_option_and_result())
		ret_is_fixed_array := g.table.sym(ret_typ).is_array_fixed()
		if node.val_is_mut {
			if ret_typ.has_flag(.option) {
				g.writeln('\t${val_styp}* ${val} = (${val_styp}*)${t_var}.data;')
			} else {
				g.writeln('\t${val_styp} ${val} = (${val_styp})${t_var}.data;')
			}
		} else {
			if ret_is_fixed_array {
				g.writeln('\t${val_styp} ${val} = {0};')
				g.write('\tmemcpy(${val}, ${t_var}.data, sizeof(${val_styp}));')
			} else {
				g.writeln('\t${val_styp} ${val} = *(${val_styp}*)${t_var}.data;')
			}
		}
	} else if node.kind == .aggregate {
		for_type := (g.table.sym(node.cond_type).info as ast.Aggregate).types[g.aggregate_type_idx]
		val_type := g.table.value_type(for_type)

		node.scope.update_var_type(node.val_var, val_type)

		g.for_in_stmt(ast.ForInStmt{
			cond:       node.cond
			cond_type:  for_type
			kind:       g.table.sym(for_type).kind
			stmts:      node.stmts
			val_type:   val_type
			val_var:    node.val_var
			val_is_mut: node.val_is_mut
			val_is_ref: node.val_is_ref
		})

		g.loop_depth--
		return
	} else {
		typ_str := g.table.type_to_str(node.cond_type)
		g.error('for in: unhandled symbol `${node.cond}` of type `${typ_str}`', node.pos)
	}
	g.stmts(node.stmts)
	if node.label.len > 0 {
		g.writeln('\t${node.label}__continue: {}')
	}

	if node.kind == .map {
		// diff := g.new_tmp_var()
		// g.writeln('int $diff = $cond_var${arw_or_pt}key_values.len - $map_len;')
		// g.writeln('if ($diff < 0) {')
		// g.writeln('\t$idx = -1;')
		// g.writeln('\t$map_len = $cond_var${arw_or_pt}key_values.len;')
		// g.writeln('}')
	}

	g.writeln('}')
	if node.label.len > 0 {
		g.writeln('\t${node.label}__break: {}')
	}
	g.loop_depth--
}

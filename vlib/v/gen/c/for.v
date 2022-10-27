// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) for_c_stmt(node ast.ForCStmt) {
	g.loop_depth++
	if node.is_multi {
		g.is_vlines_enabled = false
		g.inside_for_c_stmt = true
		if node.label.len > 0 {
			g.writeln('$node.label:')
		}
		g.writeln('{')
		g.indent++
		if node.has_init {
			g.stmt(node.init)
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
			g.writeln('$node.label:')
		}
		g.write('for (')
		if !node.has_init {
			g.write('; ')
		} else {
			g.stmt(node.init)
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
			if node.inc is ast.ExprStmt {
				if node.inc.expr is ast.ConcatExpr {
					for inc_expr_idx, inc_expr in node.inc.expr.vals {
						g.expr(inc_expr)
						if inc_expr_idx < node.inc.expr.vals.len - 1 {
							g.write(', ')
						}
					}
					processed = true
				}
			}
			if !processed {
				g.stmt(node.inc)
			}
		}
		g.writeln(') {')
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
		g.writeln('$node.label:')
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
	if node.kind == .any {
		g.inside_for_in_any_cond = true
		unwrapped_typ := g.unwrap_generic(node.cond_type)
		unwrapped_sym := g.table.sym(unwrapped_typ)
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
		g.writeln('\t$node.label: {}')
	}
	if node.is_range {
		// `for x in 1..10 {`
		i := if node.val_var == '_' { g.new_tmp_var() } else { c_name(node.val_var) }
		val_typ := ast.mktyp(node.val_type)
		g.write('for (${g.typ(val_typ)} $i = ')
		g.expr(node.cond)
		g.write('; $i < ')
		g.expr(node.high)
		g.writeln('; ++$i) {')
	} else if node.kind == .array {
		// `for num in nums {`
		// g.writeln('// FOR IN array')
		styp := g.typ(node.val_type)
		val_sym := g.table.sym(node.val_type)
		mut cond_var := ''
		if node.cond is ast.Ident || node.cond is ast.SelectorExpr {
			cond_var = g.expr_string(node.cond)
		} else {
			cond_var = g.new_tmp_var()
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		field_accessor := if node.cond_type.is_ptr() { '->' } else { '.' }
		share_accessor := if node.cond_type.share() == .shared_t { 'val.' } else { '' }
		op_field := field_accessor + share_accessor
		g.empty_line = true
		g.writeln('for (int $i = 0; $i < $cond_var${op_field}len; ++$i) {')
		if node.val_var != '_' {
			if val_sym.kind == .function {
				g.write('\t')
				g.write_fn_ptr_decl(val_sym.info as ast.FnType, c_name(node.val_var))
				g.writeln(' = ((voidptr*)$cond_var${op_field}data)[$i];')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				right := '(($styp*)$cond_var${op_field}data)[$i]'
				g.writeln('\t$styp ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*($styp*)${c_name(node.val_var)}, (byte*)$right, sizeof($styp));')
			} else {
				// If val is mutable (pointer behind the scenes), we need to generate
				// `int* val = ((int*)arr.data) + i;`
				// instead of
				// `int* val = ((int**)arr.data)[i];`
				// right := if node.val_is_mut { styp } else { styp + '*' }
				right := if node.val_is_mut || node.val_is_ref {
					'(($styp)$cond_var${op_field}data) + $i'
				} else {
					'(($styp*)$cond_var${op_field}data)[$i]'
				}
				g.writeln('\t$styp ${c_name(node.val_var)} = $right;')
			}
		}
	} else if node.kind == .array_fixed {
		mut cond_var := ''
		cond_type_is_ptr := node.cond_type.is_ptr()
		cond_is_literal := node.cond is ast.ArrayInit
		if cond_is_literal {
			cond_var = g.new_tmp_var()
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		} else if cond_type_is_ptr {
			cond_var = g.new_tmp_var()
			cond_var_type := g.typ(node.cond_type).trim('*')
			if !node.cond.is_lvalue() {
				g.write('$cond_var_type *$cond_var = (($cond_var_type)')
			} else {
				g.write('$cond_var_type *$cond_var = (')
			}
			g.expr(node.cond)
			g.writeln(');')
		} else {
			cond_var = g.expr_string(node.cond)
		}
		idx := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		cond_sym := g.table.sym(node.cond_type)
		info := cond_sym.info as ast.ArrayFixed
		g.writeln('for (int $idx = 0; $idx != $info.size; ++$idx) {')
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			is_fixed_array := val_sym.kind == .array_fixed && !node.val_is_mut
			if val_sym.kind == .function {
				g.write('\t')
				g.write_fn_ptr_decl(val_sym.info as ast.FnType, c_name(node.val_var))
			} else if is_fixed_array {
				styp := g.typ(node.val_type)
				g.writeln('\t$styp ${c_name(node.val_var)};')
				g.writeln('\tmemcpy(*($styp*)${c_name(node.val_var)}, (byte*)$cond_var[$idx], sizeof($styp));')
			} else {
				styp := g.typ(node.val_type)
				g.write('\t$styp ${c_name(node.val_var)}')
			}
			if !is_fixed_array {
				addr := if node.val_is_mut { '&' } else { '' }
				if cond_type_is_ptr {
					g.writeln(' = ${addr}(*$cond_var)[$idx];')
				} else if cond_is_literal {
					g.writeln(' = $addr$cond_var[$idx];')
				} else {
					g.write(' = $addr')
					g.expr(node.cond)
					g.writeln('[$idx];')
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
			g.write(g.typ(node.cond_type))
			g.write(' $cond_var = ')
			g.expr(node.cond)
			g.writeln(';')
		}
		mut arw_or_pt := if node.cond_type.is_ptr() { '->' } else { '.' }
		if node.cond_type.has_flag(.shared_f) {
			arw_or_pt = '->val.'
		}
		idx := g.new_tmp_var()
		map_len := g.new_tmp_var()
		g.empty_line = true
		g.writeln('int $map_len = $cond_var${arw_or_pt}key_values.len;')
		g.writeln('for (int $idx = 0; $idx < $map_len; ++$idx ) {')
		// TODO: don't have this check when the map has no deleted elements
		g.indent++
		diff := g.new_tmp_var()
		g.writeln('int $diff = $cond_var${arw_or_pt}key_values.len - $map_len;')
		g.writeln('$map_len = $cond_var${arw_or_pt}key_values.len;')
		// TODO: optimize this
		g.writeln('if ($diff < 0) {')
		g.writeln('\t$idx = -1;')
		g.writeln('\tcontinue;')
		g.writeln('}')
		g.writeln('if (!DenseArray_has_index(&$cond_var${arw_or_pt}key_values, $idx)) { continue;}')
		if node.key_var != '_' {
			key_styp := g.typ(node.key_type)
			key := c_name(node.key_var)
			g.writeln('$key_styp $key = /*key*/ *($key_styp*)DenseArray_key(&$cond_var${arw_or_pt}key_values, $idx);')
			// TODO: analyze whether node.key_type has a .clone() method and call .clone() for all types:
			if node.key_type == ast.string_type {
				g.writeln('$key = string_clone($key);')
			}
		}
		if node.val_var != '_' {
			val_sym := g.table.sym(node.val_type)
			if val_sym.kind == .function {
				g.write_fn_ptr_decl(val_sym.info as ast.FnType, c_name(node.val_var))
				g.write(' = (*(voidptr*)')
				g.writeln('DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx));')
			} else if val_sym.kind == .array_fixed && !node.val_is_mut {
				val_styp := g.typ(node.val_type)
				g.writeln('$val_styp ${c_name(node.val_var)};')
				g.writeln('memcpy(*($val_styp*)${c_name(node.val_var)}, (byte*)DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx), sizeof($val_styp));')
			} else {
				val_styp := g.typ(node.val_type)
				if node.val_type.is_ptr() {
					if node.val_is_mut || node.val_is_ref {
						g.write('$val_styp ${c_name(node.val_var)} = &(*($val_styp)')
					} else {
						g.write('$val_styp ${c_name(node.val_var)} = (*($val_styp*)')
					}
				} else {
					g.write('$val_styp ${c_name(node.val_var)} = (*($val_styp*)')
				}
				g.writeln('DenseArray_value(&$cond_var${arw_or_pt}key_values, $idx));')
			}
		}
		g.indent--
	} else if node.kind == .string {
		cond := if node.cond is ast.StringLiteral || node.cond is ast.StringInterLiteral {
			ast.Expr(g.new_ctemp_var_then_gen(node.cond, ast.string_type))
		} else {
			node.cond
		}
		field_accessor := if node.cond_type.is_ptr() { '->' } else { '.' }
		i := if node.key_var in ['', '_'] { g.new_tmp_var() } else { node.key_var }
		g.write('for (int $i = 0; $i < ')
		g.expr(cond)
		g.writeln('${field_accessor}len; ++$i) {')
		if node.val_var != '_' {
			g.write('\tu8 ${c_name(node.val_var)} = ')
			g.expr(cond)
			g.writeln('${field_accessor}str[$i];')
		}
	} else if node.kind == .struct_ {
		cond_type_sym := g.table.sym(node.cond_type)
		next_fn := cond_type_sym.find_method_with_generic_parent('next') or {
			verror('`next` method not found')
			return
		}
		ret_typ := next_fn.return_type
		t_expr := g.new_tmp_var()
		g.write('${g.typ(node.cond_type)} $t_expr = ')
		g.expr(node.cond)
		g.writeln(';')
		if node.key_var in ['', '_'] {
			g.writeln('while (1) {')
		} else {
			g.writeln('for (size_t $node.key_var = 0;; ++$node.key_var) {')
		}
		t_var := g.new_tmp_var()
		receiver_typ := g.unwrap_generic(next_fn.params[0].typ)
		receiver_styp := g.typ(receiver_typ)
		mut fn_name := receiver_styp.replace_each(['*', '', '.', '__']) + '_next'
		receiver_sym := g.table.sym(receiver_typ)
		if receiver_sym.info is ast.Struct {
			if receiver_sym.info.concrete_types.len > 0 {
				fn_name = g.generic_fn_name(receiver_sym.info.concrete_types, fn_name)
			}
		}
		g.write('\t${g.typ(ret_typ)} $t_var = ${fn_name}(')
		if !node.cond_type.is_ptr() && receiver_typ.is_ptr() {
			g.write('&')
		}
		g.writeln('$t_expr);')
		g.writeln('\tif (${t_var}.state != 0) break;')
		val := if node.val_var in ['', '_'] { g.new_tmp_var() } else { node.val_var }
		val_styp := g.typ(node.val_type)
		if node.val_is_mut {
			g.writeln('\t$val_styp $val = ($val_styp)${t_var}.data;')
		} else {
			g.writeln('\t$val_styp $val = *($val_styp*)${t_var}.data;')
		}
	} else {
		typ_str := g.table.type_to_str(node.cond_type)
		g.error('for in: unhandled symbol `$node.cond` of type `$typ_str`', node.pos)
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
	g.inside_for_in_any_cond = false
	g.loop_depth--
}

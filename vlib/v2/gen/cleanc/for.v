// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import strings
import v2.ast

fn (mut g Gen) gen_for_stmt(node ast.ForStmt) {
	if node.init is ast.ForInStmt {
		if g.gen_map_for_in_stmt(node, node.init) {
			return
		}
	}
	if g.gen_transformed_untyped_map_for_in_stmt(node) {
		return
	}
	g.write_indent()
	has_init := !is_empty_stmt(node.init)
	has_cond := !is_empty_expr(node.cond)
	has_post := !is_empty_stmt(node.post)

	if has_init || has_post {
		// C-style for loop: for (init; cond; post)
		g.sb.write_string('for (')
		if has_init {
			g.gen_stmt_inline(node.init)
		}
		g.sb.write_string('; ')
		if has_cond {
			g.expr(node.cond)
		}
		g.sb.write_string('; ')
		if has_post {
			g.gen_stmt_inline(node.post)
		}
		g.sb.writeln(') {')
	} else if has_cond {
		// while-style: for cond {
		g.sb.write_string('while (')
		g.expr(node.cond)
		g.sb.writeln(') {')
	} else {
		// Infinite loop: for {
		g.sb.writeln('for (;;) {')
	}

	g.indent++
	// Save runtime_local_types: variables declared in the loop body
	// (e.g. _filter_it in nested map/filter) can shadow outer variables.
	// Without save/restore, the inner type overwrites the outer in the flat map.
	saved_local_types := g.runtime_local_types.clone()
	g.gen_stmts(node.stmts)
	g.runtime_local_types = saved_local_types.clone()
	g.not_local_var_cache.clear()
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
}

fn (mut g Gen) gen_map_for_in_stmt(node ast.ForStmt, for_in ast.ForInStmt) bool {
	mut map_type := g.get_expr_type(for_in.expr).trim_space().trim_right('*')
	if (map_type == '' || map_type == 'int') && for_in.expr is ast.Ident {
		map_type = (g.get_local_var_c_type(for_in.expr.name) or { '' }).trim_space().trim_right('*')
	}
	if !map_type.starts_with('Map_') {
		return false
	}
	key_type, value_type := g.parse_map_kv_types(map_type['Map_'.len..])
	if key_type == '' || value_type == '' {
		return false
	}
	id := g.tmp_counter
	g.tmp_counter++
	map_tmp := '_map_iter_${id}'
	len_tmp := '_map_len_${id}'
	idx_tmp := '_map_idx_${id}'
	delta_tmp := '_map_delta_${id}'
	mut expr_sb := strings.new_builder(64)
	saved_sb := g.sb
	g.sb = expr_sb
	g.expr(for_in.expr)
	map_expr := g.sb.str()
	g.sb = saved_sb
	g.write_indent()
	g.sb.writeln('${map_type} ${map_tmp} = ${map_expr};')
	g.write_indent()
	g.sb.writeln('int ${len_tmp} = ${map_tmp}.key_values.len;')
	g.write_indent()
	g.sb.writeln('for (int ${idx_tmp} = 0; (${idx_tmp} < ${len_tmp}); ${idx_tmp} = (${idx_tmp} + 1)) {')
	g.indent++
	g.write_indent()
	g.sb.writeln('int ${delta_tmp} = (${map_tmp}.key_values.len - ${len_tmp});')
	g.write_indent()
	g.sb.writeln('${len_tmp} = ${map_tmp}.key_values.len;')
	g.write_indent()
	g.sb.writeln('if ((${delta_tmp} < 0)) {')
	g.indent++
	g.write_indent()
	g.sb.writeln('${idx_tmp} = -1;')
	g.write_indent()
	g.sb.writeln('continue;')
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
	g.write_indent()
	g.sb.writeln('if (!DenseArray__has_index(&${map_tmp}.key_values, ${idx_tmp})) {')
	g.indent++
	g.write_indent()
	g.sb.writeln('continue;')
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
	key_name := if for_in.key is ast.Ident { for_in.key.name } else { '' }
	value_name := if for_in.value is ast.Ident { for_in.value.name } else { '' }
	saved_local_types := g.runtime_local_types.clone()
	if key_name != '' && key_name != '_' {
		g.write_indent()
		g.sb.writeln('${key_type} ${key_name} = *((${key_type}*)DenseArray__key(&${map_tmp}.key_values, ${idx_tmp}));')
		if key_type == 'string' {
			g.write_indent()
			g.sb.writeln('${key_name} = string__clone(${key_name});')
		}
		g.runtime_local_types[key_name] = key_type
	}
	if value_name != '' && value_name != '_' {
		g.write_indent()
		g.sb.writeln('${value_type} ${value_name} = *((${value_type}*)DenseArray__value(&${map_tmp}.key_values, ${idx_tmp}));')
		if value_type == 'string' {
			g.write_indent()
			g.sb.writeln('${value_name} = string__clone(${value_name});')
		}
		g.runtime_local_types[value_name] = value_type
	}
	g.gen_stmts(node.stmts)
	g.runtime_local_types = saved_local_types.clone()
	g.not_local_var_cache.clear()
	g.indent--
	g.write_indent()
	g.sb.writeln('}')
	return true
}

fn (mut g Gen) gen_transformed_untyped_map_for_in_stmt(node ast.ForStmt) bool {
	if node.init !is ast.AssignStmt || node.stmts.len == 0 {
		return false
	}
	init := node.init as ast.AssignStmt
	if init.lhs.len == 0 || init.lhs[0] !is ast.Ident {
		return false
	}
	key_name := (init.lhs[0] as ast.Ident).name
	first_stmt := node.stmts[0]
	if first_stmt !is ast.AssignStmt {
		return false
	}
	value_assign := first_stmt as ast.AssignStmt
	if value_assign.lhs.len == 0 || value_assign.rhs.len == 0 || value_assign.lhs[0] !is ast.Ident
		|| value_assign.rhs[0] !is ast.IndexExpr {
		return false
	}
	value_name := (value_assign.lhs[0] as ast.Ident).name
	index_expr := value_assign.rhs[0] as ast.IndexExpr
	mut map_type := g.get_expr_type(index_expr.lhs).trim_space().trim_right('*')
	if (map_type == '' || map_type == 'int') && index_expr.lhs is ast.Ident {
		map_type =
			(g.get_local_var_c_type(index_expr.lhs.name) or { '' }).trim_space().trim_right('*')
	}
	if !map_type.starts_with('Map_') {
		return false
	}
	map_node := ast.ForStmt{
		stmts: node.stmts[1..]
	}
	return g.gen_map_for_in_stmt(map_node, ast.ForInStmt{
		key:   ast.Expr(ast.Ident{
			name: key_name
		})
		value: ast.Expr(ast.Ident{
			name: value_name
		})
		expr:  index_expr.lhs
	})
}

fn (mut g Gen) gen_stmt_inline(node ast.Stmt) {
	if !stmt_has_valid_data(node) {
		return
	}
	match node {
		ast.AssignStmt {
			lhs := node.lhs[0]
			rhs := node.rhs[0]
			if node.op == .decl_assign {
				mut name := ''
				if lhs is ast.Ident {
					name = lhs.name
				}
				typ := g.get_expr_type(rhs)
				g.sb.write_string('${typ} ${name} = ')
				g.expr(rhs)
				// Register the for-loop init variable so assignments inside
				// the loop body don't re-declare it.
				if name != '' && typ != '' {
					g.runtime_local_types[name] = typ
				}
			} else {
				g.expr(lhs)
				op_str := match node.op {
					.assign { '=' }
					.plus_assign { '+=' }
					.minus_assign { '-=' }
					.mul_assign { '*=' }
					.div_assign { '/=' }
					.mod_assign { '%=' }
					.and_assign { '&=' }
					.or_assign { '|=' }
					.xor_assign { '^=' }
					.left_shift_assign { '<<=' }
					.right_shift_assign { '>>=' }
					else { '=' }
				}

				g.sb.write_string(' ${op_str} ')
				g.expr(rhs)
			}
		}
		ast.ExprStmt {
			g.expr(node.expr)
		}
		else {}
	}
}

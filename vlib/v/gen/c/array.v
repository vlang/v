// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import strings
import v.ast

fn (mut g Gen) array_init(node ast.ArrayInit, var_name string) {
	array_type := g.unwrap(node.typ)
	mut array_styp := ''
	elem_type := g.unwrap(node.elem_type)
	mut shared_styp := '' // only needed for shared &[]{...}
	is_amp := g.is_amp
	g.is_amp = false
	if is_amp {
		g.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	if g.is_shared {
		shared_styp = g.typ(array_type.typ.set_flag(.shared_f))
		g.writeln('(${shared_styp}*)__dup_shared_array(&(${shared_styp}){.mtx = {0}, .val =')
	} else if is_amp {
		array_styp = g.typ(array_type.typ)
		g.write('HEAP(${array_styp}, ')
	}
	len := node.exprs.len
	if array_type.unaliased_sym.kind == .array_fixed {
		g.fixed_array_init(node, array_type, var_name, is_amp)
		if is_amp {
			g.write(')')
		}
	} else if len == 0 {
		// `[]int{len: 6, cap:10, init:22}`
		g.array_init_with_fields(node, elem_type, is_amp, shared_styp, var_name)
	} else {
		// `[1, 2, 3]`
		elem_styp := g.typ(elem_type.typ)
		noscan := g.check_noscan(elem_type.typ)
		if elem_type.unaliased_sym.kind == .function {
			g.write('new_array_from_c_array(${len}, ${len}, sizeof(voidptr), _MOV((voidptr[${len}]){')
		} else {
			g.write('new_array_from_c_array${noscan}(${len}, ${len}, sizeof(${elem_styp}), _MOV((${elem_styp}[${len}]){')
		}
		if len > 8 {
			g.writeln('')
			g.write('\t\t')
		}
		for i, expr in node.exprs {
			if node.expr_types[i] == ast.string_type
				&& expr !in [ast.StringLiteral, ast.StringInterLiteral] {
				g.write('string_clone(')
				g.expr(expr)
				g.write(')')
			} else {
				if node.elem_type.has_flag(.option) {
					g.expr_with_opt(expr, node.expr_types[i], node.elem_type)
				} else if elem_type.unaliased_sym.kind == .array_fixed
					&& expr in [ast.Ident, ast.SelectorExpr] {
					info := elem_type.unaliased_sym.info as ast.ArrayFixed
					g.fixed_array_var_init(expr, info.size)
				} else {
					g.expr_with_cast(expr, node.expr_types[i], node.elem_type)
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
	}
}

fn (mut g Gen) fixed_array_init(node ast.ArrayInit, array_type Type, var_name string, is_amp bool) {
	prev_inside_lambda := g.inside_lambda
	g.inside_lambda = true
	defer {
		g.inside_lambda = prev_inside_lambda
	}
	if node.has_index {
		past := g.past_tmp_var_from_var_name(var_name)
		defer {
			g.past_tmp_var_done(past)
		}

		ret_typ := g.typ(node.typ)
		elem_typ := g.typ(node.elem_type)
		if var_name.len == 0 {
			g.write('${ret_typ} ${past.tmp_var} =')
		}
		g.write('{')
		if node.has_val {
			for i in 0 .. node.exprs.len {
				g.write('0')
				if i != node.exprs.len - 1 {
					g.write(', ')
				}
			}
		} else if node.has_init {
			info := array_type.unaliased_sym.info as ast.ArrayFixed
			for i in 0 .. info.size {
				g.write('0')
				if i != info.size - 1 {
					g.write(', ')
				}
			}
		} else {
			g.write('0')
		}
		g.write('}')
		g.writeln(';')
		g.writeln('{')
		g.indent++
		g.writeln('${elem_typ}* pelem = (${elem_typ}*)${past.tmp_var};')
		g.writeln('int _len = (int)sizeof(${past.tmp_var}) / sizeof(${elem_typ});')
		g.writeln('for (int index=0; index<_len; index++, pelem++) {')
		g.set_current_pos_as_last_stmt_pos()
		g.indent++
		g.writeln('int it = index;') // FIXME: Remove this line when it is fully forbidden
		g.write('*pelem = ')
		g.expr_with_init(node)
		g.writeln(';')
		g.indent--
		g.writeln('}')
		g.indent--
		g.writeln('}')
		g.set_current_pos_as_last_stmt_pos()
		return
	}
	need_tmp_var := g.inside_call && !g.inside_struct_init && node.exprs.len == 0
	mut stmt_str := ''
	mut tmp_var := ''
	if need_tmp_var {
		tmp_var = g.new_tmp_var()
		stmt_str = g.go_before_last_stmt()
		g.empty_line = true

		ret_typ := g.typ(node.typ)
		g.write('${ret_typ} ${tmp_var} = ')
	}
	if g.inside_struct_init && g.inside_cast {
		ret_typ := g.typ(node.typ)
		g.write('(${ret_typ})')
	}
	g.write('{')
	if node.has_val {
		tmp_inside_array := g.inside_array_item
		g.inside_array_item = true
		defer {
			g.inside_array_item = tmp_inside_array
		}
		elem_type := (array_type.unaliased_sym.info as ast.ArrayFixed).elem_type
		elem_sym := g.table.final_sym(elem_type)
		for i, expr in node.exprs {
			if elem_sym.kind == .array_fixed && expr in [ast.Ident, ast.SelectorExpr] {
				info := elem_sym.info as ast.ArrayFixed
				g.fixed_array_var_init(expr, info.size)
			} else {
				if expr.is_auto_deref_var() {
					g.write('*')
				}
				g.expr(expr)
			}
			if i != node.exprs.len - 1 {
				g.write(', ')
			}
		}
	} else if node.has_init {
		info := array_type.unaliased_sym.info as ast.ArrayFixed
		for i in 0 .. info.size {
			g.expr_with_init(node)
			if i != info.size - 1 {
				g.write(', ')
			}
		}
	} else if is_amp {
		g.write('0')
	} else {
		elem_sym := g.table.final_sym(node.elem_type)
		if elem_sym.kind == .map {
			// fixed array for map -- [N]map[key_type]value_type
			info := array_type.unaliased_sym.info as ast.ArrayFixed
			map_info := elem_sym.map_info()
			for i in 0 .. info.size {
				g.expr(ast.MapInit{
					key_type: map_info.key_type
					value_type: map_info.value_type
				})
				if i != info.size - 1 {
					g.write(', ')
				}
			}
		} else if elem_sym.kind == .array_fixed {
			// nested fixed array -- [N][N]type
			info := array_type.unaliased_sym.info as ast.ArrayFixed
			arr_info := elem_sym.array_fixed_info()
			for i in 0 .. info.size {
				g.expr(ast.ArrayInit{
					exprs: [ast.IntegerLiteral{}]
					typ: node.elem_type
					elem_type: arr_info.elem_type
				})
				if i != info.size - 1 {
					g.write(', ')
				}
			}
		} else if elem_sym.kind == .chan {
			// fixed array for chan -- [N]chan
			info := array_type.unaliased_sym.info as ast.ArrayFixed
			chan_info := elem_sym.chan_info()
			for i in 0 .. info.size {
				g.expr(ast.ChanInit{
					typ: node.elem_type
					elem_type: chan_info.elem_type
				})
				if i != info.size - 1 {
					g.write(', ')
				}
			}
		} else {
			info := array_type.unaliased_sym.info as ast.ArrayFixed
			for i in 0 .. info.size {
				if node.elem_type.has_flag(.option) {
					g.expr_with_opt(ast.None{}, ast.none_type, node.elem_type)
				} else {
					g.write(g.type_default(node.elem_type))
				}
				if i != info.size - 1 {
					g.write(', ')
				}
			}
		}
	}
	g.write('}')
	if need_tmp_var {
		g.writeln(';')
		g.write(stmt_str)
		g.write(tmp_var)
	}
}

fn (mut g Gen) expr_with_init(node ast.ArrayInit) {
	if node.elem_type.has_flag(.option) {
		g.expr_with_opt(node.init_expr, node.init_type, node.elem_type)
	} else {
		g.expr_with_cast(node.init_expr, node.init_type, node.elem_type)
	}
}

fn (mut g Gen) struct_has_array_or_map_field(elem_typ ast.Type) bool {
	unaliased_sym := g.table.final_sym(elem_typ)
	if unaliased_sym.kind == .struct_ {
		info := unaliased_sym.info as ast.Struct
		for field in info.fields {
			field_sym := g.table.final_sym(field.typ)
			if field_sym.kind in [.array, .map] {
				return true
			}
		}
	}
	return false
}

// `[]int{len: 6, cap: 10, init: index * index}`
fn (mut g Gen) array_init_with_fields(node ast.ArrayInit, elem_type Type, is_amp bool, shared_styp string, var_name string) {
	prev_inside_lambda := g.inside_lambda
	g.inside_lambda = true
	defer {
		g.inside_lambda = prev_inside_lambda
	}
	elem_styp := g.typ(elem_type.typ)
	noscan := g.check_noscan(elem_type.typ)
	is_default_array := elem_type.unaliased_sym.kind == .array && node.has_init
	is_default_map := elem_type.unaliased_sym.kind == .map && node.has_init
	needs_more_defaults := node.has_len && (g.struct_has_array_or_map_field(elem_type.typ)
		|| elem_type.unaliased_sym.kind in [.array, .map])
	if node.has_index {
		// []int{len: 6, init: index * index} when variable it is used in init expression
		past := g.past_tmp_var_from_var_name(var_name)
		defer {
			g.past_tmp_var_done(past)
		}

		ret_typ := g.typ(node.typ)
		elem_typ := g.typ(node.elem_type)
		if var_name.len == 0 {
			g.write('${ret_typ} ${past.tmp_var} =')
		}
		if is_default_array {
			g.write('__new_array_with_array_default${noscan}(')
		} else if is_default_map {
			g.write('__new_array_with_map_default${noscan}(')
		} else {
			g.write('__new_array_with_default${noscan}(')
		}
		if node.has_len {
			g.expr(node.len_expr)
			g.write(', ')
		} else {
			g.write('0, ')
		}
		if node.has_cap {
			g.expr(node.cap_expr)
			g.write(', ')
		} else {
			g.write('0, ')
		}
		if elem_type.unaliased_sym.kind == .function {
			g.write('sizeof(voidptr), ')
		} else {
			g.write('sizeof(${elem_styp}), ')
		}
		if is_default_array {
			info := elem_type.unaliased_sym.info as ast.Array
			depth := if g.table.sym(info.elem_type).kind == .array {
				1
			} else {
				0
			}
			g.write('(${elem_styp}[]){')
			g.write(g.type_default(node.elem_type))
			g.write('}[0], ${depth})')
		} else if node.has_len && node.elem_type == ast.string_type {
			g.write('&(${elem_styp}[]){')
			g.write('_SLIT("")')
			g.write('})')
		} else if node.has_len && elem_type.unaliased_sym.kind in [.array, .map] {
			g.write('(voidptr)&(${elem_styp}[]){')
			g.write(g.type_default(node.elem_type))
			g.write('}[0])')
		} else {
			g.write('0)')
		}
		if g.is_shared {
			g.write('}, sizeof(${shared_styp}))')
		} else if is_amp {
			g.write(')')
		}
		g.writeln(';')
		g.writeln('{')
		g.indent++
		g.writeln('${elem_typ}* pelem = (${elem_typ}*)${past.tmp_var}.data;')
		g.writeln('for (int index=0; index<${past.tmp_var}.len; index++, pelem++) {')
		g.set_current_pos_as_last_stmt_pos()
		g.indent++
		g.writeln('int it = index;') // FIXME: Remove this line when it is fully forbidden
		g.write('*pelem = ')
		g.expr_with_init(node)
		g.writeln(';')
		g.indent--
		g.writeln('}')
		g.indent--
		g.writeln('}')
		g.set_current_pos_as_last_stmt_pos()
		return
	}
	if is_default_array {
		g.write('__new_array_with_array_default${noscan}(')
	} else if is_default_map {
		g.write('__new_array_with_map_default${noscan}(')
	} else if needs_more_defaults {
		g.write('__new_array_with_multi_default${noscan}(')
	} else {
		g.write('__new_array_with_default${noscan}(')
	}
	if node.has_len {
		g.expr(node.len_expr)
		g.write(', ')
	} else {
		g.write('0, ')
	}
	if node.has_cap {
		g.expr(node.cap_expr)
		g.write(', ')
	} else {
		g.write('0, ')
	}
	if elem_type.unaliased_sym.kind == .function {
		g.write('sizeof(voidptr), ')
	} else {
		g.write('sizeof(${elem_styp}), ')
	}
	if is_default_array {
		info := elem_type.unaliased_sym.info as ast.Array
		depth := if g.table.sym(info.elem_type).kind == .array {
			1
		} else {
			0
		}
		g.write('(${elem_styp}[]){')
		g.expr(node.init_expr)
		g.write('}[0], ${depth})')
	} else if is_default_map {
		g.write('(${elem_styp}[]){')
		g.expr(node.init_expr)
		g.write('}[0])')
	} else if needs_more_defaults {
		tmp := g.new_tmp_var()
		line := g.go_before_last_stmt().trim_space()
		g.empty_line = true

		g.write('${elem_styp}* ${tmp} = (${elem_styp}*) _v_malloc((')
		g.expr(node.len_expr)
		g.writeln(') * sizeof(${elem_styp}));')
		ind := g.new_tmp_var()
		g.write('for (int ${ind}=0; ${ind}<')
		g.expr(node.len_expr)
		g.writeln('; ${ind}++) {')
		g.write('\t${tmp}[${ind}] = ')
		if node.has_init {
			g.expr_with_init(node)
		} else {
			if node.elem_type.has_flag(.option) {
				g.expr_with_opt(ast.None{}, ast.none_type, node.elem_type)
			} else {
				g.write(g.type_default(node.elem_type))
			}
		}
		g.writeln(';')
		g.writeln('}')
		g.write(line)
		g.write(' (voidptr)${tmp})')
	} else if node.has_init {
		g.write('&(${elem_styp}[]){')
		g.expr_with_init(node)
		g.write('})')
	} else if node.has_len && node.elem_type.has_flag(.option) {
		g.write('&')
		g.expr_with_opt(ast.None{}, ast.none_type, node.elem_type)
		g.write(')')
	} else if node.has_len && node.elem_type == ast.string_type {
		g.write('&(${elem_styp}[]){')
		g.write('_SLIT("")')
		g.write('})')
	} else if node.has_len && elem_type.unaliased_sym.kind in [.struct_, .array, .map] {
		g.write('(voidptr)&(${elem_styp}[]){')
		g.write(g.type_default(node.elem_type))
		g.write('}[0])')
	} else {
		g.write('0)')
	}
	if g.is_shared {
		g.write('}, sizeof(${shared_styp}))')
	} else if is_amp {
		g.write(')')
	}
}

fn (mut g Gen) write_closure_fn(mut expr ast.AnonFn, var_name string) {
	past := g.past_tmp_var_new()
	fn_ptr_name := g.fn_var_signature(expr.decl.return_type, expr.decl.params.map(it.typ),
		past.tmp_var)
	g.write('${fn_ptr_name} = ')
	g.gen_anon_fn(mut expr)
	g.writeln(';')
	g.past_tmp_var_done(past)
	g.write('(${var_name})') // usually `it`
}

// `nums.map(it % 2 == 0)`
fn (mut g Gen) gen_array_map(node ast.CallExpr) {
	prev_inside_lambda := g.inside_lambda
	g.inside_lambda = true
	defer {
		g.inside_lambda = prev_inside_lambda
	}

	past := g.past_tmp_var_new()
	defer {
		g.past_tmp_var_done(past)
	}

	ret_typ := g.typ(node.return_type)
	ret_sym := g.table.final_sym(node.return_type)
	inp_sym := g.table.final_sym(node.receiver_type)
	ret_info := ret_sym.info as ast.Array
	mut ret_elem_type := g.typ(ret_info.elem_type)
	inp_info := inp_sym.info as ast.Array
	inp_elem_type := g.typ(inp_info.elem_type)
	if inp_sym.kind != .array {
		verror('map() requires an array')
	}

	mut expr := node.args[0].expr
	mut closure_var_decl := ''
	tmp_map_expr_result_name := g.new_tmp_var()
	if mut expr is ast.SelectorExpr {
		if expr.typ != ast.void_type {
			var_sym := g.table.sym(expr.typ)
			if var_sym.info is ast.FnType {
				ret_elem_type = 'voidptr'
				closure_var_decl = g.fn_var_signature(var_sym.info.func.return_type, var_sym.info.func.params.map(it.typ),
					tmp_map_expr_result_name)
			}
		}
	}
	noscan := g.check_noscan(ret_info.elem_type)
	has_infix_left_var_name := g.write_prepared_tmp_value(past.tmp_var, node, ret_typ,
		'{0}')
	g.writeln('${past.tmp_var} = __new_array${noscan}(0, ${past.tmp_var}_len, sizeof(${ret_elem_type}));\n')
	i := g.new_tmp_var()
	g.writeln('for (int ${i} = 0; ${i} < ${past.tmp_var}_len; ++${i}) {')
	g.indent++
	var_name := g.get_array_expr_param_name(mut expr)
	g.write_prepared_var(var_name, inp_info, inp_elem_type, past.tmp_var, i)
	g.set_current_pos_as_last_stmt_pos()
	mut is_embed_map_filter := false
	match mut expr {
		ast.AnonFn {
			g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			if expr.inherited_vars.len > 0 {
				g.write_closure_fn(mut expr, var_name)
			} else {
				g.gen_anon_fn_decl(mut expr)
				g.write('${expr.decl.name}(${var_name})')
			}
		}
		ast.Ident {
			g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(${var_name})')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym := g.table.sym(var_info.typ)
				if sym.kind == .function {
					g.write('${c_name(expr.name)}(${var_name})')
				} else {
					g.expr(expr)
				}
			} else {
				g.expr(expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter', 'all', 'any'] {
				is_embed_map_filter = true
				g.set_current_pos_as_last_stmt_pos()
			}
			g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			g.expr(expr)
		}
		ast.CastExpr {
			// value.map(Type(it)) when `value` is a comptime var
			if expr.expr is ast.Ident && node.left is ast.Ident
				&& g.comptime.is_comptime_var(node.left) {
				ctyp := g.comptime.get_comptime_var_type(node.left)
				if ctyp != ast.void_type {
					expr.expr_type = g.table.value_type(ctyp)
				}
			}
			g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			g.expr(expr)
		}
		ast.LambdaExpr {
			g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			g.expr(expr.expr)
		}
		else {
			if closure_var_decl != '' {
				g.write('${closure_var_decl} = ')
			} else {
				g.write('${ret_elem_type} ${tmp_map_expr_result_name} = ')
			}
			g.expr(expr)
		}
	}
	g.writeln(';')
	g.writeln('array_push${noscan}((array*)&${past.tmp_var}, &${tmp_map_expr_result_name});')
	g.indent--
	g.writeln('}')
	if !is_embed_map_filter {
		g.set_current_pos_as_last_stmt_pos()
	}
	if has_infix_left_var_name {
		g.indent--
		g.writeln('}')
	}
}

// `susers := users.sorted(a.age < b.age)`
fn (mut g Gen) gen_array_sorted(node ast.CallExpr) {
	past := g.past_tmp_var_new()
	defer {
		g.past_tmp_var_done(past)
	}
	atype := g.typ(node.return_type)
	sym := g.table.final_sym(node.return_type)
	info := sym.info as ast.Array
	depth := g.get_array_depth(info.elem_type)

	deref_field := if node.receiver_type.nr_muls() > node.left_type.nr_muls()
		&& node.left_type.is_ptr() {
		true
	} else {
		false
	}
	if !deref_field {
		g.write('${atype} ${past.tmp_var} = array_clone_to_depth(ADDR(${atype},')
		g.expr(node.left)
		g.writeln('), ${depth});')
	} else {
		g.write('${atype} ${past.tmp_var} = array_clone_to_depth(')
		g.expr(node.left)
		g.writeln(', ${depth});')
	}

	unsafe {
		node.left = ast.Expr(ast.Ident{
			name: past.tmp_var
		})
	}
	g.gen_array_sort(node)
	g.writeln(';')
}

// `users.sort(a.age < b.age)`
fn (mut g Gen) gen_array_sort(node ast.CallExpr) {
	// println('filter s="$s"')
	rec_sym := g.table.final_sym(node.receiver_type)
	if rec_sym.kind != .array {
		// println(rec_sym.kind)
		verror('.sort() is an array method')
	}
	if g.pref.is_bare {
		g.writeln('bare_panic(_SLIT("sort does not work with -freestanding"))')
		return
	}
	info := rec_sym.info as ast.Array
	// `users.sort(a.age > b.age)`
	// Generate a comparison function for a custom type
	elem_stype := g.typ(info.elem_type)
	mut compare_fn := 'compare_${g.unique_file_path_hash}_${elem_stype.replace('*', '_ptr')}'
	mut comparison_type := g.unwrap(ast.void_type)
	mut left_expr, mut right_expr := '', ''
	mut use_lambda := false
	mut lambda_fn_name := ''
	// the only argument can only be an infix expression like `a < b` or `b.field > a.field`
	if node.args.len == 0 {
		comparison_type = g.unwrap(info.elem_type.set_nr_muls(0))
		rlock g.array_sort_fn {
			if compare_fn in g.array_sort_fn {
				g.gen_array_sort_call(node, compare_fn)
				return
			}
		}
		left_expr = '*a'
		right_expr = '*b'
	} else if node.args[0].expr is ast.LambdaExpr {
		lambda_fn_name = node.args[0].expr.func.decl.name
		compare_fn = '${lambda_fn_name}_lambda_wrapper'
		use_lambda = true
		mut lambda_node := unsafe { node.args[0].expr }
		g.gen_anon_fn_decl(mut lambda_node.func)
	} else {
		infix_expr := node.args[0].expr as ast.InfixExpr
		comparison_type = g.unwrap(infix_expr.left_type.set_nr_muls(0))
		left_name := infix_expr.left.str()
		if left_name.len > 1 {
			compare_fn += '_by' +
				left_name[1..].replace_each(['.', '_', '[', '_', ']', '_', "'", '_', '"', '_', '(', '', ')', '', ',', ''])
		}
		// is_reverse is `true` for `.sort(a > b)` and `.sort(b < a)`
		is_reverse := (left_name.starts_with('a') && infix_expr.op == .gt)
			|| (left_name.starts_with('b') && infix_expr.op == .lt)
		if is_reverse {
			compare_fn += '_reverse'
		}
		rlock g.array_sort_fn {
			if compare_fn in g.array_sort_fn {
				g.gen_array_sort_call(node, compare_fn)
				return
			}
		}
		if left_name.starts_with('a') != is_reverse {
			left_expr = g.expr_string(infix_expr.left)
			right_expr = g.expr_string(infix_expr.right)
			if infix_expr.left is ast.Ident {
				left_expr = '*' + left_expr
			}
			if infix_expr.right is ast.Ident {
				right_expr = '*' + right_expr
			}
		} else {
			left_expr = g.expr_string(infix_expr.right)
			right_expr = g.expr_string(infix_expr.left)
			if infix_expr.left is ast.Ident {
				right_expr = '*' + right_expr
			}
			if infix_expr.right is ast.Ident {
				left_expr = '*' + left_expr
			}
		}
	}

	// Register a new custom `compare_xxx` function for qsort()
	// TODO: move to checker
	lock g.array_sort_fn {
		g.array_sort_fn << compare_fn
	}

	stype_arg := g.typ(info.elem_type)
	g.definitions.writeln('VV_LOCAL_SYMBOL ${g.static_modifier} int ${compare_fn}(${stype_arg}* a, ${stype_arg}* b) {')
	c_condition := if comparison_type.sym.has_method('<') {
		'${g.typ(comparison_type.typ)}__lt(${left_expr}, ${right_expr})'
	} else if comparison_type.unaliased_sym.has_method('<') {
		'${g.typ(comparison_type.unaliased)}__lt(${left_expr}, ${right_expr})'
	} else if use_lambda {
		'${lambda_fn_name}(a, b)'
	} else {
		'${left_expr} < ${right_expr}'
	}
	g.definitions.writeln('\tif (${c_condition}) return -1;')
	g.definitions.writeln('\telse return 1;')
	g.definitions.writeln('}\n')

	// write call to the generated function
	g.gen_array_sort_call(node, compare_fn)
}

fn (mut g Gen) gen_array_sort_call(node ast.CallExpr, compare_fn string) {
	deref_field := if node.receiver_type.nr_muls() > node.left_type.nr_muls()
		&& node.left_type.is_ptr() {
		g.dot_or_ptr(node.left_type.deref())
	} else {
		g.dot_or_ptr(node.left_type)
	}
	// eprintln('> qsort: pointer $node.left_type | deref_field: `$deref_field`')
	g.empty_line = true
	g.write('qsort(')
	g.expr(node.left)
	g.write('${deref_field}data, ')
	g.expr(node.left)
	g.write('${deref_field}len, ')
	g.expr(node.left)
	g.write('${deref_field}element_size, (int (*)(const void *, const void *))&${compare_fn})')
}

// `nums.filter(it % 2 == 0)`
fn (mut g Gen) gen_array_filter(node ast.CallExpr) {
	past := g.past_tmp_var_new()
	defer {
		g.past_tmp_var_done(past)
	}

	sym := g.table.final_sym(node.return_type)
	if sym.kind != .array {
		verror('filter() requires an array')
	}
	info := sym.info as ast.Array
	styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	noscan := g.check_noscan(info.elem_type)
	has_infix_left_var_name := g.write_prepared_tmp_value(past.tmp_var, node, styp, '{0}')
	g.writeln('${past.tmp_var} = __new_array${noscan}(0, ${past.tmp_var}_len, sizeof(${elem_type_str}));\n')
	i := g.new_tmp_var()
	g.writeln('for (int ${i} = 0; ${i} < ${past.tmp_var}_len; ++${i}) {')
	g.indent++
	mut expr := node.args[0].expr
	var_name := g.get_array_expr_param_name(mut expr)
	g.write_prepared_var(var_name, info, elem_type_str, past.tmp_var, i)
	g.set_current_pos_as_last_stmt_pos()
	mut is_embed_map_filter := false
	match mut expr {
		ast.AnonFn {
			g.write('if (')
			if expr.inherited_vars.len > 0 {
				g.write_closure_fn(mut expr, var_name)
			} else {
				g.gen_anon_fn_decl(mut expr)
				g.write('${expr.decl.name}(${var_name})')
			}
		}
		ast.Ident {
			g.write('if (')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(${var_name})')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.sym(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(${var_name})')
				} else {
					g.expr(expr)
				}
			} else {
				g.expr(expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter', 'all', 'any'] {
				is_embed_map_filter = true
				g.set_current_pos_as_last_stmt_pos()
			}
			g.write('if (')
			g.expr(expr)
		}
		ast.LambdaExpr {
			g.write('if (')
			g.expr(expr.expr)
		}
		else {
			g.write('if (')
			g.expr(expr)
		}
	}
	g.writeln(') {')
	g.writeln('\tarray_push${noscan}((array*)&${past.tmp_var}, &${var_name});')
	g.writeln('}')
	g.indent--
	g.writeln('}')
	if !is_embed_map_filter {
		g.set_current_pos_as_last_stmt_pos()
	}
	if has_infix_left_var_name {
		g.indent--
		g.writeln('}')
	}
}

// `nums.insert(0, 2)` `nums.insert(0, [2,3,4])`
fn (mut g Gen) gen_array_insert(node ast.CallExpr) {
	left_sym := g.table.sym(node.left_type)
	left_info := left_sym.info as ast.Array
	elem_type_str := g.typ(left_info.elem_type)
	arg2_sym := g.table.sym(node.args[1].typ)
	is_arg2_array := arg2_sym.kind == .array
		&& node.args[1].typ.clear_flag(.variadic) == node.left_type
	noscan := g.check_noscan(left_info.elem_type)
	addr := if node.left_type.is_ptr() { '' } else { '&' }
	if is_arg2_array {
		g.write('array_insert_many${noscan}(${addr}')
	} else {
		g.write('array_insert${noscan}(${addr}')
	}
	g.expr(node.left)
	g.write(', ')
	g.expr(node.args[0].expr)
	if is_arg2_array {
		g.write(', ')
		g.expr(node.args[1].expr)
		g.write('.data, ')
		g.expr(node.args[1].expr)
		g.write('.len)')
	} else {
		g.write(', &(${elem_type_str}[]){')
		if left_info.elem_type == ast.string_type {
			g.write('string_clone(')
		}
		g.expr_with_cast(node.args[1].expr, node.args[1].typ, left_info.elem_type)
		if left_info.elem_type == ast.string_type {
			g.write(')')
		}
		g.write('})')
	}
}

// `nums.prepend(2)` `nums.prepend([2,3,4])`
fn (mut g Gen) gen_array_prepend(node ast.CallExpr) {
	left_sym := g.table.sym(node.left_type)
	left_info := left_sym.info as ast.Array
	elem_type_str := g.typ(left_info.elem_type)
	arg_sym := g.table.sym(node.args[0].typ)
	is_arg_array := arg_sym.kind == .array && node.args[0].typ == node.left_type
	noscan := g.check_noscan(left_info.elem_type)
	addr := if node.left_type.is_ptr() { '' } else { '&' }
	if is_arg_array {
		g.write('array_prepend_many${noscan}(${addr}')
	} else {
		g.write('array_prepend${noscan}(${addr}')
	}
	g.expr(node.left)
	if is_arg_array {
		g.write(', ')
		g.expr(node.args[0].expr)
		g.write('.data, ')
		g.expr(node.args[0].expr)
		g.write('.len)')
	} else {
		g.write(', &(${elem_type_str}[]){')
		g.expr_with_cast(node.args[0].expr, node.args[0].typ, left_info.elem_type)
		g.write('})')
	}
}

fn (mut g Gen) get_array_contains_method(typ ast.Type) string {
	t := g.table.final_sym(g.unwrap_generic(typ).set_nr_muls(0)).idx
	g.array_contains_types << t
	return g.typ(t) + '_contains'
}

fn (mut g Gen) gen_array_contains_methods() {
	mut done := []ast.Type{}
	for t in g.array_contains_types {
		left_final_sym := g.table.final_sym(t)
		if left_final_sym.idx in done || g.table.sym(t).has_method('contains') {
			continue
		}
		done << t
		mut fn_builder := strings.new_builder(512)
		mut left_type_str := g.typ(t)
		fn_name := '${left_type_str}_contains'

		if left_final_sym.kind == .array {
			elem_type := (left_final_sym.info as ast.Array).elem_type
			mut elem_type_str := g.typ(elem_type)
			elem_kind := g.table.sym(elem_type).kind
			elem_is_not_ptr := elem_type.nr_muls() == 0
			if elem_kind == .function {
				left_type_str = 'Array_voidptr'
				elem_type_str = 'voidptr'
			}
			g.type_definitions.writeln('static bool ${fn_name}(${left_type_str} a, ${elem_type_str} v); // auto')
			fn_builder.writeln('static bool ${fn_name}(${left_type_str} a, ${elem_type_str} v) {')
			fn_builder.writeln('\tfor (int i = 0; i < a.len; ++i) {')
			if elem_kind == .string {
				fn_builder.writeln('\t\tif (fast_string_eq(((string*)a.data)[i], v)) {')
			} else if elem_kind == .array && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else if elem_kind == .function {
				fn_builder.writeln('\t\tif (((voidptr*)a.data)[i] == v) {')
			} else if elem_kind == .map && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else if elem_kind == .struct_ && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else if elem_kind == .interface_ && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_interface_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else if elem_kind == .sum_type && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_sumtype_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else if elem_kind == .alias && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_alias_eq(((${elem_type_str}*)a.data)[i], v)) {')
			} else {
				fn_builder.writeln('\t\tif (((${elem_type_str}*)a.data)[i] == v) {')
			}
		} else if left_final_sym.kind == .array_fixed {
			left_info := left_final_sym.info as ast.ArrayFixed
			size := left_info.size
			elem_type := left_info.elem_type
			mut elem_type_str := g.typ(elem_type)
			elem_kind := g.table.sym(elem_type).kind
			elem_is_not_ptr := elem_type.nr_muls() == 0
			if elem_kind == .function {
				left_type_str = 'Array_voidptr'
				elem_type_str = 'voidptr'
			}
			g.type_definitions.writeln('static bool ${fn_name}(${left_type_str} a, ${elem_type_str} v); // auto')
			fn_builder.writeln('static bool ${fn_name}(${left_type_str} a, ${elem_type_str} v) {')
			fn_builder.writeln('\tfor (int i = 0; i < ${size}; ++i) {')
			if elem_kind == .string {
				fn_builder.writeln('\t\tif (fast_string_eq(a[i], v)) {')
			} else if elem_kind == .array && elem_is_not_ptr {
				ptr_typ := g.equality_fn(left_info.elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(a[i], v)) {')
			} else if elem_kind == .function {
				fn_builder.writeln('\t\tif (a[i] == v) {')
			} else if elem_kind == .map && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(a[i], v)) {')
			} else if elem_kind == .struct_ && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(a[i], v)) {')
			} else if elem_kind == .interface_ && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_interface_eq(a[i], v)) {')
			} else if elem_kind == .sum_type && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_sumtype_eq(a[i], v)) {')
			} else if elem_kind == .alias && elem_is_not_ptr {
				ptr_typ := g.equality_fn(elem_type)
				fn_builder.writeln('\t\tif (${ptr_typ}_alias_eq(a[i], v)) {')
			} else {
				fn_builder.writeln('\t\tif (a[i] == v) {')
			}
		}
		fn_builder.writeln('\t\t\treturn true;')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn false;')
		fn_builder.writeln('}')
		g.auto_fn_definitions << fn_builder.str()
	}
}

// `nums.contains(2)`
fn (mut g Gen) gen_array_contains(left_type ast.Type, left ast.Expr, right_type ast.Type, right ast.Expr) {
	fn_name := g.get_array_contains_method(left_type)
	g.write('${fn_name}(')
	g.write(strings.repeat(`*`, left_type.nr_muls()))
	if left_type.share() == .shared_t {
		g.go_back(1)
	}
	g.expr(left)
	if left_type.share() == .shared_t {
		g.write('->val')
	}
	g.write(', ')
	left_sym := g.table.final_sym(left_type)
	elem_typ := if left_sym.kind == .array {
		left_sym.array_info().elem_type
	} else {
		left_sym.array_fixed_info().elem_type
	}
	if (right.is_auto_deref_var() && !elem_typ.is_ptr())
		|| (g.table.sym(elem_typ).kind !in [.interface_, .sum_type, .struct_] && right is ast.Ident
		&& right.info is ast.IdentVar
		&& g.table.sym(right.obj.typ).kind in [.interface_, .sum_type]) {
		g.write('*')
	}
	if g.table.sym(elem_typ).kind in [.interface_, .sum_type] {
		g.expr_with_cast(right, right_type, elem_typ)
	} else {
		g.expr(right)
	}
	g.write(')')
}

fn (mut g Gen) get_array_index_method(typ ast.Type) string {
	t := g.unwrap_generic(typ).set_nr_muls(0)
	g.array_index_types << t
	return g.typ(t) + '_index'
}

fn (mut g Gen) gen_array_index_methods() {
	mut done := []ast.Type{}
	for t in g.array_index_types {
		if t in done || g.table.sym(t).has_method('index') {
			continue
		}
		done << t
		final_left_sym := g.table.final_sym(t)
		mut left_type_str := g.typ(t)
		fn_name := '${left_type_str}_index'
		info := final_left_sym.info as ast.Array
		mut elem_type_str := g.typ(info.elem_type)
		elem_sym := g.table.sym(info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
			elem_type_str = 'voidptr'
		}
		g.type_definitions.writeln('static int ${fn_name}(${left_type_str} a, ${elem_type_str} v); // auto')
		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('static int ${fn_name}(${left_type_str} a, ${elem_type_str} v) {')
		fn_builder.writeln('\t${elem_type_str}* pelem = a.data;')
		fn_builder.writeln('\tfor (int i = 0; i < a.len; ++i, ++pelem) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (fast_string_eq(*pelem, v)) {')
		} else if elem_sym.kind == .array && !info.elem_type.is_ptr() {
			ptr_typ := g.equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(*pelem, v)) {')
		} else if elem_sym.kind == .function && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif ( pelem == v) {')
		} else if elem_sym.kind == .map && !info.elem_type.is_ptr() {
			ptr_typ := g.equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_map_eq((*pelem, v))) {')
		} else if elem_sym.kind == .struct_ && !info.elem_type.is_ptr() {
			ptr_typ := g.equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(*pelem, v)) {')
		} else if elem_sym.kind == .interface_ {
			ptr_typ := g.equality_fn(info.elem_type)
			if info.elem_type.is_ptr() {
				fn_builder.writeln('\t\tif (${ptr_typ}_interface_eq(**pelem, *v)) {')
			} else {
				fn_builder.writeln('\t\tif (${ptr_typ}_interface_eq(*pelem, v)) {')
			}
		} else if elem_sym.kind == .sum_type {
			ptr_typ := g.equality_fn(info.elem_type)
			if info.elem_type.is_ptr() {
				fn_builder.writeln('\t\tif (${ptr_typ}_sumtype_eq(**pelem, *v)) {')
			} else {
				fn_builder.writeln('\t\tif (${ptr_typ}_sumtype_eq(*pelem, v)) {')
			}
		} else if elem_sym.kind == .alias {
			ptr_typ := g.equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_alias_eq(*pelem, v)) {')
		} else {
			fn_builder.writeln('\t\tif (*pelem == v) {')
		}
		fn_builder.writeln('\t\t\treturn i;')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn -1;')
		fn_builder.writeln('}')
		g.auto_fn_definitions << fn_builder.str()
	}
}

// `nums.index(2)`
fn (mut g Gen) gen_array_index(node ast.CallExpr) {
	fn_name := g.get_array_index_method(node.left_type)
	g.write('${fn_name}(')
	if node.left_type.is_ptr() {
		g.write('*')
	}
	g.expr(node.left)
	g.write(', ')
	if node.args[0].expr.is_auto_deref_var() {
		g.write('*')
	}
	elem_typ := g.table.sym(node.left_type).array_info().elem_type
	if g.table.sym(elem_typ).kind in [.interface_, .sum_type] {
		g.expr_with_cast(node.args[0].expr, node.args[0].typ, elem_typ)
	} else {
		g.expr(node.args[0].expr)
	}
	g.write(')')
}

fn (mut g Gen) gen_array_wait(node ast.CallExpr) {
	arr := g.table.sym(g.unwrap_generic(node.receiver_type))
	thread_type := arr.array_info().elem_type
	thread_sym := g.table.sym(thread_type)
	thread_ret_type := thread_sym.thread_info().return_type
	eltyp := g.table.sym(thread_ret_type).cname
	fn_name := g.register_thread_array_wait_call(eltyp)
	g.write('${fn_name}(')
	g.expr(node.left)
	g.write(')')
}

fn (mut g Gen) gen_fixed_array_wait(node ast.CallExpr) {
	arr := g.table.sym(g.unwrap_generic(node.receiver_type))
	thread_type := arr.array_fixed_info().elem_type
	thread_sym := g.table.sym(thread_type)
	thread_ret_type := thread_sym.thread_info().return_type
	eltyp := g.table.sym(thread_ret_type).cname
	fn_name := g.register_thread_fixed_array_wait_call(node, eltyp)
	g.write('${fn_name}(')
	g.expr(node.left)
	g.write(')')
}

fn (mut g Gen) gen_array_any(node ast.CallExpr) {
	past := g.past_tmp_var_new()
	defer {
		g.past_tmp_var_done(past)
	}

	sym := g.table.final_sym(node.left_type)
	info := sym.info as ast.Array
	// styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	has_infix_left_var_name := g.write_prepared_tmp_value(past.tmp_var, node, 'bool',
		'false')
	i := g.new_tmp_var()
	g.writeln('for (int ${i} = 0; ${i} < ${past.tmp_var}_len; ++${i}) {')
	g.indent++
	mut expr := node.args[0].expr
	var_name := g.get_array_expr_param_name(mut expr)
	g.write_prepared_var(var_name, info, elem_type_str, past.tmp_var, i)
	g.set_current_pos_as_last_stmt_pos()
	mut is_embed_map_filter := false
	match mut expr {
		ast.AnonFn {
			g.write('if (')
			if expr.inherited_vars.len > 0 {
				g.write_closure_fn(mut expr, var_name)
			} else {
				g.gen_anon_fn_decl(mut expr)
				g.write('${expr.decl.name}(${var_name})')
			}
		}
		ast.Ident {
			g.write('if (')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(${var_name})')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.sym(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(${var_name})')
				} else {
					g.expr(expr)
				}
			} else {
				g.expr(expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter', 'all', 'any'] {
				is_embed_map_filter = true
				g.set_current_pos_as_last_stmt_pos()
			}
			g.write('if (')
			g.expr(expr)
		}
		ast.LambdaExpr {
			g.write('if (')
			g.expr(expr.expr)
		}
		else {
			g.write('if (')
			g.expr(expr)
		}
	}
	g.writeln(') {')
	g.writeln('\t${past.tmp_var} = true;')
	g.writeln('\tbreak;')
	g.writeln('}')
	g.indent--
	g.writeln('}')
	if !is_embed_map_filter {
		g.set_current_pos_as_last_stmt_pos()
	}
	if has_infix_left_var_name {
		g.indent--
		g.writeln('}')
		g.set_current_pos_as_last_stmt_pos()
	}
}

fn (mut g Gen) gen_array_all(node ast.CallExpr) {
	past := g.past_tmp_var_new()
	defer {
		g.past_tmp_var_done(past)
	}

	sym := g.table.final_sym(node.left_type)
	info := sym.info as ast.Array
	// styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)

	has_infix_left_var_name := g.write_prepared_tmp_value(past.tmp_var, node, 'bool',
		'true')
	i := g.new_tmp_var()
	g.writeln('for (int ${i} = 0; ${i} < ${past.tmp_var}_len; ++${i}) {')
	g.indent++
	mut expr := node.args[0].expr
	var_name := g.get_array_expr_param_name(mut expr)
	g.write_prepared_var(var_name, info, elem_type_str, past.tmp_var, i)
	g.empty_line = true
	g.set_current_pos_as_last_stmt_pos()
	mut is_embed_map_filter := false
	match mut expr {
		ast.AnonFn {
			g.write('if (!(')
			if expr.inherited_vars.len > 0 {
				g.write_closure_fn(mut expr, var_name)
			} else {
				g.gen_anon_fn_decl(mut expr)
				g.write('${expr.decl.name}(${var_name})')
			}
		}
		ast.Ident {
			g.write('if (!(')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(${var_name})')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.sym(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(${var_name})')
				} else {
					g.expr(expr)
				}
			} else {
				g.expr(expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter', 'all', 'any'] {
				is_embed_map_filter = true
				g.set_current_pos_as_last_stmt_pos()
			}
			g.write('if (!(')
			g.expr(expr)
		}
		ast.LambdaExpr {
			g.write('if (!(')
			g.expr(expr.expr)
		}
		else {
			g.write('if (!(')
			g.expr(expr)
		}
	}
	g.writeln(')) {')
	g.writeln('\t${past.tmp_var} = false;')
	g.writeln('\tbreak;')
	g.writeln('}')
	g.indent--
	g.writeln('}')
	if !is_embed_map_filter {
		g.set_current_pos_as_last_stmt_pos()
	}
	if has_infix_left_var_name {
		g.indent--
		g.writeln('}')
		g.set_current_pos_as_last_stmt_pos()
	}
}

fn (mut g Gen) write_prepared_tmp_value(tmp string, node &ast.CallExpr, tmp_stype string, initial_value string) bool {
	g.writeln('${tmp_stype} ${tmp} = ${initial_value};')
	has_infix_left_var_name := g.infix_left_var_name.len > 0
	if has_infix_left_var_name {
		g.writeln('if (${g.infix_left_var_name}) {')
		g.infix_left_var_name = ''
		g.indent++
	}
	left_type := if node.left_type.has_flag(.shared_f) {
		node.left_type.clear_flag(.shared_f).deref()
	} else if node.left_type.is_ptr() {
		node.left_type.deref()
	} else {
		node.left_type
	}
	g.write('${g.typ(left_type)} ${tmp}_orig = ')
	if !node.left_type.has_flag(.shared_f) && node.left_type.is_ptr() {
		g.write('*')
	}
	g.expr(node.left)
	if node.left_type.has_flag(.shared_f) {
		g.write('->val')
	}
	g.writeln(';')
	g.writeln('int ${tmp}_len = ${tmp}_orig.len;')
	return has_infix_left_var_name
}

fn (mut g Gen) write_prepared_var(var_name string, inp_info ast.Array, inp_elem_type string, tmp string, i string) {
	if g.table.sym(inp_info.elem_type).kind == .array_fixed {
		g.writeln('${inp_elem_type} ${var_name};')
		g.writeln('memcpy(&${var_name}, ((${inp_elem_type}*) ${tmp}_orig.data)[${i}], sizeof(${inp_elem_type}));')
	} else {
		g.writeln('${inp_elem_type} ${var_name} = ((${inp_elem_type}*) ${tmp}_orig.data)[${i}];')
	}
}

fn (mut g Gen) fixed_array_var_init(expr ast.Expr, size int) {
	g.write('{')
	for i in 0 .. size {
		if expr.is_auto_deref_var() {
			g.write('(*')
		}
		g.expr(expr)
		if expr.is_auto_deref_var() {
			g.write(')')
		}
		g.write('[${i}]')
		if i != size - 1 {
			g.write(', ')
		}
	}
	g.write('}')
}

fn (mut g Gen) get_array_expr_param_name(mut expr ast.Expr) string {
	return if mut expr is ast.LambdaExpr {
		expr.params[0].name
	} else {
		'it'
	}
}

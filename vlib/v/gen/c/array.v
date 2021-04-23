// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import strings
import v.ast

fn (mut g Gen) array_init(node ast.ArrayInit) {
	type_sym := g.table.get_type_symbol(node.typ)
	styp := g.typ(node.typ)
	mut shared_styp := '' // only needed for shared &[]{...}
	is_amp := g.is_amp
	g.is_amp = false
	if is_amp {
		g.out.go_back(1) // delete the `&` already generated in `prefix_expr()
	}
	if g.is_shared {
		mut shared_typ := node.typ.set_flag(.shared_f)
		shared_styp = g.typ(shared_typ)
		g.writeln('($shared_styp*)__dup_shared_array(&($shared_styp){.mtx = {0}, .val =')
	} else if is_amp {
		g.write('HEAP($styp, ')
	}
	if type_sym.kind == .array_fixed {
		g.write('{')
		if node.has_val {
			for i, expr in node.exprs {
				if expr.is_auto_deref_var() {
					g.write('*')
				}
				g.expr(expr)
				if i != node.exprs.len - 1 {
					g.write(', ')
				}
			}
		} else if node.has_default {
			g.expr(node.default_expr)
			info := type_sym.info as ast.ArrayFixed
			for _ in 1 .. info.size {
				g.write(', ')
				g.expr(node.default_expr)
			}
		} else {
			g.write('0')
		}
		g.write('}')
		return
	}
	elem_type_str := g.typ(node.elem_type)
	if node.exprs.len == 0 {
		elem_sym := g.table.get_type_symbol(node.elem_type)
		is_default_array := elem_sym.kind == .array && node.has_default
		noscan := g.check_noscan(node.elem_type)
		if is_default_array {
			g.write('__new_array_with_array_default${noscan}(')
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
		if elem_sym.kind == .function {
			g.write('sizeof(voidptr), ')
		} else {
			g.write('sizeof($elem_type_str), ')
		}
		if is_default_array {
			g.write('($elem_type_str[]){')
			g.expr(node.default_expr)
			g.write('}[0])')
		} else if node.has_default {
			g.write('&($elem_type_str[]){')
			g.expr(node.default_expr)
			g.write('})')
		} else if node.has_len && node.elem_type == ast.string_type {
			g.write('&($elem_type_str[]){')
			g.write('_SLIT("")')
			g.write('})')
		} else if node.has_len && elem_sym.kind in [.array, .map] {
			g.write('(voidptr)&($elem_type_str[]){')
			g.write(g.type_default(node.elem_type))
			g.write('}[0])')
		} else {
			g.write('0)')
		}
		if g.is_shared {
			g.write('}, sizeof($shared_styp))')
		} else if is_amp {
			g.write(')')
		}
		return
	}
	len := node.exprs.len
	elem_sym := g.table.get_type_symbol(node.elem_type)
	if elem_sym.kind == .function {
		g.write('new_array_from_c_array($len, $len, sizeof(voidptr), _MOV((voidptr[$len]){')
	} else {
		g.write('new_array_from_c_array($len, $len, sizeof($elem_type_str), _MOV(($elem_type_str[$len]){')
	}
	if len > 8 {
		g.writeln('')
		g.write('\t\t')
	}
	for i, expr in node.exprs {
		g.expr_with_cast(expr, node.expr_types[i], node.elem_type)
		if i != len - 1 {
			g.write(', ')
		}
	}
	g.write('}))')
	if g.is_shared {
		g.write('}, sizeof($shared_styp))')
	} else if is_amp {
		g.write('), sizeof($styp))')
	}
}

// `nums.map(it % 2 == 0)`
fn (mut g Gen) gen_array_map(node ast.CallExpr) {
	g.inside_lambda = true
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	// println('filter s="$s"')
	ret_typ := g.typ(node.return_type)
	// inp_typ := g.typ(node.receiver_type)
	ret_sym := g.table.get_type_symbol(node.return_type)
	inp_sym := g.table.get_type_symbol(node.receiver_type)
	ret_info := ret_sym.info as ast.Array
	ret_elem_type := g.typ(ret_info.elem_type)
	inp_info := inp_sym.info as ast.Array
	inp_elem_type := g.typ(inp_info.elem_type)
	if inp_sym.kind != .array {
		verror('map() requires an array')
	}
	g.empty_line = true
	g.write('${g.typ(node.left_type)} ${tmp}_orig = ')
	g.expr(node.left)
	g.writeln(';')
	g.writeln('int ${tmp}_len = ${tmp}_orig.len;')
	noscan := g.check_noscan(ret_info.elem_type)
	g.writeln('$ret_typ $tmp = __new_array${noscan}(0, ${tmp}_len, sizeof($ret_elem_type));\n')
	i := g.new_tmp_var()
	g.writeln('for (int $i = 0; $i < ${tmp}_len; ++$i) {')
	g.writeln('\t$inp_elem_type it = (($inp_elem_type*) ${tmp}_orig.data)[$i];')
	mut is_embed_map_filter := false
	mut expr := node.args[0].expr
	match mut expr {
		ast.AnonFn {
			g.write('\t$ret_elem_type ti = ')
			g.gen_anon_fn_decl(mut expr)
			g.write('${expr.decl.name}(it)')
		}
		ast.Ident {
			g.write('\t$ret_elem_type ti = ')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(it)')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym := g.table.get_type_symbol(var_info.typ)
				if sym.kind == .function {
					g.write('${c_name(expr.name)}(it)')
				} else {
					g.expr(node.args[0].expr)
				}
			} else {
				g.expr(node.args[0].expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter'] {
				is_embed_map_filter = true
				g.stmt_path_pos << g.out.len
			}
			g.write('\t$ret_elem_type ti = ')
			g.expr(node.args[0].expr)
		}
		else {
			g.write('\t$ret_elem_type ti = ')
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(';')
	g.writeln('\tarray_push((array*)&$tmp, &ti);')
	g.writeln('}')
	if !is_embed_map_filter {
		g.stmt_path_pos << g.out.len
	}
	g.write('\n')
	g.write(s)
	g.write(tmp)
	g.inside_lambda = false
}

// `users.sort(a.age < b.age)`
fn (mut g Gen) gen_array_sort(node ast.CallExpr) {
	// println('filter s="$s"')
	rec_sym := g.table.get_type_symbol(node.receiver_type)
	if rec_sym.kind != .array {
		println(node.name)
		println(g.typ(node.receiver_type))
		// println(rec_sym.kind)
		verror('.sort() is an array method')
	}
	info := rec_sym.info as ast.Array
	// No arguments means we are sorting an array of builtins (e.g. `numbers.sort()`)
	// The type for the comparison fns is the type of the element itself.
	mut typ := info.elem_type
	mut is_default := false
	mut is_reverse := false
	mut compare_fn := ''
	if node.args.len == 0 {
		is_default = true
	} else {
		infix_expr := node.args[0].expr as ast.InfixExpr
		left_name := '$infix_expr.left'
		is_default = left_name in ['a', 'b'] && '$infix_expr.right' in ['a', 'b']
		is_reverse = (left_name.starts_with('a') && infix_expr.op == .gt)
			|| (left_name.starts_with('b') && infix_expr.op == .lt)
	}
	if is_default {
		// users.sort() or users.sort(a > b)
		compare_fn = match typ {
			ast.int_type, ast.int_type.to_ptr() { 'compare_ints' }
			ast.string_type, ast.string_type.to_ptr() { 'compare_strings' }
			else { '' }
		}
		if compare_fn != '' && is_reverse {
			compare_fn += '_reverse'
		}
	}
	if compare_fn == '' {
		// `users.sort(a.age > b.age)`
		// Generate a comparison function for a custom type
		tmp_name := g.new_tmp_var()
		styp := g.typ(typ).trim('*')
		compare_fn = 'compare_${tmp_name}_$styp'
		if is_reverse {
			compare_fn += '_reverse'
		}
		// Register a new custom `compare_xxx` function for qsort()
		g.table.register_fn(name: compare_fn, return_type: ast.int_type)

		if node.args.len == 0 {
			styp_arg := g.typ(typ)
			g.definitions.writeln('int $compare_fn ($styp_arg* a, $styp_arg* b) {')
			sym := g.table.get_type_symbol(typ)
			if !is_reverse && sym.has_method('<') {
				g.definitions.writeln('\tif (${styp}__lt(*a, *b)) { return -1; } else { return 1; }}')
			} else if is_reverse && sym.has_method('<') {
				g.definitions.writeln('\tif (${styp}__lt(*b, *a)) { return -1; } else { return 1; }}')
			} else {
				g.definitions.writeln('if (*a < *b) return -1;')
				g.definitions.writeln('if (*a > *b) return 1; else return 0; }\n')
			}
		} else {
			infix_expr := node.args[0].expr as ast.InfixExpr
			// Variables `a` and `b` are used in the `.sort(a < b)` syntax, so we can reuse them
			// when generating the function as long as the args are named the same.
			styp_arg := g.typ(typ)
			g.definitions.writeln('int $compare_fn ($styp_arg* a, $styp_arg* b) {')
			sym := g.table.get_type_symbol(typ)
			if !is_reverse && sym.has_method('<') && infix_expr.left.str().len == 1 {
				g.definitions.writeln('\tif (${styp}__lt(*a, *b)) { return -1; } else { return 1; }}')
			} else if is_reverse && sym.has_method('<') && infix_expr.left.str().len == 1 {
				g.definitions.writeln('\tif (${styp}__lt(*b, *a)) { return -1; } else { return 1; }}')
			} else {
				field_type := g.typ(infix_expr.left_type)
				mut left_expr_str := g.write_expr_to_string(infix_expr.left)
				mut right_expr_str := g.write_expr_to_string(infix_expr.right)
				if typ.is_ptr() {
					left_expr_str = left_expr_str.replace_once('a', '(*a)')
					right_expr_str = right_expr_str.replace_once('b', '(*b)')
				}
				g.definitions.writeln('$field_type a_ = $left_expr_str;')
				g.definitions.writeln('$field_type b_ = $right_expr_str;')
				mut op1, mut op2 := '', ''
				if infix_expr.left_type == ast.string_type {
					if is_reverse {
						op1 = 'string_gt(a_, b_)'
						op2 = 'string_lt(a_, b_)'
					} else {
						op1 = 'string_lt(a_, b_)'
						op2 = 'string_gt(a_, b_)'
					}
				} else {
					deref_str := if infix_expr.left_type.is_ptr() { '*' } else { '' }
					if is_reverse {
						op1 = '${deref_str}a_ > ${deref_str}b_'
						op2 = '${deref_str}a_ < ${deref_str}b_'
					} else {
						op1 = '${deref_str}a_ < ${deref_str}b_'
						op2 = '${deref_str}a_ > ${deref_str}b_'
					}
				}
				g.definitions.writeln('if ($op1) return -1;')
				g.definitions.writeln('if ($op2) return 1; else return 0; }\n')
			}
		}
	}
	if is_reverse && !compare_fn.ends_with('_reverse') {
		compare_fn += '_reverse'
	}
	//
	deref := if node.left_type.is_ptr() || node.left_type.is_pointer() { '->' } else { '.' }
	// eprintln('> qsort: pointer $node.left_type | deref: `$deref`')
	g.empty_line = true
	g.write('qsort(')
	g.expr(node.left)
	g.write('${deref}data, ')
	g.expr(node.left)
	g.write('${deref}len, ')
	g.expr(node.left)
	g.write('${deref}element_size, (int (*)(const void *, const void *))&$compare_fn)')
}

// `nums.filter(it % 2 == 0)`
fn (mut g Gen) gen_array_filter(node ast.CallExpr) {
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	// println('filter s="$s"')
	sym := g.table.get_type_symbol(node.return_type)
	if sym.kind != .array {
		verror('filter() requires an array')
	}
	info := sym.info as ast.Array
	styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	g.empty_line = true
	g.write('${g.typ(node.left_type)} ${tmp}_orig = ')
	g.expr(node.left)
	g.writeln(';')
	g.writeln('int ${tmp}_len = ${tmp}_orig.len;')
	noscan := g.check_noscan(info.elem_type)
	g.writeln('$styp $tmp = __new_array${noscan}(0, ${tmp}_len, sizeof($elem_type_str));\n')
	i := g.new_tmp_var()
	g.writeln('for (int $i = 0; $i < ${tmp}_len; ++$i) {')
	g.writeln('\t$elem_type_str it = (($elem_type_str*) ${tmp}_orig.data)[$i];')
	mut is_embed_map_filter := false
	mut expr := node.args[0].expr
	match mut expr {
		ast.AnonFn {
			g.write('\tif (')
			g.gen_anon_fn_decl(mut expr)
			g.write('${expr.decl.name}(it)')
		}
		ast.Ident {
			g.write('\tif (')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(it)')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.get_type_symbol(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(it)')
				} else {
					g.expr(node.args[0].expr)
				}
			} else {
				g.expr(node.args[0].expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter'] {
				is_embed_map_filter = true
				g.stmt_path_pos << g.out.len
			}
			g.write('\tif (')
			g.expr(node.args[0].expr)
		}
		else {
			g.write('\tif (')
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(') {')
	g.writeln('\t\tarray_push((array*)&$tmp, &it); \n\t\t}')
	g.writeln('}')
	if !is_embed_map_filter {
		g.stmt_path_pos << g.out.len
	}
	g.write('\n')
	g.write(s)
	g.write(tmp)
}

// `nums.insert(0, 2)` `nums.insert(0, [2,3,4])`
fn (mut g Gen) gen_array_insert(node ast.CallExpr) {
	left_sym := g.table.get_type_symbol(node.left_type)
	left_info := left_sym.info as ast.Array
	elem_type_str := g.typ(left_info.elem_type)
	arg2_sym := g.table.get_type_symbol(node.args[1].typ)
	is_arg2_array := arg2_sym.kind == .array && node.args[1].typ == node.left_type
	if is_arg2_array {
		g.write('array_insert_many(&')
	} else {
		g.write('array_insert(&')
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
		g.write(', &($elem_type_str[]){')
		if left_info.elem_type == ast.string_type {
			g.write('string_clone(')
		}
		g.expr(node.args[1].expr)
		if left_info.elem_type == ast.string_type {
			g.write(')')
		}
		g.write('})')
	}
}

// `nums.prepend(2)` `nums.prepend([2,3,4])`
fn (mut g Gen) gen_array_prepend(node ast.CallExpr) {
	left_sym := g.table.get_type_symbol(node.left_type)
	left_info := left_sym.info as ast.Array
	elem_type_str := g.typ(left_info.elem_type)
	arg_sym := g.table.get_type_symbol(node.args[0].typ)
	is_arg_array := arg_sym.kind == .array && node.args[0].typ == node.left_type
	if is_arg_array {
		g.write('array_prepend_many(&')
	} else {
		g.write('array_prepend(&')
	}
	g.expr(node.left)
	if is_arg_array {
		g.write(', ')
		g.expr(node.args[0].expr)
		g.write('.data, ')
		g.expr(node.args[0].expr)
		g.write('.len)')
	} else {
		g.write(', &($elem_type_str[]){')
		g.expr(node.args[0].expr)
		g.write('})')
	}
}

fn (mut g Gen) gen_array_contains_method(left_type ast.Type) string {
	mut left_sym := g.table.get_type_symbol(left_type)
	mut left_type_str := g.typ(left_type).replace('*', '')
	fn_name := '${left_type_str}_contains'
	if !left_sym.has_method('contains') {
		left_info := left_sym.info as ast.Array
		mut elem_type_str := g.typ(left_info.elem_type)
		elem_sym := g.table.get_type_symbol(left_info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
			elem_type_str = 'voidptr'
		}
		g.type_definitions.writeln('static bool ${fn_name}($left_type_str a, $elem_type_str v); // auto')
		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('static bool ${fn_name}($left_type_str a, $elem_type_str v) {')
		fn_builder.writeln('\tfor (int i = 0; i < a.len; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (string_eq((*(string*)array_get(a, i)), v)) {')
		} else if elem_sym.kind == .array && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_array_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else if elem_sym.kind == .function {
			fn_builder.writeln('\t\tif ((*(voidptr*)array_get(a, i)) == v) {')
		} else if elem_sym.kind == .map && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_map_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else if elem_sym.kind == .struct_ && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_struct_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else {
			fn_builder.writeln('\t\tif ((*($elem_type_str*)array_get(a, i)) == v) {')
		}
		fn_builder.writeln('\t\t\treturn true;')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn false;')
		fn_builder.writeln('}')
		g.auto_fn_definitions << fn_builder.str()
		left_sym.register_method(&ast.Fn{
			name: 'contains'
			params: [ast.Param{
				typ: left_type
			}, ast.Param{
				typ: left_info.elem_type
			}]
		})
	}
	return fn_name
}

// `nums.contains(2)`
fn (mut g Gen) gen_array_contains(node ast.CallExpr) {
	fn_name := g.gen_array_contains_method(node.left_type)
	g.write('${fn_name}(')
	if node.left_type.is_ptr() {
		g.write('*')
	}
	g.expr(node.left)
	g.write(', ')
	g.expr(node.args[0].expr)
	g.write(')')
}

fn (mut g Gen) gen_array_index_method(left_type ast.Type) string {
	mut left_sym := g.table.get_type_symbol(left_type)
	mut left_type_str := g.typ(left_type).trim('*')
	fn_name := '${left_type_str}_index'
	if !left_sym.has_method('index') {
		info := left_sym.info as ast.Array
		mut elem_type_str := g.typ(info.elem_type)
		elem_sym := g.table.get_type_symbol(info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
			elem_type_str = 'voidptr'
		}
		g.type_definitions.writeln('static int ${fn_name}($left_type_str a, $elem_type_str v); // auto')
		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('static int ${fn_name}($left_type_str a, $elem_type_str v) {')
		fn_builder.writeln('\tfor (int i = 0; i < a.len; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (string_eq((*(string*)array_get(a, i)), v)) {')
		} else if elem_sym.kind == .array && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_array_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else if elem_sym.kind == .function && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif ((*(voidptr*)array_get(a, i)) == v) {')
		} else if elem_sym.kind == .map && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_map_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else if elem_sym.kind == .struct_ && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_struct_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(*($elem_type_str*)array_get(a, i), v)) {')
		} else {
			fn_builder.writeln('\t\tif ((*($elem_type_str*)array_get(a, i)) == v) {')
		}
		fn_builder.writeln('\t\t\treturn i;')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn -1;')
		fn_builder.writeln('}')
		g.auto_fn_definitions << fn_builder.str()
		left_sym.register_method(&ast.Fn{
			name: 'index'
			params: [ast.Param{
				typ: left_type
			}, ast.Param{
				typ: info.elem_type
			}]
		})
	}
	return fn_name
}

// `nums.index(2)`
fn (mut g Gen) gen_array_index(node ast.CallExpr) {
	fn_name := g.gen_array_index_method(node.left_type)
	g.write('${fn_name}(')
	if node.left_type.is_ptr() {
		g.write('*')
	}
	g.expr(node.left)
	g.write(', ')
	g.expr(node.args[0].expr)
	g.write(')')
}

fn (mut g Gen) gen_array_wait(node ast.CallExpr) {
	arr := g.table.get_type_symbol(node.receiver_type)
	thread_type := arr.array_info().elem_type
	thread_sym := g.table.get_type_symbol(thread_type)
	thread_ret_type := thread_sym.thread_info().return_type
	eltyp := g.table.get_type_symbol(thread_ret_type).cname
	fn_name := g.register_thread_array_wait_call(eltyp)
	g.write('${fn_name}(')
	g.expr(node.left)
	g.write(')')
}

fn (mut g Gen) gen_array_any(node ast.CallExpr) {
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	sym := g.table.get_type_symbol(node.left_type)
	info := sym.info as ast.Array
	// styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	g.empty_line = true
	g.write('${g.typ(node.left_type)} ${tmp}_orig = ')
	g.expr(node.left)
	g.writeln(';')
	g.writeln('int ${tmp}_len = ${tmp}_orig.len;')
	g.writeln('bool $tmp = false;')
	i := g.new_tmp_var()
	g.writeln('for (int $i = 0; $i < ${tmp}_len; ++$i) {')
	g.writeln('\t$elem_type_str it = (($elem_type_str*) ${tmp}_orig.data)[$i];')
	mut is_embed_map_filter := false
	mut expr := node.args[0].expr
	match mut expr {
		ast.AnonFn {
			g.write('\tif (')
			g.gen_anon_fn_decl(mut expr)
			g.write('${expr.decl.name}(it)')
		}
		ast.Ident {
			g.write('\tif (')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(it)')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.get_type_symbol(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(it)')
				} else {
					g.expr(node.args[0].expr)
				}
			} else {
				g.expr(node.args[0].expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter'] {
				is_embed_map_filter = true
				g.stmt_path_pos << g.out.len
			}
			g.write('\tif (')
			g.expr(node.args[0].expr)
		}
		else {
			g.write('\tif (')
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(') {')
	g.writeln('\t\t$tmp = true;\n\t\t\tbreak;\n\t\t}')
	g.writeln('}')
	if !is_embed_map_filter {
		g.stmt_path_pos << g.out.len
	}
	g.write('\n')
	g.write(s)
	g.write(tmp)
}

fn (mut g Gen) gen_array_all(node ast.CallExpr) {
	tmp := g.new_tmp_var()
	s := g.go_before_stmt(0)
	sym := g.table.get_type_symbol(node.left_type)
	info := sym.info as ast.Array
	// styp := g.typ(node.return_type)
	elem_type_str := g.typ(info.elem_type)
	g.empty_line = true
	g.write('${g.typ(node.left_type)} ${tmp}_orig = ')
	g.expr(node.left)
	g.writeln(';')
	g.writeln('int ${tmp}_len = ${tmp}_orig.len;')
	g.writeln('bool $tmp = true;')
	i := g.new_tmp_var()
	g.writeln('for (int $i = 0; $i < ${tmp}_len; ++$i) {')
	g.writeln('\t$elem_type_str it = (($elem_type_str*) ${tmp}_orig.data)[$i];')
	mut is_embed_map_filter := false
	mut expr := node.args[0].expr
	match mut expr {
		ast.AnonFn {
			g.write('\tif (!(')
			g.gen_anon_fn_decl(mut expr)
			g.write('${expr.decl.name}(it)')
		}
		ast.Ident {
			g.write('\tif (!(')
			if expr.kind == .function {
				g.write('${c_name(expr.name)}(it)')
			} else if expr.kind == .variable {
				var_info := expr.var_info()
				sym_t := g.table.get_type_symbol(var_info.typ)
				if sym_t.kind == .function {
					g.write('${c_name(expr.name)}(it)')
				} else {
					g.expr(node.args[0].expr)
				}
			} else {
				g.expr(node.args[0].expr)
			}
		}
		ast.CallExpr {
			if expr.name in ['map', 'filter'] {
				is_embed_map_filter = true
				g.stmt_path_pos << g.out.len
			}
			g.write('\tif (!(')
			g.expr(node.args[0].expr)
		}
		else {
			g.write('\tif (!(')
			g.expr(node.args[0].expr)
		}
	}
	g.writeln(')) {')
	g.writeln('\t\t$tmp = false;\n\t\t\tbreak;\n\t\t}')
	g.writeln('}')
	if !is_embed_map_filter {
		g.stmt_path_pos << g.out.len
	}
	g.write('\n')
	g.write(s)
	g.write(tmp)
}

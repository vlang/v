module js

import v.ast
import strings

const (
	special_array_methods = [
		'sort',
		'insert',
		'prepend',
		'index',
		'contains',
	]
)

fn (mut g JsGen) gen_array_index_method(left_type ast.Type) string {
	unwrap_left_type := g.unwrap_generic(left_type)
	mut left_sym := g.table.sym(unwrap_left_type)
	mut left_type_str := g.typ(unwrap_left_type).trim('*')
	fn_name := '${left_type_str}_index'

	if !left_sym.has_method('index') {
		info := left_sym.info as ast.Array
		elem_sym := g.table.sym(info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
		}

		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('function ${fn_name}(a, v) {')
		fn_builder.writeln('\tlet pelem = a.arr;')
		fn_builder.writeln('\tfor (let i = 0; i < pelem.arr.length; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (pelem.get(new int(i)).str == v.str) {')
		} else if elem_sym.kind == .array && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_array_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(pelem.get(new int(i)), v).val) {')
		} else if elem_sym.kind == .function && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif (pelem.get(new int(i)) == v) {')
		} else if elem_sym.kind == .map && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_map_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(pelem.get(new int(i)), v).val) {')
		} else if elem_sym.kind == .struct_ && !info.elem_type.is_ptr() {
			ptr_typ := g.gen_struct_equality_fn(info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(pelem.get(new int(i)), v)) {')
		} else {
			fn_builder.writeln('\t\tif (vEq(pelem.get(new int(i)), v)) {')
		}
		fn_builder.writeln('\t\t\treturn new int(i);')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn new int(-1);')
		fn_builder.writeln('}')
		g.definitions.writeln(fn_builder.str())
		left_sym.register_method(&ast.Fn{
			name: 'index'
			params: [ast.Param{
				typ: unwrap_left_type
			}, ast.Param{
				typ: info.elem_type
			}]
		})
	}

	return fn_name
}

fn (mut g JsGen) gen_array_method_call(it ast.CallExpr) {
	node := it

	match node.name {
		'index' {
			g.gen_array_index(node)
			return
		}
		'contains' {
			g.gen_array_contains(node)
			return
		}
		'insert' {
			g.write('array_')
			arg2_sym := g.table.sym(node.args[1].typ)
			is_arg2_array := arg2_sym.kind == .array && node.args[1].typ == node.left_type
			if is_arg2_array {
				g.write('insert_many(')
			} else {
				g.write('insert(')
			}
			g.expr(it.left)
			mut ltyp := it.left_type
			for ltyp.is_ptr() {
				g.write('.val')
				ltyp = ltyp.deref()
			}
			g.write(',')
			g.expr(node.args[0].expr)
			g.write(',')
			if is_arg2_array {
				g.expr(node.args[1].expr)
				g.write('.arr,')
				g.expr(node.args[1].expr)
				g.write('.len')
			} else {
				g.expr(node.args[1].expr)
			}
			g.write(')')
			return
		}
		'prepend' {
			g.write('array_')
			arg_sym := g.table.sym(node.args[0].typ)
			is_arg_array := arg_sym.kind == .array && node.args[0].typ == node.left_type
			if is_arg_array {
				g.write('prepend_many(')
			} else {
				g.write('prepend(')
			}
			g.expr(it.left)
			mut ltyp := it.left_type
			for ltyp.is_ptr() {
				g.write('.val')
				ltyp = ltyp.deref()
			}
			g.write(',')
			if is_arg_array {
				g.expr(node.args[0].expr)
				g.write('.arr, ')
				g.expr(node.args[0].expr)
				g.write('.len')
			} else {
				g.expr(node.args[0].expr)
			}
			g.write(')')
			return
		}
		'sort' {
			g.gen_array_sort(node)
		}
		else {}
	}
}

fn (mut g JsGen) gen_array_index(node ast.CallExpr) {
	fn_name := g.gen_array_index_method(node.left_type)
	g.write('${fn_name}(')
	g.expr(node.left)
	g.gen_deref_ptr(node.left_type)
	g.write(',')
	g.expr(node.args[0].expr)
	g.write(')')
}

fn (mut g JsGen) gen_array_contains(node ast.CallExpr) {
	fn_name := g.gen_array_contains_method(node.left_type)
	g.write('${fn_name}(')
	g.expr(node.left)
	g.gen_deref_ptr(node.left_type)
	g.write(',')
	g.expr(node.args[0].expr)
	g.write(')')
}

fn (mut g JsGen) gen_array_contains_method(left_type ast.Type) string {
	mut unwrap_left_type := g.unwrap_generic(left_type)
	if unwrap_left_type.share() == .shared_t {
		unwrap_left_type = unwrap_left_type.clear_flag(.shared_f)
	}
	mut left_sym := g.table.sym(unwrap_left_type)
	left_final_sym := g.table.final_sym(unwrap_left_type)
	mut left_type_str := g.typ(unwrap_left_type).replace('*', '')
	fn_name := '${left_type_str}_contains'
	if !left_sym.has_method('contains') {
		left_info := left_final_sym.info as ast.Array
		elem_sym := g.table.sym(left_info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
		}

		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('function ${fn_name}(a,v) {')
		fn_builder.writeln('\tfor (let i = 0; i < a.len; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (a.arr.get(new int(i)).str ==  v.str) {')
		} else if elem_sym.kind == .array && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_array_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_arr_eq(a.arr.get(new int(i)),v).val) {')
		} else if elem_sym.kind == .function {
			fn_builder.writeln('\t\tif (a.arr.get(new int(i)) == v) {')
		} else if elem_sym.kind == .map && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_map_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_map_eq(a.arr.get(new int(i)),v).val) {')
		} else if elem_sym.kind == .struct_ && left_info.elem_type.nr_muls() == 0 {
			ptr_typ := g.gen_struct_equality_fn(left_info.elem_type)
			fn_builder.writeln('\t\tif (${ptr_typ}_struct_eq(a.arr.get(new int(i)),v).val) {')
		} else {
			fn_builder.writeln('\t\tif (vEq(a.arr.get(new int(i)),v)) {')
		}
		fn_builder.writeln('\t\t\treturn new bool(true);')
		fn_builder.writeln('\t\t}')
		fn_builder.writeln('\t}')
		fn_builder.writeln('\treturn new bool(false);')
		fn_builder.writeln('}')
		g.definitions.writeln(fn_builder.str())
		left_sym.register_method(&ast.Fn{
			name: 'contains'
			params: [ast.Param{
				typ: unwrap_left_type
			}, ast.Param{
				typ: left_info.elem_type
			}]
		})
	}
	return fn_name
}

fn (mut g JsGen) gen_array_sort(node ast.CallExpr) {
	rec_sym := g.table.sym(node.receiver_type)
	if rec_sym.kind != .array {
		println(node.name)
		verror('.sort() is an array method')
	}

	info := rec_sym.info as ast.Array

	elem_stype := g.typ(info.elem_type)
	mut compare_fn := 'compare_${elem_stype.replace('*', '_ptr')}'
	mut comparison_type := g.unwrap(ast.void_type)
	mut left_expr, mut right_expr := '', ''

	if node.args.len == 0 {
		comparison_type = g.unwrap(info.elem_type.set_nr_muls(0))
		if compare_fn in g.array_sort_fn {
			g.gen_array_sort_call(node, compare_fn)
			return
		}

		left_expr = 'a'
		right_expr = 'b'
	} else {
		infix_expr := node.args[0].expr as ast.InfixExpr
		comparison_type = g.unwrap(infix_expr.left_type.set_nr_muls(0))
		left_name := infix_expr.left.str()
		if left_name.len > 1 {
			compare_fn += '_by' + left_name[1..].replace_each(['.', '_', '[', '_', ']', '_'])
		}
		// is_reverse is `true` for `.sort(a > b)` and `.sort(b < a)`
		is_reverse := (left_name.starts_with('a') && infix_expr.op == .gt)
			|| (left_name.starts_with('b') && infix_expr.op == .lt)
		if is_reverse {
			compare_fn += '_reverse'
		}
		if compare_fn in g.array_sort_fn {
			g.gen_array_sort_call(node, compare_fn)
			return
		}
		if left_name.starts_with('a') != is_reverse {
			left_expr = g.expr_string(infix_expr.left)
			right_expr = g.expr_string(infix_expr.right)
		} else {
			left_expr = g.expr_string(infix_expr.right)
			right_expr = g.expr_string(infix_expr.left)
		}
	}

	// Register a new custom `compare_xxx` function for qsort()
	// TODO: move to checker
	g.table.register_fn(name: compare_fn, return_type: ast.int_type)
	g.array_sort_fn[compare_fn] = true

	g.definitions.writeln('function ${compare_fn}(a,b) {')
	c_condition := if comparison_type.sym.has_method('<') {
		'${g.typ(comparison_type.typ)}__lt(${left_expr}, ${right_expr})'
	} else if comparison_type.unaliased_sym.has_method('<') {
		'${g.typ(comparison_type.unaliased)}__lt(${left_expr}, ${right_expr})'
	} else {
		'${left_expr}.valueOf() < ${right_expr}.valueOf()'
	}
	g.definitions.writeln('\tif (${c_condition}) return -1;')
	g.definitions.writeln('\telse return 1;')
	g.definitions.writeln('}\n')

	// write call to the generated function
	g.gen_array_sort_call(node, compare_fn)
}

fn (mut g JsGen) gen_array_sort_call(node ast.CallExpr, compare_fn string) {
	g.write('v_sort(')
	g.expr(node.left)
	g.gen_deref_ptr(node.left_type)
	g.write(',${compare_fn})')
}

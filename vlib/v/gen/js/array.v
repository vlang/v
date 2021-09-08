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
	mut left_sym := g.table.get_type_symbol(unwrap_left_type)
	mut left_type_str := g.typ(unwrap_left_type).trim('*')
	fn_name := '${left_type_str}_index'

	if !left_sym.has_method('index') {
		info := left_sym.info as ast.Array
		elem_sym := g.table.get_type_symbol(info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
		}

		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('function ${fn_name}(a, v) {')
		fn_builder.writeln('\tlet pelem = a.arr;')
		fn_builder.writeln('\tfor (let i = 0; i < pelem.length; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (pelem[i].str == v.str) {')
		} else if elem_sym.kind == .array && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif (vEq(pelem[i], v)) {')
		} else if elem_sym.kind == .function && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif ( vEq(pelem[i], v)) {')
		} else if elem_sym.kind == .map && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif (vEq(pelem[i], v)) {')
		} else if elem_sym.kind == .struct_ && !info.elem_type.is_ptr() {
			fn_builder.writeln('\t\tif (vEq(pelem[i], v)) {')
		} else {
			fn_builder.writeln('\t\tif (pelem[i].valueOf() == v.valueOf()) {')
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
			arg2_sym := g.table.get_type_symbol(node.args[1].typ)
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
			arg_sym := g.table.get_type_symbol(node.args[0].typ)
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
			g.write('array')
			rec_sym := g.table.get_type_symbol(node.receiver_type)
			if rec_sym.kind != .array {
				println(node.name)
				println(g.typ(node.receiver_type))
				// println(rec_sym.kind)
				verror('.sort() is an array method')
			}

			// `users.sort(a.age > b.age)`

			if node.args.len == 0 {
				g.write('_sort(')
				g.expr(it.left)
				mut ltyp := it.left_type
				for ltyp.is_ptr() {
					g.write('.val')
					ltyp = ltyp.deref()
				}

				g.write(')')
				return
			} else {
				g.expr(it.left)
				mut ltyp := it.left_type
				for ltyp.is_ptr() {
					g.write('.val')
					ltyp = ltyp.deref()
				}
				g.write('.')
				infix_expr := node.args[0].expr as ast.InfixExpr
				left_name := infix_expr.left.str()
				is_reverse := (left_name.starts_with('a') && infix_expr.op == .gt)
					|| (left_name.starts_with('b') && infix_expr.op == .lt)
				if is_reverse {
					g.write('arr.sort(function (b,a) {')
				} else {
					g.write('arr.sort(function (a,b) {')
				}
				g.write('return ')
				g.write('\$sortComparator(a,b)')
				g.write('})')
			}
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
	mut left_sym := g.table.get_type_symbol(unwrap_left_type)
	left_final_sym := g.table.get_final_type_symbol(unwrap_left_type)
	mut left_type_str := g.typ(unwrap_left_type).replace('*', '')
	fn_name := '${left_type_str}_contains'
	if !left_sym.has_method('contains') {
		left_info := left_final_sym.info as ast.Array
		elem_sym := g.table.get_type_symbol(left_info.elem_type)
		if elem_sym.kind == .function {
			left_type_str = 'Array_voidptr'
		}

		mut fn_builder := strings.new_builder(512)
		fn_builder.writeln('function ${fn_name}(a,v) {')
		fn_builder.writeln('\tfor (let i = 0; i < a.len; ++i) {')
		if elem_sym.kind == .string {
			fn_builder.writeln('\t\tif (a.arr[i].str ==  v.str) {')
		} else if elem_sym.kind == .array && left_info.elem_type.nr_muls() == 0 {
			fn_builder.writeln('\t\tif (vEq(a.arr[i], v)) {')
		} else if elem_sym.kind == .function {
			fn_builder.writeln('\t\tif (a.arr[i] == v) {')
		} else if elem_sym.kind == .map && left_info.elem_type.nr_muls() == 0 {
			fn_builder.writeln('\t\tif (vEq(a.arr[i], v)) {')
		} else if elem_sym.kind == .struct_ && left_info.elem_type.nr_muls() == 0 {
			fn_builder.writeln('\t\tif (vEq(a.arr[i],v)) {')
		} else {
			fn_builder.writeln('\t\tif (a.arr[i].valueOf() == v.valueOf()) {')
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

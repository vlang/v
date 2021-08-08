module js

import v.ast

const (
	special_array_methods = [
		'sort',
		'insert',
		'prepend',
	]
)

fn (mut g JsGen) gen_array_method_call(it ast.CallExpr) {
	node := it
	match node.name {
		'insert' {
			arg2_sym := g.table.get_type_symbol(node.args[1].typ)
			is_arg2_array := arg2_sym.kind == .array && node.args[1].typ == node.left_type
			if is_arg2_array {
				g.write('insert_many(')
			} else {
				g.write('insert(')
			}

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
			arg_sym := g.table.get_type_symbol(node.args[0].typ)
			is_arg_array := arg_sym.kind == .array && node.args[0].typ == node.left_type
			if is_arg_array {
				g.write('prepend_many(')
			} else {
				g.write('prepend(')
			}

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
			rec_sym := g.table.get_type_symbol(node.receiver_type)
			if rec_sym.kind != .array {
				println(node.name)
				println(g.typ(node.receiver_type))
				// println(rec_sym.kind)
				verror('.sort() is an array method')
			}

			// `users.sort(a.age > b.age)`

			if node.args.len == 0 {
				g.write('sort()')
				return
			} else {
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

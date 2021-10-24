module js

import v.ast
import v.util
import v.parser
import strings

fn (mut g JsGen) js_mname(name_ string) string {
	mut is_js := false
	is_overload := ['+', '-', '*', '/', '==', '<', '>']
	mut name := name_
	if name.starts_with('JS.') {
		name = name[3..]
		is_js = true
	}
	ns := get_ns(name)
	name = if name in is_overload {
		match name {
			'+' {
				'\$add'
			}
			'-' {
				'\$sub'
			}
			'/' {
				'\$div'
			}
			'*' {
				'\$mul'
			}
			'%' {
				'\$mod'
			}
			'==' {
				'eq'
			}
			'>' {
				'\$gt'
			}
			'<' {
				'\$lt'
			}
			else {
				''
			}
		}
	} else if g.ns == 0 {
		name
	} else if ns == g.ns.name {
		name.split('.').last()
	} else {
		g.get_alias(name)
	}
	mut parts := name.split('.')
	if !is_js {
		for i, p in parts {
			if p in js_reserved {
				parts[i] = 'v_$p'
			}
		}
	}
	return parts.join('.')
}

fn (mut g JsGen) js_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	g.write('${g.js_mname(it.name)}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end call
	g.write(')')
	g.call_stack.delete_last()
}

fn (mut g JsGen) js_method_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	g.expr(it.left)
	g.write('.${g.js_mname(it.name)}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end method call
	g.write(')')
	g.call_stack.delete_last()
}

fn (mut g JsGen) method_call(node ast.CallExpr) {
	g.call_stack << node
	it := node
	if it.name == 'str' {
		g.gen_expr_to_string(node.left, node.left_type)
		return
	}
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return unwrap(')
	}
	if node.name == 'str' {
		mut rec_type := node.receiver_type
		if rec_type.has_flag(.shared_f) {
			rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
		}
		g.get_str_fn(rec_type)
	}
	mut unwrapped_rec_type := node.receiver_type
	if g.table.cur_fn.generic_names.len > 0 {
		unwrapped_rec_type = g.unwrap_generic(node.receiver_type)
	} else {
		sym := g.table.get_type_symbol(node.receiver_type)
		match sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				generic_names := sym.info.generic_types.map(g.table.get_type_symbol(it).name)
				if utyp := g.table.resolve_generic_to_concrete(node.receiver_type, generic_names,
					sym.info.concrete_types)
				{
					unwrapped_rec_type = utyp
				}
			}
			else {}
		}
	}

	mut typ_sym := g.table.get_type_symbol(unwrapped_rec_type)
	rec_cc_type := g.cc_type(unwrapped_rec_type, false)
	mut receiver_type_name := util.no_dots(rec_cc_type)
	// alias type that undefined this method (not include `str`) need to use parent type
	if typ_sym.kind == .alias && node.name != 'str' && !typ_sym.has_method(node.name) {
		unwrapped_rec_type = (typ_sym.info as ast.Alias).parent_type
		typ_sym = g.table.get_type_symbol(unwrapped_rec_type)
	}

	if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method(node.name) {
		g.expr(it.left)
		g.gen_deref_ptr(it.left_type)
		g.write('.${it.name}(')
		for i, arg in it.args {
			g.expr(arg.expr)
			if i != it.args.len - 1 {
				g.write(', ')
			}
		}
		g.write(')')
		return
	}

	left_sym := g.table.get_type_symbol(node.left_type)
	final_left_sym := g.table.get_final_type_symbol(node.left_type)

	if final_left_sym.kind == .array {
		if final_left_sym.kind == .array && it.name in ['map', 'filter'] {
			g.expr(it.left)
			mut ltyp := it.left_type
			for ltyp.is_ptr() {
				g.write('.val')
				ltyp = ltyp.deref()
			}
			g.write('.')
			// Prevent 'it' from getting shadowed inside the match

			g.write(it.name)
			g.write('(')
			expr := node.args[0].expr
			match expr {
				ast.AnonFn {
					g.gen_fn_decl(expr.decl)
					g.write(')')
					return
				}
				ast.Ident {
					if expr.kind == .function {
						g.write(g.js_name(expr.name))
						g.write(')')
						return
					} else if expr.kind == .variable {
						v_sym := g.table.get_type_symbol(expr.var_info().typ)
						if v_sym.kind == .function {
							g.write(g.js_name(expr.name))
							g.write(')')
							return
						}
					}
				}
				else {}
			}

			g.write('it => ')
			g.expr(node.args[0].expr)
			g.write(')')
			return
		}

		if final_left_sym.kind == .array {
			if it.name in special_array_methods {
				g.gen_array_method_call(it)
				return
			}
		}
	}
	if final_left_sym.kind == .array
		&& node.name in ['repeat', 'sort_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice', 'pointers'] {
		if !(left_sym.info is ast.Alias && typ_sym.has_method(node.name)) {
			// `array_Xyz_clone` => `array_clone`
			receiver_type_name = 'array'
		}
	}
	mut name := util.no_dots('${receiver_type_name}_$node.name')

	name = g.generic_fn_name(node.concrete_types, name, false)
	g.write('${name}(')
	g.expr(it.left)
	g.gen_deref_ptr(it.left_type)
	g.write(',')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	g.write(')')

	if call_return_is_optional {
		// end unwrap
		g.writeln(')')
		g.dec_indent()
		// begin catch block
		g.writeln('} catch(err) {')
		g.inc_indent()
		// gen or block contents
		match it.or_block.kind {
			.block {
				if it.or_block.stmts.len > 1 {
					g.stmts(it.or_block.stmts[..it.or_block.stmts.len - 1])
				}
				// g.write('return ')
				g.stmt(it.or_block.stmts.last())
			}
			.propagate {
				panicstr := '`optional not set (\${err.valueOf().msg})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return builtin__panic($panicstr)')
				} else {
					g.writeln('js_throw(err)')
				}
			}
			else {}
		}
		// end catch
		g.dec_indent()
		g.writeln('}')
		// end anon fn
		g.dec_indent()
		g.write('})()')
	}
	g.call_stack.delete_last()
}

fn (mut g JsGen) gen_call_expr(it ast.CallExpr) {
	if it.is_method && g.table.get_type_symbol(it.receiver_type).name.starts_with('JS.') {
		g.js_method_call(it)
		return
	} else if it.name.starts_with('JS.') {
		g.js_call(it)
		return
	}
	if it.is_method {
		g.method_call(it)
		return
	}
	node := it
	g.call_stack << it
	mut name := g.js_name(it.name)

	is_print := name in ['print', 'println', 'eprint', 'eprintln', 'panic']
	if name in parser.builtin_functions {
		name = 'builtin__$name'
	}
	print_method := name
	ret_sym := g.table.get_type_symbol(it.return_type)
	if it.language == .js && ret_sym.name in v_types && ret_sym.name != 'void' {
		g.write('new ')
		g.write(ret_sym.name)
		g.write('(')
	}
	call_return_is_optional := it.return_type.has_flag(.optional)
	if call_return_is_optional {
		g.writeln('(function(){')
		g.inc_indent()
		g.writeln('try {')
		g.inc_indent()
		g.write('return unwrap(')
	}
	if is_print {
		mut typ := node.args[0].typ

		expr := node.args[0].expr
		g.write('$print_method (')
		g.gen_expr_to_string(expr, typ)
		g.write(')')
		return
	}
	g.expr(it.left)

	g.write('${name}(')
	for i, arg in it.args {
		g.expr(arg.expr)
		if i != it.args.len - 1 {
			g.write(', ')
		}
	}
	// end method call
	g.write(')')
	if call_return_is_optional {
		// end unwrap
		g.writeln(')')
		g.dec_indent()
		// begin catch block
		g.writeln('} catch(err) {')
		g.inc_indent()
		// gen or block contents
		match it.or_block.kind {
			.block {
				if it.or_block.stmts.len > 1 {
					g.stmts(it.or_block.stmts[..it.or_block.stmts.len - 1])
				}

				//	g.write('return ')
				g.stmt(it.or_block.stmts.last())
			}
			.propagate {
				panicstr := '`optional not set (\${err.valueOf().msg})`'
				if g.file.mod.name == 'main' && g.fn_decl.name == 'main.main' {
					g.writeln('return builtin__panic($panicstr)')
				} else {
					g.writeln('js_throw(err)')
				}
			}
			else {}
		}
		// end catch
		g.dec_indent()
		g.writeln('}')
		// end anon fn
		g.dec_indent()
		g.write('})()')
	}
	if it.language == .js && ret_sym.name in v_types && ret_sym.name != 'void' {
		g.write(')')
	}
	g.call_stack.delete_last()
}

enum FnGenType {
	function
	struct_method
	alias_method
	iface_method
}

fn (g &JsGen) fn_gen_type(it &ast.FnDecl) FnGenType {
	if it.is_method && g.table.get_type_symbol(it.params[0].typ).kind == .alias {
		return .alias_method
	} else if it.is_method && g.table.get_type_symbol(it.params[0].typ).kind == .interface_ {
		return .iface_method
	} else if it.is_method || it.no_body {
		return .struct_method
	} else {
		return .function
	}
}

fn (mut g JsGen) is_used_by_main(node ast.FnDecl) bool {
	mut is_used_by_main := true
	if g.pref.skip_unused {
		fkey := node.fkey()
		is_used_by_main = g.table.used_fns[fkey]
		$if trace_skip_unused_fns ? {
			println('> is_used_by_main: $is_used_by_main | node.name: $node.name | fkey: $fkey | node.is_method: $node.is_method')
		}
		if !is_used_by_main {
			$if trace_skip_unused_fns_in_js_code ? {
				g.writeln('// trace_skip_unused_fns_in_js_code, $node.name, fkey: $fkey')
			}
		}
	} else {
		$if trace_skip_unused_fns_in_js_code ? {
			g.writeln('// trace_skip_unused_fns_in_js_code, $node.name, fkey: $node.fkey()')
		}
	}
	return is_used_by_main
}

fn (mut g JsGen) gen_fn_decl(it ast.FnDecl) {
	res := g.fn_gen_type(it)
	if it.language == .js {
		for attr in it.attrs {
			match attr.name {
				'wasm_import' {
					mut x := g.wasm_export[attr.arg] or { []string{} }
					x << it.name
					g.wasm_import[attr.arg] = x
				}
				else {}
			}
		}

		return
	}
	if g.inside_builtin {
		g.builtin_fns << it.name
	}
	if !g.is_used_by_main(it) {
		return
	}
	if it.should_be_skipped {
		return
	}
	cur_fn_decl := g.fn_decl
	g.gen_method_decl(it, res)
	g.fn_decl = cur_fn_decl
}

fn fn_has_go(node ast.FnDecl) bool {
	mut has_go := false
	for stmt in node.stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.GoExpr {
				has_go = true
				break
			}
		}
	}
	return has_go
}

fn (mut g JsGen) generic_fn_name(types []ast.Type, before string, is_decl bool) string {
	if types.len == 0 {
		return before
	}

	mut name := before + '_T'
	for typ in types {
		name += '_' + strings.repeat_string('__ptr__', typ.nr_muls()) + g.typ(typ.set_nr_muls(0))
	}
	return name
}

fn (mut g JsGen) gen_method_decl(it ast.FnDecl, typ FnGenType) {
	unsafe {
		g.fn_decl = &it
	}
	cur_fn_save := g.table.cur_fn
	defer {
		g.table.cur_fn = cur_fn_save
	}
	unsafe {
		g.table.cur_fn = &it
	}
	node := it
	mut name := it.name
	if name in ['+', '-', '*', '/', '%', '<', '=='] {
		name = util.replace_op(name)
	}

	if node.is_method {
		unwrapped_rec_sym := g.table.get_type_symbol(g.unwrap_generic(node.receiver.typ))
		if unwrapped_rec_sym.kind == .placeholder {
			return
		}
		name = g.cc_type(node.receiver.typ, false) + '_' + name
	}

	name = g.js_name(name)

	name = g.generic_fn_name(g.table.cur_concrete_types, name, true)
	if name in parser.builtin_functions {
		name = 'builtin__$name'
	}
	has_go := fn_has_go(it)
	if it.is_pub && !it.is_method {
		g.push_pub_var(name)
	}
	is_main := it.name == 'main.main'
	g.gen_attrs(it.attrs)
	if is_main {
		// there is no concept of main in JS but we do have iife
		g.writeln('/* program entry point */')

		// g.write('(')
		if has_go {
			g.write('async ')
		}
		g.write('function js_main(')
	} else if it.is_anon {
		g.write('function (')
	} else {
		c := name[0]
		if c in [`+`, `-`, `*`, `/`] {
			name = util.replace_op(name)
		}
		// type_name := g.typ(it.return_type)
		// generate jsdoc for the function
		g.doc.gen_fn(it)
		if has_go {
			g.write('async ')
		}

		g.write('function ')

		g.write('${name}(')
		if it.is_pub && !it.is_method {
			g.push_pub_var(name)
		}
	}
	mut args := it.params

	g.fn_args(args, it.is_variadic)
	g.write(') {')
	for i, arg in args {
		is_varg := i == args.len - 1 && it.is_variadic
		arg_name := g.js_name(arg.name)
		if is_varg {
			g.writeln('$arg_name = new array(new array_buffer({arr: $arg_name,len: new int(${arg_name}.length),index_start: new int(0)}));')
		} else {
			if arg.typ.is_ptr() || arg.is_mut {
				g.writeln('$arg_name = new \$ref($arg_name)')
			}
		}
	}
	g.stmts(it.stmts)
	g.writeln('}')

	if is_main {
		// g.write(')')
	}
	g.writeln('')
	for attr in it.attrs {
		match attr.name {
			'export' {
				g.writeln('globalThis.$attr.arg = ${g.js_name(it.name)};')
			}
			'wasm_export' {
				mut x := g.wasm_export[attr.arg] or { []string{} }
				g.write('function \$wasm${g.js_name(it.name)}(')
				g.fn_args(args, it.is_variadic)
				g.writeln(') {')
				g.write('\treturn $name (')
				for i, arg in args {
					is_varg := i == args.len - 1 && it.is_variadic
					arg_name := g.js_name(arg.name)
					if is_varg {
						g.write('...$arg_name')
					} else {
						g.gen_cast_tmp(arg_name, arg.typ)
					}
					if i != args.len - 1 {
						g.write(',')
					}
				}
				g.writeln(').valueOf();')
				g.writeln('}')
				x << it.name
				g.wasm_export[attr.arg] = x
			}
			'wasm_import' {
				mut x := g.wasm_export[attr.arg] or { []string{} }
				x << name
				g.wasm_import[attr.arg] = x
			}
			else {}
		}
	}

	g.fn_decl = voidptr(0)
}

fn (mut g JsGen) fn_args(args []ast.Param, is_variadic bool) {
	for i, arg in args {
		name := g.js_name(arg.name)
		is_varg := i == args.len - 1 && is_variadic
		if is_varg {
			g.write('...$name')
		} else {
			g.write(name)
		}
		// if its not the last argument
		if i < args.len - 1 {
			g.write(', ')
		}
	}
}

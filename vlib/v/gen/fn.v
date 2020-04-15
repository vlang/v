// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import (
	v.ast
	v.table
	v.util
)

fn (g mut Gen) gen_fn_decl(it ast.FnDecl) {
	if it.is_c {
		// || it.no_body {
		return
	}
	g.reset_tmp_count()
	is_main := it.name == 'main'
	if is_main {
		if g.pref.os == .windows {
			g.write('int wmain(int ___argc, wchar_t *___argv[], wchar_t *___envp[]')
		} else {
			g.write('int ${it.name}(int ___argc, char** ___argv')
		}
	} else {
		mut name := it.name
		c := name[0]
		if c in [`+`, `-`, `*`, `/`, `%`] {
			name = util.replace_op(name)
		}
		if it.is_method {
			name = g.table.get_type_symbol(it.receiver.typ).name + '_' + name
		}
		if it.is_c {
			name = name.replace('.', '__')
		} else {
			name = c_name(name)
		}
		// if g.pref.show_cc && it.is_builtin {
		// println(name)
		// }
		// type_name := g.table.Type_to_str(it.return_type)
		type_name := g.typ(it.return_type)
		g.write('$type_name ${name}(')
		g.definitions.write('$type_name ${name}(')
	}
	// Receiver is the first argument
	/*
	if it.is_method {
		mut styp := g.typ(it.receiver.typ)
		// if table.type_nr_muls(it.receiver.typ) > 0 {
		// if it.rec_mut {
		// styp += '*'
		// }
		g.write('$styp $it.receiver.name ')
		// TODO mut
		g.definitions.write('$styp $it.receiver.name')
		if it.args.len > 0 {
			g.write(', ')
			g.definitions.write(', ')
		}
	}
*/
	//
	g.fn_args(it.args, it.is_variadic)
	if it.no_body || (g.pref.is_cache && it.is_builtin) {
		// Just a function header.
		// Builtin function bodies are defined in builtin.o
		g.definitions.writeln(');')
		g.writeln(');')
		return
	}
	g.writeln(') {')
	if !is_main {
		g.definitions.writeln(');')
	}
	if is_main {
		g.writeln('\t_vinit();')
		if g.is_importing_os() {
			if g.autofree {
				g.writeln('free(_const_os__args.data); // empty, inited in _vinit()')
			}
			if g.pref.os == .windows {
				g.writeln('\t_const_os__args = os__init_os_args_wide(___argc, ___argv);')
			} else {
				g.writeln('\t_const_os__args = os__init_os_args(___argc, (byteptr*)___argv);')
			}
		}
	}
	g.stmts(it.stmts)
	// ////////////
	if g.autofree {
		g.free_scope_vars(it.pos.pos - 1)
	}
	// /////////
	if is_main {
		if g.autofree {
			g.writeln('\t_vcleanup();')
		}
		if g.is_test {
			verror('test files cannot have function `main`')
		}
	}
	if g.defer_stmts.len > 0 {
		g.write_defer_stmts()
	}
	if is_main {
		g.writeln('\treturn 0;')
	}
	g.writeln('}')
	g.defer_stmts = []
	g.fn_decl = 0
}

fn (g mut Gen) fn_args(args []table.Arg, is_variadic bool) {
	no_names := args.len > 0 && args[0].name == 'arg_1'
	for i, arg in args {
		arg_type_sym := g.table.get_type_symbol(arg.typ)
		mut arg_type_name := g.typ(arg.typ)		// arg_type_sym.name.replace('.', '__')
		is_varg := i == args.len - 1 && is_variadic
		if is_varg {
			varg_type_str := int(arg.typ).str()
			if !(varg_type_str in g.variadic_args) {
				g.variadic_args[varg_type_str] = 0
			}
			arg_type_name = 'varg_' + g.typ(arg.typ).replace('*', '_ptr')
		}
		if arg_type_sym.kind == .function {
			info := arg_type_sym.info as table.FnType
			func := info.func
			if !info.is_anon {
				g.write(arg_type_name + ' ' + arg.name)
				g.definitions.write(arg_type_name + ' ' + arg.name)
			} else {
				g.write('${g.typ(func.return_type)} (*$arg.name)(')
				g.definitions.write('${g.typ(func.return_type)} (*$arg.name)(')
				g.fn_args(func.args, func.is_variadic)
				g.write(')')
				g.definitions.write(')')
			}
		} else if no_names {
			g.write(arg_type_name)
			g.definitions.write(arg_type_name)
		} else {
			mut nr_muls := table.type_nr_muls(arg.typ)
			s := arg_type_name + ' ' + arg.name
			if arg.is_mut {
				// mut arg needs one *
				nr_muls = 1
			}
			// if nr_muls > 0 && !is_varg {
			// s = arg_type_name + strings.repeat(`*`, nr_muls) + ' ' + arg.name
			// }
			g.write(s)
			g.definitions.write(s)
		}
		if i < args.len - 1 {
			g.write(', ')
			g.definitions.write(', ')
		}
	}
}

fn (g mut Gen) call_expr(node ast.CallExpr) {
	gen_or := !g.is_assign_rhs && node.or_block.stmts.len > 0
	tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
	if gen_or {
		styp := g.typ(node.return_type)
		g.write('$styp $tmp_opt = ')
	}
	if node.is_method {
		g.method_call(node)
	} else {
		g.fn_call(node)
	}
	if gen_or {
		g.or_block(tmp_opt, node.or_block.stmts, node.return_type)
	}
}

fn (g mut Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		verror('method receiver type is 0, this means there are some uchecked exprs')
	}
	typ_sym := g.table.get_type_symbol(node.receiver_type)
	// rec_sym := g.table.get_type_symbol(node.receiver_type)
	mut receiver_name := typ_sym.name
	if typ_sym.kind == .array && node.name == 'filter' {
		g.gen_filter(node)
		return
	}
	// TODO performance, detect `array` method differently
	if typ_sym.kind == .array && node.name in ['repeat', 'sort_with_compare', 'free', 'push_many',
		'trim', 'first', 'last', 'clone', 'reverse', 'slice'] {
		// && rec_sym.name == 'array' {
		// && rec_sym.name == 'array' && receiver_name.starts_with('array') {
		// `array_byte_clone` => `array_clone`
		receiver_name = 'array'
		if node.name in ['last', 'first'] {
			return_type_str := g.typ(node.return_type)
			g.write('*($return_type_str*)')
		}
	}
	name := '${receiver_name}_$node.name'.replace('.', '__')
	// if node.receiver_type != 0 {
	// g.write('/*${g.typ(node.receiver_type)}*/')
	// g.write('/*expr_type=${g.typ(node.left_type)} rec type=${g.typ(node.receiver_type)}*/')
	// }
	g.write('${name}(')
	if table.type_is_ptr(node.receiver_type) && !table.type_is_ptr(node.left_type) {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO same logic in call_args()
		g.write('&')
	} else if !table.type_is_ptr(node.receiver_type) && table.type_is_ptr(node.left_type) {
		g.write('/*rec*/*')
	}
	g.expr(node.left)
	is_variadic := node.expected_arg_types.len > 0 && table.type_is(node.expected_arg_types[node.expected_arg_types.len -
		1], .variadic)
	if node.args.len > 0 || is_variadic {
		g.write(', ')
	}
	// /////////
	/*
	if name.contains('subkeys') {
	println('call_args $name $node.arg_types.len')
	for t in node.arg_types {
		sym := g.table.get_type_symbol(t)
		print('$sym.name ')
	}
	println('')
}
*/
	// ///////
	g.call_args(node.args, node.expected_arg_types)
	g.write(')')
	// if node.or_block.stmts.len > 0 {
	// g.or_block(node.or_block.stmts, node.return_type)
	// }
}

fn (g mut Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	if node.left_type != 0 {
		g.expr(node.left)
		if table.type_is_ptr(node.left_type) {
			g.write('->')
		}
		else {
			g.write('.')
		}
	}
	mut name := node.name
	is_print := name == 'println' || name == 'print'
	print_method := if name == 'println' { 'println' } else { 'print' }
	g.is_json_fn = name == 'json.encode'
	mut json_type_str := ''
	if g.is_json_fn {
		g.write('json__json_print(')
		g.gen_json_for_type(node.args[0].typ)
		json_type_str = g.table.get_type_symbol(node.args[0].typ).name
	}
	if node.is_c {
		// Skip "C."
		g.is_c_call = true
		name = name[2..].replace('.', '__')
	} else {
		name = c_name(name)
	}
	if g.is_json_fn {
		// `json__decode` => `json__decode_User`
		name += '_' + json_type_str
	}
	// Generate tmp vars for values that have to be freed.
	/*
	mut tmps := []string
	for arg in node.args {
		if arg.typ == table.string_type_idx || is_print {
			tmp := g.new_tmp_var()
			tmps << tmp
			g.write('string $tmp = ')
			g.expr(arg.expr)
			g.writeln('; //memory')
		}
	}
*/
	if is_print && node.args[0].typ != table.string_type {
		typ := node.args[0].typ
		mut styp := g.typ(typ)
		sym := g.table.get_type_symbol(typ)
		if table.type_is_ptr(typ) {
			styp = styp.replace('*', '')
		}
		g.gen_str_for_type(sym, styp)
		if g.autofree && !table.type_is(typ, .optional) {
			// Create a temporary variable so that the value can be freed
			tmp := g.new_tmp_var()
			// tmps << tmp
			g.write('string $tmp = ${styp}_str(')
			g.expr(node.args[0].expr)
			g.writeln('); ${print_method}($tmp); string_free($tmp); //MEM2 $styp')
		} else {
			expr := node.args[0].expr
			is_var := match expr {
				ast.SelectorExpr {
					true
				}
				ast.Ident {
					true
				}
				else {
					false
				}
			}
			if table.type_is_ptr(typ) && sym.kind != .struct_ {
				// ptr_str() for pointers
				styp = 'ptr'
			}
			if sym.kind == .enum_ {
				if is_var {
					g.write('${print_method}(${styp}_str(')
				} else {
					// when no var, print string directly
					g.write('${print_method}(tos3("')
				}
				if table.type_is_ptr(typ) {
					// dereference
					g.write('*')
				}
				g.enum_expr(expr)
				if !is_var {
					// end of string
					g.write('"')
				}
			} else {
				g.write('${print_method}(${styp}_str(')
				if table.type_is_ptr(typ) && sym.kind == .struct_ {
					// dereference
					g.write('*')
				}
				g.expr(expr)
				if sym.kind == .struct_ && styp != 'ptr' && !sym.has_method('str') {
					g.write(', 0')					// trailing 0 is initial struct indent count
				}
			}
			g.write('))')
		}
	} else {
		g.write('${name}(')
		g.call_args(node.args, node.expected_arg_types)
		g.write(')')
	}
	// if node.or_block.stmts.len > 0 {
	// g.or_block(node.or_block.stmts, node.return_type)
	// }
	g.is_c_call = false
	if g.is_json_fn {
		g.write(')')
		g.is_json_fn = false
	}
}

fn (g mut Gen) call_args(args []ast.CallArg, expected_types []table.Type) {
	is_variadic := expected_types.len > 0 && table.type_is(expected_types[expected_types.len -
		1], .variadic)
	mut arg_no := 0
	for arg in args {
		if is_variadic && arg_no == expected_types.len - 1 {
			break
		}
		// some c fn definitions dont have args (cfns.v) or are not updated in checker
		// when these are fixed we wont need this check
		if arg_no < expected_types.len {
			g.ref_or_deref_arg(arg, expected_types[arg_no])
		} else {
			g.expr(arg.expr)
		}
		if arg_no < args.len - 1 || is_variadic {
			g.write(', ')
		}
		arg_no++
	}
	if is_variadic {
		varg_type := expected_types[expected_types.len - 1]
		struct_name := 'varg_' + g.typ(varg_type).replace('*', '_ptr')
		variadic_count := args.len - arg_no
		varg_type_str := int(varg_type).str()
		if variadic_count > g.variadic_args[varg_type_str] {
			g.variadic_args[varg_type_str] = variadic_count
		}
		g.write('($struct_name){.len=$variadic_count,.args={')
		if variadic_count > 0 {
			for j in arg_no .. args.len {
				g.ref_or_deref_arg(args[j], varg_type)
				if j < args.len - 1 {
					g.write(', ')
				}
			}
		} else {
			g.write('0')
		}
		g.write('}}')
	}
}

[inline]
fn (g mut Gen) ref_or_deref_arg(arg ast.CallArg, expected_type table.Type) {
	arg_is_ptr := table.type_is_ptr(expected_type) || table.type_idx(expected_type) in table.pointer_type_idxs
	expr_is_ptr := table.type_is_ptr(arg.typ) || table.type_idx(arg.typ) in table.pointer_type_idxs
	if arg.is_mut && !arg_is_ptr {
		g.write('&/*mut*/')
	} else if arg_is_ptr && !expr_is_ptr {
		if arg.is_mut {
			sym := g.table.get_type_symbol(expected_type)
			if sym.kind == .array {
				// Special case for mutable arrays. We can't `&` function
				// results,	have to use `(array[]){ expr }[0]` hack.
				g.write('&/*111*/(array[]){')
				g.expr(arg.expr)
				g.write('}[0]')
				return
			}
		}
		if !g.is_json_fn {
			g.write('&/*qq*/')
		}
	} else if !arg_is_ptr && expr_is_ptr {
		// Dereference a pointer if a value is required
		g.write('*/*d*/')
	}
	g.expr_with_cast(arg.expr, arg.typ, expected_type)
}

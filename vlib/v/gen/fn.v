// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.ast
import v.table
import v.util

fn (mut g Gen) gen_fn_decl(it ast.FnDecl) {
	if it.is_c {
		// || it.no_body {
		return
	}
	g.reset_tmp_count()
	is_main := it.name == 'main'
	if is_main {
		if g.pref.os == .windows {
			if g.is_gui_app() {
				// GUI application
				g.writeln('int WINAPI wWinMain(HINSTANCE instance, HINSTANCE prev_instance, LPWSTR cmd_line, int show_cmd')
				g.last_fn_c_name = 'wWinMain'
			} else {
				// Console application
				g.writeln('int wmain(int ___argc, wchar_t* ___argv[], wchar_t* ___envp[]')
				g.last_fn_c_name = 'wmain'
			}
		} else {
			g.write('int ${it.name}(int ___argc, char** ___argv')
			g.last_fn_c_name = it.name
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
		g.last_fn_c_name = name
		g.definitions.write('$type_name ${name}(')
	}
	// Receiver is the first argument
	/*
	if it.is_method {
		mut styp := g.typ(it.receiver.typ)
		// if it.receiver.typ.nr_muls() > 0 {
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
		if g.pref.os == .windows && g.is_gui_app() {
			g.writeln('\ttypedef LPWSTR*(WINAPI *cmd_line_to_argv)(LPCWSTR, int*);')
			g.writeln('\tHMODULE shell32_module = LoadLibrary(L"shell32.dll");')
			g.writeln('\tcmd_line_to_argv CommandLineToArgvW = (cmd_line_to_argv)GetProcAddress(shell32_module, "CommandLineToArgvW");')
			g.writeln('\tint ___argc;')
			g.writeln('\twchar_t** ___argv = CommandLineToArgvW(cmd_line, &___argc);')
		}
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
	// Profiling mode? Start counting at the beginning of the function (save current time).
	if g.pref.is_prof {
		if is_main {
			g.writeln('')
			g.writeln('\tatexit(vprint_profile_stats);')
			g.writeln('')
		}
		if it.name == 'time.vpc_now' {
			g.defer_profile_code = ''
		} else {
			fn_profile_counter_name := 'vpc_${g.last_fn_c_name}'
			g.writeln('')
			g.writeln('\tdouble _PROF_FN_START = time__vpc_now(); ${fn_profile_counter_name}_calls++; // $it.name')
			g.writeln('')
			g.defer_profile_code = '\t${fn_profile_counter_name} += time__vpc_now() - _PROF_FN_START;'
			g.pcs_declarations.writeln('double ${fn_profile_counter_name} = 0.0; u64 ${fn_profile_counter_name}_calls = 0;')
			g.pcs[g.last_fn_c_name] = fn_profile_counter_name
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
	g.write_defer_stmts_when_needed()
	if is_main {
		if g.pref.is_prof {
			g.pcs_declarations.writeln('void vprint_profile_stats(){')
			if g.pref.profile_file == '-' {
				for pfn_name, pcounter_name in g.pcs {
					g.pcs_declarations.writeln('\tif (${pcounter_name}_calls) printf("%llu %f %f ${pfn_name} \\n", ${pcounter_name}_calls, $pcounter_name, $pcounter_name / ${pcounter_name}_calls );')
				}
			} else {
				g.pcs_declarations.writeln('\tFILE * fp;')
				g.pcs_declarations.writeln('\tfp = fopen ("${g.pref.profile_file}", "w+");')
				for pfn_name, pcounter_name in g.pcs {
					g.pcs_declarations.writeln('\tif (${pcounter_name}_calls) fprintf(fp, "%llu %f %f ${pfn_name} \\n", ${pcounter_name}_calls, $pcounter_name, $pcounter_name / ${pcounter_name}_calls );')
				}
				g.pcs_declarations.writeln('\tfclose(fp);')
			}
			g.pcs_declarations.writeln('}')
		}
		g.writeln('\treturn 0;')
	}
	g.writeln('}')
	g.defer_stmts = []
	g.fn_decl = 0
}

fn (mut g Gen) write_defer_stmts_when_needed() {
	if g.defer_profile_code.len > 0 {
		g.writeln('')
		g.writeln('\t// defer_profile_code')
		g.writeln(g.defer_profile_code)
		g.writeln('')
	}
	if g.defer_stmts.len > 0 {
		g.write_defer_stmts()
	}
}

fn (mut g Gen) fn_args(args []table.Arg, is_variadic bool) {
	no_names := args.len > 0 && args[0].name == 'arg_1'
	for i, arg in args {
		arg_type_sym := g.table.get_type_symbol(arg.typ)
		mut arg_type_name := g.typ(arg.typ) // arg_type_sym.name.replace('.', '__')
		is_varg := i == args.len - 1 && is_variadic
		if is_varg {
			varg_type_str := int(arg.typ).str()
			if varg_type_str !in g.variadic_args {
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
			mut nr_muls := arg.typ.nr_muls()
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

fn (mut g Gen) call_expr(node ast.CallExpr) {
	if node.should_be_skipped {
		return
	}
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

fn (mut g Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		verror('method receiver type is 0, this means there are some uchecked exprs')
	}
	typ_sym := g.table.get_type_symbol(node.receiver_type)
	mut receiver_name := typ_sym.name
	if typ_sym.kind == .interface_ {
		// Find the index of the method
		mut idx := -1
		for i, method in typ_sym.methods {
			if method.name == node.name {
				idx = i
			}
		}
		if idx == -1 {
			verror('method_call: cannot find interface method index')
		}
		sret_type := g.typ(node.return_type)
		g.writeln('// interface method call')
		// `((void (*)())(Speaker_name_table[s._interface_idx][1]))(s._object);`
		g.write('(($sret_type (*)())(${receiver_name}_name_table[')
		g.expr(node.left)
		g.write('._interface_idx][$idx]))(')
		g.expr(node.left)
		g.write('._object)')
		return
	}
	// rec_sym := g.table.get_type_symbol(node.receiver_type)
	if typ_sym.kind == .array && node.name == 'filter' {
		g.gen_filter(node)
		return
	}
	// TODO performance, detect `array` method differently
	if typ_sym.kind == .array && node.name in ['repeat', 'sort_with_compare', 'free', 'push_many',
		'trim',
		'first',
		'last',
		'clone',
		'reverse',
		'slice'
	] {
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
	if node.receiver_type.is_ptr() && !node.left_type.is_ptr() {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO same logic in call_args()
		g.write('&')
	} else if !node.receiver_type.is_ptr() && node.left_type.is_ptr() {
		g.write('/*rec*/*')
	}
	g.expr(node.left)
	is_variadic := node.expected_arg_types.len > 0 && node.expected_arg_types[node.expected_arg_types.len -
		1].flag_is(.variadic)
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

fn (mut g Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	if node.left_type != 0 {
		g.expr(node.left)
		if node.left_type.is_ptr() {
			g.write('->')
		} else {
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
	mut tmps := []string{}
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
		if typ.is_ptr() {
			styp = styp.replace('*', '')
		}
		mut str_fn_name := g.gen_str_for_type_with_styp(typ, styp)
		if g.autofree && !typ.flag_is(.optional) {
			// Create a temporary variable so that the value can be freed
			tmp := g.new_tmp_var()
			// tmps << tmp
			g.write('string $tmp = ${str_fn_name}(')
			g.expr(node.args[0].expr)
			g.writeln('); ${print_method}($tmp); string_free($tmp); //MEM2 $styp')
		} else {
			expr := node.args[0].expr
			is_var := match expr {
				ast.SelectorExpr { true }
				ast.Ident { true }
				else { false }
			}
			if typ.is_ptr() && sym.kind != .struct_ {
				// ptr_str() for pointers
				styp = 'ptr'
				str_fn_name = 'ptr_str'
			}
			if sym.kind == .enum_ {
				if is_var {
					g.write('${print_method}(${str_fn_name}(')
				} else {
					// when no var, print string directly
					g.write('${print_method}(tos3("')
				}
				if typ.is_ptr() {
					// dereference
					g.write('*')
				}
				g.enum_expr(expr)
				if !is_var {
					// end of string
					g.write('"')
				}
			} else {
				g.write('${print_method}(${str_fn_name}(')
				if typ.is_ptr() && sym.kind == .struct_ {
					// dereference
					g.write('*')
				}
				g.expr(expr)
				if !typ.flag_is(.variadic) && sym.kind == .struct_ && styp != 'ptr' && !sym.has_method('str') {
					g.write(', 0') // trailing 0 is initial struct indent count
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

fn (mut g Gen) call_args(args []ast.CallArg, expected_types []table.Type) {
	is_variadic := expected_types.len > 0 && expected_types[expected_types.len - 1].flag_is(.variadic)
	is_forwarding_varg := args.len > 0 && args[args.len - 1].typ.flag_is(.variadic)
	gen_vargs := is_variadic && !is_forwarding_varg
	mut arg_no := 0
	for i, arg in args {
		if gen_vargs && arg_no == expected_types.len - 1 {
			break
		}
		// if arg.typ.name.starts_with('I') {
		// }
		mut is_interface := false
		// some c fn definitions dont have args (cfns.v) or are not updated in checker
		// when these are fixed we wont need this check
		if arg_no < expected_types.len {
			if expected_types[arg_no] != 0 {
				// Cast a type to interface
				// `foo(dog)` => `foo(I_Dog_to_Animal(dog))`
				exp_sym := g.table.get_type_symbol(expected_types[arg_no])
				sym := g.table.get_type_symbol(arg.typ)
				if exp_sym.kind == .interface_ {
					g.write('I_${sym.name}_to_${exp_sym.name}(')
					is_interface = true
				}
			}
			g.ref_or_deref_arg(arg, expected_types[arg_no])
		} else {
			g.expr(arg.expr)
		}
		if is_interface {
			g.write(')')
		}
		if arg_no < args.len - 1 || gen_vargs {
			g.write(', ')
		}
		arg_no++
	}
	if gen_vargs {
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
fn (mut g Gen) ref_or_deref_arg(arg ast.CallArg, expected_type table.Type) {
	arg_is_ptr := expected_type.is_ptr() || expected_type.idx() in table.pointer_type_idxs
	expr_is_ptr := arg.typ.is_ptr() || arg.typ.idx() in table.pointer_type_idxs
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

fn (mut g Gen) is_gui_app() bool {
	$if windows {
		for cf in g.table.cflags {
			if cf.value == 'gdi32' {
				return true
			}
		}
	}
	return false
}

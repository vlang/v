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

// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util

enum SpawnGoMode {
	spawn_
	go_
}

fn (mut g Gen) spawn_and_go_expr(node ast.SpawnExpr, mode SpawnGoMode) {
	is_spawn := mode == .spawn_
	is_go := mode == .go_
	if is_spawn {
		g.writeln('/*spawn (thread) */')
	} else {
		g.writeln('/*go (coroutine) */')
	}
	line := g.go_before_stmt(0)
	mut handle := ''
	tmp := g.new_tmp_var()
	mut expr := node.call_expr
	mut name := expr.name
	mut use_tmp_fn_var := false
	tmp_fn := g.new_tmp_var()

	if expr.concrete_types.len > 0 {
		name = g.generic_fn_name(expr.concrete_types, name)
	} else if expr.is_fn_var && expr.fn_var_type.has_flag(.generic) {
		fn_var_type := g.unwrap_generic(expr.fn_var_type)
		name = g.typ(fn_var_type)
	}

	if expr.is_method {
		receiver_sym := g.table.sym(g.unwrap_generic(expr.receiver_type))
		name = receiver_sym.cname + '_' + name
	} else if mut expr.left is ast.AnonFn {
		if expr.left.inherited_vars.len > 0 {
			fn_var := g.fn_var_signature(expr.left.decl.return_type, expr.left.decl.params.map(it.typ),
				tmp_fn)
			g.write('\t${fn_var} = ')
			g.gen_anon_fn(mut expr.left)
			g.writeln(';')
			use_tmp_fn_var = true
		} else {
			g.gen_anon_fn_decl(mut expr.left)
			name = expr.left.decl.name
		}
	} else if expr.left is ast.IndexExpr {
		if expr.is_fn_var {
			fn_sym := g.table.sym(expr.fn_var_type)
			func := (fn_sym.info as ast.FnType).func
			fn_var := g.fn_var_signature(func.return_type, func.params.map(it.typ), tmp_fn)
			g.write('\t${fn_var} = ')
			g.expr(expr.left)
			g.writeln(';')
			name = fn_sym.cname
			use_tmp_fn_var = true
		}
	}
	name = util.no_dots(name)
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
		mut key := expr.name
		if expr.is_method {
			sym := g.table.sym(expr.receiver_type)
			key = sym.name + '.' + expr.name
		}
		g.write('/* obf go: ${key} */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "${key}" not found, this should never happen')
		}
	}
	g.empty_line = true
	g.writeln('// start go')
	wrapper_struct_name := 'thread_arg_' + name
	wrapper_fn_name := name + '_thread_wrapper'
	arg_tmp_var := 'arg_' + tmp
	if is_spawn {
		g.writeln('${wrapper_struct_name} *${arg_tmp_var} = (${wrapper_struct_name} *) _v_malloc(sizeof(thread_arg_${name}));')
	} else if is_go {
		g.writeln('${wrapper_struct_name} ${arg_tmp_var};')
	}
	dot := if is_spawn { '->' } else { '.' }
	fn_name := if use_tmp_fn_var {
		tmp_fn
	} else if expr.is_fn_var {
		expr.name
	} else {
		name
	}
	if !(expr.is_method && g.table.sym(expr.receiver_type).kind == .interface_) {
		g.writeln('${arg_tmp_var}${dot}fn = ${fn_name};')
	}
	if expr.is_method {
		g.write('${arg_tmp_var}${dot}arg0 = ')
		g.expr(expr.left)
		g.writeln(';')
	}
	for i, arg in expr.args {
		g.write('${arg_tmp_var}${dot}arg${i + 1} = ')
		g.expr(arg.expr)
		g.writeln(';')
	}
	s_ret_typ := g.typ(node.call_expr.return_type)
	if g.pref.os == .windows && node.call_expr.return_type != ast.void_type {
		g.writeln('${arg_tmp_var}->ret_ptr = malloc(sizeof(${s_ret_typ}));')
	}
	is_opt := node.call_expr.return_type.has_flag(.option)
	is_res := node.call_expr.return_type.has_flag(.result)
	mut gohandle_name := ''
	if node.call_expr.return_type == ast.void_type {
		if is_opt {
			gohandle_name = '__v_thread_Option_void'
		} else if is_res {
			gohandle_name = '__v_thread_Result_void'
		} else {
			gohandle_name = '__v_thread'
		}
	} else {
		opt := if is_opt { '${option_name}_' } else { '' }
		res := if is_res { '${result_name}_' } else { '' }
		gohandle_name = '__v_thread_${opt}${res}${g.table.sym(g.unwrap_generic(node.call_expr.return_type)).cname}'
	}
	if is_spawn {
		if g.pref.os == .windows {
			simple_handle := if node.is_expr && node.call_expr.return_type != ast.void_type {
				'thread_handle_${tmp}'
			} else {
				'thread_${tmp}'
			}
			stack_size := g.get_cur_thread_stack_size(expr.name)
			g.writeln('HANDLE ${simple_handle} = CreateThread(0, ${stack_size}, (LPTHREAD_START_ROUTINE)${wrapper_fn_name}, ${arg_tmp_var}, 0, 0); // fn: ${expr.name}')
			g.writeln('if (!${simple_handle}) panic_lasterr(tos3("`go ${name}()`: "));')
			if node.is_expr && node.call_expr.return_type != ast.void_type {
				g.writeln('${gohandle_name} thread_${tmp} = {')
				g.writeln('\t.ret_ptr = ${arg_tmp_var}->ret_ptr,')
				g.writeln('\t.handle = thread_handle_${tmp}')
				g.writeln('};')
			}
			if !node.is_expr {
				g.writeln('CloseHandle(thread_${tmp});')
			}
		} else {
			g.writeln('pthread_t thread_${tmp};')
			mut sthread_attributes := 'NULL'
			if g.pref.os != .vinix {
				g.writeln('pthread_attr_t thread_${tmp}_attributes;')
				g.writeln('pthread_attr_init(&thread_${tmp}_attributes);')
				size := g.get_cur_thread_stack_size(expr.name)
				g.writeln('pthread_attr_setstacksize(&thread_${tmp}_attributes, ${size}); // fn: ${expr.name}')
				sthread_attributes = '&thread_${tmp}_attributes'
			}
			g.writeln('int ${tmp}_thr_res = pthread_create(&thread_${tmp}, ${sthread_attributes}, (void*)${wrapper_fn_name}, ${arg_tmp_var});')
			g.writeln('if (${tmp}_thr_res) panic_error_number(tos3("`go ${name}()`: "), ${tmp}_thr_res);')
			if !node.is_expr {
				g.writeln('pthread_detach(thread_${tmp});')
			}
		}
	} else if is_go {
		g.writeln('photon_thread_create((void*)${wrapper_fn_name}, &${arg_tmp_var});')
	}
	g.writeln('// end go')
	if node.is_expr {
		handle = 'thread_${tmp}'
		// create wait handler for this return type if none exists
		waiter_fn_name := gohandle_name + '_wait'
		mut should_register := false
		lock g.waiter_fns {
			if waiter_fn_name !in g.waiter_fns {
				g.waiter_fns << waiter_fn_name
				should_register = true
			}
		}
		if should_register {
			g.gowrappers.writeln('\n${s_ret_typ} ${waiter_fn_name}(${gohandle_name} thread) {')
			mut c_ret_ptr_ptr := 'NULL'
			if node.call_expr.return_type != ast.void_type {
				g.gowrappers.writeln('\t${s_ret_typ}* ret_ptr;')
				c_ret_ptr_ptr = '&ret_ptr'
			}
			if g.pref.os == .windows {
				if node.call_expr.return_type == ast.void_type {
					g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread, INFINITE);')
				} else {
					g.gowrappers.writeln('\tu32 stat = WaitForSingleObject(thread.handle, INFINITE);')
					g.gowrappers.writeln('\tret_ptr = thread.ret_ptr;')
				}
			} else {
				g.gowrappers.writeln('\tint stat = pthread_join(thread, (void **)${c_ret_ptr_ptr});')
			}
			g.gowrappers.writeln('\tif (stat != 0) { _v_panic(_SLIT("unable to join thread")); }')
			if g.pref.os == .windows {
				if node.call_expr.return_type == ast.void_type {
					g.gowrappers.writeln('\tCloseHandle(thread);')
				} else {
					g.gowrappers.writeln('\tCloseHandle(thread.handle);')
				}
			}
			if node.call_expr.return_type != ast.void_type {
				g.gowrappers.writeln('\t${s_ret_typ} ret = *ret_ptr;')
				g.gowrappers.writeln('\tfree(ret_ptr);')
				g.gowrappers.writeln('\treturn ret;')
			}
			g.gowrappers.writeln('}')
		}
	}
	// Register the wrapper type and function
	mut should_register := false
	lock g.threaded_fns {
		if name !in g.threaded_fns {
			g.threaded_fns << name
			should_register = true
		}
	}
	if should_register {
		g.type_definitions.writeln('\ntypedef struct ${wrapper_struct_name} {')
		mut fn_var := ''
		if node.call_expr.is_fn_var {
			fn_sym := g.table.sym(node.call_expr.fn_var_type)
			info := fn_sym.info as ast.FnType
			fn_var = g.fn_var_signature(info.func.return_type, info.func.params.map(it.typ),
				'fn')
		} else if node.call_expr.left is ast.AnonFn {
			f := node.call_expr.left.decl
			fn_var = g.fn_var_signature(f.return_type, f.params.map(it.typ), 'fn')
		} else {
			if node.call_expr.is_method {
				rec_sym := g.table.sym(g.unwrap_generic(node.call_expr.receiver_type))
				if f := rec_sym.find_method_with_generic_parent(node.call_expr.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					return_type := muttable.resolve_generic_to_concrete(f.return_type,
						f.generic_names, node.call_expr.concrete_types) or { f.return_type }
					mut arg_types := f.params.map(it.typ)
					arg_types = arg_types.map(muttable.resolve_generic_to_concrete(it,
						f.generic_names, node.call_expr.concrete_types) or { it })
					fn_var = g.fn_var_signature(return_type, arg_types, 'fn')
				}
			} else {
				if f := g.table.find_fn(node.call_expr.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					return_type := muttable.resolve_generic_to_concrete(f.return_type,
						f.generic_names, node.call_expr.concrete_types) or { f.return_type }
					mut arg_types := f.params.map(it.typ)
					arg_types = arg_types.map(muttable.resolve_generic_to_concrete(it,
						f.generic_names, node.call_expr.concrete_types) or { it })
					fn_var = g.fn_var_signature(return_type, arg_types, 'fn')
				}
			}
		}
		g.type_definitions.writeln('\t${fn_var};')
		if expr.is_method {
			styp := g.typ(expr.receiver_type)
			g.type_definitions.writeln('\t${styp} arg0;')
		}
		need_return_ptr := g.pref.os == .windows && node.call_expr.return_type != ast.void_type
		for i, arg in expr.args {
			arg_sym := g.table.sym(arg.typ)
			if arg_sym.info is ast.FnType {
				sig := g.fn_var_signature(arg_sym.info.func.return_type, arg_sym.info.func.params.map(it.typ),
					'arg${i + 1}')
				g.type_definitions.writeln('\t' + sig + ';')
			} else {
				styp := g.typ(arg.typ)
				g.type_definitions.writeln('\t${styp} arg${i + 1};')
			}
		}
		if need_return_ptr {
			g.type_definitions.writeln('\tvoid* ret_ptr;')
		}
		g.type_definitions.writeln('} ${wrapper_struct_name};')
		thread_ret_type := if g.pref.os == .windows { 'u32' } else { 'void*' }
		g.type_definitions.writeln('${g.static_modifier} ${thread_ret_type} ${wrapper_fn_name}(${wrapper_struct_name} *arg);')
		g.gowrappers.writeln('${thread_ret_type} ${wrapper_fn_name}(${wrapper_struct_name} *arg) {')
		if node.call_expr.return_type != ast.void_type {
			if g.pref.os == .windows {
				g.gowrappers.write_string('\t*((${s_ret_typ}*)(arg->ret_ptr)) = ')
			} else {
				g.gowrappers.writeln('\t${s_ret_typ}* ret_ptr = malloc(sizeof(${s_ret_typ}));')
				g.gowrappers.write_string('\t*ret_ptr = ')
			}
		} else {
			g.gowrappers.write_string('\t')
		}
		if expr.is_method {
			unwrapped_rec_type := g.unwrap_generic(expr.receiver_type)
			typ_sym := g.table.sym(unwrapped_rec_type)
			if typ_sym.kind == .interface_
				&& (typ_sym.info as ast.Interface).defines_method(expr.name) {
				rec_cc_type := g.cc_type(unwrapped_rec_type, false)
				receiver_type_name := util.no_dots(rec_cc_type)
				g.gowrappers.write_string('${c_name(receiver_type_name)}_name_table[')
				g.gowrappers.write_string('arg->arg0')
				idot := if expr.left_type.is_ptr() { '->' } else { '.' }
				mname := c_name(expr.name)
				g.gowrappers.write_string('${idot}_typ]._method_${mname}(')
				g.gowrappers.write_string('arg->arg0')
				g.gowrappers.write_string('${idot}_object')
			} else {
				g.gowrappers.write_string('arg->fn(')
				g.gowrappers.write_string('arg->arg0')
			}
			if expr.args.len > 0 {
				g.gowrappers.write_string(', ')
			}
		} else {
			g.gowrappers.write_string('arg->fn(')
		}
		if expr.args.len > 0 {
			mut has_cast := false
			for i in 0 .. expr.args.len {
				if g.table.sym(expr.expected_arg_types[i]).kind == .interface_
					&& g.table.sym(expr.args[i].typ).kind != .interface_ {
					has_cast = true
					break
				}
			}
			if has_cast {
				pos := g.out.len
				g.call_args(expr)
				mut call_args_str := g.out.after(pos)
				g.go_back(call_args_str.len)
				mut rep_group := []string{cap: 2 * expr.args.len}
				for i in 0 .. expr.args.len {
					rep_group << g.expr_string(expr.args[i].expr)
					rep_group << 'arg->arg${i + 1}'
				}
				call_args_str = call_args_str.replace_each(rep_group)
				g.gowrappers.write_string(call_args_str)
			} else if expr.name in ['print', 'println', 'eprint', 'eprintln', 'panic']
				&& expr.args[0].typ != ast.string_type {
				pos := g.out.len
				g.gen_expr_to_string(expr.args[0].expr, expr.args[0].typ)
				mut call_args_str := g.out.after(pos)
				g.out.go_back(call_args_str.len)
				mut rep_group := []string{cap: 2 * expr.args.len}
				for i in 0 .. expr.args.len {
					rep_group << g.expr_string(expr.args[i].expr)
					rep_group << 'arg->arg${i + 1}'
				}
				call_args_str = call_args_str.replace_each(rep_group)
				g.gowrappers.write_string(call_args_str)
			} else {
				for i in 0 .. expr.args.len {
					expected_nr_muls := expr.expected_arg_types[i].nr_muls()
					arg_nr_muls := expr.args[i].typ.nr_muls()
					if arg_nr_muls > expected_nr_muls {
						g.gowrappers.write_string('*'.repeat(arg_nr_muls - expected_nr_muls))
					} else if arg_nr_muls < expected_nr_muls {
						g.gowrappers.write_string('&'.repeat(expected_nr_muls - arg_nr_muls))
					}
					g.gowrappers.write_string('arg->arg${i + 1}')
					if i != expr.args.len - 1 {
						g.gowrappers.write_string(', ')
					}
				}
			}
		}
		g.gowrappers.writeln(');')
		g.gowrappers.writeln('\tfree(arg);')
		if g.pref.os != .windows && node.call_expr.return_type != ast.void_type {
			g.gowrappers.writeln('\treturn ret_ptr;')
		} else {
			g.gowrappers.writeln('\treturn 0;')
		}
		g.gowrappers.writeln('}')
	}
	if node.is_expr {
		g.empty_line = false
		g.write(line)
		g.write(handle)
	}
}

// fn (mut g Gen) go_expr(node ast.GoExpr) {
//}

// get current thread size, if fn hasn't defined return default
[inline]
fn (mut g Gen) get_cur_thread_stack_size(name string) string {
	ast_fn := g.table.fns[name] or { return '${g.pref.thread_stack_size}' }
	attrs := ast_fn.attrs
	if isnil(attrs) {
		return '${g.pref.thread_stack_size}'
	}
	for attr in attrs {
		if attr.name == 'spawn_stack' {
			return attr.arg
		}
	}
	return '${g.pref.thread_stack_size}'
}

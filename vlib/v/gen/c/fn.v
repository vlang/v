// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import strings
import v.ast
import v.util

const c_fn_name_escape_seq = ['[', '_T_', ']', '']

fn (mut g Gen) is_used_by_main(node ast.FnDecl) bool {
	mut is_used_by_main := true
	if g.pref.skip_unused {
		fkey := node.fkey()
		is_used_by_main = g.table.used_fns[fkey]
		$if trace_skip_unused_fns ? {
			println('> is_used_by_main: ${is_used_by_main} | node.name: ${node.name} | fkey: ${fkey} | node.is_method: ${node.is_method}')
		}
		if !is_used_by_main {
			$if trace_skip_unused_fns_in_c_code ? {
				g.writeln('// trace_skip_unused_fns_in_c_code, ${node.name}, fkey: ${fkey}')
			}
		}
	} else {
		$if trace_skip_unused_fns_in_c_code ? {
			g.writeln('// trace_skip_unused_fns_in_c_code, ${node.name}, fkey: ${node.fkey()}')
		}
	}
	return is_used_by_main
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.should_be_skipped {
		return
	}
	if node.is_test {
		g.test_function_names << node.name
	}
	if node.ninstances == 0 && node.generic_names.len > 0 {
		$if trace_generics ? {
			eprintln('skipping generic fn with no concrete instances: ${node.mod} ${node.name}')
		}
		return
	}
	if !g.is_used_by_main(node) {
		return
	}
	if g.is_builtin_mod && g.pref.gc_mode == .boehm_leak && node.name == 'malloc' {
		g.definitions.write_string('#define _v_malloc GC_MALLOC\n')
		return
	}
	if g.pref.parallel_cc {
		if node.is_anon {
			g.write('static ')
			g.definitions.write_string('static ')
		}
		if !node.is_anon {
			g.out_fn_start_pos << g.out.len
		}
	}
	prev_is_direct_array_access := g.is_direct_array_access
	g.is_direct_array_access = node.is_direct_arr || g.pref.no_bounds_checking
	defer {
		g.is_direct_array_access = prev_is_direct_array_access
	}
	g.gen_attrs(node.attrs)
	mut skip := false
	pos := g.out.len
	should_bundle_module := util.should_bundle_module(node.mod)
	if g.pref.build_mode == .build_module {
		// TODO true for not just "builtin"
		// TODO: clean this up
		mod := if g.is_builtin_mod { 'builtin' } else { node.name.all_before_last('.') }
		// for now dont skip generic functions as they are being marked as static
		// when -usecache is enabled, until a better solution is implemented.
		if ((mod != g.module_built && node.mod != g.module_built.after('/'))
			|| should_bundle_module) && node.generic_names.len == 0 {
			// Skip functions that don't have to be generated for this module.
			// println('skip bm $node.name mod=$node.mod module_built=$g.module_built')
			skip = true
		}
		if g.is_builtin_mod && g.module_built == 'builtin' && node.mod == 'builtin' {
			skip = false
		}
		if !skip && g.pref.is_verbose {
			println('build module `${g.module_built}` fn `${node.name}`')
		}
	}
	if g.pref.use_cache {
		// We are using prebuilt modules, we do not need to generate
		// their functions in main.c.
		if node.mod != 'main' && node.mod != 'help' && !should_bundle_module && !g.pref.is_test
			&& node.generic_names.len == 0 {
			skip = true
		}
	}
	keep_fn_decl := g.fn_decl
	unsafe {
		g.fn_decl = &node
	}
	if node.is_main {
		g.has_main = true
	}
	// TODO PERF remove this from here
	is_backtrace := node.name.starts_with('backtrace')
		&& node.name in ['backtrace_symbols', 'backtrace', 'backtrace_symbols_fd']
	if is_backtrace {
		g.write('\n#ifndef __cplusplus\n')
	}
	g.gen_fn_decl(node, skip)
	if is_backtrace {
		g.write('\n#endif\n')
	}
	g.fn_decl = keep_fn_decl
	if skip {
		g.go_back_to(pos)
	}
	if !g.pref.skip_unused {
		if node.language != .c {
			g.writeln('')
		}
	}
	// Write the next function into another parallel C file
	// g.out_idx++
	// if g.out_idx >= g.out_parallel.len {
	// g.out_idx = 0
	//}
}

fn (mut g Gen) gen_fn_decl(node &ast.FnDecl, skip bool) {
	// TODO For some reason, build fails with autofree with this line
	// as it's only informative, comment it for now
	// g.gen_attrs(it.attrs)
	if node.language == .c {
		return
	}

	old_is_vlines_enabled := g.is_vlines_enabled
	g.is_vlines_enabled = true
	defer {
		g.is_vlines_enabled = old_is_vlines_enabled
	}

	tmp_defer_vars := g.defer_vars // must be here because of workflow
	if !g.anon_fn {
		g.defer_vars = []string{}
	} else {
		if node.defer_stmts.len > 0 {
			g.defer_vars = []string{}
			defer {
				g.defer_vars = tmp_defer_vars
			}
		}
	}
	// Skip [if xxx] if xxx is not defined
	/*
	for attr in node.attrs {
		if !attr.is_comptime_define {
			continue
		}
		if attr.name !in g.pref.compile_defines_all {
			// println('skipping [if]')
			return
		}
	}
	*/

	g.returned_var_name = ''
	old_g_autofree := g.is_autofree
	if node.is_manualfree {
		g.is_autofree = false
	}
	defer {
		g.is_autofree = old_g_autofree
	}
	if node.generic_names.len > 0 && g.cur_concrete_types.len == 0 {
		// need the cur_concrete_type check to avoid inf. recursion
		// loop thru each generic type and generate a function
		nkey := node.fkey()
		generic_types_by_fn := g.table.fn_generic_types[nkey]
		$if trace_post_process_generic_fns ? {
			eprintln('>> gen_fn_decl, nkey: ${nkey} | generic_types_by_fn: ${generic_types_by_fn}')
		}
		for concrete_types in generic_types_by_fn {
			if g.pref.is_verbose {
				syms := concrete_types.map(g.table.sym(it))
				the_type := syms.map(it.name).join(', ')
				println('gen fn `${node.name}` for type `${the_type}`')
			}
			g.cur_concrete_types = concrete_types
			g.gen_fn_decl(node, skip)
		}
		g.cur_concrete_types = []
		return
	}
	cur_fn_save := g.cur_fn
	defer {
		g.cur_fn = cur_fn_save
	}
	unsafe {
		// TODO remove unsafe
		g.cur_fn = node
	}
	fn_start_pos := g.out.len
	is_closure := node.scope.has_inherited_vars()
	mut cur_closure_ctx := ''
	if is_closure {
		cur_closure_ctx = g.closure_ctx(node)
		// declare the struct before its implementation
		g.definitions.write_string(cur_closure_ctx)
		g.definitions.writeln(';')
	}

	g.write_v_source_line_info(node.pos)
	fn_attrs := g.write_fn_attrs(node.attrs)
	// Live
	is_livefn := node.attrs.contains('live')
	is_livemain := g.pref.is_livemain && is_livefn
	is_liveshared := g.pref.is_liveshared && is_livefn
	is_livemode := g.pref.is_livemain || g.pref.is_liveshared
	is_live_wrap := is_livefn && is_livemode
	if is_livefn && !is_livemode {
		eprintln('INFO: compile with `v -live ${g.pref.path} `, if you want to use the [live] function ${node.name} .')
	}

	mut name := g.c_fn_name(node)
	mut type_name := g.typ(g.unwrap_generic(node.return_type))

	ret_sym := g.table.sym(g.unwrap_generic(node.return_type))
	if node.return_type.has_flag(.generic) && !node.return_type.has_option_or_result()
		&& ret_sym.kind == .array_fixed {
		type_name = '_v_${type_name}'
	} else if ret_sym.kind == .alias && !node.return_type.has_option_or_result() {
		unalias_typ := g.table.unaliased_type(node.return_type)
		unalias_sym := g.table.sym(unalias_typ)
		if unalias_sym.kind == .array_fixed {
			type_name = if !(unalias_sym.info as ast.ArrayFixed).is_fn_ret { '_v_' } else { '' } +
				g.typ(unalias_typ)
		}
	}

	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') && !node.is_main
		&& node.name != 'str' {
		mut key := node.name
		if node.is_method {
			sym := g.table.sym(node.receiver.typ)
			key = sym.name + '.' + node.name
		}
		g.writeln('/* obf: ${key} */')
		name = g.obf_table[key] or {
			panic('cgen: fn_decl: obf name "${key}" not found, this should never happen')
		}
	}
	// Live functions are protected by a mutex, because otherwise they
	// can be changed by the live reload thread, *while* they are
	// running, with unpredictable results (usually just crashing).
	// For this purpose, the actual body of the live function,
	// is put under a non publicly accessible function, that is prefixed
	// with 'impl_live_' .
	if is_livemode {
		if is_livefn {
			g.hotcode_fn_names << name
		}
		g.hotcode_fpaths << g.file.path
	}
	mut impl_fn_name := name
	if is_live_wrap {
		impl_fn_name = 'impl_live_${name}'
	}
	last_fn_c_name_save := g.last_fn_c_name
	defer {
		g.last_fn_c_name = last_fn_c_name_save
	}
	g.last_fn_c_name = impl_fn_name
	//
	if is_live_wrap {
		if is_livemain {
			g.definitions.write_string('${type_name} (* ${impl_fn_name})(')
			g.write('${type_name} no_impl_${name}(')
		}
		if is_liveshared {
			g.definitions.write_string('${type_name} ${impl_fn_name}(')
			g.write('${type_name} ${impl_fn_name}(')
		}
	} else {
		if !(node.is_pub || g.pref.is_debug) {
			// Private functions need to marked as static so that they are not exportable in the
			// binaries
			if g.pref.build_mode != .build_module && !g.pref.use_cache {
				// If we are building vlib/builtin, we need all private functions like array_get
				// to be public, so that all V programs can access them.
				g.write('VV_LOCAL_SYMBOL ')
				g.definitions.write_string('VV_LOCAL_SYMBOL ')
			}
		}
		// as a temp solution generic functions are marked static
		// when -usecache is enabled to fix duplicate symbols with clang
		// TODO: implement a better sulution
		visibility_kw := if g.cur_concrete_types.len > 0
			&& (g.pref.build_mode == .build_module || g.pref.use_cache) {
			'static '
		} else {
			''
		}
		fn_header := '${visibility_kw}${type_name} ${fn_attrs}${name}('
		g.definitions.write_string(fn_header)
		g.write(fn_header)
	}
	arg_start_pos := g.out.len
	fargs, fargtypes, heap_promoted := g.fn_decl_params(node.params, node.scope, node.is_variadic)
	if is_closure {
		g.nr_closures++
	}
	arg_str := g.out.after(arg_start_pos)
	if node.no_body || ((g.pref.use_cache && g.pref.build_mode != .build_module) && node.is_builtin
		&& !g.pref.is_test) || skip {
		// Just a function header. Builtin function bodies are defined in builtin.o
		g.definitions.writeln(');') // NO BODY')
		g.writeln(');')
		return
	}
	if node.params.len == 0 {
		g.definitions.write_string('void')
	}
	g.definitions.writeln(');')
	g.writeln(') {')
	if is_closure {
		g.writeln('${cur_closure_ctx}* ${c.closure_ctx} = __CLOSURE_GET_DATA();')
	}
	for i, is_promoted in heap_promoted {
		if is_promoted {
			g.writeln('${fargtypes[i]}* ${fargs[i]} = HEAP(${fargtypes[i]}, _v_toheap_${fargs[i]});')
		}
	}
	g.indent++
	for defer_stmt in node.defer_stmts {
		g.writeln('bool ${g.defer_flag_var(defer_stmt)} = false;')
		for var in defer_stmt.defer_vars {
			if var.name in fargs || var.kind == .constant {
				continue
			}
			if var.kind == .variable {
				if var.name !in g.defer_vars {
					g.defer_vars << var.name
					mut deref := ''
					if v := var.scope.find_var(var.name) {
						if v.is_auto_heap {
							deref = '*'
						}
					}
					info := var.obj as ast.Var
					if g.table.sym(info.typ).kind != .function {
						g.writeln('${g.typ(info.typ)}${deref} ${c_name(var.name)};')
					}
				}
			}
		}
	}
	g.indent--
	if is_live_wrap {
		// The live function just calls its implementation dual, while ensuring
		// that the call is wrapped by the mutex lock & unlock calls.
		// Adding the mutex lock/unlock inside the body of the implementation
		// function is not reliable, because the implementation function can do
		// an early exit, which will leave the mutex locked.
		mut fn_args_list := []string{}
		for ia, fa in fargs {
			fn_args_list << '${fargtypes[ia]} ${fa}'
		}
		mut live_fncall := '${impl_fn_name}(' + fargs.join(', ') + ');'
		mut live_fnreturn := ''
		if type_name != 'void' {
			live_fncall = '${type_name} res = ${live_fncall}'
			live_fnreturn = 'return res;'
		}
		g.definitions.writeln('${type_name} ${name}(' + fn_args_list.join(', ') + ');')
		g.hotcode_definitions.writeln('${type_name} ${name}(' + fn_args_list.join(', ') + '){')
		g.hotcode_definitions.writeln('  pthread_mutex_lock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  ${live_fncall}')
		g.hotcode_definitions.writeln('  pthread_mutex_unlock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  ${live_fnreturn}')
		g.hotcode_definitions.writeln('}')
	}
	// Profiling mode? Start counting at the beginning of the function (save current time).
	if g.pref.is_prof && g.pref.build_mode != .build_module {
		g.profile_fn(node)
	}
	// we could be in an anon fn so save outer fn defer stmts
	prev_defer_stmts := g.defer_stmts
	g.defer_stmts = []
	ctmp := g.tmp_count
	g.tmp_count = 0
	defer {
		g.tmp_count = ctmp
	}
	prev_inside_ternary := g.inside_ternary
	g.inside_ternary = 0
	prev_indent := g.indent
	g.indent = 0
	defer {
		g.indent = prev_indent
	}
	g.stmts(node.stmts)
	g.inside_ternary = prev_inside_ternary
	if node.is_noreturn {
		g.writeln('\twhile(1);')
	}
	// clear g.fn_mut_arg_names

	if !node.has_return {
		g.write_defer_stmts_when_needed()
	}
	if node.is_anon {
		g.defer_stmts = prev_defer_stmts
	} else {
		g.defer_stmts = []
	}
	if node.return_type != ast.void_type && node.stmts.len > 0 && node.stmts.last() !is ast.Return
		&& !node.attrs.contains('_naked') {
		default_expr := g.type_default(node.return_type)
		// TODO: perf?
		if default_expr == '{0}' {
			g.writeln('\treturn (${type_name})${default_expr};')
		} else {
			g.writeln('\treturn ${default_expr};')
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && g.last_fn_c_name in g.pref.printfn_list {
		println(g.out.after(fn_start_pos))
	}
	for attr in node.attrs {
		if attr.name == 'export' {
			weak := if node.attrs.any(it.name == 'weak') { 'VWEAK ' } else { '' }
			g.writeln('// export alias: ${attr.arg} -> ${name}')
			export_alias := '${weak}${type_name} ${fn_attrs}${attr.arg}(${arg_str})'
			g.definitions.writeln('VV_EXPORTED_SYMBOL ${export_alias}; // exported fn ${node.name}')
			g.writeln('${export_alias} {')
			g.write('\treturn ${name}(')
			g.write(fargs.join(', '))
			g.writeln(');')
			g.writeln('}')
		}
	}
}

fn (mut g Gen) c_fn_name(node &ast.FnDecl) string {
	mut name := node.name
	if name in ['+', '-', '*', '/', '%', '<', '=='] {
		name = util.replace_op(name)
	}
	if node.is_method {
		unwrapped_rec_typ := g.unwrap_generic(node.receiver.typ)
		name = g.cc_type(unwrapped_rec_typ, false) + '_' + name
		if g.table.sym(unwrapped_rec_typ).kind == .placeholder {
			name = name.replace_each(c.c_fn_name_escape_seq)
		}
	}
	if node.language == .c {
		name = util.no_dots(name)
	} else {
		name = c_fn_name(name)
	}

	if node.generic_names.len > 0 {
		name = g.generic_fn_name(g.cur_concrete_types, name)
	}

	if g.pref.translated || g.file.is_translated || node.is_file_translated {
		if cattr := node.attrs.find_first('c') {
			// This fixes unknown symbols errors when building separate .c => .v files into .o files
			// example:
			// [c: 'P_TryMove'] fn p_trymove(thing &Mobj_t, x int, y int) bool
			// translates to:
			// bool P_TryMove(main__Mobj_t* thing, int x, int y);
			// In fn_call every time `p_trymove` is called, `P_TryMove` will be generated instead.
			name = cattr.arg
		}
	}
	return name
}

const closure_ctx = '_V_closure_ctx'

fn (mut g Gen) closure_ctx(node ast.FnDecl) string {
	mut fn_name := node.name
	if node.generic_names.len > 0 {
		fn_name = g.generic_fn_name(g.cur_concrete_types, fn_name)
	}
	return 'struct _V_${fn_name}_Ctx'
}

fn (mut g Gen) gen_anon_fn(mut node ast.AnonFn) {
	g.gen_anon_fn_decl(mut node)
	mut fn_name := node.decl.name
	if node.decl.generic_names.len > 0 {
		fn_name = g.generic_fn_name(g.cur_concrete_types, fn_name)
	}

	if !node.decl.scope.has_inherited_vars() {
		g.write(fn_name)
		return
	}
	ctx_struct := g.closure_ctx(node.decl)
	// it may be possible to optimize `memdup` out if the closure never leaves current scope
	// TODO in case of an assignment, this should only call "__closure_set_data" and "__closure_set_function" (and free the former data)
	g.write('__closure_create(${fn_name}, (${ctx_struct}*) memdup_uncollectable(&(${ctx_struct}){')
	g.indent++
	for var in node.inherited_vars {
		mut has_inherited := false
		if obj := node.decl.scope.find(var.name) {
			if obj is ast.Var {
				if obj.has_inherited {
					has_inherited = true
					var_sym := g.table.sym(var.typ)
					if var_sym.info is ast.ArrayFixed {
						g.write('.${var.name} = {')
						for i in 0 .. var_sym.info.size {
							g.write('${c.closure_ctx}->${var.name}[${i}],')
						}
						g.writeln('},')
					} else {
						g.writeln('.${var.name} = ${c.closure_ctx}->${var.name},')
					}
				}
			}
		}
		if !has_inherited {
			var_sym := g.table.sym(var.typ)
			if var_sym.info is ast.ArrayFixed {
				g.write('.${var.name} = {')
				for i in 0 .. var_sym.info.size {
					g.write('${var.name}[${i}],')
				}
				g.writeln('},')
			} else {
				g.writeln('.${var.name} = ${var.name},')
			}
		}
	}
	g.indent--
	g.write('}, sizeof(${ctx_struct})))')

	g.empty_line = false
}

fn (mut g Gen) gen_anon_fn_decl(mut node ast.AnonFn) {
	mut fn_name := node.decl.name
	if node.decl.generic_names.len > 0 {
		fn_name = g.generic_fn_name(g.cur_concrete_types, fn_name)
	}
	if node.has_gen[fn_name] {
		return
	}
	node.has_gen[fn_name] = true
	mut builder := strings.new_builder(256)
	builder.writeln('/*F*/')
	if node.inherited_vars.len > 0 {
		ctx_struct := g.closure_ctx(node.decl)
		builder.writeln('${ctx_struct} {')
		for var in node.inherited_vars {
			var_sym := g.table.sym(var.typ)
			if var_sym.info is ast.FnType {
				sig := g.fn_var_signature(var_sym.info.func.return_type, var_sym.info.func.params.map(it.typ),
					var.name)
				builder.writeln('\t' + sig + ';')
			} else {
				styp := g.typ(var.typ)
				builder.writeln('\t${styp} ${var.name};')
			}
		}
		builder.writeln('};\n')
	}
	pos := g.out.len
	was_anon_fn := g.anon_fn
	g.anon_fn = true
	g.fn_decl(node.decl)
	g.anon_fn = was_anon_fn
	builder.write_string(g.out.cut_to(pos))
	g.anon_fn_definitions << builder.str()
}

fn (g &Gen) defer_flag_var(stmt &ast.DeferStmt) string {
	return '${g.last_fn_c_name}_defer_${stmt.idx_in_fn}'
}

fn (mut g Gen) write_defer_stmts_when_needed() {
	// unlock all mutexes, in case we are in a lock statement. defers are not allowed in lock statements
	g.unlock_locks()
	if g.defer_stmts.len > 0 {
		g.write_defer_stmts()
	}
	if g.defer_profile_code.len > 0 {
		g.writeln('')
		g.writeln('\t// defer_profile_code')
		g.writeln(g.defer_profile_code)
		g.writeln('')
	}
}

fn (mut g Gen) fn_decl_params(params []ast.Param, scope &ast.Scope, is_variadic bool) ([]string, []string, []bool) {
	mut fparams := []string{}
	mut fparamtypes := []string{}
	mut heap_promoted := []bool{}
	if params.len == 0 {
		// in C, `()` is untyped, unlike `(void)`
		g.write('void')
	}
	for i, param in params {
		mut caname := if param.name == '_' {
			g.new_tmp_declaration_name()
		} else {
			c_name(param.name)
		}
		mut typ := g.unwrap_generic(param.typ)
		if g.pref.translated && g.file.is_translated && param.typ.has_flag(.variadic) {
			typ = g.table.sym(typ).array_info().elem_type.set_flag(.variadic)
		}
		param_type_sym := g.table.sym(typ)
		mut param_type_name := g.typ(typ).replace_each(c.c_fn_name_escape_seq)
		if param_type_sym.kind == .function && !typ.has_flag(.option) {
			info := param_type_sym.info as ast.FnType
			func := info.func
			g.write('${g.typ(func.return_type)} (*${caname})(')
			g.definitions.write_string('${g.typ(func.return_type)} (*${caname})(')
			g.fn_decl_params(func.params, unsafe { nil }, func.is_variadic)
			g.write(')')
			g.definitions.write_string(')')
			fparams << caname
			fparamtypes << param_type_name
			heap_promoted << false
		} else {
			mut heap_prom := false
			if scope != unsafe { nil } {
				if param.name != '_' {
					if v := scope.find_var(param.name) {
						if !v.is_stack_obj && v.is_auto_heap {
							heap_prom = true
						}
					}
				}
			}
			var_name_prefix := if heap_prom { '_v_toheap_' } else { '' }
			const_prefix := if param.typ.is_any_kind_of_pointer() && !param.is_mut
				&& param.name.starts_with('const_') {
				'const '
			} else {
				''
			}
			s := '${const_prefix}${param_type_name} ${var_name_prefix}${caname}'
			g.write(s)
			g.definitions.write_string(s)
			fparams << caname
			fparamtypes << param_type_name
			heap_promoted << heap_prom
		}
		if i < params.len - 1 {
			g.write(', ')
			g.definitions.write_string(', ')
		}
	}
	if g.pref.translated && is_variadic {
		g.write(', ... ')
		g.definitions.write_string(', ... ')
	}
	return fparams, fparamtypes, heap_promoted
}

fn (mut g Gen) get_anon_fn_type_name(mut node ast.AnonFn, var_name string) string {
	mut builder := strings.new_builder(64)
	return_styp := g.typ(node.decl.return_type)
	builder.write_string('${return_styp} (*${var_name}) (')
	if node.decl.params.len == 0 {
		builder.write_string('void)')
	} else {
		for i, param in node.decl.params {
			param_styp := g.typ(param.typ)
			builder.write_string('${param_styp} ${param.name}')
			if i != node.decl.params.len - 1 {
				builder.write_string(', ')
			}
		}
		builder.write_string(')')
	}
	return builder.str()
}

fn (mut g Gen) call_expr(node ast.CallExpr) {
	if node.should_be_skipped {
		return
	}
	// NOTE: everything could be done this way
	// see my comment in parser near anon_fn
	if node.left is ast.AnonFn {
		if node.left.inherited_vars.len > 0 {
			tmp_var := g.new_tmp_var()
			fn_type := g.fn_var_signature(node.left.decl.return_type, node.left.decl.params.map(it.typ),
				tmp_var)
			line := g.go_before_last_stmt().trim_space()
			g.empty_line = true
			g.write('${fn_type} = ')
			g.expr(node.left)
			g.writeln(';')
			g.write(line)
			if g.out.last_n(1) != '\n' {
				g.writeln('')
			}
			g.write(tmp_var)
		} else if node.or_block.kind == .absent {
			g.expr(node.left)
		}
	} else if node.left is ast.IndexExpr && node.name == '' {
		g.is_fn_index_call = true
		g.expr(node.left)
		g.is_fn_index_call = false
	} else if !g.inside_curry_call && node.left is ast.CallExpr && node.name == '' {
		if node.or_block.kind == .absent {
			g.expr(node.left)
		} else {
			old_inside_curry_call := g.inside_curry_call
			g.inside_curry_call = true
			ret_typ := node.return_type

			line := g.go_before_last_stmt()
			g.empty_line = true

			tmp_res := g.new_tmp_var()
			g.write('${g.typ(ret_typ)} ${tmp_res} = ')

			g.last_tmp_call_var << tmp_res
			g.expr(node.left)
			g.expr(node)

			g.inside_curry_call = old_inside_curry_call
			g.write(line)
			g.write('*(${g.base_type(ret_typ)}*)${tmp_res}.data')
			return
		}
	}
	old_inside_call := g.inside_call
	g.inside_call = true
	defer {
		g.inside_call = old_inside_call
	}
	gen_keep_alive := node.is_keep_alive && node.return_type != ast.void_type
		&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt]
	gen_or := node.or_block.kind != .absent // && !g.is_autofree
	is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
	mut cur_line := if !g.inside_curry_call && (is_gen_or_and_assign_rhs || gen_keep_alive) { // && !g.is_autofree {
		// `x := foo() or { ...}`
		// cut everything that has been generated to prepend option variable creation
		line := g.go_before_last_stmt()
		g.out.write_string(util.tabs(g.indent))
		line
	} else {
		''
	}
	// g.write('/*EE line="$cur_line"*/')
	tmp_opt := if gen_or || gen_keep_alive {
		if g.inside_curry_call && g.last_tmp_call_var.len > 0 {
			g.last_tmp_call_var.pop()
		} else {
			g.new_tmp_var()
		}
	} else {
		''
	}
	if gen_or || gen_keep_alive {
		mut ret_typ := node.return_type
		if g.table.sym(ret_typ).kind == .alias {
			unaliased_type := g.table.unaliased_type(ret_typ)
			if unaliased_type.has_option_or_result() {
				ret_typ = unaliased_type
			}
		}
		mut styp := g.typ(ret_typ)
		if gen_or && !is_gen_or_and_assign_rhs {
			cur_line = g.go_before_last_stmt()
		}
		if gen_or && g.infix_left_var_name.len > 0 {
			g.writeln('${styp} ${tmp_opt};')
			g.writeln('if (${g.infix_left_var_name}) {')
			g.indent++
			g.write('${tmp_opt} = ')
		} else if !g.inside_curry_call {
			if g.assign_ct_type != 0 && node.or_block.kind in [.propagate_option, .propagate_result] {
				styp = g.typ(g.assign_ct_type.derive(ret_typ))
			}
			g.write('${styp} ${tmp_opt} = ')
			if node.left is ast.AnonFn {
				g.expr(node.left)
			}
		}
	}
	if node.is_method && !node.is_field {
		if node.name == 'writeln' && g.pref.experimental && node.args.len > 0
			&& node.args[0].expr is ast.StringInterLiteral
			&& g.table.sym(node.receiver_type).name == 'strings.Builder' {
			g.string_inter_literal_sb_optimized(node)
		} else {
			g.method_call(node)
		}
	} else {
		g.fn_call(node)
	}
	if gen_or {
		g.or_block(tmp_opt, node.or_block, node.return_type)
		mut unwrapped_typ := node.return_type.clear_option_and_result()
		if g.table.sym(unwrapped_typ).kind == .alias {
			unaliased_type := g.table.unaliased_type(unwrapped_typ)
			if unaliased_type.has_option_or_result() {
				unwrapped_typ = unaliased_type.clear_option_and_result()
			}
		}
		mut unwrapped_styp := g.typ(unwrapped_typ)
		if g.infix_left_var_name.len > 0 {
			g.indent--
			g.writeln('}')
			g.set_current_pos_as_last_stmt_pos()
		}
		if unwrapped_typ == ast.void_type {
			g.write('\n ${cur_line}')
		} else if !g.inside_curry_call {
			if !g.inside_const_opt_or_res {
				if g.assign_ct_type != 0
					&& node.or_block.kind in [.propagate_option, .propagate_result] {
					unwrapped_styp = g.typ(g.assign_ct_type.derive(node.return_type).clear_option_and_result())
				}
				if g.table.sym(node.return_type).kind == .array_fixed
					&& unwrapped_styp.starts_with('_v_') {
					unwrapped_styp = unwrapped_styp[3..]
				}
				g.write('\n ${cur_line} (*(${unwrapped_styp}*)${tmp_opt}.data)')
			} else {
				g.write('\n ${cur_line} ${tmp_opt}')
			}
		}
	} else if gen_keep_alive {
		if node.return_type == ast.void_type {
			g.write('\n ${cur_line}')
		} else {
			g.write('\n ${cur_line} ${tmp_opt}')
		}
	}
	if node.is_noreturn {
		if g.inside_ternary == 0 {
			g.writeln(';')
			g.write('VUNREACHABLE()')
		} else {
			$if msvc {
				// MSVC has no support for the statement expressions used below
			} $else {
				g.write(', ({VUNREACHABLE();})')
			}
		}
	}
}

fn (mut g Gen) conversion_function_call(prefix string, postfix string, node ast.CallExpr) {
	g.write('${prefix}( (')
	g.expr(node.left)
	dot := if node.left_type.is_ptr() { '->' } else { '.' }
	g.write(')${dot}_typ )${postfix}')
}

@[inline]
fn (mut g Gen) gen_arg_from_type(node_type ast.Type, node ast.Expr) {
	if node_type.has_flag(.shared_f) {
		if node_type.is_ptr() {
			g.write('&')
		}
		g.expr(node)
		g.write('->val')
	} else {
		if node_type.is_ptr() {
			g.expr(node)
		} else {
			g.write('&')
			g.expr(node)
		}
	}
}

fn (mut g Gen) gen_map_method_call(node ast.CallExpr, left_type ast.Type, left_sym ast.TypeSymbol) bool {
	match node.name {
		'clear' {
			g.write('map_clear(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(')')
			return true
		}
		'delete' {
			left_info := left_sym.info as ast.Map
			elem_type_str := g.typ(left_info.key_type)
			g.write('map_delete(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(', &(${elem_type_str}[]){')
			g.expr(node.args[0].expr)
			g.write('})')
			return true
		}
		else {}
	}
	return false
}

fn (mut g Gen) gen_array_method_call(node ast.CallExpr, left_type ast.Type) bool {
	match node.name {
		'filter' {
			g.gen_array_filter(node)
			return true
		}
		'sort' {
			g.gen_array_sort(node)
			return true
		}
		'sorted' {
			g.gen_array_sorted(node)
			return true
		}
		'insert' {
			g.gen_array_insert(node)
			return true
		}
		'map' {
			g.gen_array_map(node)
			return true
		}
		'prepend' {
			g.gen_array_prepend(node)
			return true
		}
		'contains' {
			g.gen_array_contains(left_type, node.left, node.args[0].typ, node.args[0].expr)
			return true
		}
		'index' {
			g.gen_array_index(node)
			return true
		}
		'wait' {
			g.gen_array_wait(node)
			return true
		}
		'any' {
			g.gen_array_any(node)
			return true
		}
		'all' {
			g.gen_array_all(node)
			return true
		}
		'delete', 'drop' {
			g.write('array_${node.name}(')
			g.gen_arg_from_type(left_type, node.left)
			g.write(', ')
			g.expr(node.args[0].expr)
			g.write(')')
			return true
		}
		else {}
	}
	return false
}

fn (mut g Gen) gen_to_str_method_call(node ast.CallExpr) bool {
	mut rec_type := node.receiver_type
	if rec_type.has_flag(.shared_f) {
		rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
	}
	left_node := node.left
	if left_node is ast.ComptimeSelector {
		key_str := g.comptime.get_comptime_selector_key_type(left_node)
		if key_str != '' {
			rec_type = g.comptime.type_map[key_str] or { rec_type }
			g.gen_expr_to_string(left_node, rec_type)
			return true
		}
	} else if left_node is ast.PostfixExpr {
		rec_type = g.resolve_comptime_type(left_node.expr, rec_type)
		if left_node.op == .question {
			rec_type = rec_type.clear_flag(.option)
		}
		g.gen_expr_to_string(left_node, rec_type)
		return true
	} else if left_node is ast.ComptimeCall {
		if left_node.method_name == 'method' {
			sym := g.table.sym(g.unwrap_generic(left_node.left_type))
			if m := sym.find_method(g.comptime.comptime_for_method) {
				rec_type = m.return_type
				g.gen_expr_to_string(left_node, rec_type)
				return true
			}
		}
	} else if left_node is ast.Ident {
		if left_node.obj is ast.Var {
			if left_node.obj.ct_type_var != .no_comptime {
				rec_type = g.comptime.get_comptime_var_type(left_node)
				g.gen_expr_to_string(left_node, rec_type)
				return true
			} else if g.comptime.type_map.len > 0 {
				rec_type = left_node.obj.typ
				g.gen_expr_to_string(left_node, rec_type)
				return true
			} else if left_node.obj.smartcasts.len > 0 {
				rec_type = g.unwrap_generic(left_node.obj.smartcasts.last())
				cast_sym := g.table.sym(rec_type)
				if cast_sym.info is ast.Aggregate {
					rec_type = cast_sym.info.types[g.aggregate_type_idx]
				}
				g.gen_expr_to_string(left_node, rec_type)
				return true
			}
		}
	} else if left_node is ast.None {
		g.gen_expr_to_string(left_node, ast.none_type)
		return true
	} else if node.left_type.has_flag(.option) {
		g.gen_expr_to_string(left_node, g.unwrap_generic(node.left_type))
		return true
	}
	g.get_str_fn(rec_type)
	return false
}

fn (mut g Gen) get_gn_var_type(var ast.Ident) ast.Type {
	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 {
		for k, cur_param in g.cur_fn.params {
			if (k == 0 && g.cur_fn.is_method) || !cur_param.typ.has_flag(.generic)
				|| var.name != cur_param.name {
				continue
			}
			mut typ := cur_param.typ
			mut cparam_type_sym := g.table.sym(g.unwrap_generic(typ))

			if cparam_type_sym.kind == .array {
				typ = g.unwrap_generic((cparam_type_sym.info as ast.Array).elem_type)
			} else if cparam_type_sym.kind == .array_fixed {
				typ = g.unwrap_generic((cparam_type_sym.info as ast.ArrayFixed).elem_type)
			}
			return typ
		}
	}
	return ast.void_type
}

fn (g Gen) get_generic_array_element_type(array ast.Array) ast.Type {
	mut cparam_elem_info := array as ast.Array
	mut cparam_elem_sym := g.table.sym(cparam_elem_info.elem_type)
	mut typ := ast.void_type
	for {
		if cparam_elem_sym.kind == .array {
			cparam_elem_info = cparam_elem_sym.info as ast.Array
			cparam_elem_sym = g.table.sym(cparam_elem_info.elem_type)
		} else {
			typ = cparam_elem_info.elem_type
			if cparam_elem_info.elem_type.nr_muls() > 0 && typ.nr_muls() > 0 {
				typ = typ.set_nr_muls(0)
			}
			break
		}
	}
	return typ
}

fn (mut g Gen) resolve_comptime_args(func ast.Fn, mut node_ ast.CallExpr, concrete_types []ast.Type) map[int]ast.Type {
	mut comptime_args := map[int]ast.Type{}
	has_dynamic_vars := (g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0)
		|| g.comptime.comptime_for_field_var != ''
	if has_dynamic_vars {
		offset := if func.is_method { 1 } else { 0 }
		mut k := -1
		for i, mut call_arg in node_.args {
			param := if func.is_variadic && i >= func.params.len - (offset + 1) {
				func.params.last()
			} else {
				func.params[offset + i]
			}
			k++
			if !param.typ.has_flag(.generic) {
				continue
			}
			param_typ := param.typ
			if mut call_arg.expr is ast.Ident {
				if mut call_arg.expr.obj is ast.Var {
					node_.args[i].typ = call_arg.expr.obj.typ
					if call_arg.expr.obj.ct_type_var !in [.generic_param, .no_comptime] {
						mut ctyp := g.comptime.get_comptime_var_type(call_arg.expr)
						if ctyp != ast.void_type {
							arg_sym := g.table.sym(ctyp)
							param_sym := g.table.final_sym(param_typ)
							if arg_sym.info is ast.Array && param_sym.kind == .array {
								ctyp = arg_sym.info.elem_type
							} else if arg_sym.info is ast.Map && param_sym.info is ast.Map {
								if call_arg.expr.obj.ct_type_var == .value_var {
									ctyp = arg_sym.info.value_type
									if param_sym.info.value_type.nr_muls() > 0 && ctyp.nr_muls() > 0 {
										ctyp = ctyp.set_nr_muls(0)
									}
								} else if call_arg.expr.obj.ct_type_var == .key_var {
									ctyp = arg_sym.info.key_type
									if param_sym.info.key_type.nr_muls() > 0 && ctyp.nr_muls() > 0 {
										ctyp = ctyp.set_nr_muls(0)
									}
								}
							}
							comptime_args[k] = ctyp
						}
					} else if call_arg.expr.obj.ct_type_var == .generic_param {
						mut ctyp := g.comptime.get_comptime_var_type(call_arg.expr)
						if ctyp != ast.void_type {
							arg_sym := g.table.final_sym(call_arg.typ)
							param_typ_sym := g.table.sym(param_typ)
							if param_typ.has_flag(.variadic) {
								ctyp = ast.mktyp(ctyp)
								comptime_args[k] = ctyp
							} else if arg_sym.kind == .array && param_typ.has_flag(.generic)
								&& param_typ_sym.kind == .array {
								ctyp = g.get_generic_array_element_type(arg_sym.info as ast.Array)
								comptime_args[k] = ctyp
							} else if arg_sym.kind in [.struct_, .interface_, .sum_type] {
								mut generic_types := []ast.Type{}
								match arg_sym.info {
									ast.Struct, ast.Interface, ast.SumType {
										if param_typ_sym.generic_types.len > 0 {
											generic_types = param_typ_sym.generic_types.clone()
										} else {
											generic_types = arg_sym.info.generic_types.clone()
										}
									}
									else {}
								}
								generic_names := generic_types.map(g.table.sym(it).name)
								for _, gt_name in g.cur_fn.generic_names {
									if gt_name in generic_names
										&& generic_types.len == concrete_types.len {
										idx := generic_names.index(gt_name)
										comptime_args[k] = concrete_types[idx]
										break
									}
								}
							} else if arg_sym.kind == .any {
								cparam_type_sym := g.table.sym(g.unwrap_generic(ctyp))
								if param_typ_sym.kind == .array && cparam_type_sym.info is ast.Array {
									comptime_args[k] = cparam_type_sym.info.elem_type
								} else if param_typ_sym.kind == .map
									&& cparam_type_sym.info is ast.Map {
									comptime_args[k] = cparam_type_sym.info.key_type
									comptime_args[k + 1] = cparam_type_sym.info.value_type
									k++
								} else {
									if node_.args[i].expr.is_auto_deref_var() {
										ctyp = ctyp.deref()
									}
									if ctyp.nr_muls() > 0 && param_typ.nr_muls() > 0 {
										ctyp = ctyp.set_nr_muls(0)
									}
									comptime_args[k] = ctyp
								}
							} else {
								comptime_args[k] = ctyp
							}
						}
					}
				}
			} else if mut call_arg.expr is ast.PrefixExpr {
				if call_arg.expr.right is ast.ComptimeSelector {
					comptime_args[k] = g.comptime.comptime_for_field_type
					comptime_args[k] = comptime_args[k].deref()
					if param_typ.nr_muls() > 0 && comptime_args[k].nr_muls() > 0 {
						comptime_args[k] = comptime_args[k].set_nr_muls(0)
					}
				}
			} else if mut call_arg.expr is ast.ComptimeSelector {
				comptime_args[k] = g.comptime.comptime_for_field_type
				arg_sym := g.table.final_sym(call_arg.typ)
				param_typ_sym := g.table.sym(param_typ)
				if arg_sym.kind == .array && param_typ.has_flag(.generic)
					&& param_typ_sym.kind == .array {
					comptime_args[k] = g.get_generic_array_element_type(arg_sym.info as ast.Array)
				}
				if param_typ.nr_muls() > 0 && comptime_args[k].nr_muls() > 0 {
					comptime_args[k] = comptime_args[k].set_nr_muls(0)
				}
			} else if mut call_arg.expr is ast.ComptimeCall {
				if call_arg.expr.method_name == 'method' {
					sym := g.table.sym(g.unwrap_generic(call_arg.expr.left_type))
					// `app.$method()`
					if m := sym.find_method(g.comptime.comptime_for_method) {
						comptime_args[k] = m.return_type
					}
				}
			} else if mut call_arg.expr is ast.CastExpr {
				cparam_type_sym := g.table.sym(g.unwrap_generic(call_arg.expr.typ))
				param_typ_sym := g.table.sym(param_typ)
				if param_typ_sym.kind == .map && cparam_type_sym.info is ast.Map {
					comptime_args[k] = cparam_type_sym.info.key_type
					comptime_args[k + 1] = cparam_type_sym.info.value_type
				}
			}
		}
	}
	return comptime_args
}

fn (mut g Gen) resolve_receiver_name(node ast.CallExpr, unwrapped_rec_type ast.Type, final_left_sym ast.TypeSymbol, left_sym ast.TypeSymbol, typ_sym ast.TypeSymbol) string {
	mut receiver_type_name := util.no_dots(g.cc_type(unwrapped_rec_type, false))
	if final_left_sym.kind == .map && node.name in ['clone', 'move'] {
		receiver_type_name = 'map'
	}
	if final_left_sym.kind == .array && !(left_sym.kind == .alias && left_sym.has_method(node.name))
		&& node.name in ['clear', 'repeat', 'sort_with_compare', 'sorted_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice', 'pointers'] {
		if !(left_sym.info is ast.Alias && typ_sym.has_method(node.name)) {
			// `array_Xyz_clone` => `array_clone`
			receiver_type_name = 'array'
		}
	}
	return receiver_type_name
}

fn (mut g Gen) resolve_receiver_type(node ast.CallExpr) (ast.Type, &ast.TypeSymbol) {
	left_type := g.unwrap_generic(node.left_type)
	mut unwrapped_rec_type := node.receiver_type
	if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0 { // in generic fn
		unwrapped_rec_type = g.unwrap_generic(node.receiver_type)
	} else { // in non-generic fn
		sym := g.table.sym(node.receiver_type)
		match sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				generic_names := sym.info.generic_types.map(g.table.sym(it).name)
				// see comment at top of vlib/v/gen/c/utils.v
				mut muttable := unsafe { &ast.Table(g.table) }
				if utyp := muttable.resolve_generic_to_concrete(node.receiver_type, generic_names,
					sym.info.concrete_types)
				{
					unwrapped_rec_type = utyp
				}
			}
			else {}
		}
	}
	if node.from_embed_types.len == 0 && node.left is ast.Ident {
		if node.left.obj is ast.Var {
			if node.left.obj.smartcasts.len > 0 {
				unwrapped_rec_type = g.unwrap_generic(node.left.obj.smartcasts.last())
				cast_sym := g.table.sym(unwrapped_rec_type)
				if cast_sym.info is ast.Aggregate {
					unwrapped_rec_type = cast_sym.info.types[g.aggregate_type_idx]
				}
			}
		}
	}
	mut typ_sym := g.table.sym(unwrapped_rec_type)
	// non-option alias type that undefined this method (not include `str`) need to use parent type
	if !left_type.has_flag(.option) && mut typ_sym.info is ast.Alias && node.name != 'str'
		&& !typ_sym.has_method(node.name) {
		unwrapped_rec_type = typ_sym.info.parent_type
		typ_sym = g.table.sym(unwrapped_rec_type)
	} else if mut typ_sym.info is ast.Array && !typ_sym.has_method(node.name) && node.name != 'str' {
		typ := g.table.unaliased_type(typ_sym.info.elem_type)
		typ_idx := g.table.find_type_idx(g.table.array_name(typ))
		if typ_idx > 0 {
			unwrapped_rec_type = ast.Type(typ_idx)
			typ_sym = g.table.sym(unwrapped_rec_type)
		}
	}
	if node.from_embed_types.len > 0 && !typ_sym.has_method(node.name) {
		unwrapped_rec_type = node.from_embed_types.last()
		typ_sym = g.table.sym(unwrapped_rec_type)
	}
	return unwrapped_rec_type, typ_sym
}

fn (mut g Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		g.checker_bug('CallExpr.left_type is 0 in method_call', node.pos)
	}
	if node.receiver_type == 0 {
		g.checker_bug('CallExpr.receiver_type is 0 in method_call', node.pos)
	}
	left_type := g.unwrap_generic(node.left_type)
	mut unwrapped_rec_type, typ_sym := g.resolve_receiver_type(node)

	rec_cc_type := g.cc_type(unwrapped_rec_type, false)
	mut receiver_type_name := util.no_dots(rec_cc_type)
	if typ_sym.info is ast.Interface && typ_sym.info.defines_method(node.name) {
		// Speaker_name_table[s._interface_idx].speak(s._object)
		$if debug_interface_method_call ? {
			eprintln('>>> interface typ_sym.name: ${typ_sym.name} | receiver_type_name: ${receiver_type_name} | pos: ${node.pos}')
		}

		left_cc_type := g.cc_type(g.table.unaliased_type(left_type), false)
		left_type_name := util.no_dots(left_cc_type)
		g.write('${c_name(left_type_name)}_name_table[')
		if node.left.is_auto_deref_var() && left_type.nr_muls() > 1 {
			g.write('(')
			g.write('*'.repeat(left_type.nr_muls() - 1))
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		dot := g.dot_or_ptr(left_type)
		mname := c_fn_name(node.name)
		g.write('${dot}_typ]._method_${mname}(')
		if node.left.is_auto_deref_var() && left_type.nr_muls() > 1 {
			g.write('(')
			g.write('*'.repeat(left_type.nr_muls() - 1))
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		g.write('${dot}_object')
		is_variadic := node.expected_arg_types.len > 0
			&& node.expected_arg_types.last().has_flag(.variadic)
		if node.args.len > 0 || is_variadic {
			g.write(', ')
			g.call_args(node)
		}
		g.write(')')
		return
	}
	left_sym := g.table.sym(left_type)
	final_left_sym := g.table.final_sym(left_type)
	if left_sym.kind == .array || (final_left_sym.kind == .array
		&& node.name in ['filter', 'map', 'sort', 'sorted', 'contains', 'any', 'all']) {
		if g.gen_array_method_call(node, left_type) {
			return
		}
	} else if left_sym.kind == .array_fixed && node.name == 'wait' {
		g.gen_fixed_array_wait(node)
		return
	}

	if final_left_sym.kind == .map {
		if g.gen_map_method_call(node, left_type, final_left_sym) {
			return
		}
	}

	if left_sym.kind in [.sum_type, .interface_] {
		prefix_name := if left_sym.kind == .sum_type { 'sumtype' } else { 'interface' }
		match node.name {
			'type_name' {
				if left_sym.kind in [.sum_type, .interface_] {
					g.conversion_function_call('charptr_vstring_literal( /* ${left_sym.name} */ v_typeof_${prefix_name}_${typ_sym.cname}',
						')', node)
					return
				}
			}
			'type_idx' {
				if left_sym.kind in [.sum_type, .interface_] {
					g.conversion_function_call('/* ${left_sym.name} */ v_typeof_${prefix_name}_idx_${typ_sym.cname}',
						'', node)
					return
				}
			}
			else {}
		}
	}

	if node.name == 'str' {
		if g.gen_to_str_method_call(node) {
			return
		}
	} else if node.name == 'free' {
		mut rec_type := node.receiver_type
		if rec_type.has_flag(.shared_f) {
			rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
		}
		g.get_free_method(rec_type)
	}
	mut cast_n := 0

	receiver_type_name = g.resolve_receiver_name(node, unwrapped_rec_type, final_left_sym,
		left_sym, typ_sym)
	if final_left_sym.kind == .array && !(left_sym.kind == .alias && left_sym.has_method(node.name))
		&& node.name in ['clear', 'repeat', 'sort_with_compare', 'sorted_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice', 'pointers'] {
		if node.name in ['last', 'first', 'pop'] {
			return_type_str := g.typ(node.return_type)
			cast_n++
			g.write('(*(${return_type_str}*)')
		}
	}
	mut name := util.no_dots('${receiver_type_name}_${node.name}')
	mut array_depth := -1
	mut noscan := ''
	if left_sym.info is ast.Array {
		needs_depth := node.name in ['clone', 'repeat']
		if needs_depth {
			array_depth = g.get_array_depth(left_sym.info.elem_type)
		}
		maybe_noscan := needs_depth
			|| node.name in ['pop', 'push', 'push_many', 'reverse', 'grow_cap', 'grow_len']
		if maybe_noscan {
			info := left_sym.info as ast.Array
			noscan = g.check_noscan(info.elem_type)
		}
	} else if left_sym.kind == .chan && node.name in ['close', 'try_pop', 'try_push'] {
		name = 'sync__Channel_${node.name}'
	} else if final_left_sym.kind == .map && node.name in ['keys', 'values'] {
		name = 'map_${node.name}'
	}
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__')
		&& node.name != 'str' {
		sym := g.table.sym(node.receiver_type)
		key := sym.name + '.' + node.name
		g.write('/* obf method call: ${key} */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "${key}" not found, this should never happen')
		}
	}
	// Check if expression is: arr[a..b].clone(), arr[a..].clone()
	// if so, then instead of calling array_clone(&array_slice(...))
	// call array_clone_static(array_slice(...))
	mut is_range_slice := false
	if node.receiver_type.is_ptr() && !left_type.is_ptr() {
		if node.left is ast.IndexExpr {
			idx := node.left.index
			if idx is ast.RangeExpr {
				// expr is arr[range].clone()
				// use array_clone_static instead of array_clone
				name = util.no_dots('${receiver_type_name}_${node.name}_static')
				is_range_slice = true
			}
		}
	}

	if node.concrete_types.len > 0 {
		mut rec_len := 0
		if node.left_type.has_flag(.generic) {
			rec_sym := g.table.final_sym(g.unwrap_generic(node.left_type))
			match rec_sym.info {
				ast.Struct, ast.Interface, ast.SumType {
					rec_len += rec_sym.info.generic_types.len
				}
				else {}
			}
		}
		mut concrete_types := node.concrete_types.map(g.unwrap_generic(it))
		if m := g.table.find_method(g.table.sym(node.left_type), node.name) {
			mut node_ := unsafe { node }
			comptime_args := g.resolve_comptime_args(m, mut node_, concrete_types)
			for k, v in comptime_args {
				if (rec_len + k) < concrete_types.len {
					if !node.concrete_types[k].has_flag(.generic) {
						concrete_types[rec_len + k] = g.unwrap_generic(v)
					}
				}
			}
			name = g.generic_fn_name(concrete_types, name)
		} else {
			name = g.generic_fn_name(concrete_types, name)
		}
	}
	// TODO2
	// g.generate_tmp_autofree_arg_vars(node, name)
	if !node.receiver_type.is_ptr() && left_type.is_ptr() && node.name == 'str' {
		g.write('ptr_str(')
	} else if node.receiver_type.is_ptr() && left_type.is_ptr() && node.name == 'str'
		&& !left_sym.has_method('str') {
		g.gen_expr_to_string(node.left, left_type)
		return
	} else {
		if left_sym.kind == .array {
			if array_depth >= 0 {
				name = name + '_to_depth'
			}
			g.write('${name}${noscan}(')
		} else {
			g.write('${name}(')
		}
	}
	is_array_method_first_last_repeat := final_left_sym.kind == .array
		&& node.name in ['first', 'last', 'repeat']
	is_interface := left_sym.kind == .interface_
		&& g.table.sym(node.receiver_type).kind == .interface_
	if node.receiver_type.is_ptr() && (!left_type.is_ptr()
		|| node.from_embed_types.len != 0 || (left_type.has_flag(.shared_f) && node.name != 'str')) {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO same logic in call_args()
		if !is_range_slice {
			if !node.left.is_lvalue() {
				g.write('ADDR(${rec_cc_type}, ')
				cast_n++
			} else if !is_array_method_first_last_repeat && !(left_type.has_flag(.shared_f)
				&& left_type == node.receiver_type) {
				g.write('&')
			}
		}
	} else if !node.receiver_type.is_ptr() && left_type.is_ptr() && node.name != 'str'
		&& node.from_embed_types.len == 0 {
		if !left_type.has_flag(.shared_f) {
			g.write('/*rec*/*')
		}
	} else if !is_range_slice && node.from_embed_types.len == 0 && node.name != 'str' {
		diff := left_type.nr_muls() - node.receiver_type.nr_muls()
		if diff < 0 {
			// TODO
			// g.write('&')
		} else if diff > 0 {
			g.write('/*diff=${diff}*/')
			g.write([]u8{len: diff, init: `*`}.bytestr())
		}
	}

	if g.is_autofree && node.free_receiver && !g.inside_lambda && !g.is_builtin_mod {
		// The receiver expression needs to be freed, use the temp var.
		fn_name := node.name.replace('.', '_')
		arg_name := '_arg_expr_${fn_name}_0_${node.pos.pos}'
		g.write('/*af receiver arg*/' + arg_name)
	} else {
		mut is_array := left_sym.kind == .array
		if !is_array && left_sym.kind == .alias {
			unaliased_type := g.table.unaliased_type(left_type)
			unaliased_sym := g.table.sym(unaliased_type)
			is_array = unaliased_sym.kind == .array
		}
		if is_array && node.left.is_auto_deref_var() && is_array_method_first_last_repeat {
			g.write('*')
		}
		if node.left is ast.MapInit {
			g.write('(map[]){')
			g.expr(node.left)
			g.write('}[0]')
		} else if !is_interface && node.from_embed_types.len > 0 {
			n_ptr := node.left_type.nr_muls() - 1
			if n_ptr > 0 {
				g.write('(')
				g.write('*'.repeat(n_ptr))
				g.expr(node.left)
				g.write(')')
			} else {
				g.expr(node.left)
			}
		} else if is_interface && node.from_embed_types.len > 0 {
			if g.out.last_n(1) == '&' {
				g.go_back(1)
			}
			if node.receiver_type.is_ptr() && left_type.is_ptr() {
				// (main__IFoo*)bar
				g.write('(')
				g.write(g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)')
				g.expr(node.left)
			} else if node.receiver_type.is_ptr() && !left_type.is_ptr() {
				// (main__IFoo*)&bar
				g.write('(')
				g.write(g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)&')
				g.expr(node.left)
			} else if !node.receiver_type.is_ptr() && left_type.is_ptr() {
				// *((main__IFoo*)bar)
				g.write('*((')
				g.write(g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)')
				g.expr(node.left)
				g.write(')')
			} else {
				// *((main__IFoo*)&bar)
				g.write('*((')
				g.write(g.table.sym(node.from_embed_types.last()).cname)
				g.write('*)&')
				g.expr(node.left)
				g.write(')')
			}
		} else {
			g.expr(node.left)
		}
		if !is_interface || node.from_embed_types.len == 0 {
			for i, embed in node.from_embed_types {
				embed_sym := g.table.sym(embed)
				embed_name := embed_sym.embed_name()
				is_left_ptr := if i == 0 {
					left_type.is_ptr()
				} else {
					node.from_embed_types[i - 1].is_ptr()
				}
				if is_left_ptr {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(embed_name)
			}
		}
		if left_type.has_flag(.shared_f)
			&& (left_type != node.receiver_type || is_array_method_first_last_repeat) {
			g.write('->val')
		}
	}
	if cast_n > 0 {
		g.write(')'.repeat(cast_n))
	}
	is_variadic := node.expected_arg_types.len > 0
		&& node.expected_arg_types.last().has_flag(.variadic)
	if node.args.len > 0 || is_variadic {
		g.write(', ')
	}
	g.call_args(node)
	if array_depth >= 0 {
		g.write(', ${array_depth}')
	}
	g.write(')')
	if node.return_type != 0 && !node.return_type.has_option_or_result()
		&& g.table.final_sym(node.return_type).kind == .array_fixed {
		// it's non-option fixed array, requires accessing .ret_arr member to get the array
		g.write('.ret_arr')
	}
}

fn (mut g Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	mut is_interface_call := false
	mut is_selector_call := false
	if node.left_type != 0 {
		mut fn_typ := ast.Type(0)
		left_sym := g.table.sym(node.left_type)
		if node.is_field {
			if field := g.table.find_field_with_embeds(left_sym, node.name) {
				fn_typ = field.typ
			}
		}
		if left_sym.kind == .interface_ || fn_typ.is_ptr() {
			is_interface_call = true
			g.write('(*')
		}
		g.expr(node.left)
		if node.left_type.is_ptr() {
			g.write('->')
		} else {
			g.write('.')
		}
		for embed in node.from_embed_types {
			embed_sym := g.table.sym(embed)
			embed_name := embed_sym.embed_name()
			g.write(embed_name)
			if embed.is_ptr() {
				g.write('->')
			} else {
				g.write('.')
			}
		}
		is_selector_call = true
	}
	mut name := node.name
	is_print := name in ['print', 'println', 'eprint', 'eprintln', 'panic']
	print_method := name
	is_json_encode := name == 'json.encode'
	is_json_encode_pretty := name == 'json.encode_pretty'
	is_json_decode := name == 'json.decode'
	is_json_fn := is_json_encode || is_json_encode_pretty || is_json_decode
	mut json_type_str := ''
	mut json_obj := ''
	if is_json_fn {
		g.is_json_fn = true
		json_obj = g.new_tmp_var()
		mut tmp2 := ''
		cur_line := g.go_before_last_stmt()
		if is_json_encode || is_json_encode_pretty {
			g.gen_json_for_type(node.args[0].typ)
			json_type_str = g.typ(node.args[0].typ)
			// `json__encode` => `json__encode_User`
			encode_name := js_enc_name(json_type_str)
			g.empty_line = true
			g.writeln('// json.encode')
			g.write('cJSON* ${json_obj} = ${encode_name}(')
			g.call_args(node)
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			if is_json_encode {
				g.writeln('string ${tmp2} = json__json_print(${json_obj});')
			} else {
				g.writeln('string ${tmp2} = json__json_print_pretty(${json_obj});')
			}
		} else {
			ast_type := node.args[0].expr as ast.TypeNode
			// `json.decode(User, s)` => json.decode_User(s)
			typ := c_name(g.typ(ast_type.typ))
			fn_name := c_fn_name(name) + '_' + typ
			g.gen_json_for_type(ast_type.typ)
			g.empty_line = true
			g.writeln('// json.decode')
			g.write('cJSON* ${json_obj} = json__json_parse(')
			// Skip the first argument in json.decode which is a type
			// its name was already used to generate the function call
			g.is_js_call = true
			g.call_args(node)
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			g.writeln('${result_name}_${typ} ${tmp2} = ${fn_name}(${json_obj});')
		}
		if !g.is_autofree {
			g.write('cJSON_Delete(${json_obj}); // del')
		}
		g.write('\n${cur_line}')
		name = ''
		json_obj = tmp2
	}
	if name == '__addr' {
		name = '&'
	}
	if node.language == .c {
		// Skip "C."
		name = util.no_dots(name[2..])
	} else {
		name = c_fn_name(name)
	}
	if g.pref.translated || g.file.is_translated || node.is_file_translated {
		// For `[c: 'P_TryMove'] fn p_trymove( ... `
		// every time `p_trymove` is called, `P_TryMove` must be generated instead.
		if f := g.table.find_fn(node.name) {
			// TODO PERF fn lookup for each fn call in translated mode
			if cattr := f.attrs.find_first('c') {
				name = cattr.arg
			}
		}
	}
	// Obfuscate only functions in the main module for now
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
		key := node.name
		g.write('/* obf call: ${key} */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "${key}" not found, this should never happen')
		}
	}
	if !is_selector_call {
		if func := g.table.find_fn(node.name) {
			mut concrete_types := node.concrete_types.map(g.unwrap_generic(it))
			mut node_ := unsafe { node }
			comptime_args := g.resolve_comptime_args(func, mut node_, concrete_types)
			if concrete_types.len > 0 {
				for k, v in comptime_args {
					if k < concrete_types.len {
						if !node.concrete_types[k].has_flag(.generic) {
							concrete_types[k] = g.unwrap_generic(v)
						}
					}
				}
				name = g.generic_fn_name(concrete_types, name)
			}
		}
	}
	if node.is_fn_a_const {
		name = g.c_const_name(node.const_name.replace('.', '__'))
	}
	// TODO2
	// cgen shouldn't modify ast nodes, this should be moved
	// g.generate_tmp_autofree_arg_vars(node, name)
	// Handle `print(x)`
	mut print_auto_str := false
	if is_print && (node.args[0].typ != ast.string_type
		|| g.comptime.comptime_for_method.len > 0
		|| g.comptime.is_comptime_var(node.args[0].expr)) {
		g.inside_interface_deref = true
		defer {
			g.inside_interface_deref = false
		}
		mut typ := node.args[0].typ
		if g.comptime.is_comptime_var(node.args[0].expr) {
			ctyp := g.comptime.get_comptime_var_type(node.args[0].expr)
			if ctyp != ast.void_type {
				typ = ctyp
			}
		}
		if typ == 0 {
			g.checker_bug('print arg.typ is 0', node.pos)
		}
		if typ != ast.string_type || g.comptime.comptime_for_method.len > 0 {
			expr := node.args[0].expr
			typ_sym := g.table.sym(typ)
			if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method('str') {
				g.write('${c_fn_name(print_method)}(')
				rec_type_name := util.no_dots(g.cc_type(typ, false))
				g.write('${c_name(rec_type_name)}_name_table[')
				g.expr(expr)
				dot := if typ.is_ptr() { '->' } else { '.' }
				g.write('${dot}_typ]._method_str(')
				g.expr(expr)
				g.write('${dot}_object')
				g.writeln('));')
				return
			}
			if g.is_autofree && !typ.has_flag(.option) && !typ.has_flag(.result) {
				// Create a temporary variable so that the value can be freed
				tmp := g.new_tmp_var()
				g.write('string ${tmp} = ')
				g.gen_expr_to_string(expr, typ)
				g.writeln('; ${c_fn_name(print_method)}(${tmp}); string_free(&${tmp});')
			} else {
				g.write('${c_fn_name(print_method)}(')
				if expr is ast.ComptimeSelector {
					key_str := g.comptime.get_comptime_selector_key_type(expr)
					if key_str != '' {
						typ = g.comptime.type_map[key_str] or { typ }
					}
				} else if expr is ast.ComptimeCall {
					if expr.method_name == 'method' {
						sym := g.table.sym(g.unwrap_generic(expr.left_type))
						if m := sym.find_method(g.comptime.comptime_for_method) {
							typ = m.return_type
						}
					}
				} else if expr is ast.Ident {
					if expr.obj is ast.Var {
						typ = expr.obj.typ
						if expr.obj.smartcasts.len > 0 {
							typ = g.unwrap_generic(expr.obj.smartcasts.last())
							cast_sym := g.table.sym(typ)
							if cast_sym.info is ast.Aggregate {
								typ = cast_sym.info.types[g.aggregate_type_idx]
							} else if expr.obj.ct_type_var == .smartcast {
								typ = g.unwrap_generic(g.comptime.get_comptime_var_type(expr))
							}
						}
						// handling println( var or { ... })
						if typ.has_flag(.option) && expr.or_expr.kind != .absent {
							typ = typ.clear_flag(.option)
						}
					}
				}
				g.gen_expr_to_string(expr, typ)
				g.write(')')
			}
			print_auto_str = true
		}
	}
	if !print_auto_str {
		if is_print {
			g.inside_interface_deref = true
			defer {
				g.inside_interface_deref = false
			}
		}
		if g.pref.is_debug && node.name == 'panic' {
			paline, pafile, pamod, pafn := g.panic_debug_info(node.pos)
			g.write('panic_debug(${paline}, tos3("${pafile}"), tos3("${pamod}"), tos3("${pafn}"),  ')
			g.call_args(node)
			g.write(')')
		} else if node.name.ends_with('__static__from_string') && !g.table.known_fn(node.name) {
			if node.name !in g.str_fn_names {
				g.gen_enum_static_from_string(node.name)
				g.str_fn_names << node.name
			}
			g.write('${util.no_dots(node.name)}(')
			g.call_args(node)
			g.write(')')
		} else {
			// Simple function call
			// if free_tmp_arg_vars {
			// g.writeln(';')
			// g.write(cur_line + ' /* <== af cur line*/')
			// }
			mut is_fn_var := false
			if obj := node.scope.find(node.name) {
				match obj {
					ast.Var {
						// Temp fix generate call fn error when the struct type of sumtype
						// has the fn field and is same to the struct name.
						mut is_cast_needed := true
						if node.left_type != 0 {
							left_sym := g.table.sym(node.left_type)
							if left_sym.kind == .struct_ && node.name == obj.name {
								is_cast_needed = false
							}
						}
						if obj.smartcasts.len > 0 && is_cast_needed {
							for typ in obj.smartcasts {
								sym := g.table.sym(g.unwrap_generic(typ))
								if obj.orig_type.has_flag(.option) && sym.kind == .function {
									g.write('(*(${sym.cname}*)(')
								} else {
									g.write('(*(${sym.cname})(')
								}
							}
							for i, typ in obj.smartcasts {
								cast_sym := g.table.sym(g.unwrap_generic(typ))
								mut is_ptr := false
								if i == 0 {
									if obj.is_inherited {
										g.write(c.closure_ctx + '->' + node.name)
									} else {
										g.write(node.name)
									}
									if obj.orig_type.is_ptr() {
										is_ptr = true
									}
								}
								dot := if is_ptr { '->' } else { '.' }
								if cast_sym.info is ast.Aggregate {
									sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
									g.write('${dot}_${sym.cname}')
								} else if cast_sym.kind == .function
									&& obj.orig_type.has_flag(.option) {
									g.write('.data')
								} else {
									g.write('${dot}_${cast_sym.cname}')
								}
								g.write('))')
							}
							is_fn_var = true
						} else if obj.is_inherited {
							g.write(c.closure_ctx + '->' + node.name)
							is_fn_var = true
						}
					}
					else {}
				}
			}
			if !is_fn_var {
				g.write(g.get_ternary_name(name))
			}
			if is_interface_call {
				g.write(')')
			}
			mut tmp_cnt_save := -1
			if name != '&' {
				g.write('(')
			}
			if is_json_fn {
				g.write(json_obj)
			} else {
				if node.is_keep_alive
					&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt] {
					cur_line := g.go_before_last_stmt()
					tmp_cnt_save = g.keep_alive_call_pregen(node)
					g.write(cur_line)
					for i in 0 .. node.args.len {
						if i > 0 {
							g.write(', ')
						}
						g.write('__tmp_arg_${tmp_cnt_save + i}')
					}
				} else {
					g.call_args(node)
				}
			}
			if name != '&' {
				g.write(')')
			}
			if node.return_type != 0 && !node.return_type.has_option_or_result()
				&& g.table.final_sym(node.return_type).kind == .array_fixed {
				// it's non-option fixed array, requires accessing .ret_arr member to get the array
				g.write('.ret_arr')
			}
			if tmp_cnt_save >= 0 {
				g.writeln(';')
				g.keep_alive_call_postgen(node, tmp_cnt_save)
			}
		}
	}
	g.is_json_fn = false
}

fn (mut g Gen) autofree_call_pregen(node ast.CallExpr) {
	// g.writeln('// autofree_call_pregen()')
	// Create a temporary var before fn call for each argument in order to free it (only if it's a complex expression,
	// like `foo(get_string())` or `foo(a + b)`
	mut free_tmp_arg_vars := g.is_autofree && !g.is_builtin_mod && node.args.len > 0
		&& !node.args[0].typ.has_flag(.option)
		&& !node.args[0].typ.has_flag(.result) // TODO copy pasta checker.v
	if !free_tmp_arg_vars {
		return
	}
	if g.is_js_call {
		return
	}
	if g.inside_const {
		return
	}
	free_tmp_arg_vars = false // set the flag to true only if we have at least one arg to free
	g.tmp_count_af++
	mut scope := g.file.scope.innermost(node.pos.pos)
	// prepend the receiver for now (TODO turn the receiver into a CallArg everywhere?)
	mut args := [
		ast.CallArg{
			typ: node.receiver_type
			expr: node.left
			is_tmp_autofree: node.free_receiver
		},
	]
	args << node.args
	for i, arg in args {
		if !arg.is_tmp_autofree {
			continue
		}
		if arg.expr is ast.CallExpr {
			// Any argument can be an expression that has to be freed. Generate a tmp expression
			// for each of those recursively.
			g.autofree_call_pregen(arg.expr)
		}
		free_tmp_arg_vars = true
		fn_name := node.name.replace('.', '_') // can't use name...
		t := '_arg_expr_${fn_name}_${i}_${node.pos.pos}'
		used := false // scope.known_var(t)
		mut s := '${t} = '
		if used {
			// This means this tmp var name was already used (the same function was called and
			// `_arg_fnname_1` was already generated).
			// We do not need to declare this variable again, so just generate `t = ...`
			// instead of `string t = ...`, and we need to mark this variable as unused,
			// so that it's freed after the call. (Used tmp arg vars are not freed to avoid double frees).
			if mut x := scope.find(t) {
				match mut x {
					ast.Var { x.is_used = false }
					else {}
				}
			}
			s = '${t} = '
		} else {
			scope.register(ast.Var{
				name: t
				typ: ast.string_type
				is_autofree_tmp: true
				pos: node.pos
			})
			s = 'string ${t} = '
		}
		s += g.expr_string(arg.expr)
		s += ';// new af2 pre'
		g.strs_to_free0 << s
		// This tmp arg var will be freed with the rest of the vars at the end of the scope.
	}
}

fn (mut g Gen) autofree_call_postgen(node_pos int) {
	// if g.strs_to_free.len == 0 {
	// 	return
	// }
	// g.writeln('\n/* strs_to_free3: $g.nr_vars_to_free */')
	// if g.nr_vars_to_free <= 0 {
	// 	return
	// }
	/*
	for s in g.strs_to_free {
		g.writeln('string_free(&$s);')
	}
	if !g.inside_or_block {
		// we need to free the vars both inside the or block (in case of an error) and after it
		// if we reset the array here, then the vars will not be freed after the block.
		g.strs_to_free = []
	}
	*/
	if g.inside_vweb_tmpl {
		return
	}
	// g.doing_autofree_tmp = true
	// g.write('/* postgen */')
	mut scope := g.file.scope.innermost(node_pos)
	for _, mut obj in scope.objects {
		match mut obj {
			ast.Var {
				// if var.typ == 0 {
				// // TODO why 0?
				// continue
				// }
				is_option := obj.typ.has_flag(.option)
				if is_option {
					// TODO: free options
					continue
				}
				is_result := obj.typ.has_flag(.result)
				if is_result {
					// TODO: free results
					continue
				}
				if !obj.is_autofree_tmp {
					continue
				}
				if obj.is_used {
					// this means this tmp expr var has already been freed
					continue
				}
				obj.is_used = true // TODO bug? sets all vars is_used to true
				g.autofree_variable(obj)
				// g.nr_vars_to_free--
			}
			else {}
		}
	}
	// g.write('/* postgen end */')
	// g.doing_autofree_tmp = false
}

fn (mut g Gen) call_args(node ast.CallExpr) {
	g.expected_fixed_arr = true
	defer {
		g.expected_fixed_arr = false
	}
	args := if g.is_js_call {
		if node.args.len < 1 {
			g.error('node should have at least 1 arg', node.pos)
		}
		g.is_js_call = false
		node.args[1..]
	} else {
		node.args
	}
	mut expected_types := node.expected_arg_types.map(g.unwrap_generic(it))
	// unwrap generics fn/method arguments to concretes
	if node.concrete_types.len > 0 && node.concrete_types.all(!it.has_flag(.generic)) {
		if node.is_method {
			if func := g.table.find_method(g.table.sym(node.left_type), node.name) {
				if func.generic_names.len > 0 {
					for i in 0 .. expected_types.len {
						mut muttable := unsafe { &ast.Table(g.table) }
						if utyp := muttable.resolve_generic_to_concrete(node.expected_arg_types[i],
							func.generic_names, node.concrete_types)
						{
							expected_types[i] = utyp
						}
					}
				}
			}
		} else {
			if func := g.table.find_fn(node.name) {
				if func.generic_names.len > 0 {
					for i in 0 .. expected_types.len {
						mut muttable := unsafe { &ast.Table(g.table) }
						if utyp := muttable.resolve_generic_to_concrete(node.expected_arg_types[i],
							func.generic_names, node.concrete_types)
						{
							expected_types[i] = utyp
						}
					}
				}
			}
		}
	}
	// only v variadic, C variadic args will be appended like normal args
	is_variadic := expected_types.len > 0 && expected_types.last().has_flag(.variadic)
		&& node.language == .v
	for i, arg in args {
		if is_variadic && i == expected_types.len - 1 {
			break
		}
		if arg.expr is ast.Ident {
			if arg.expr.obj is ast.Var {
				if arg.expr.obj.smartcasts.len > 0 {
					exp_sym := g.table.sym(expected_types[i])
					orig_sym := g.table.sym(arg.expr.obj.orig_type)
					if orig_sym.kind != .interface_ && (exp_sym.kind != .sum_type
						&& expected_types[i] != arg.expr.obj.orig_type) {
						expected_types[i] = g.unwrap_generic(arg.expr.obj.smartcasts.last())
						cast_sym := g.table.sym(expected_types[i])
						if cast_sym.info is ast.Aggregate {
							expected_types[i] = cast_sym.info.types[g.aggregate_type_idx]
						}
					}
				}
			}
		} else if arg.expr is ast.ArrayDecompose {
			mut d_count := 0
			for d_i in i .. expected_types.len {
				g.write('*(${g.typ(expected_types[d_i])}*)array_get(')
				g.expr(arg.expr)
				g.write(', ${d_count})')

				if d_i < expected_types.len - 1 {
					g.write(', ')
				}
				d_count++
			}
			continue
		}
		use_tmp_var_autofree := g.is_autofree && arg.typ == ast.string_type && arg.is_tmp_autofree
			&& !g.inside_const && !g.is_builtin_mod
		// g.write('/* af=$arg.is_tmp_autofree */')
		// some c fn definitions dont have args (cfns.v) or are not updated in checker
		// when these are fixed we wont need this check
		if i < expected_types.len {
			if use_tmp_var_autofree {
				if arg.is_tmp_autofree { // && !g.is_js_call {
					// We saved expressions in temp variables so that they can be freed later.
					// `foo(str + str2) => x := str + str2; foo(x); x.free()`
					// g.write('_arg_expr_${g.called_fn_name}_$i')
					// Use these variables here.
					fn_name := node.name.replace('.', '_')
					// name := '_tt${g.tmp_count_af}_arg_expr_${fn_name}_$i'
					name := '_arg_expr_${fn_name}_${i + 1}_${node.pos.pos}'
					g.write('/*autofree arg*/' + name)
				}
			} else {
				g.ref_or_deref_arg(arg, expected_types[i], node.language)
			}
		} else {
			if use_tmp_var_autofree {
				// TODO copypasta, move to an inline fn
				fn_name := node.name.replace('.', '_')
				name := '_arg_expr_${fn_name}_${i + 1}_${node.pos.pos}'
				g.write('/*af arg2*/' + name)
			} else {
				g.expr(arg.expr)
			}
		}
		if i < args.len - 1 || is_variadic {
			g.write(', ')
		}
	}
	arg_nr := expected_types.len - 1
	if is_variadic {
		varg_type := expected_types.last()
		variadic_count := args.len - arg_nr
		arr_sym := g.table.sym(varg_type)
		mut arr_info := arr_sym.info as ast.Array
		if varg_type.has_flag(.generic) {
			if node.is_method {
				left_sym := g.table.sym(node.left_type)
				if fn_def := left_sym.find_method_with_generic_parent(node.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					if utyp := muttable.resolve_generic_to_concrete(arr_info.elem_type,
						fn_def.generic_names, node.concrete_types)
					{
						arr_info.elem_type = utyp
					}
				} else {
					g.error('unable to find method ${node.name}', node.pos)
				}
			} else {
				if fn_def := g.table.find_fn(node.name) {
					mut muttable := unsafe { &ast.Table(g.table) }
					if utyp := muttable.resolve_generic_to_concrete(arr_info.elem_type,
						fn_def.generic_names, node.concrete_types)
					{
						arr_info.elem_type = utyp
					}
				} else {
					g.error('unable to find function ${node.name}', node.pos)
				}
			}
		}
		elem_type := g.typ(arr_info.elem_type)
		if (g.pref.translated || g.file.is_translated) && args.len == 1 {
			// Handle `foo(c'str')` for `fn foo(args ...&u8)`
			// TODOC2V handle this in a better place
			g.expr(args[0].expr)
		} else if args.len > 0 && args.last().expr is ast.ArrayDecompose {
			g.expr(args.last().expr)
		} else {
			if variadic_count > 0 {
				if g.pref.translated || g.file.is_translated {
					// Handle passing e.g. C string literals to `...` C varargs:
					// void DEH_snprintf(char *buffer, size_t len, const char *fmt, ...)
					// deh_snprintf(buffer, 9, c'STCFN%.3d', j++)
					for j in arg_nr .. args.len {
						g.expr(args[j].expr)
						if j < args.len - 1 {
							g.write(', ')
						}
					}
				} else {
					noscan := g.check_noscan(arr_info.elem_type)
					g.write('new_array_from_c_array${noscan}(${variadic_count}, ${variadic_count}, sizeof(${elem_type}), _MOV((${elem_type}[${variadic_count}]){')
					for j in arg_nr .. args.len {
						g.ref_or_deref_arg(args[j], arr_info.elem_type, node.language)
						if j < args.len - 1 {
							g.write(', ')
						}
					}
					g.write('}))')
				}
			} else {
				g.write('__new_array(0, 0, sizeof(${elem_type}))')
			}
		}
	}
}

// similar to `autofree_call_pregen()` but only to to handle [keep_args_alive] for C functions
fn (mut g Gen) keep_alive_call_pregen(node ast.CallExpr) int {
	g.empty_line = true
	g.writeln('// keep_alive_call_pregen()')
	// reserve the next tmp_vars for arguments
	tmp_cnt_save := g.tmp_count + 1
	g.tmp_count += node.args.len
	for i, arg in node.args {
		// save all arguments in temp vars (not only pointers) to make sure the
		// evaluation order is preserved
		expected_type := node.expected_arg_types[i]
		typ := g.table.sym(expected_type).cname
		g.write('${typ} __tmp_arg_${tmp_cnt_save + i} = ')
		g.ref_or_deref_arg(arg, expected_type, node.language)
		g.writeln(';')
	}
	g.empty_line = false
	return tmp_cnt_save
}

fn (mut g Gen) keep_alive_call_postgen(node ast.CallExpr, tmp_cnt_save int) {
	g.writeln('// keep_alive_call_postgen()')
	for i, expected_type in node.expected_arg_types {
		if expected_type.is_any_kind_of_pointer() {
			g.writeln('GC_reachable_here(__tmp_arg_${tmp_cnt_save + i});')
		}
	}
}

@[inline]
fn (mut g Gen) ref_or_deref_arg(arg ast.CallArg, expected_type ast.Type, lang ast.Language) {
	arg_typ := if arg.expr is ast.ComptimeSelector {
		g.unwrap_generic(g.comptime.get_comptime_var_type(arg.expr))
	} else {
		g.unwrap_generic(arg.typ)
	}
	arg_sym := g.table.sym(arg_typ)
	exp_is_ptr := expected_type.is_any_kind_of_pointer()
	arg_is_ptr := arg_typ.is_any_kind_of_pointer()
	if expected_type == 0 {
		g.checker_bug('ref_or_deref_arg expected_type is 0', arg.pos)
	}
	exp_sym := g.table.sym(expected_type)
	mut needs_closing := false
	if arg.is_mut && !exp_is_ptr {
		g.write('&/*mut*/')
	} else if exp_is_ptr && !arg_is_ptr && !(arg_sym.kind == .alias
		&& g.table.unaliased_type(arg_typ).is_pointer() && expected_type.is_pointer()) {
		if arg.is_mut {
			if exp_sym.kind == .array {
				if (arg.expr is ast.Ident && arg.expr.kind == .variable)
					|| arg.expr is ast.SelectorExpr {
					g.write('&/*arr*/')
					g.expr(arg.expr)
				} else {
					// Special case for mutable arrays. We can't `&` function
					// results,	have to use `(array[]){ expr }[0]` hack.
					g.write('&/*111*/(array[]){')
					g.expr(arg.expr)
					g.write('}[0]')
				}
				return
			} else if arg_sym.kind == .sum_type && exp_sym.kind == .sum_type
				&& arg.expr in [ast.Ident, ast.SelectorExpr] {
				g.write('&/*sum*/')
				g.expr(arg.expr)
				return
			} else if arg_sym.kind == .interface_ && exp_sym.kind == .interface_
				&& arg.expr in [ast.Ident, ast.SelectorExpr] {
				g.write('&/*iface*/')
				g.expr(arg.expr)
				return
			}
		}
		if !g.is_json_fn {
			if arg_typ == 0 {
				g.checker_bug('ref_or_deref_arg arg.typ is 0', arg.pos)
			}
			arg_typ_sym := g.table.sym(arg_typ)
			expected_deref_type := if expected_type.is_ptr() {
				expected_type.deref()
			} else {
				expected_type
			}
			deref_sym := g.table.sym(expected_deref_type)
			if arg_typ_sym.kind != .function && deref_sym.kind !in [.sum_type, .interface_]
				&& lang != .c {
				if arg.expr.is_lvalue() {
					if expected_type.has_flag(.option) {
						g.expr_with_opt(arg.expr, arg_typ, expected_type)
						return
					} else {
						g.write('(voidptr)&/*qq*/')
					}
				} else {
					mut atype := expected_deref_type
					if atype.has_flag(.generic) {
						atype = g.unwrap_generic(atype)
					}
					if atype.has_flag(.generic) || arg.expr is ast.StructInit {
						g.write('(voidptr)&/*qq*/')
					} else {
						needs_closing = true
						if arg_typ_sym.kind in [.sum_type, .interface_] {
							atype = arg_typ
						}
						g.write('ADDR(${g.typ(atype)}/*qq*/, ')
					}
				}
			} else if arg_sym.kind == .sum_type && exp_sym.kind == .sum_type {
				// Automatically passing sum types by reference if the argument expects it,
				// not only the argument is mutable.
				if arg.expr is ast.SelectorExpr {
					g.write('&/*sum*/')
					g.expr(arg.expr)
					return
				} else if arg.expr is ast.CastExpr {
					g.write('ADDR(${g.typ(expected_deref_type)}/*sum*/, ')
					g.expr_with_cast(arg.expr, arg_typ, expected_type)
					g.write(')')
					return
				}
			} else if arg_sym.kind == .interface_ && exp_sym.kind == .interface_ {
				if exp_is_ptr && !arg_is_ptr {
					g.write('&')
				}
			}
		}
	} else if arg_typ.has_flag(.shared_f) && !expected_type.has_flag(.shared_f) {
		if expected_type.is_ptr() {
			g.write('&')
		}
		g.expr(arg.expr)
		g.write('->val')
		return
	} else if expected_type.has_flag(.option) {
		g.expr_with_opt(arg.expr, arg_typ, expected_type)
		return
	} else if arg.expr is ast.ArrayInit {
		if arg.expr.is_fixed {
			if !arg.expr.has_index {
				g.write('(${g.typ(arg.expr.typ)})')
			}
		}
	}
	g.expr_with_cast(arg.expr, arg_typ, expected_type)
	if needs_closing {
		g.write(')')
	}
}

fn (mut g Gen) is_gui_app() bool {
	if g.pref.os == .windows {
		if g.force_main_console {
			return false
		}
		for cf in g.table.cflags {
			if cf.value.to_lower() == 'gdi32' {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) fileis(s string) bool {
	return g.file.path.contains(s)
}

fn (mut g Gen) write_fn_attrs(attrs []ast.Attr) string {
	mut fn_attrs := ''
	for attr in attrs {
		match attr.name {
			'inline' {
				g.write('inline ')
			}
			'noinline' {
				// since these are supported by GCC, clang and MSVC, we can consider them officially supported.
				g.write('__NOINLINE ')
			}
			'weak' {
				if attrs.any(it.name == 'export') {
					// only the exported wrapper should be weak; otherwise x86_64-w64-mingw32-gcc complains
					continue
				}
				// a `[weak]` tag tells the C compiler, that the next declaration will be weak, i.e. when linking,
				// if there is another declaration of a symbol with the same name (a 'strong' one), it should be
				// used instead, *without linker errors about duplicate symbols*.
				g.write('VWEAK ')
			}
			'noreturn' {
				// a `[noreturn]` tag tells the compiler, that a function
				// *DOES NOT RETURN* to its callsites.
				// See: https://en.cppreference.com/w/c/language/_Noreturn
				// Such functions should have no return type. They can be used
				// in places where `panic(err)` or `exit(0)` can be used.
				// panic/1 and exit/0 themselves will also be marked as
				// `[noreturn]` soon.
				// These functions should have busy `for{}` loops injected
				// at their end, when they do not end by calling other fns
				// marked by `[noreturn]`.
				g.write('VNORETURN ')
			}
			'irq_handler' {
				g.write('__IRQHANDLER ')
			}
			'_cold' {
				// GCC/clang attributes
				// prefixed by _ to indicate they're for advanced users only and not really supported by V.
				// source for descriptions: https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#Common-Function-Attributes
				// The cold attribute on functions is used to inform the compiler that the function is unlikely
				// to be executed. The function is optimized for size rather than speed and on many targets it
				// is placed into a special subsection of the text section so all cold functions appear close
				// together, improving code locality of non-cold parts of program.
				g.write('__attribute__((cold)) ')
			}
			'_constructor' {
				// The constructor attribute causes the function to be called automatically before execution
				// enters main ().
				g.write('__attribute__((constructor)) ')
			}
			'_destructor' {
				// The destructor attribute causes the function to be called automatically after main ()
				// completes or exit () is called.
				g.write('__attribute__((destructor)) ')
			}
			'_flatten' {
				// Generally, inlining into a function is limited. For a function marked with this attribute,
				// every call inside this function is inlined, if possible.
				g.write('__attribute__((flatten)) ')
			}
			'_hot' {
				// The hot attribute on a function is used to inform the compiler that the function is a hot
				// spot of the compiled program.
				g.write('__attribute__((hot)) ')
			}
			'_malloc' {
				// This tells the compiler that a function is malloc-like, i.e., that the pointer P returned by
				// the function cannot alias any other pointer valid when the function returns, and moreover no
				// pointers to valid objects occur in any storage addressed by P.
				g.write('__attribute__((malloc)) ')
			}
			'_pure' {
				// Calls to functions whose return value is not affected by changes to the observable state
				// of the program and that have no observable effects on such state other than to return a
				// value may lend themselves to optimizations such as common subexpression elimination.
				// Declaring such functions with the const attribute allows GCC to avoid emitting some calls in
				// repeated invocations of the function with the same argument values.
				g.write('__attribute__((const)) ')
			}
			'_naked' {
				g.write('__attribute__((naked)) ')
			}
			'windows_stdcall' {
				// windows attributes (msvc/mingw)
				// prefixed by windows to indicate they're for advanced users only and not really supported by V.
				fn_attrs += call_convention_attribute('stdcall', g.is_cc_msvc)
			}
			'_fastcall' {
				fn_attrs += call_convention_attribute('fastcall', g.is_cc_msvc)
			}
			'callconv' {
				fn_attrs += call_convention_attribute(attr.arg, g.is_cc_msvc)
			}
			'console' {
				g.force_main_console = true
			}
			else {
				// nothing but keep V happy
			}
		}
	}
	return fn_attrs
}

fn call_convention_attribute(cconvention string, is_cc_msvc bool) string {
	return if is_cc_msvc { '__${cconvention} ' } else { '__attribute__((${cconvention})) ' }
}

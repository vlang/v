// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import strings
import v.ast
import v.util

fn (mut g Gen) is_used_by_main(node ast.FnDecl) bool {
	mut is_used_by_main := true
	if g.pref.skip_unused {
		fkey := node.fkey()
		is_used_by_main = g.table.used_fns[fkey]
		$if trace_skip_unused_fns ? {
			println('> is_used_by_main: $is_used_by_main | node.name: $node.name | fkey: $fkey | node.is_method: $node.is_method')
		}
		if !is_used_by_main {
			$if trace_skip_unused_fns_in_c_code ? {
				g.writeln('// trace_skip_unused_fns_in_c_code, $node.name, fkey: $fkey')
			}
		}
	} else {
		$if trace_skip_unused_fns_in_c_code ? {
			g.writeln('// trace_skip_unused_fns_in_c_code, $node.name, fkey: $node.fkey()')
		}
	}
	return is_used_by_main
}

fn (mut g Gen) fn_decl(node ast.FnDecl) {
	if node.should_be_skipped {
		return
	}
	if node.ninstances == 0 && node.generic_names.len > 0 {
		$if trace_generics ? {
			eprintln('skipping generic fn with no concrete instances: $node.mod $node.name')
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
	g.gen_attrs(node.attrs)
	mut skip := false
	pos := g.out.len
	should_bundle_module := util.should_bundle_module(node.mod)
	/*
	if node.name.contains('i_error') {
		println(g.table.type_str(node.params[0].typ))
	}
	*/
	if g.pref.build_mode == .build_module {
		// if node.name.contains('parse_text') {
		// println('!!! $node.name mod=$node.mod, built=$g.module_built')
		// }
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
			println('build module `$g.module_built` fn `$node.name`')
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
		g.out.go_back_to(pos)
	}
	if !g.pref.skip_unused {
		if node.language != .c {
			g.writeln('')
		}
	}
}

fn (mut g Gen) gen_fn_decl(node &ast.FnDecl, skip bool) {
	// TODO For some reason, build fails with autofree with this line
	// as it's only informative, comment it for now
	// g.gen_attrs(it.attrs)
	if node.language == .c {
		// || node.no_body {
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
	//
	old_g_autofree := g.is_autofree
	if node.is_manualfree {
		g.is_autofree = false
	}
	defer {
		g.is_autofree = old_g_autofree
	}
	//
	// if g.fileis('vweb.v') {
	// println('\ngen_fn_decl() $node.name $node.is_generic $g.cur_generic_type')
	// }
	if node.generic_names.len > 0 && g.cur_concrete_types.len == 0 { // need the cur_concrete_type check to avoid inf. recursion
		// loop thru each generic type and generate a function
		nkey := node.fkey()
		generic_types_by_fn := g.table.fn_generic_types[nkey]
		$if trace_post_process_generic_fns ? {
			eprintln('>> gen_fn_decl, nkey: $nkey | generic_types_by_fn: $generic_types_by_fn')
		}
		for concrete_types in generic_types_by_fn {
			if g.pref.is_verbose {
				syms := concrete_types.map(g.table.sym(it))
				the_type := syms.map(it.name).join(', ')
				println('gen fn `$node.name` for type `$the_type`')
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
		cur_closure_ctx = closure_ctx_struct(node)
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
		eprintln('INFO: compile with `v -live $g.pref.path `, if you want to use the [live] function $node.name .')
	}
	//
	mut name := g.c_fn_name(node) or { return }
	mut type_name := g.typ(g.unwrap_generic(node.return_type))

	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') && !node.is_main
		&& node.name != 'str' {
		mut key := node.name
		if node.is_method {
			sym := g.table.sym(node.receiver.typ)
			key = sym.name + '.' + node.name
		}
		g.writeln('/* obf: $key */')
		name = g.obf_table[key] or {
			panic('cgen: fn_decl: obf name "$key" not found, this should never happen')
		}
	}
	// if g.pref.show_cc && it.is_builtin {
	// println(name)
	// }
	// type_name := g.ast.Type_to_str(it.return_type)
	// Live functions are protected by a mutex, because otherwise they
	// can be changed by the live reload thread, *while* they are
	// running, with unpredictable results (usually just crashing).
	// For this purpose, the actual body of the live function,
	// is put under a non publicly accessible function, that is prefixed
	// with 'impl_live_' .
	if is_livemain {
		g.hotcode_fn_names << name
	}
	mut impl_fn_name := name
	if is_live_wrap {
		impl_fn_name = 'impl_live_$name'
	}
	last_fn_c_name_save := g.last_fn_c_name
	defer {
		g.last_fn_c_name = last_fn_c_name_save
	}
	g.last_fn_c_name = impl_fn_name
	//
	if is_live_wrap {
		if is_livemain {
			g.definitions.write_string('$type_name (* $impl_fn_name)(')
			g.write('$type_name no_impl_${name}(')
		}
		if is_liveshared {
			g.definitions.write_string('$type_name ${impl_fn_name}(')
			g.write('$type_name ${impl_fn_name}(')
		}
	} else {
		if !(node.is_pub || g.pref.is_debug) {
			// Private functions need to marked as static so that they are not exportable in the
			// binaries
			if g.pref.build_mode != .build_module && !g.pref.use_cache {
				// if !(g.pref.build_mode == .build_module && g.is_builtin_mod) {
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
		fn_header := '$visibility_kw$type_name $fn_attrs${name}('
		g.definitions.write_string(fn_header)
		g.write(fn_header)
	}
	arg_start_pos := g.out.len
	fargs, fargtypes, heap_promoted := g.fn_decl_params(node.params, node.scope, node.is_variadic)
	if is_closure {
		mut s := '$cur_closure_ctx *$c.closure_ctx'
		if node.params.len > 0 {
			s = ', ' + s
		} else {
			// remove generated `void`
			g.out.cut_to(arg_start_pos)
		}
		g.definitions.write_string(s)
		g.write(s)
		g.nr_closures++
		if g.pref.os == .windows {
			g.error('closures are not yet implemented on windows', node.pos)
		}
	}
	arg_str := g.out.after(arg_start_pos)
	if node.no_body || ((g.pref.use_cache && g.pref.build_mode != .build_module) && node.is_builtin
		&& !g.pref.is_test) || skip {
		// Just a function header. Builtin function bodies are defined in builtin.o
		g.definitions.writeln(');') // // NO BODY')
		g.writeln(');')
		return
	}
	if node.params.len == 0 {
		g.definitions.write_string('void')
	}
	g.definitions.writeln(');')
	g.writeln(') {')
	for i, is_promoted in heap_promoted {
		if is_promoted {
			g.writeln('${fargtypes[i]}* ${fargs[i]} = HEAP(${fargtypes[i]}, _v_toheap_${fargs[i]});')
		}
	}
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
						g.writeln('${g.typ(info.typ)}$deref $var.name;')
					}
				}
			}
		}
	}
	if is_live_wrap {
		// The live function just calls its implementation dual, while ensuring
		// that the call is wrapped by the mutex lock & unlock calls.
		// Adding the mutex lock/unlock inside the body of the implementation
		// function is not reliable, because the implementation function can do
		// an early exit, which will leave the mutex locked.
		mut fn_args_list := []string{}
		for ia, fa in fargs {
			fn_args_list << '${fargtypes[ia]} $fa'
		}
		mut live_fncall := '${impl_fn_name}(' + fargs.join(', ') + ');'
		mut live_fnreturn := ''
		if type_name != 'void' {
			live_fncall = '$type_name res = $live_fncall'
			live_fnreturn = 'return res;'
		}
		g.definitions.writeln('$type_name ${name}(' + fn_args_list.join(', ') + ');')
		g.hotcode_definitions.writeln('$type_name ${name}(' + fn_args_list.join(', ') + '){')
		g.hotcode_definitions.writeln('  pthread_mutex_lock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  $live_fncall')
		g.hotcode_definitions.writeln('  pthread_mutex_unlock(&live_fn_mutex);')
		g.hotcode_definitions.writeln('  $live_fnreturn')
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
	g.stmts(node.stmts)
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
			// if node.return_type.idx() == 1 && node.return_type.has_flag(.optional) {
			// 	// The default return for anonymous functions that return `?,
			// 	// should have .ok = true set, otherwise calling them with
			// 	// optfn() or { panic(err) } will cause a panic:
			// 	g.writeln('\treturn (Option_void){0};')
			// } else {
			g.writeln('\treturn ($type_name)$default_expr;')
			// }
		} else {
			g.writeln('\treturn $default_expr;')
		}
	}
	g.writeln('}')
	if g.pref.printfn_list.len > 0 && g.last_fn_c_name in g.pref.printfn_list {
		println(g.out.after(fn_start_pos))
	}
	for attr in node.attrs {
		if attr.name == 'export' {
			g.writeln('// export alias: $attr.arg -> $name')
			export_alias := '$type_name $fn_attrs${attr.arg}($arg_str)'
			g.definitions.writeln('VV_EXPORTED_SYMBOL $export_alias; // exported fn $node.name')
			g.writeln('$export_alias {')
			g.write('\treturn ${name}(')
			g.write(fargs.join(', '))
			g.writeln(');')
			g.writeln('}')
		}
	}
}

fn (mut g Gen) c_fn_name(node &ast.FnDecl) ?string {
	mut name := node.name
	if name in ['+', '-', '*', '/', '%', '<', '=='] {
		name = util.replace_op(name)
	}
	if node.is_method {
		unwrapped_rec_sym := g.table.sym(g.unwrap_generic(node.receiver.typ))
		if unwrapped_rec_sym.kind == .placeholder {
			return none
		}
		name = g.cc_type(node.receiver.typ, false) + '_' + name
		// name = g.table.sym(node.receiver.typ).name + '_' + name
	}
	if node.language == .c {
		name = util.no_dots(name)
	} else {
		name = c_name(name)
	}

	if node.generic_names.len > 0 {
		name = g.generic_fn_name(g.cur_concrete_types, name, true)
	}

	if (g.pref.translated || g.file.is_translated) && node.attrs.contains('c') {
		// This fixes unknown symbols errors when building separate .c => .v files
		// into .o files
		//
		// example:
		// [c: 'P_TryMove']
		// fn p_trymove(thing &Mobj_t, x int, y int) bool
		//
		// =>
		//
		// bool P_TryMove(main__Mobj_t* thing, int x, int y);
		//
		// In fn_call every time `p_trymove` is called, `P_TryMove` will be generated instead.
		name = node.attrs[0].arg
	}
	return name
}

const closure_ctx = '_V_closure_ctx'

fn closure_ctx_struct(node ast.FnDecl) string {
	return 'struct _V_${node.name}_Ctx'
}

fn (mut g Gen) gen_anon_fn(mut node ast.AnonFn) {
	g.gen_anon_fn_decl(mut node)
	if !node.decl.scope.has_inherited_vars() {
		g.write(node.decl.name)
		return
	}
	// it may be possible to optimize `memdup` out if the closure never leaves current scope
	ctx_struct := closure_ctx_struct(node.decl)
	// TODO in case of an assignment, this should only call "__closure_set_data" and "__closure_set_function" (and free the former data)

	mut size_sb := strings.new_builder(node.decl.params.len * 50)
	for param in node.decl.params {
		size_sb.write_string('_REG_WIDTH_BOUNDED(${g.typ(param.typ)}) + ')
	}
	if g.pref.arch == .amd64 && node.decl.return_type != ast.void_type {
		size_sb.write_string('(_REG_WIDTH(${g.typ(node.decl.return_type)}) > 2) + ')
	}
	size_sb.write_string('1')
	args_size := size_sb.str()
	g.writeln('')

	// ensure that nargs maps to a known entry in the __closure_thunk array
	// TODO make it a compile-time error (you can't call `sizeof()` inside preprocessor `#if`s)
	// NB: this won't be necessary when (if) we have functions that return the machine code for
	// an arbitrary number of arguments
	g.write('__closure_create($node.decl.name, __closure_check_nargs($args_size), ($ctx_struct*) memdup(&($ctx_struct){')
	g.indent++
	for var in node.inherited_vars {
		g.writeln('.$var.name = $var.name,')
	}
	g.indent--
	g.write('}, sizeof($ctx_struct)))')
	g.empty_line = false
}

fn (mut g Gen) gen_anon_fn_decl(mut node ast.AnonFn) {
	if node.has_gen {
		return
	}
	node.has_gen = true
	mut builder := strings.new_builder(256)
	if node.inherited_vars.len > 0 {
		ctx_struct := closure_ctx_struct(node.decl)
		builder.writeln('$ctx_struct {')
		for var in node.inherited_vars {
			styp := g.typ(var.typ)
			builder.writeln('\t$styp $var.name;')
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
	return '${g.last_fn_c_name}_defer_$stmt.idx_in_fn'
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
	mut fargs := []string{}
	mut fargtypes := []string{}
	mut heap_promoted := []bool{}
	if params.len == 0 {
		// in C, `()` is untyped, unlike `(void)`
		g.write('void')
	}
	for i, arg in params {
		mut caname := if arg.name == '_' { g.new_tmp_declaration_name() } else { c_name(arg.name) }
		typ := g.unwrap_generic(arg.typ)
		arg_type_sym := g.table.sym(typ)
		mut arg_type_name := g.typ(typ) // util.no_dots(arg_type_sym.name)
		if arg_type_sym.kind == .function {
			info := arg_type_sym.info as ast.FnType
			func := info.func
			g.write('${g.typ(func.return_type)} (*$caname)(')
			g.definitions.write_string('${g.typ(func.return_type)} (*$caname)(')
			g.fn_decl_params(func.params, voidptr(0), func.is_variadic)
			g.write(')')
			g.definitions.write_string(')')
			fargs << caname
			fargtypes << arg_type_name
		} else {
			mut heap_prom := false
			if scope != voidptr(0) {
				if arg.name != '_' {
					if v := scope.find_var(arg.name) {
						if !v.is_stack_obj && v.is_auto_heap {
							heap_prom = true
						}
					}
				}
			}
			var_name_prefix := if heap_prom { '_v_toheap_' } else { '' }
			const_prefix := if arg.typ.is_any_kind_of_pointer() && !arg.is_mut
				&& arg.name.starts_with('const_') {
				'const '
			} else {
				''
			}
			s := '$const_prefix$arg_type_name $var_name_prefix$caname'
			g.write(s)
			g.definitions.write_string(s)
			fargs << caname
			fargtypes << arg_type_name
			heap_promoted << heap_prom
		}
		if i < params.len - 1 {
			g.write(', ')
			g.definitions.write_string(', ')
		}
	}
	if g.pref.translated && is_variadic {
		g.write(', ...')
		g.definitions.write_string(', ...')
	}
	return fargs, fargtypes, heap_promoted
}

fn (mut g Gen) call_expr(node ast.CallExpr) {
	// g.write('/*call expr*/')
	// NOTE: everything could be done this way
	// see my comment in parser near anon_fn
	if node.left is ast.AnonFn {
		g.expr(node.left)
	}
	if node.left is ast.IndexExpr && node.name == '' {
		g.is_fn_index_call = true
		g.expr(node.left)
		g.is_fn_index_call = false
	}
	if node.should_be_skipped {
		return
	}
	g.inside_call = true
	defer {
		g.inside_call = false
	}
	gen_keep_alive := node.is_keep_alive && node.return_type != ast.void_type
		&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt]
	gen_or := node.or_block.kind != .absent // && !g.is_autofree
	is_gen_or_and_assign_rhs := gen_or && !g.discard_or_result
	mut cur_line := if is_gen_or_and_assign_rhs || gen_keep_alive { // && !g.is_autofree {
		// `x := foo() or { ...}`
		// cut everything that has been generated to prepend optional variable creation
		line := g.go_before_stmt(0)
		g.out.write_string(util.tabs(g.indent))
		// g.write('/*is_gen_or_and_assign_rhs*/')
		line
	} else {
		''
	}
	tmp_opt := if gen_or || gen_keep_alive { g.new_tmp_var() } else { '' }
	if gen_or || gen_keep_alive {
		mut ret_typ := node.return_type
		if gen_or {
			ret_typ = ret_typ.set_flag(.optional)
		}
		styp := g.typ(ret_typ)
		if gen_or && !is_gen_or_and_assign_rhs {
			cur_line = g.go_before_stmt(0)
		}
		g.write('$styp $tmp_opt = ')
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
	if gen_or { // && !g.autofree {
		// if !g.is_autofree {
		g.or_block(tmp_opt, node.or_block, node.return_type)
		//}
		unwrapped_typ := node.return_type.clear_flag(.optional)
		unwrapped_styp := g.typ(unwrapped_typ)
		if unwrapped_typ == ast.void_type {
			g.write('\n $cur_line')
		} else if g.table.sym(node.return_type).kind == .multi_return {
			g.write('\n $cur_line $tmp_opt /*U*/')
		} else {
			if !g.inside_const {
				g.write('\n $cur_line (*($unwrapped_styp*)${tmp_opt}.data)')
			} else {
				g.write('\n $cur_line $tmp_opt')
			}
		}
	} else if gen_keep_alive {
		if node.return_type == ast.void_type {
			g.write('\n $cur_line')
		} else {
			g.write('\n $cur_line $tmp_opt')
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
	g.write(')${dot}_typ )$postfix')
}

fn (mut g Gen) method_call(node ast.CallExpr) {
	// TODO: there are still due to unchecked exprs (opt/some fn arg)
	if node.left_type == 0 {
		g.checker_bug('CallExpr.left_type is 0 in method_call', node.pos)
	}
	if node.receiver_type == 0 {
		g.checker_bug('CallExpr.receiver_type is 0 in method_call', node.pos)
	}
	mut unwrapped_rec_type := node.receiver_type
	if g.cur_fn != 0 && g.cur_fn.generic_names.len > 0 { // in generic fn
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
	mut typ_sym := g.table.sym(unwrapped_rec_type)
	// alias type that undefined this method (not include `str`) need to use parent type
	if typ_sym.kind == .alias && node.name != 'str' && !typ_sym.has_method(node.name) {
		unwrapped_rec_type = (typ_sym.info as ast.Alias).parent_type
		typ_sym = g.table.sym(unwrapped_rec_type)
	}
	rec_cc_type := g.cc_type(unwrapped_rec_type, false)
	mut receiver_type_name := util.no_dots(rec_cc_type)
	if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method(node.name) {
		// Speaker_name_table[s._interface_idx].speak(s._object)
		$if debug_interface_method_call ? {
			eprintln('>>> interface typ_sym.name: $typ_sym.name | receiver_type_name: $receiver_type_name | pos: $node.pos')
		}

		left_is_shared := node.left_type.has_flag(.shared_f)
		left_cc_type := g.cc_type(node.left_type, false)
		left_type_name := util.no_dots(left_cc_type)
		g.write('${c_name(left_type_name)}_name_table[')
		g.expr(node.left)
		dot := if left_is_shared {
			'->val.'
		} else if node.left_type.is_ptr() {
			'->'
		} else {
			'.'
		}
		mname := c_name(node.name)
		g.write('${dot}_typ]._method_${mname}(')
		g.expr(node.left)
		g.write('${dot}_object')
		if node.args.len > 0 {
			g.write(', ')
			g.call_args(node)
		}
		g.write(')')
		return
	}
	left_sym := g.table.sym(node.left_type)
	final_left_sym := g.table.final_sym(node.left_type)
	if left_sym.kind == .array {
		match node.name {
			'filter' {
				g.gen_array_filter(node)
				return
			}
			'sort' {
				g.gen_array_sort(node)
				return
			}
			'insert' {
				g.gen_array_insert(node)
				return
			}
			'map' {
				g.gen_array_map(node)
				return
			}
			'prepend' {
				g.gen_array_prepend(node)
				return
			}
			'contains' {
				g.gen_array_contains(node.left_type, node.left, node.args[0].expr)
				return
			}
			'index' {
				g.gen_array_index(node)
				return
			}
			'wait' {
				g.gen_array_wait(node)
				return
			}
			'any' {
				g.gen_array_any(node)
				return
			}
			'all' {
				g.gen_array_all(node)
				return
			}
			else {}
		}
	}

	if left_sym.kind == .map && node.name == 'delete' {
		left_info := left_sym.info as ast.Map
		elem_type_str := g.typ(left_info.key_type)
		g.write('map_delete(')
		if node.left_type.is_ptr() {
			g.expr(node.left)
		} else {
			g.write('&')
			g.expr(node.left)
		}
		g.write(', &($elem_type_str[]){')
		g.expr(node.args[0].expr)
		g.write('})')
		return
	} else if left_sym.kind == .array && node.name == 'delete' {
		g.write('array_delete(')
		if node.left_type.is_ptr() {
			g.expr(node.left)
		} else {
			g.write('&')
			g.expr(node.left)
		}
		g.write(', ')
		g.expr(node.args[0].expr)
		g.write(')')
		return
	}

	if left_sym.kind in [.sum_type, .interface_] {
		if node.name == 'type_name' {
			if left_sym.kind == .sum_type {
				g.conversion_function_call('charptr_vstring_literal( /* $left_sym.name */ v_typeof_sumtype_$typ_sym.cname',
					')', node)
				return
			}
			if left_sym.kind == .interface_ {
				g.conversion_function_call('charptr_vstring_literal( /* $left_sym.name */ v_typeof_interface_$typ_sym.cname',
					')', node)
				return
			}
		}
		if node.name == 'type_idx' {
			if left_sym.kind == .sum_type {
				g.conversion_function_call('/* $left_sym.name */ v_typeof_sumtype_idx_$typ_sym.cname',
					'', node)
				return
			}
			if left_sym.kind == .interface_ {
				g.conversion_function_call('/* $left_sym.name */ v_typeof_interface_idx_$typ_sym.cname',
					'', node)
				return
			}
		}
	}

	if node.name == 'str' {
		mut rec_type := node.receiver_type
		if rec_type.has_flag(.shared_f) {
			rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
		}
		if node.left is ast.ComptimeSelector {
			if node.left.field_expr is ast.SelectorExpr {
				if node.left.field_expr.expr is ast.Ident {
					key_str := '${node.left.field_expr.expr.name}.typ'
					rec_type = g.comptime_var_type_map[key_str] or { rec_type }
					g.gen_expr_to_string(node.left, rec_type)
					return
				}
			}
		} else if node.left is ast.ComptimeCall {
			if node.left.method_name == 'method' {
				sym := g.table.sym(g.unwrap_generic(node.left.left_type))
				if m := sym.find_method(g.comptime_for_method) {
					rec_type = m.return_type
					g.gen_expr_to_string(node.left, rec_type)
					return
				}
			}
		} else if node.left is ast.Ident {
			if node.left.obj is ast.Var {
				if g.comptime_var_type_map.len > 0 {
					rec_type = node.left.obj.typ
					g.gen_expr_to_string(node.left, rec_type)
					return
				} else if node.left.obj.smartcasts.len > 0 {
					rec_type = node.left.obj.smartcasts.last()
					cast_sym := g.table.sym(rec_type)
					if cast_sym.info is ast.Aggregate {
						rec_type = cast_sym.info.types[g.aggregate_type_idx]
					}
					g.gen_expr_to_string(node.left, rec_type)
					return
				}
			}
		}
		g.get_str_fn(rec_type)
	} else if node.name == 'free' {
		mut rec_type := node.receiver_type
		if rec_type.has_flag(.shared_f) {
			rec_type = rec_type.clear_flag(.shared_f).set_nr_muls(0)
		}
		g.get_free_method(rec_type)
	}
	mut has_cast := false
	if left_sym.kind == .map && node.name in ['clone', 'move'] {
		receiver_type_name = 'map'
	}
	if final_left_sym.kind == .array
		&& node.name in ['repeat', 'sort_with_compare', 'free', 'push_many', 'trim', 'first', 'last', 'pop', 'clone', 'reverse', 'slice', 'pointers'] {
		if !(left_sym.info is ast.Alias && typ_sym.has_method(node.name)) {
			// `array_Xyz_clone` => `array_clone`
			receiver_type_name = 'array'
		}
		if node.name in ['last', 'first', 'pop'] {
			return_type_str := g.typ(node.return_type)
			has_cast = true
			g.write('(*($return_type_str*)')
		}
	}
	mut name := util.no_dots('${receiver_type_name}_$node.name')
	mut array_depth := -1
	mut noscan := ''
	if left_sym.kind == .array {
		needs_depth := node.name in ['clone', 'repeat']
		if needs_depth {
			elem_type := (left_sym.info as ast.Array).elem_type
			array_depth = g.get_array_depth(elem_type)
		}
		maybe_noscan := needs_depth
			|| node.name in ['pop', 'push', 'push_many', 'reverse', 'grow_cap', 'grow_len']
		if maybe_noscan {
			info := left_sym.info as ast.Array
			noscan = g.check_noscan(info.elem_type)
		}
	} else if left_sym.kind == .chan {
		if node.name in ['close', 'try_pop', 'try_push'] {
			name = 'sync__Channel_$node.name'
		}
	} else if final_left_sym.kind == .map {
		if node.name == 'keys' {
			name = 'map_keys'
		}
	}
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__')
		&& node.name != 'str' {
		sym := g.table.sym(node.receiver_type)
		key := sym.name + '.' + node.name
		g.write('/* obf method call: $key */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "$key" not found, this should never happen')
		}
	}
	// Check if expression is: arr[a..b].clone(), arr[a..].clone()
	// if so, then instead of calling array_clone(&array_slice(...))
	// call array_clone_static(array_slice(...))
	mut is_range_slice := false
	if node.receiver_type.is_ptr() && !node.left_type.is_ptr() {
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
	name = g.generic_fn_name(node.concrete_types, name, false)
	// TODO2
	// g.generate_tmp_autofree_arg_vars(node, name)
	//
	// if node.receiver_type != 0 {
	// g.write('/*${g.typ(node.receiver_type)}*/')
	// g.write('/*expr_type=${g.typ(node.left_type)} rec type=${g.typ(node.receiver_type)}*/')
	// }
	if !node.receiver_type.is_ptr() && node.left_type.is_ptr() && node.name == 'str' {
		g.write('ptr_str(')
	} else if node.receiver_type.is_ptr() && node.left_type.is_ptr() && node.name == 'str'
		&& !left_sym.has_method('str') {
		g.gen_expr_to_string(node.left, node.left_type)
		return
	} else {
		if left_sym.kind == .array {
			if array_depth >= 0 {
				name = name + '_to_depth'
			}
			g.write('$name${noscan}(')
		} else {
			g.write('${name}(')
		}
	}
	if node.receiver_type.is_ptr()
		&& (!node.left_type.is_ptr() || node.left_type.has_flag(.variadic)
		|| node.from_embed_types.len != 0
		|| (node.left_type.has_flag(.shared_f) && node.name != 'str')) {
		// The receiver is a reference, but the caller provided a value
		// Add `&` automatically.
		// TODO same logic in call_args()
		if !is_range_slice {
			if !node.left.is_lvalue() {
				g.write('ADDR($rec_cc_type, ')
				has_cast = true
			} else {
				g.write('&')
			}
		}
	} else if !node.receiver_type.is_ptr() && node.left_type.is_ptr() && node.name != 'str'
		&& node.from_embed_types.len == 0 {
		if !node.left_type.has_flag(.shared_f) {
			g.write('/*rec*/*')
		}
	} else if !is_range_slice && node.from_embed_types.len == 0 && node.name != 'str' {
		diff := node.left_type.nr_muls() - node.receiver_type.nr_muls()
		if diff < 0 {
			// TODO
			// g.write('&')
		} else if diff > 0 {
			g.write('/*diff=$diff*/')
			g.write([]byte{len: diff, init: `*`}.bytestr())
		}
	}

	// if node.left_type.idx() != node.receiver_type.idx() {
	// 	println('${g.typ(node.left_type)} ${g.typ(node.receiver_type)}')
	// }

	if g.is_autofree && node.free_receiver && !g.inside_lambda && !g.is_builtin_mod {
		// The receiver expression needs to be freed, use the temp var.
		fn_name := node.name.replace('.', '_')
		arg_name := '_arg_expr_${fn_name}_0_$node.pos.pos'
		g.write('/*af receiver arg*/' + arg_name)
	} else {
		if left_sym.kind == .array && node.left.is_auto_deref_var()
			&& node.name in ['first', 'last', 'repeat'] {
			g.write('*')
		}
		if node.left is ast.MapInit {
			g.write('(map[]){')
			g.expr(node.left)
			g.write('}[0]')
		} else {
			g.expr(node.left)
		}
		for i, embed in node.from_embed_types {
			embed_sym := g.table.sym(embed)
			embed_name := embed_sym.embed_name()
			is_left_ptr := if i == 0 {
				node.left_type.is_ptr()
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
		if node.left_type.has_flag(.shared_f) {
			g.write('->val')
		}
	}
	if has_cast {
		g.write(')')
	}
	is_variadic := node.expected_arg_types.len > 0
		&& node.expected_arg_types[node.expected_arg_types.len - 1].has_flag(.variadic)
	if node.args.len > 0 || is_variadic {
		g.write(', ')
	}
	// /////////
	/*
	if name.contains('subkeys') {
	println('call_args $name $node.arg_types.len')
	for t in node.arg_types {
		sym := g.table.sym(t)
		print('$sym.name ')
	}
	println('')
}
	*/
	// ///////
	g.call_args(node)
	if array_depth >= 0 {
		g.write(', $array_depth')
	}
	g.write(')')
}

fn (mut g Gen) fn_call(node ast.CallExpr) {
	// call struct field with fn type
	// TODO: test node.left instead
	// left & left_type will be `x` and `x type` in `x.fieldfn()`
	// will be `0` for `foo()`
	mut is_interface_call := false
	mut is_selector_call := false
	if node.left_type != 0 {
		left_sym := g.table.sym(node.left_type)
		if left_sym.kind == .interface_ {
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
	if g.inside_comptime_for_field {
		mut node_ := unsafe { node }
		for i, mut call_arg in node_.args {
			if mut call_arg.expr is ast.Ident {
				if mut call_arg.expr.obj is ast.Var {
					node_.args[i].typ = call_arg.expr.obj.typ
				}
			}
		}
	}
	mut name := node.name
	is_print := name in ['print', 'println', 'eprint', 'eprintln', 'panic']
	print_method := name
	is_json_encode := name == 'json.encode'
	is_json_encode_pretty := name == 'json.encode_pretty'
	is_json_decode := name == 'json.decode'
	g.is_json_fn = is_json_encode || is_json_encode_pretty || is_json_decode
	mut json_type_str := ''
	mut json_obj := ''
	if g.is_json_fn {
		json_obj = g.new_tmp_var()
		mut tmp2 := ''
		cur_line := g.go_before_stmt(0)
		if is_json_encode || is_json_encode_pretty {
			g.gen_json_for_type(node.args[0].typ)
			json_type_str = g.typ(node.args[0].typ)
			// `json__encode` => `json__encode_User`
			// encode_name := c_name(name) + '_' + util.no_dots(json_type_str)
			encode_name := js_enc_name(json_type_str)
			g.writeln('// json.encode')
			g.write('cJSON* $json_obj = ${encode_name}(')
			if node.args[0].typ.is_ptr() {
				g.write('*')
			}
			g.call_args(node)
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			if is_json_encode {
				g.writeln('string $tmp2 = json__json_print($json_obj);')
			} else {
				g.writeln('string $tmp2 = json__json_print_pretty($json_obj);')
			}
		} else {
			ast_type := node.args[0].expr as ast.TypeNode
			// `json.decode(User, s)` => json.decode_User(s)
			typ := c_name(g.typ(ast_type.typ))
			fn_name := c_name(name) + '_' + typ
			g.gen_json_for_type(ast_type.typ)
			g.writeln('// json.decode')
			g.write('cJSON* $json_obj = json__json_parse(')
			// Skip the first argument in json.decode which is a type
			// its name was already used to generate the function call
			g.is_js_call = true
			g.call_args(node)
			g.is_js_call = false
			g.writeln(');')
			tmp2 = g.new_tmp_var()
			g.writeln('Option_$typ $tmp2 = $fn_name ($json_obj);')
		}
		if !g.is_autofree {
			g.write('cJSON_Delete($json_obj); //del')
		}
		g.write('\n$cur_line')
		name = ''
		json_obj = tmp2
	}
	if node.language == .c {
		// Skip "C."
		name = util.no_dots(name[2..])
	} else {
		name = c_name(name)
	}
	if g.pref.translated || g.file.is_translated {
		// For `[c: 'P_TryMove'] fn p_trymove( ... `
		// every time `p_trymove` is called, `P_TryMove` must be generated instead.
		if f := g.table.find_fn(node.name) {
			// TODO PERF fn lookup for each fn call in translated mode
			if f.attrs.contains('c') {
				name = f.attrs[0].arg
			}
		}
	}
	// Obfuscate only functions in the main module for now
	if g.pref.obfuscate && g.cur_mod.name == 'main' && name.starts_with('main__') {
		key := node.name
		g.write('/* obf call: $key */')
		name = g.obf_table[key] or {
			panic('cgen: obf name "$key" not found, this should never happen')
		}
	}
	if !is_selector_call {
		if func := g.table.find_fn(node.name) {
			if func.generic_names.len > 0 {
				if g.comptime_for_field_type != 0 && g.inside_comptime_for_field {
					name = g.generic_fn_name([g.comptime_for_field_type], name, false)
				} else {
					name = g.generic_fn_name(node.concrete_types, name, false)
				}
			}
		}
	}
	// TODO2
	// cgen shouldn't modify ast nodes, this should be moved
	// g.generate_tmp_autofree_arg_vars(node, name)
	// Handle `print(x)`
	mut print_auto_str := false
	if is_print && (node.args[0].typ != ast.string_type || g.comptime_for_method.len > 0) {
		mut typ := node.args[0].typ
		if typ == 0 {
			g.checker_bug('print arg.typ is 0', node.pos)
		}
		if typ != ast.string_type || g.comptime_for_method.len > 0 {
			expr := node.args[0].expr
			typ_sym := g.table.sym(typ)
			if typ_sym.kind == .interface_ && (typ_sym.info as ast.Interface).defines_method('str') {
				g.write('${c_name(print_method)}(')
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
			if g.is_autofree && !typ.has_flag(.optional) {
				// Create a temporary variable so that the value can be freed
				tmp := g.new_tmp_var()
				// tmps << tmp
				g.write('string $tmp = ')
				g.gen_expr_to_string(expr, typ)
				g.writeln('; ${c_name(print_method)}($tmp); string_free(&$tmp);')
			} else {
				g.write('${c_name(print_method)}(')
				if expr is ast.ComptimeSelector {
					if expr.field_expr is ast.SelectorExpr {
						if expr.field_expr.expr is ast.Ident {
							key_str := '${expr.field_expr.expr.name}.typ'
							typ = g.comptime_var_type_map[key_str] or { typ }
						}
					}
				} else if expr is ast.ComptimeCall {
					if expr.method_name == 'method' {
						sym := g.table.sym(g.unwrap_generic(expr.left_type))
						if m := sym.find_method(g.comptime_for_method) {
							typ = m.return_type
						}
					}
				} else if expr is ast.Ident {
					if expr.obj is ast.Var {
						typ = expr.obj.typ
						if expr.obj.smartcasts.len > 0 {
							typ = expr.obj.smartcasts.last()
							cast_sym := g.table.sym(typ)
							if cast_sym.info is ast.Aggregate {
								typ = cast_sym.info.types[g.aggregate_type_idx]
							}
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
		if g.pref.is_debug && node.name == 'panic' {
			paline, pafile, pamod, pafn := g.panic_debug_info(node.pos)
			g.write('panic_debug($paline, tos3("$pafile"), tos3("$pamod"), tos3("$pafn"),  ')
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
						if obj.smartcasts.len > 0 {
							for _ in obj.smartcasts {
								g.write('(*')
							}
							for i, typ in obj.smartcasts {
								cast_sym := g.table.sym(g.unwrap_generic(typ))
								mut is_ptr := false
								if i == 0 {
									g.write(node.name)
									if obj.orig_type.is_ptr() {
										is_ptr = true
									}
								}
								dot := if is_ptr { '->' } else { '.' }
								if mut cast_sym.info is ast.Aggregate {
									sym := g.table.sym(cast_sym.info.types[g.aggregate_type_idx])
									g.write('${dot}_$sym.cname')
								} else {
									g.write('${dot}_$cast_sym.cname')
								}
								g.write(')')
							}
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
			g.write('(')
			if g.is_json_fn {
				g.write(json_obj)
			} else {
				if node.is_keep_alive
					&& g.pref.gc_mode in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt] {
					cur_line := g.go_before_stmt(0)
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
			g.write(')')
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
		&& !node.args[0].typ.has_flag(.optional) // TODO copy pasta checker.v
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
	// for i, arg in node.args {
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
		// t := g.new_tmp_var() + '_arg_expr_${name}_$i'
		fn_name := node.name.replace('.', '_') // can't use name...
		// t := '_tt${g.tmp_count_af}_arg_expr_${fn_name}_$i'
		t := '_arg_expr_${fn_name}_${i}_$node.pos.pos'
		// g.called_fn_name = name
		used := false // scope.known_var(t)
		mut s := '$t = '
		if used {
			// This means this tmp var name was already used (the same function was called and
			// `_arg_fnname_1` was already generated).
			// We do not need to declare this variable again, so just generate `t = ...`
			// instead of `string t = ...`, and we need to mark this variable as unused,
			// so that it's freed after the call. (Used tmp arg vars are not freed to avoid double frees).
			if x := scope.find(t) {
				match mut x {
					ast.Var { x.is_used = false }
					else {}
				}
			}
			s = '$t = '
		} else {
			scope.register(ast.Var{
				name: t
				typ: ast.string_type
				is_autofree_tmp: true
				pos: node.pos
			})
			s = 'string $t = '
		}
		// g.expr(arg.expr)
		s += g.expr_string(arg.expr)
		// g.writeln(';// new af pre')
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
	scope := g.file.scope.innermost(node_pos)
	for _, obj in scope.objects {
		match mut obj {
			ast.Var {
				// if var.typ == 0 {
				// // TODO why 0?
				// continue
				// }
				is_optional := obj.typ.has_flag(.optional)
				if is_optional {
					// TODO: free optionals
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
	args := if g.is_js_call {
		if node.args.len < 1 {
			g.error('node should have at least 1 arg', node.pos)
		}
		node.args[1..]
	} else {
		node.args
	}
	mut expected_types := node.expected_arg_types
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
	// only v variadic, C variadic args will be appeneded like normal args
	is_variadic := expected_types.len > 0 && expected_types.last().has_flag(.variadic)
		&& node.language == .v
	for i, arg in args {
		if is_variadic && i == expected_types.len - 1 {
			break
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
					name := '_arg_expr_${fn_name}_${i + 1}_$node.pos.pos'
					g.write('/*af arg*/' + name)
				}
			} else {
				g.ref_or_deref_arg(arg, expected_types[i], node.language)
			}
		} else {
			if use_tmp_var_autofree {
				// TODO copypasta, move to an inline fn
				fn_name := node.name.replace('.', '_')
				// name := '_tt${g.tmp_count_af}_arg_expr_${fn_name}_$i'
				name := '_arg_expr_${fn_name}_${i + 1}_$node.pos.pos'
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
			if fn_def := g.table.find_fn(node.name) {
				mut muttable := unsafe { &ast.Table(g.table) }
				if utyp := muttable.resolve_generic_to_concrete(arr_info.elem_type, fn_def.generic_names,
					node.concrete_types)
				{
					arr_info.elem_type = utyp
				}
			} else {
				g.error('unable to find function $node.name', node.pos)
			}
		}
		elem_type := g.typ(arr_info.elem_type)
		if (g.pref.translated || g.file.is_translated) && args.len == 1 {
			// Handle `foo(c'str')` for `fn foo(args ...&u8)`
			// TODOC2V handle this in a better place
			// println(g.table.type_to_str(args[0].typ))
			g.expr(args[0].expr)
		} else if args.len > 0 && args[args.len - 1].expr is ast.ArrayDecompose {
			g.expr(args[args.len - 1].expr)
		} else {
			if variadic_count > 0 {
				noscan := g.check_noscan(arr_info.elem_type)
				g.write('new_array_from_c_array${noscan}($variadic_count, $variadic_count, sizeof($elem_type), _MOV(($elem_type[$variadic_count]){')
				for j in arg_nr .. args.len {
					g.ref_or_deref_arg(args[j], arr_info.elem_type, node.language)
					if j < args.len - 1 {
						g.write(', ')
					}
				}
				g.write('}))')
			} else {
				g.write('__new_array(0, 0, sizeof($elem_type))')
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
		g.write('$typ __tmp_arg_${tmp_cnt_save + i} = ')
		// g.expr(arg.expr)
		g.ref_or_deref_arg(arg, expected_type, node.language)
		g.writeln(';')
	}
	g.empty_line = false
	return tmp_cnt_save
}

fn (mut g Gen) keep_alive_call_postgen(node ast.CallExpr, tmp_cnt_save int) {
	g.writeln('// keep_alive_call_postgen()')
	for i, expected_type in node.expected_arg_types {
		if expected_type.is_ptr() || expected_type.is_pointer() {
			g.writeln('GC_reachable_here(__tmp_arg_${tmp_cnt_save + i});')
		}
	}
}

[inline]
fn (mut g Gen) ref_or_deref_arg(arg ast.CallArg, expected_type ast.Type, lang ast.Language) {
	arg_is_ptr := expected_type.is_ptr() || expected_type.idx() in ast.pointer_type_idxs
	expr_is_ptr := arg.typ.is_ptr() || arg.typ.idx() in ast.pointer_type_idxs
	if expected_type == 0 {
		g.checker_bug('ref_or_deref_arg expected_type is 0', arg.pos)
	}
	exp_sym := g.table.sym(expected_type)
	arg_typ := g.unwrap_generic(arg.typ)
	mut needs_closing := false
	if arg.is_mut && !arg_is_ptr {
		g.write('&/*mut*/')
	} else if arg_is_ptr && !expr_is_ptr {
		if arg.is_mut {
			arg_sym := g.table.sym(arg_typ)
			if exp_sym.kind == .array {
				if (arg.expr is ast.Ident && (arg.expr as ast.Ident).kind == .variable)
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
				&& (arg.expr is ast.Ident || arg.expr is ast.SelectorExpr) {
				g.write('&/*sum*/')
				g.expr(arg.expr)
				return
			} else if arg_sym.kind == .interface_ && exp_sym.kind == .interface_
				&& (arg.expr is ast.Ident || arg.expr is ast.SelectorExpr) {
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
			if !((arg_typ_sym.kind == .function)
				|| deref_sym.kind in [.sum_type, .interface_]) && lang != .c {
				if arg.expr.is_lvalue() {
					g.write('(voidptr)&/*qq*/')
				} else {
					mut atype := expected_deref_type
					if atype.has_flag(.generic) {
						atype = g.unwrap_generic(atype)
					}
					if atype.has_flag(.generic) {
						g.write('(voidptr)&/*qq*/')
					} else {
						needs_closing = true
						g.write('ADDR(${g.typ(atype)}/*qq*/, ')
					}
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
				fn_attrs += '__stdcall '
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

// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

import v.ast
import v.util
import v.pref

// mark_used walks the AST, starting at main() and marks all used fns transitively.
pub fn mark_used(mut table ast.Table, mut pref_ pref.Preferences, ast_files []&ast.File) {
	mut all_fns, all_consts, all_globals, all_decltypes, all_structs := all_global_decl(ast_files)
	util.timing_start('MARKUSED')
	defer {
		util.timing_measure('MARKUSED')
	}

	trace_skip_unused := pref_.compile_values['trace_skip_unused'] == 'true'
	trace_skip_unused_all_fns := pref_.compile_values['trace_skip_unused_all_fns'] == 'true'
	trace_skip_unused_fn_names := pref_.compile_values['trace_skip_unused_fn_names'] == 'true'
	trace_skip_unused_just_unused_fns := pref_.compile_values['trace_skip_unused_just_unused_fns'] == 'true'
	used_fns := pref_.compile_values['used_fns']

	charptr_idx_str := ast.charptr_type_idx.str()
	string_idx_str := ast.string_type_idx.str()
	array_idx_str := ast.array_type_idx.str()
	map_idx_str := ast.map_type_idx.str()
	ref_map_idx_str := int(ast.map_type.ref()).str()
	ref_densearray_idx_str := int(table.find_type('DenseArray').ref()).str()
	ref_array_idx_str := int(ast.array_type.ref()).str()

	// Functions that must be generated and can't be skipped
	mut all_fn_root_names := []string{}
	mut include_panic_deps := false
	if used_fns != '' {
		aused_fns := used_fns.split(',')
		all_fns_keys := all_fns.keys()
		mut matching := []string{}
		for ufn in aused_fns {
			if ufn.contains('*') {
				matching_fns := all_fns_keys.filter(it.match_glob(ufn))
				if matching_fns.len > 0 {
					matching << matching_fns
				}
			} else {
				matching << ufn
			}
		}
		all_fn_root_names << matching
		for m in matching {
			println('> used_fn, found matching symbol: ${m}')
		}
	}
	if pref_.backend == .native {
		// Note: this is temporary, until the native backend supports more features!
		all_fn_root_names << 'main.main'
	} else {
		mut core_fns := [
			'main.main',
		]
		if pref_.is_bare {
			core_fns << 'init_global_allocator' // needed for linux_bare and wasm_bare
		}
		if 'use_libbacktrace' in pref_.compile_defines {
			core_fns << 'print_libbacktrace'
		}
		if 'callstack' in pref_.compile_defines {
			core_fns << ref_array_idx_str + '.push'
			core_fns << ref_array_idx_str + '.pop'
		}
		if pref_.autofree {
			core_fns << string_idx_str + '.clone_static'
			core_fns << string_idx_str + '.option_clone_static'
		}
		if table.used_features.auto_str || pref_.is_shared {
			include_panic_deps = true
			core_fns << 'isnil'
			core_fns << '__new_array'
			core_fns << '__new_array_noscan'
			core_fns << '__new_array_with_multi_default'
			core_fns << '__new_array_with_multi_default_noscan'
			core_fns << '__new_array_with_array_default'
			core_fns << '__new_array_with_array_default_noscan'
			core_fns << 'new_array_from_c_array'
		}
		if table.used_features.arr_prepend {
			core_fns << ref_array_idx_str + '.prepend_many'
		}
		if table.used_features.arr_reverse {
			core_fns << array_idx_str + '.reverse'
		}
		if table.used_features.arr_pop_left {
			core_fns << ref_array_idx_str + '.pop_left'
			core_fns << ref_array_idx_str + '.pop_left_noscan'
		}
		if table.used_features.arr_pop {
			core_fns << ref_array_idx_str + '.pop'
			core_fns << ref_array_idx_str + '.pop_noscan'
		}
		if table.used_features.arr_first {
			core_fns << array_idx_str + '.first'
		}
		if table.used_features.arr_last {
			core_fns << array_idx_str + '.last'
		}
		if table.used_features.arr_insert {
			core_fns << ref_array_idx_str + '.insert_many'
			core_fns << ref_array_idx_str + '.insert_noscan'
		}
		if table.used_features.print_options {
			include_panic_deps = true
			core_fns << '_option_ok'
			core_fns << '_result_ok'
		}
		if table.used_features.anon_fn {
			core_fns << 'memdup_uncollectable'
			core_fns << 'builtin.closure.closure_alloc'
			core_fns << 'builtin.closure.closure_init'
			core_fns << 'builtin.closure.closure_create'
		}
		if table.used_features.arr_map {
			include_panic_deps = true
			core_fns << '__new_array_with_map_default'
			core_fns << 'new_map_noscan_key'
			core_fns << ref_map_idx_str + '.clone'
			core_fns << ref_densearray_idx_str + '.clone'
			core_fns << map_idx_str + '.clone'
		}
		if table.used_features.type_name {
			core_fns << charptr_idx_str + '.vstring_literal'
		}
		if pref_.trace_calls || pref_.trace_fns.len > 0 {
			include_panic_deps = true
			core_fns << 'vgettid'
			core_fns << 'C.gettid'
			core_fns << 'v.trace_calls.on_c_main'
			core_fns << 'v.trace_calls.current_time'
			core_fns << 'v.trace_calls.on_call'
		}
		if 'C.cJSON_Parse' in all_fns {
			core_fns << '_result_ok'
			core_fns << 'tos5'
			core_fns << 'time.unix' // used by json
			core_fns << 'error'
			include_panic_deps = true
		}
		if pref_.should_use_segfault_handler() {
			core_fns << 'v_segmentation_fault_handler'
		}
		all_fn_root_names << core_fns
	}
	if pref_.is_bare {
		all_fn_root_names << [
			'strlen',
			'memcmp',
			'memcpy',
			'realloc',
			'vsnprintf',
			'vsprintf',
		]
	}

	is_noscan_whitelisted := pref_.gc_mode in [.boehm_full_opt, .boehm_incr_opt]

	has_noscan := all_fn_root_names.any(it.contains('noscan')
		&& it !in ['vcalloc_noscan', 'malloc_noscan'])
	for k, mut mfn in all_fns {
		if trace_skip_unused_all_fns {
			println('k: ${k} | mfn: ${mfn.name}')
		}
		// public/exported functions can not be skipped,
		// especially when producing a shared library:
		if mfn.is_pub && pref_.is_shared {
			all_fn_root_names << k
			continue
		}
		if pref_.translated && mfn.attrs.any(it.name == 'c') {
			all_fn_root_names << k
			continue
		}
		// _noscan functions/methods are selected when the `-gc boehm` is on:
		if has_noscan && is_noscan_whitelisted && mfn.name.ends_with('_noscan') {
			all_fn_root_names << k
			continue
		}
		if mfn.is_method {
			method_receiver_typename := table.type_to_str(mfn.receiver.typ)
			if method_receiver_typename == '&sync.Channel' {
				all_fn_root_names << k
				continue
			}
		}
		has_dot := k.contains('.')
		if has_dot {
			if k.ends_with('.init') || k.ends_with('.cleanup') {
				all_fn_root_names << k
				continue
			}
			if pref_.is_prof && (k.starts_with('time.vpc_now') || k.starts_with('v.profile.')) {
				// needed for -profile
				all_fn_root_names << k
				continue
			}
			if k.ends_with('.lock') || k.ends_with('.unlock') || k.ends_with('.rlock')
				|| k.ends_with('.runlock') {
				all_fn_root_names << k
				continue
			}
		}

		if k.ends_with('before_request') {
			// TODO: add a more specific check for the .before_request() method in vweb apps
			all_fn_root_names << k
			continue
		}
		// testing framework:
		if pref_.is_test {
			if k.starts_with('test_')
				|| (has_dot && (k.contains('.test_') || k.contains('.vtest_'))) {
				all_fn_root_names << k
				continue
			}
			if k.starts_with('testsuite_') || (has_dot && k.contains('.testsuite_')) {
				all_fn_root_names << k
				continue
			}
		}
		if pref_.prealloc && k.starts_with('prealloc_') {
			all_fn_root_names << k
			continue
		}
	}

	// handle assertions and testing framework callbacks:
	if pref_.is_debug {
		all_fn_root_names << 'panic_debug'
		all_fn_root_names << 'tos3'
	}
	if pref_.is_test {
		all_fn_root_names << 'main.cb_assertion_ok'
		all_fn_root_names << 'main.cb_assertion_failed'
		if benched_tests_sym := table.find_sym('main.BenchedTests') {
			bts_type := benched_tests_sym.methods[0].params[0].typ.str()
			all_fn_root_names << bts_type + '.testing_step_start'
			all_fn_root_names << bts_type + '.testing_step_end'
			all_fn_root_names << bts_type + '.end_testing'
			all_fn_root_names << 'main.start_testing'
		}
	}

	handle_vweb(mut table, mut all_fn_root_names, 'veb.Result', 'veb.filter', 'veb.Context')
	handle_vweb(mut table, mut all_fn_root_names, 'vweb.Result', 'vweb.filter', 'vweb.Context')
	handle_vweb(mut table, mut all_fn_root_names, 'x.vweb.Result', 'x.vweb.filter', 'x.vweb.Context')

	if 'debug_used_features' in pref_.compile_defines {
		eprintln('> debug_used_features: ${table.used_features}')
	}

	mut walker := Walker.new(
		table:         table
		all_fns:       all_fns
		all_consts:    all_consts
		all_globals:   all_globals
		all_decltypes: all_decltypes
		all_structs:   all_structs
		pref:          pref_
		trace_enabled: 'trace_skip_unused_walker' in pref_.compile_defines
	)
	walker.mark_markused_consts() // tagged with `@[markused]`
	walker.mark_markused_globals() // tagged with `@[markused]`
	walker.mark_markused_syms() // tagged with `@[markused]`
	walker.mark_markused_fns() // tagged with `@[markused]`, `@[export]` and veb actions
	walker.mark_markused_decltypes() // tagged with `@[markused]`
	walker.mark_generic_types()

	if pref_.use_cache {
		walker.mark_by_sym_name('IError')
	}

	walker.mark_root_fns(all_fn_root_names)

	walker.mark_by_sym_name('vweb.RedirectParams')
	walker.mark_by_sym_name('vweb.RequestParams')

	for kcon, con in all_consts {
		if pref_.is_shared && con.is_pub {
			walker.mark_const_as_used(kcon)
			continue
		}
		if pref_.translated && con.attrs.any(it.name == 'export') {
			walker.mark_const_as_used(kcon)
			continue
		}
	}

	if trace_skip_unused_fn_names {
		for key, _ in walker.used_fns {
			println('> used fn key: ${key}')
		}
	}

	walker.finalize(include_panic_deps)

	table.used_features.used_none = walker.used_none
	if walker.used_none == 0 {
		walker.used_fns.delete('${int(ast.none_type)}.str')
	}

	table.used_features.used_fns = walker.used_fns.move()
	table.used_features.used_consts = walker.used_consts.move()
	table.used_features.used_globals = walker.used_globals.move()
	table.used_features.used_syms = walker.used_syms.move()
	table.used_features.used_closures = walker.used_closures

	if trace_skip_unused {
		eprintln('>> t.used_fns: ${table.used_features.used_fns.keys()}')
		eprintln('>> t.used_consts: ${table.used_features.used_consts.keys()}')
		eprintln('>> t.used_globals: ${table.used_features.used_globals.keys()}')
		eprintln('>> t.used_syms: ${table.used_features.used_syms.keys()}')
		eprintln('>> t.used_maps: ${table.used_features.used_maps}')
		eprintln('>> t.used_closures: ${table.used_features.used_closures}')
	}
	if trace_skip_unused_just_unused_fns {
		all_fns_keys := all_fns.keys()
		used_fns_keys := table.used_features.used_fns.keys()
		for k in all_fns_keys {
			if k in used_fns_keys {
				continue
			}
			println('> k: ${k}')
		}
	}
}

fn all_global_decl_in_stmts(stmts []ast.Stmt, mut all_fns map[string]ast.FnDecl, mut all_consts map[string]ast.ConstField, mut all_globals map[string]ast.GlobalField, mut all_decltypes map[string]ast.TypeDecl, mut all_structs map[string]ast.StructDecl) {
	for node in stmts {
		match node {
			ast.FnDecl {
				fkey := node.fkey()
				if fkey !in all_fns || !node.no_body {
					all_fns[fkey] = node
				}
			}
			ast.ConstDecl {
				for cfield in node.fields {
					ckey := cfield.name
					all_consts[ckey] = cfield
				}
			}
			ast.GlobalDecl {
				for gfield in node.fields {
					gkey := gfield.name
					all_globals[gkey] = gfield
				}
			}
			ast.StructDecl {
				all_structs[node.name] = node
			}
			ast.TypeDecl {
				if node.is_markused {
					all_decltypes[node.name] = node
				}
			}
			ast.ExprStmt {
				match node.expr {
					ast.IfExpr {
						if node.expr.is_comptime {
							// top level comptime $if
							for branch in node.expr.branches {
								all_global_decl_in_stmts(branch.stmts, mut all_fns, mut
									all_consts, mut all_globals, mut all_decltypes, mut
									all_structs)
							}
						}
					}
					ast.MatchExpr {
						if node.expr.is_comptime {
							// top level comptime $match
							for branch in node.expr.branches {
								all_global_decl_in_stmts(branch.stmts, mut all_fns, mut
									all_consts, mut all_globals, mut all_decltypes, mut
									all_structs)
							}
						}
					}
					else {}
				}
			}
			else {}
		}
	}
}

fn all_global_decl(ast_files []&ast.File) (map[string]ast.FnDecl, map[string]ast.ConstField, map[string]ast.GlobalField, map[string]ast.TypeDecl, map[string]ast.StructDecl) {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut all_fns := map[string]ast.FnDecl{}
	mut all_consts := map[string]ast.ConstField{}
	mut all_globals := map[string]ast.GlobalField{}
	mut all_decltypes := map[string]ast.TypeDecl{}
	mut all_structs := map[string]ast.StructDecl{}
	for i in 0 .. ast_files.len {
		all_global_decl_in_stmts(ast_files[i].stmts, mut all_fns, mut all_consts, mut
			all_globals, mut all_decltypes, mut all_structs)
	}
	return all_fns, all_consts, all_globals, all_decltypes, all_structs
}

fn mark_all_methods_used(mut table ast.Table, mut all_fn_root_names []string, typ ast.Type) {
	sym := table.sym(typ)
	styp := int(typ).str()
	for method in sym.methods {
		all_fn_root_names << styp + '.' + method.name
	}
}

fn handle_vweb(mut table ast.Table, mut all_fn_root_names []string, result_name string, filter_name string,
	context_name string) {
	// handle vweb magic router methods:
	result_type_idx := table.find_type(result_name)
	if result_type_idx == 0 {
		return
	}
	all_fn_root_names << filter_name
	typ_vweb_context := table.find_type(context_name).set_nr_muls(1)
	mark_all_methods_used(mut table, mut all_fn_root_names, typ_vweb_context)
	for vgt in table.used_features.used_veb_types {
		sym_app := table.sym(vgt)
		pvgt := int(vgt.set_nr_muls(1)).str()
		for m in sym_app.methods {
			mut skip := true
			if m.name == 'before_request' {
				// TODO: handle expansion of method calls in generic functions in a more universal way
				skip = false
			}
			if m.return_type == result_type_idx {
				skip = false
			}
			if skip {
				continue
			}
			// eprintln('vgt: $vgt | pvgt: $pvgt | sym_app.name: $sym_app.name | m.name: $m.name')
			all_fn_root_names << pvgt + '.' + m.name
		}
	}
}

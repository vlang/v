// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

import v.ast
import v.util
import v.pref

// mark_used walks the AST, starting at main() and marks all used fns transitively
pub fn mark_used(mut table ast.Table, mut pref_ pref.Preferences, ast_files []&ast.File) {
	mut all_fns, all_consts, all_globals := all_fn_const_and_global(ast_files)
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut allow_noscan := true
	// Functions that must be generated and can't be skipped
	mut all_fn_root_names := []string{}
	if pref_.backend == .native {
		// Note: this is temporary, until the native backend supports more features!
		all_fn_root_names << 'main.main'
	} else {
		byteptr_idx_str := '${ast.byteptr_type_idx}'
		charptr_idx_str := '${ast.charptr_type_idx}'
		string_idx_str := '${ast.string_type_idx}'
		array_idx_str := '${ast.array_type_idx}'
		map_idx_str := '${ast.map_type_idx}'
		ref_array_idx_str := '${int(ast.array_type.ref())}'
		mut core_fns := [
			'main.main',
			'init_global_allocator', // needed for linux_bare and wasm_bare
			'v_realloc', // needed for _STR
			'malloc',
			'malloc_noscan',
			'vcalloc',
			'vcalloc_noscan',
			'memdup',
			'vstrlen',
			'tos',
			'tos2',
			'error',
			'builtin_init',
			'fast_string_eq',
			// TODO: process the _vinit const initializations automatically too
			'main.vtest_init',
			'main.vtest_new_metainfo',
			'main.vtest_new_filemetainfo',
		]
		$if debug_used_features ? {
			dump(table.used_features)
		}
		panic_deps := [
			'__new_array_with_default',
			'str_intp',
			ref_array_idx_str + '.push',
			string_idx_str + '.substr',
			array_idx_str + '.slice',
			array_idx_str + '.get',
			'v_fixed_index',
		]
		// real world apps
		if table.used_features.builtin_types || table.used_features.as_cast
			|| table.used_features.auto_str {
			core_fns << panic_deps
			core_fns << '__new_array'
			core_fns << '__new_array_with_multi_default'
			core_fns << '__new_array_with_array_default'
			core_fns << 'new_array_from_c_array'
			// byteptr and charptr
			core_fns << byteptr_idx_str + '.vstring'
			core_fns << byteptr_idx_str + '.vstring_with_len'
			core_fns << byteptr_idx_str + '.vstring_literal'
			core_fns << charptr_idx_str + '.vstring'
			core_fns << charptr_idx_str + '.vstring_with_len'
			core_fns << charptr_idx_str + '.vstring_literal'

			if table.used_features.index {
				core_fns << string_idx_str + '.at_with_check'
				core_fns << string_idx_str + '.clone'
				core_fns << string_idx_str + '.clone_static'
				core_fns << string_idx_str + '.at'
				core_fns << array_idx_str + '.set'
				core_fns << ref_array_idx_str + '.set'
				core_fns << map_idx_str + '.get'
				core_fns << map_idx_str + '.set'
			}
			if table.used_features.range_index {
				core_fns << string_idx_str + '.substr_with_check'
				core_fns << string_idx_str + '.substr_ni'
				core_fns << array_idx_str + '.slice_ni'
				core_fns << array_idx_str + '.get_with_check' // used for `x := a[i] or {}`
				core_fns << array_idx_str + '.clone_static_to_depth'
				core_fns << array_idx_str + '.clone_to_depth'
			}
			if table.used_features.cast_ptr {
				core_fns << 'ptr_str' // TODO: remove this. It is currently needed for the auto str methods for &u8, fn types, etc; See `./v -skip-unused vlib/builtin/int_test.v`
			}
			if table.used_features.auto_str {
				core_fns << string_idx_str + '.repeat'
				core_fns << 'tos3'
			}
			if table.used_features.auto_str_ptr {
				core_fns << 'isnil'
			}
			if table.used_features.arr_prepend {
				core_fns << ref_array_idx_str + '.prepend_many'
			}
			if table.used_features.arr_pop {
				core_fns << ref_array_idx_str + '.pop'
			}
			if table.used_features.arr_first {
				core_fns << array_idx_str + '.first'
			}
			if table.used_features.arr_last {
				core_fns << array_idx_str + '.last'
			}
		} else {
			// TODO: this *should not* depend on the used compiler, which is brittle, but only on info in the AST...
			// hello world apps
			if pref_.ccompiler_type != .tinyc && 'no_backtrace' !in pref_.compile_defines {
				// with backtrace on gcc/clang more code needs be generated
				allow_noscan = true
				core_fns << panic_deps
			} else {
				allow_noscan = false
			}
		}
		if table.used_features.option_or_result {
			core_fns << '_option_ok'
			core_fns << '_result_ok'
			if !allow_noscan {
				core_fns << panic_deps
				allow_noscan = true
			}
		}
		if table.used_features.as_cast {
			core_fns << '__as_cast'
		}
		if table.used_features.anon_fn {
			core_fns << 'memdup_uncollectable'
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

	for k, mut mfn in all_fns {
		$if trace_skip_unused_all_fns ? {
			println('k: ${k} | mfn: ${mfn.name}')
		}
		// _noscan functions/methods are selected when the `-gc boehm` is on:
		if allow_noscan && is_noscan_whitelisted && mfn.name.ends_with('_noscan') {
			all_fn_root_names << k
			continue
		}
		mut method_receiver_typename := ''
		if mfn.is_method {
			method_receiver_typename = table.type_to_str(mfn.receiver.typ)
		}
		if method_receiver_typename == '&wyrand.WyRandRNG' {
			// WyRandRNG is the default rand pseudo random generator
			all_fn_root_names << k
			continue
		}
		if method_receiver_typename == '&strings.Builder'
			&& (table.used_features.builtin_types || table.used_features.auto_str) {
			// implicit string builders are generated in auto_eq_methods.v
			all_fn_root_names << k
			continue
		}
		// auto generated string interpolation functions, may
		// call .str or .auto_str methods for user types:
		if k.ends_with('.str') || k.ends_with('.auto_str') {
			if table.used_features.auto_str
				|| table.used_features.print_types[mfn.receiver.typ.idx()] {
				all_fn_root_names << k
			}
			continue
		}
		if k.ends_with('.init') {
			all_fn_root_names << k
			continue
		}
		if table.used_features.builtin_types && k.ends_with('.free') {
			all_fn_root_names << k
			continue
		}
		// if mfn.name == 'before_request' {
		// all_fn_root_names << k
		//}

		// sync:
		if k == 'sync.new_channel_st' {
			all_fn_root_names << k
			continue
		}
		if k == 'sync.channel_select' {
			all_fn_root_names << k
			continue
		}
		if pref_.is_prof {
			if k.starts_with('time.vpc_now') || k.starts_with('v.profile.') {
				// needed for -profile
				all_fn_root_names << k
				continue
			}
		}

		if k.ends_with('before_request') {
			// TODO: add a more specific check for the .before_request() method in vweb apps
			all_fn_root_names << k
			continue
		}
		if method_receiver_typename == '&sync.Channel' {
			all_fn_root_names << k
			continue
		}
		if k.ends_with('.lock') || k.ends_with('.unlock') || k.ends_with('.rlock')
			|| k.ends_with('.runlock') {
			all_fn_root_names << k
			continue
		}
		if mfn.receiver.typ != ast.void_type && mfn.generic_names.len > 0 {
			// generic methods may be used in cgen after specialisation :-|
			// TODO: move generic method specialisation from cgen to before markused
			all_fn_root_names << k
			continue
		}
		// testing framework:
		if pref_.is_test {
			if k.starts_with('test_') || k.contains('.test_') {
				all_fn_root_names << k
				continue
			}
			if k.starts_with('testsuite_') || k.contains('.testsuite_') {
				// eprintln('>>> test suite: $k')
				all_fn_root_names << k
				continue
			}
		}
		// public/exported functions can not be skipped,
		// especially when producing a shared library:
		if mfn.is_pub && pref_.is_shared {
			all_fn_root_names << k
			continue
		}
		if mfn.name in ['+', '-', '*', '%', '/', '<', '=='] {
			// TODO: mark the used operators in the checker
			all_fn_root_names << k
			continue
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
	if table.used_features.option_or_result {
		all_fn_root_names << 'panic_option_not_set'
		all_fn_root_names << 'panic_result_not_set'
	}
	if pref_.is_test {
		all_fn_root_names << 'main.cb_assertion_ok'
		all_fn_root_names << 'main.cb_assertion_failed'
		if benched_tests_sym := table.find_sym('main.BenchedTests') {
			bts_type := benched_tests_sym.methods[0].params[0].typ
			all_fn_root_names << '${bts_type}.testing_step_start'
			all_fn_root_names << '${bts_type}.testing_step_end'
			all_fn_root_names << '${bts_type}.end_testing'
			all_fn_root_names << 'main.start_testing'
		}
	}

	// handle interface implementation methods:
	for isym in table.type_symbols {
		if isym.kind != .interface {
			continue
		}
		if isym.info !is ast.Interface {
			// Do not remove this check, isym.info could be &IError.
			continue
		}
		interface_info := isym.info as ast.Interface
		if interface_info.methods.len == 0 {
			continue
		}
		for itype in interface_info.types {
			ptype := itype.set_nr_muls(1)
			ntype := itype.set_nr_muls(0)
			interface_types := [ptype, ntype]
			for method in interface_info.methods {
				for typ in interface_types {
					interface_implementation_method_name := '${int(typ)}.${method.name}'
					$if trace_skip_unused_interface_methods ? {
						eprintln('>> isym.name: ${isym.name} | interface_implementation_method_name: ${interface_implementation_method_name}')
					}
					all_fn_root_names << interface_implementation_method_name
				}
			}
		}
	}

	handle_vweb(mut table, mut all_fn_root_names, 'veb.Result', 'veb.filter', 'veb.Context')
	handle_vweb(mut table, mut all_fn_root_names, 'vweb.Result', 'vweb.filter', 'vweb.Context')
	handle_vweb(mut table, mut all_fn_root_names, 'x.vweb.Result', 'x.vweb.filter', 'x.vweb.Context')

	// handle ORM drivers:
	orm_connection_implementations := table.iface_types['orm.Connection'] or { []ast.Type{} }
	if orm_connection_implementations.len > 0 {
		for k, _ in all_fns {
			if k.starts_with('orm.') {
				all_fn_root_names << k
			}
		}
		for orm_type in orm_connection_implementations {
			all_fn_root_names << '${int(orm_type)}.select'
			all_fn_root_names << '${int(orm_type)}.insert'
			all_fn_root_names << '${int(orm_type)}.update'
			all_fn_root_names << '${int(orm_type)}.delete'
			all_fn_root_names << '${int(orm_type)}.create'
			all_fn_root_names << '${int(orm_type)}.drop'
			all_fn_root_names << '${int(orm_type)}.last_id'
		}
	}

	if 'C.cJSON_Parse' in all_fns {
		all_fn_root_names << 'tos5'
		all_fn_root_names << 'time.unix' // used by json
		table.used_features.used_maps++ // json needs new_map etc
	}
	mut walker := Walker.new(
		table:       table
		files:       ast_files
		all_fns:     all_fns
		all_consts:  all_consts
		all_globals: all_globals
		pref:        pref_
	)
	// println( all_fns.keys() )
	walker.mark_markused_fns() // tagged with `@[markused]`

	walker.mark_markused_consts() // tagged with `@[markused]`
	walker.mark_markused_globals() // tagged with `@[markused]`
	walker.mark_exported_fns()
	walker.mark_root_fns(all_fn_root_names)

	if walker.n_asserts > 0 {
		unsafe { walker.fn_decl(mut all_fns['__print_assert_failure']) }
	}
	if table.used_features.used_maps > 0 {
		for k, mut mfn in all_fns {
			mut method_receiver_typename := ''
			if mfn.is_method {
				method_receiver_typename = table.type_to_str(mfn.receiver.typ)
			}
			if k in ['new_map', 'new_map_init', 'map_hash_string']
				|| method_receiver_typename == '&map' || method_receiver_typename == '&DenseArray'
				|| k.starts_with('map_') {
				walker.fn_decl(mut mfn)
			}
			if pref_.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
				if k in ['new_map_noscan_key', 'new_map_noscan_value', 'new_map_noscan_key_value',
					'new_map_init_noscan_key', 'new_map_init_noscan_value',
					'new_map_init_noscan_key_value'] {
					walker.fn_decl(mut mfn)
				}
			}
		}
	} else {
		for map_fn_name in ['new_map', 'new_map_init', 'map_hash_string', 'new_dense_array'] {
			walker.used_fns.delete(map_fn_name)
		}
		for k, mut mfn in all_fns {
			if !mfn.is_method {
				continue
			}
			method_receiver_typename := table.type_to_str(mfn.receiver.typ)
			if method_receiver_typename in ['&map', '&mapnode', '&SortedMap', '&DenseArray'] {
				walker.used_fns.delete(k)
			}
		}
	}

	$if trace_skip_unused_fn_names ? {
		for key, _ in walker.used_fns {
			println('> used fn key: ${key}')
		}
	}

	for kcon, con in all_consts {
		if pref_.is_shared && con.is_pub {
			walker.mark_const_as_used(kcon)
		}
		if !pref_.is_shared && con.is_pub && con.name.starts_with('main.') {
			walker.mark_const_as_used(kcon)
		}
	}

	table.used_features.used_fns = walker.used_fns.move()
	table.used_features.used_consts = walker.used_consts.move()
	table.used_features.used_globals = walker.used_globals.move()

	$if trace_skip_unused ? {
		eprintln('>> t.used_fns: ${table.used_features.used_fns.keys()}')
		eprintln('>> t.used_consts: ${table.used_features.used_consts.keys()}')
		eprintln('>> t.used_globals: ${table.used_features.used_globals.keys()}')
		eprintln('>> walker.table.used_features.used_maps: ${walker.table.used_features.used_maps}')
	}
}

fn all_fn_const_and_global(ast_files []&ast.File) (map[string]ast.FnDecl, map[string]ast.ConstField, map[string]ast.GlobalField) {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut all_fns := map[string]ast.FnDecl{}
	mut all_consts := map[string]ast.ConstField{}
	mut all_globals := map[string]ast.GlobalField{}
	for i in 0 .. ast_files.len {
		file := ast_files[i]
		for node in file.stmts {
			match node {
				ast.FnDecl {
					fkey := node.fkey()
					all_fns[fkey] = node
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
				else {}
			}
		}
	}
	return all_fns, all_consts, all_globals
}

fn handle_vweb(mut table ast.Table, mut all_fn_root_names []string, result_name string, filter_name string,
	context_name string) {
	// handle vweb magic router methods:
	result_type_idx := table.find_type(result_name)
	if result_type_idx != 0 {
		all_fn_root_names << filter_name
		typ_vweb_context := table.find_type(context_name).set_nr_muls(1)
		all_fn_root_names << '${int(typ_vweb_context)}.html'
		for vgt in table.used_features.used_veb_types {
			sym_app := table.sym(vgt)
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
				pvgt := vgt.set_nr_muls(1)
				// eprintln('vgt: $vgt | pvgt: $pvgt | sym_app.name: $sym_app.name | m.name: $m.name')
				all_fn_root_names << '${int(pvgt)}.${m.name}'
			}
		}
	}
}

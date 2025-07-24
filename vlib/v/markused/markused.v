// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

import v.ast
import v.util
import v.pref

// mark_used walks the AST, starting at main() and marks all used fns transitively.
pub fn mark_used(mut table ast.Table, mut pref_ pref.Preferences, ast_files []&ast.File) {
	mut all_fns, all_consts, all_globals, all_fields, all_decltypes, all_structs := all_global_decl(ast_files)
	util.timing_start('MARKUSED')
	defer {
		util.timing_measure('MARKUSED')
	}

	trace_skip_unused := pref_.compile_values['trace_skip_unused'] == 'true'
	trace_skip_unused_all_fns := pref_.compile_values['trace_skip_unused_all_fns'] == 'true'
	trace_skip_unused_fn_names := pref_.compile_values['trace_skip_unused_fn_names'] == 'true'
	trace_skip_unused_interface_methods := pref_.compile_values['trace_skip_unused_interface_methods'] == 'true'
	trace_skip_unused_just_unused_fns := pref_.compile_values['trace_skip_unused_just_unused_fns'] == 'true'
	used_fns := pref_.compile_values['used_fns']

	byteptr_idx_str := ast.byteptr_type_idx.str()
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
			'init_global_allocator', // needed for linux_bare and wasm_bare
			'memdup',
			'tos',
			'tos2',
			'error',
			'builtin_init',
			'fast_string_eq',
			'println',
			'ptr_str',
		]
		if ast.float_literal_type.idx() in table.used_features.print_types
			|| ast.f64_type_idx in table.used_features.print_types
			|| ast.f32_type_idx in table.used_features.print_types {
			include_panic_deps = true
		}
		if 'use_libbacktrace' in pref_.compile_defines {
			core_fns << 'print_libbacktrace'
		}
		if 'callstack' in pref_.compile_defines {
			core_fns << ref_array_idx_str + '.push'
			core_fns << ref_array_idx_str + '.pop'
		}
		if table.used_features.external_types {
			include_panic_deps = true
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
			// byteptr and charptr
			core_fns << byteptr_idx_str + '.vstring'
			core_fns << byteptr_idx_str + '.vstring_with_len'
			core_fns << byteptr_idx_str + '.vstring_literal'
			core_fns << charptr_idx_str + '.vstring'
			core_fns << charptr_idx_str + '.vstring_with_len'
			core_fns << charptr_idx_str + '.vstring_literal'
		}
		if table.used_features.index || pref_.is_shared {
			include_panic_deps = true
			core_fns << string_idx_str + '.at_with_check'
			core_fns << string_idx_str + '.clone'
			core_fns << string_idx_str + '.clone_static'
			core_fns << string_idx_str + '.at'
			core_fns << array_idx_str + '.set'
			core_fns << array_idx_str + '.get_with_check' // used for `x := a[i] or {}`
			core_fns << ref_array_idx_str + '.set'
			core_fns << map_idx_str + '.get'
			core_fns << map_idx_str + '.set'
			core_fns << '__new_array_noscan'
			core_fns << ref_array_idx_str + '.push_noscan'
			core_fns << ref_array_idx_str + '.push_many_noscan'
		}
		if table.used_features.range_index || pref_.is_shared {
			core_fns << string_idx_str + '.substr_with_check'
			core_fns << string_idx_str + '.substr_ni'
			core_fns << string_idx_str + '.substr'
			core_fns << array_idx_str + '.slice_ni'
			core_fns << array_idx_str + '.get_with_check' // used for `x := a[i] or {}`
			core_fns << array_idx_str + '.clone_static_to_depth'
			core_fns << array_idx_str + '.clone_to_depth'
		}
		if table.used_features.auto_str || table.used_features.dump {
			core_fns << string_idx_str + '.repeat'
			core_fns << 'tos3'
		}
		if table.used_features.auto_str_ptr {
			include_panic_deps = true
			core_fns << 'isnil'
		}
		if table.used_features.arr_prepend {
			core_fns << ref_array_idx_str + '.prepend_many'
		}
		if table.used_features.arr_reverse {
			core_fns << array_idx_str + '.reverse'
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
		if table.used_features.arr_insert {
			core_fns << ref_array_idx_str + '.insert_many'
		}
		if table.used_features.dump {
			include_panic_deps = true
			builderptr_idx := int(table.find_type('strings.Builder').ref()).str()
			core_fns << [
				builderptr_idx + '.str',
				builderptr_idx + '.free',
				builderptr_idx + '.write_rune',
			]
		}
		if !table.used_features.arr_init {
			table.used_features.arr_init = table.used_features.print_types.keys().any(table.type_to_str(it).contains('[]'))
		}
		if table.used_features.arr_init || table.used_features.comptime_for {
			include_panic_deps = true
			core_fns << '__new_array'
			core_fns << 'new_array_from_c_array'
			core_fns << 'new_array_from_c_array_noscan'
			core_fns << '__new_array_with_multi_default'
			core_fns << '__new_array_with_multi_default_noscan'
			core_fns << '__new_array_with_array_default'
			core_fns << ref_array_idx_str + '.set'
		}
		if table.used_features.print_options {
			include_panic_deps = true
			core_fns << '_option_ok'
			core_fns << '_result_ok'
			core_fns << charptr_idx_str + '.vstring_literal'
		}
		if table.used_features.anon_fn {
			core_fns << 'memdup_uncollectable'
		}
		if table.used_features.arr_map {
			include_panic_deps = true
			core_fns << '__new_array_with_map_default'
			core_fns << 'new_map_noscan_key'
			core_fns << ref_map_idx_str + '.clone'
			core_fns << ref_densearray_idx_str + '.clone'
			core_fns << map_idx_str + '.clone'
			table.used_features.used_maps++
		}
		if table.used_features.map_update {
			include_panic_deps = true
			core_fns << 'new_map_update_init'
			table.used_features.used_maps++
		}
		if table.used_features.asserts {
			include_panic_deps = true
			core_fns << '__print_assert_failure'
			core_fns << 'isnil'
		}
		if table.used_features.type_name {
			core_fns << charptr_idx_str + '.vstring_literal'
		}
		if table.used_features.memory_align {
			core_fns << 'memdup_align'
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
			table.used_features.used_maps++ // json needs new_map etc
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
			if method_receiver_typename == '&wyrand.WyRandRNG' {
				// WyRandRNG is the default rand pseudo random generator
				all_fn_root_names << k
				continue
			}
			if table.used_features.auto_str && method_receiver_typename == '&strings.Builder' {
				// implicit string builders are generated in auto_eq_methods.v
				all_fn_root_names << k
				continue
			}
			if method_receiver_typename == '&sync.Channel' {
				all_fn_root_names << k
				continue
			}
			if mfn.name in ['+', '-', '*', '%', '/', '<', '=='] {
				// TODO: mark the used operators in the checker
				all_fn_root_names << k
				continue
			}
		}
		has_dot := k.contains('.')
		// auto generated string interpolation functions, may
		// call .str or .auto_str methods for user types:
		if table.used_features.auto_str || table.used_features.dump || table.used_features.asserts
			|| table.used_features.debugger || table.used_features.external_types
			|| table.used_features.print_types[mfn.receiver.typ.idx()] {
			if (has_dot && (k.ends_with('.str') || k.ends_with('.auto_str')))
				|| (k.starts_with('_Atomic_') && k.ends_with('_str')) {
				all_fn_root_names << k
				continue
			}
		}
		if has_dot {
			if k.ends_with('.init') || k.ends_with('.cleanup') {
				all_fn_root_names << k
				continue
			}
			// sync:
			if k in ['sync.new_channel_st', 'sync.channel_select'] {
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
			if (pref_.autofree || table.used_features.external_types) && k.ends_with('.free') {
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
		if mfn.receiver.typ != ast.void_type && mfn.generic_names.len > 0 {
			// generic methods may be used in cgen after specialisation :-|
			// TODO: move generic method specialisation from cgen to before markused
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
					interface_implementation_method_name := int(typ.clear_flags()).str() + '.' +
						method.name
					if trace_skip_unused_interface_methods {
						eprintln('>> isym.name: ${isym.name} | interface_implementation_method_name: ${interface_implementation_method_name}')
					}
					all_fn_root_names << interface_implementation_method_name
				}
			}
			for embed_method in table.get_embed_methods(table.sym(itype)) {
				interface_implementation_method_name :=
					int(embed_method.params[0].typ.clear_flags()).str() + '.' + embed_method.name
				if trace_skip_unused_interface_methods {
					eprintln('>> isym.name: ${isym.name} | interface_implementation_method_name: ${interface_implementation_method_name} (embeded)')
				}
				all_fn_root_names << interface_implementation_method_name
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
			typ := int(orm_type).str()
			all_fn_root_names << typ + '.select'
			all_fn_root_names << typ + '.insert'
			all_fn_root_names << typ + '.update'
			all_fn_root_names << typ + '.delete'
			all_fn_root_names << typ + '.create'
			all_fn_root_names << typ + '.drop'
			all_fn_root_names << typ + '.last_id'
		}
	}

	if 'debug_used_features' in pref_.compile_defines {
		eprintln('> debug_used_features: ${table.used_features}')
	}

	mut walker := Walker.new(
		table:         table
		files:         ast_files
		all_fns:       all_fns
		all_consts:    all_consts
		all_globals:   all_globals
		all_fields:    all_fields
		all_decltypes: all_decltypes
		all_structs:   all_structs
		pref:          pref_
	)
	walker.mark_markused_consts() // tagged with `@[markused]`
	walker.mark_markused_globals() // tagged with `@[markused]`
	walker.mark_markused_syms() // tagged with `@[markused]`
	walker.mark_markused_fns() // tagged with `@[markused]`, `@[export]` and veb actions
	walker.mark_markused_decltypes() // tagged with `@[markused]`
	walker.mark_struct_field_default_expr()

	for k, _ in table.used_features.comptime_calls {
		walker.fn_by_name(k)
		// println('>>>>> ${k}')
	}

	for k, _ in table.used_features.comptime_syms {
		walker.mark_by_sym(table.sym(k))
		// println('>>>>> ${k}')
	}
	// println(all_fn_root_names)

	walker.mark_root_fns(all_fn_root_names)

	walker.mark_by_sym_name('vweb.RedirectParams')
	walker.mark_by_sym_name('vweb.RequestParams')

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
		for map_fn_name in ['new_map', 'new_map_init', 'map_hash_string', 'new_dense_array',
			'new_dense_array_noscan'] {
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

	if include_panic_deps || walker.used_interp > 0 {
		walker.mark_panic_deps()
	}

	if walker.used_panic > 0 {
		walker.mark_fn_as_used('panic_option_not_set')
		walker.mark_fn_as_used('panic_result_not_set')
	}
	if walker.used_none > 0 || table.used_features.auto_str {
		walker.mark_fn_as_used('_option_none')
		walker.mark_by_sym_name('_option')
	}
	if walker.used_option > 0 {
		walker.mark_fn_as_used('_option_clone')
		walker.mark_fn_as_used('_option_ok')
		walker.mark_by_sym_name('_option')
	}
	if walker.used_result > 0 {
		walker.mark_fn_as_used('_result_ok')
		walker.mark_by_sym_name('_result')
	}
	if (walker.used_option + walker.used_result + walker.used_none) > 0 {
		walker.mark_const_as_used('none__')
	}
	walker.mark_by_sym_name('array')

	if table.used_features.asserts {
		walker.mark_by_sym_name('VAssertMetaInfo')
	}

	if trace_skip_unused_fn_names {
		for key, _ in walker.used_fns {
			println('> used fn key: ${key}')
		}
	}

	table.used_features.used_none = walker.used_none
	if walker.used_none == 0 {
		walker.used_fns.delete('${int(ast.none_type)}.str')
	}

	walker.remove_unused_fn_generic_types()
	walker.remove_unused_dump_type()

	table.used_features.used_fns = walker.used_fns.move()
	table.used_features.used_consts = walker.used_consts.move()
	table.used_features.used_globals = walker.used_globals.move()
	table.used_features.used_syms = walker.used_syms.move()

	if trace_skip_unused {
		eprintln('>> t.used_fns: ${table.used_features.used_fns.keys()}')
		eprintln('>> t.used_consts: ${table.used_features.used_consts.keys()}')
		eprintln('>> t.used_globals: ${table.used_features.used_globals.keys()}')
		eprintln('>> t.used_syms: ${table.used_features.used_syms.keys()}')
		eprintln('>> walker.table.used_features.used_maps: ${walker.table.used_features.used_maps}')
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

fn all_global_decl(ast_files []&ast.File) (map[string]ast.FnDecl, map[string]ast.ConstField, map[string]ast.GlobalField, map[string]ast.StructField, map[string]ast.Type, map[string]ast.StructDecl) {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut all_fns := map[string]ast.FnDecl{}
	mut all_consts := map[string]ast.ConstField{}
	mut all_globals := map[string]ast.GlobalField{}
	mut all_fields := map[string]ast.StructField{}
	mut all_decltypes := map[string]ast.Type{}
	mut all_structs := map[string]ast.StructDecl{}
	for i in 0 .. ast_files.len {
		for node in ast_files[i].stmts {
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
					for sfield in node.fields {
						sfkey := sfield.sfkey()
						all_fields[sfkey] = sfield
					}
					all_structs[node.name] = node
				}
				ast.TypeDecl {
					all_decltypes[node.name] = node.typ
				}
				else {}
			}
		}
	}
	return all_fns, all_consts, all_globals, all_fields, all_decltypes, all_structs
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

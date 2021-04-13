// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

import v.ast
import v.util
import v.pref

// mark_used walks the AST, starting at main() and marks all used fns transitively
pub fn mark_used(mut table ast.Table, pref &pref.Preferences, ast_files []ast.File) {
	mut all_fns, all_consts := all_fn_and_const(ast_files)

	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}

	mut all_fn_root_names := [
		'main.main',
		'__new_array',
		'__new_array_with_default',
		'__new_array_with_array_default',
		'new_array_from_c_array',
		'v_fixed_index',
		'memdup',
		'vstrlen',
		'__as_cast',
		'tos',
		'tos2',
		'tos3',
		'isnil',
		'opt_ok',
		'error',
		'__print_assert_failure',
		// utf8_str_visible_length is used by c/str.v
		'utf8_str_visible_length',
		'compare_ints',
		'compare_u64s',
		'compare_strings',
		'compare_ints_reverse',
		'compare_u64s_reverse',
		'compare_strings_reverse',
		'builtin_init',
		// byteptr and charptr
		'3.vstring',
		'3.vstring_with_len',
		'4.vstring',
		'4.vstring_with_len',
		// byte. methods
		'9.str_escaped',
		// string. methods
		'18.add',
		'18.trim_space',
		'18.replace',
		'18.clone',
		'18.clone_static',
		'18.trim',
		'18.substr',
		'18.at',
		'18.index_kmp',
		// string. ==, !=, etc...
		'18.eq',
		'18.ne',
		'18.lt',
		'18.gt',
		'18.le',
		'18.ge',
		// ustring. ==, !=, etc...
		'19.eq',
		'19.ne',
		'19.lt',
		'19.gt',
		'19.le',
		'19.ge',
		'19.add',
		// other array methods
		'21.get',
		'21.set',
		'21.get_unsafe',
		'21.set_unsafe',
		'21.clone_static',
		'21.first',
		'21.last',
		'21.reverse',
		'21.repeat',
		'21.slice',
		'21.slice2',
		'59.get',
		'59.set',
		'65557.last',
		'65557.pop',
		'65557.push',
		'65557.insert_many',
		'65557.prepend_many',
		'65557.reverse',
		'65557.set',
		'65557.set_unsafe',
		// TODO: process the _vinit const initializations automatically too
		'os.getwd',
		'os.init_os_args',
		'os.init_os_args_wide',
	]
	if pref.gc_mode in [.boehm_full_opt, .boehm_incr_opt] {
		all_fn_root_names << [
			'__new_array_noscan',
			'__new_array_with_default_noscan',
			'__new_array_with_array_default_noscan',
			'new_array_from_c_array_noscan',
		]
	}

	for k, mut mfn in all_fns {
		mut method_receiver_typename := ''
		if mfn.is_method {
			method_receiver_typename = table.type_to_str(mfn.receiver.typ)
		}
		if method_receiver_typename == '&wyrand.WyRandRNG' {
			// WyRandRNG is the default rand pseudo random generator
			all_fn_root_names << k
			continue
		}
		if method_receiver_typename == '&strings.Builder' {
			// implicit string builders are generated in auto_eq_methods.v
			all_fn_root_names << k
			continue
		}
		if k.ends_with('.str') {
			all_fn_root_names << k
			continue
		}
		if k.ends_with('.init') {
			all_fn_root_names << k
			continue
		}
		if k.ends_with('.free') {
			all_fn_root_names << k
			continue
		}
		if k.ends_with('.lock') || k.ends_with('.unlock') || k.ends_with('.rlock')
			|| k.ends_with('.runlock') {
			all_fn_root_names << k
			continue
		}
		if pref.is_test {
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
		if mfn.is_pub && pref.is_shared {
			all_fn_root_names << k
			continue
		}
	}
	if pref.is_debug {
		all_fn_root_names << 'panic_debug'
	}
	all_fn_root_names << 'panic_optional_not_set'
	if pref.is_test {
		all_fn_root_names << 'main.cb_assertion_ok'
		all_fn_root_names << 'main.cb_assertion_failed'
		if benched_tests_sym := table.find_type('main.BenchedTests') {
			bts_type := benched_tests_sym.methods[0].params[0].typ
			all_fn_root_names << '${bts_type}.testing_step_start'
			all_fn_root_names << '${bts_type}.testing_step_end'
			all_fn_root_names << '${bts_type}.end_testing'
			all_fn_root_names << 'main.start_testing'
		}
	}

	mut walker := Walker{
		table: table
		files: ast_files
		all_fns: all_fns
		all_consts: all_consts
	}
	// println( all_fns.keys() )
	walker.mark_root_fns(all_fn_root_names)

	if walker.n_asserts > 0 {
		walker.fn_decl(mut all_fns['__print_assert_failure'])
	}
	if walker.n_maps > 0 {
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
		}
	}

	$if trace_skip_unused_fn_names ? {
		for key, _ in walker.used_fns {
			println('> used fn key: $key')
		}
	}

	table.used_fns = walker.used_fns.move()
	table.used_consts = walker.used_consts.move()

	$if trace_skip_unused ? {
		eprintln('>> t.used_fns: $table.used_fns.keys()')
		eprintln('>> t.used_consts: $table.used_consts.keys()')
		eprintln('>> walker.n_maps: $walker.n_maps')
	}
}

fn all_fn_and_const(ast_files []ast.File) (map[string]ast.FnDecl, map[string]ast.ConstField) {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure(@METHOD)
	}
	mut all_fns := map[string]ast.FnDecl{}
	mut all_consts := map[string]ast.ConstField{}
	for i in 0 .. ast_files.len {
		file := unsafe { &ast_files[i] }
		for node in file.stmts {
			match node {
				ast.FnDecl {
					fkey := if node.is_method {
						'${int(node.receiver.typ)}.$node.name'
					} else {
						node.name
					}
					all_fns[fkey] = node
				}
				ast.ConstDecl {
					for cfield in node.fields {
						ckey := cfield.name
						all_consts[ckey] = cfield
					}
				}
				else {}
			}
		}
	}
	return all_fns, all_consts
}

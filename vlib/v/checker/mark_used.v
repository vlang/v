module checker

import v.ast
import v.table
import v.util
import v.checker.mark_used_walker

// mark_used walks the AST, starting at main() and marks all used fns transitively
fn (mut c Checker) mark_used(ast_files []ast.File) {
	util.timing_start(@METHOD)
	util.timing_start('all_fn_and_const')
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
	util.timing_measure('all_fn_and_const')

	mut all_fn_root_names := [
		'main.main',
		'__new_array',
		'__new_array_with_default',
		'new_array_from_c_array',
		'memdup',
		'vstrlen',
		'tos',
		'tos2',
		'tos3',
		'isnil',
		/* utf8_str_visible_length is used by c/str.v */
		'utf8_str_visible_length',
		'builtin_init',
		/* byteptr and charptr */
		'3.vstring',
		'3.vstring_with_len',
		'4.vstring',
		'4.vstring_with_len',
		/* string. methods */
		'18.add',
		'18.trim_space',
		'18.replace',
		'18.clone',
		'18.trim',
		'18.substr',
		'18.at',
		'18.index_kmp',
		/* string. ==, !=, etc... */
		'18.eq',
		'18.ne',
		'18.lt',
		'18.gt',
		'18.le',
		'18.ge',
		/* ustring. ==, !=, etc... */
		'19.eq',
		'19.ne',
		'19.lt',
		'19.gt',
		'19.le',
		'19.ge',
		'19.add',
		/* other array methods */
		'21.get',
		'21.set',
		'21.get_unsafe',
		'21.set_unsafe',
		'21.slice',
		'21.slice2',
		'59.get',
		'59.set',
		'65557.push',
		'65557.set',
		'65557.set_unsafe',
		/* TODO: process the _vinit const initializations automatically too */
		'os.getwd',
		'os.init_os_args',
	]

	// implicit string builders are generated in auto_eq_methods.v
	mut sb_mut_type := ''
	if sbfn := c.table.find_fn('strings.new_builder') {
		sb_mut_type = sbfn.return_type.set_nr_muls(1).str() + '.'
	}

	for k, _ in all_fns {
		if k.ends_with('.init') {
			all_fn_root_names << k
		}
		if k.ends_with('.free') {
			all_fn_root_names << k
		}
		if c.pref.is_test && (k.starts_with('test_') || k.contains('.test_')) {
			all_fn_root_names << k
		}
		if sb_mut_type != '' && k.starts_with(sb_mut_type) {
			all_fn_root_names << k
		}
	}
	if c.pref.is_debug {
		all_fn_root_names << 'panic_debug'
	}
	if c.pref.is_test {
		all_fn_root_names << 'main.cb_assertion_ok'
		all_fn_root_names << 'main.cb_assertion_failed'
		if benched_tests_sym := c.table.find_type('main.BenchedTests') {
			bts_type := benched_tests_sym.methods[0].params[0].typ
			all_fn_root_names << '${bts_type}.testing_step_start'
			all_fn_root_names << '${bts_type}.testing_step_end'
			all_fn_root_names << '${bts_type}.end_testing'
			all_fn_root_names << 'main.start_testing'
		}
	}

	mut walker := mark_used_walker.Walker{
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
			if k == 'new_map_2' || k.starts_with('map_') || k.ends_with('set_1')
				|| k.ends_with('exists_1') || k.ends_with('get_1') {
				walker.fn_decl(mut mfn)
			}
		}
	}

	$if trace_skip_unused_fn_names ? {
		for key, _ in walker.used_fns {
			println('> used fn key: $key')
		}
	}

	c.table.used_fns = walker.used_fns
	c.table.used_consts = walker.used_consts

	$if trace_skip_unused ? {
		eprintln('>> c.table.used_fns: $c.table.used_fns.keys()')
		eprintln('>> c.table.used_consts: $c.table.used_consts.keys()')
		eprintln('>> walker.n_maps: $walker.n_maps')
	}
	util.timing_measure(@METHOD)
}

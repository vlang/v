module types

import v.flat

// `os.uname` and the `fn C.uname()` extern next to it are both registered under
// the bare `uname` key, the way register_fn_signature lowers C names, and so is
// every `builtin` function.
fn shadow_checker() (&flat.FlatAst, &TypeChecker) {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'main.v')
	a.add_val(.module_decl, 'main')
	a.add_val(.fn_decl, 'helper')
	a.add_val(.file, 'builtin.v')
	a.add_val(.module_decl, 'builtin')
	a.add_node(flat.Node{ kind: .fn_decl, value: 'println', op: .arrow })
	a.add_val(.fn_decl, 'new_node')
	a.add_val(.file, 'os.v')
	a.add_val(.module_decl, 'os')
	a.add_node(flat.Node{ kind: .fn_decl, value: 'uname', op: .arrow })
	a.add_node(flat.Node{ kind: .fn_decl, value: 'C.uname', op: .arrow })
	mut tc := TypeChecker.new(&a)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	tc.collect_declaration_visibility()
	tc.fn_ret_types['helper'] = Type(int_)
	tc.fn_ret_types['println'] = Type(void_)
	tc.fn_ret_types['new_node'] = Type(void_)
	tc.fn_ret_types['os.uname'] = Type(int_)
	tc.fn_ret_types['uname'] = Type(int_)
	return &a, tc
}

fn shadowed_key(mut tc TypeChecker, module_name string, name string) string {
	return shadowed_key_in(mut tc, module_name, 'main.v', name)
}

fn shadowed_key_in(mut tc TypeChecker, module_name string, file string, name string) string {
	tc.cur_module = module_name
	tc.cur_file = file
	return tc.shadowed_local_fn_key(name) or { '' }
}

fn test_local_variable_shadows_a_function_of_the_same_module() {
	_, mut tc := shadow_checker()
	assert shadowed_key(mut tc, 'main', 'helper') == 'helper'
	assert shadowed_key(mut tc, '', 'helper') == 'helper'
	assert shadowed_key(mut tc, 'os', 'uname') == 'os.uname'
}

fn test_local_variable_shadows_a_public_builtin_function_everywhere() {
	_, mut tc := shadow_checker()
	assert shadowed_key(mut tc, 'main', 'println') == 'println'
	assert shadowed_key(mut tc, 'builtin', 'println') == 'println'
	assert shadowed_key(mut tc, 'foo', 'println') == 'println'
}

fn test_local_variable_does_not_shadow_a_function_of_another_module() {
	_, mut tc := shadow_checker()
	assert shadowed_key(mut tc, 'main', 'uname') == ''
	assert shadowed_key(mut tc, 'foo', 'uname') == ''
	assert shadowed_key(mut tc, 'os', 'helper') == ''
	// A private `builtin` helper is not callable outside `builtin`.
	assert shadowed_key(mut tc, 'main', 'new_node') == ''
	assert shadowed_key(mut tc, 'foo', 'new_node') == ''
	assert shadowed_key(mut tc, 'builtin', 'new_node') == 'new_node'
}

fn test_a_script_shadows_the_os_functions_it_calls_unqualified() {
	mut a, mut tc := shadow_checker()
	// Without a script in the compilation an `os` function stays out of reach.
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'uname') == ''
	a.has_vsh_source = true
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'uname') == 'os.uname'
	assert shadowed_key_in(mut tc, 'foo', 'main.vsh', 'uname') == 'os.uname'
	// Unqualified `os` reaches only into the script, not into a plain `.v` file
	// compiled beside it.
	assert shadowed_key_in(mut tc, 'main', 'companion.v', 'uname') == ''
	// Only a public `os` declaration is reachable that way, and a name that has
	// no `os` function behind it is not shadowed by anything.
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'new_node') == ''
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'absent') == ''
	// The module tiers still answer first.
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'helper') == 'helper'
	assert shadowed_key_in(mut tc, 'main', 'main.vsh', 'println') == 'println'
}

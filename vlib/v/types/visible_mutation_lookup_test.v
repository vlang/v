module types

import v.flat

fn visible_lookup_checker() &TypeChecker {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.set_fresh_type_cache(true)
	return tc
}

fn visible_lookup_register(mut tc TypeChecker, idx int, module_name string, source_name string) {
	qname := checker_qualified_fn_name(module_name, source_name)
	tc.register_visible_mutation_fn_decl_with_lowered(idx, module_name, qname, source_name,
		qname.replace('.', '__'), source_name.replace('.', '__'))
}

fn test_visible_lookup_complete_index_matches_module_priority() {
	mut tc := visible_lookup_checker()
	for ready in [false, true] {
		visible_lookup_register(mut tc, 10, 'alpha', 'helper')
		visible_lookup_register(mut tc, 20, 'beta', 'helper')
		visible_lookup_register(mut tc, 30, 'alpha', 'helper')
		tc.visible_mutation_cache.decl_index_ready = ready
		for module_name, expected in {
			'alpha': 10
			'beta':  20
			'':      10
		} {
			decl := tc.visible_mutation_fn_decl('helper', module_name) or { panic('missing helper') }
			assert decl.idx == expected
		}
		for name, expected in {
			'alpha.helper':  10
			'beta.helper':   20
			'alpha__helper': 10
			'beta__helper':  20
		} {
			module_name := if expected == 10 { 'alpha' } else { 'beta' }
			decl := tc.visible_mutation_fn_decl(name, module_name) or { panic('missing alias') }
			assert decl.idx == expected
		}
		assert tc.visible_mutation_fn_decl('helper', 'missing') == none
		assert tc.visible_mutation_fn_decl('absent', '') == none
	}
}

fn test_visible_lookup_generic_receiver_names_and_main_module() {
	mut tc := visible_lookup_checker()
	visible_lookup_register(mut tc, 1, '', 'plain')
	visible_lookup_register(mut tc, 2, 'alpha', 'Box[T].method')
	tc.visible_mutation_cache.decl_index_ready = true
	for module_name in ['', 'main'] {
		decl := tc.visible_mutation_fn_decl('plain', module_name) or { panic('missing main') }
		assert decl.idx == 1
		assert decl.mod == 'main'
	}
	for name in ['Box[int].method', 'alpha.Box[string].method', 'Box.method'] {
		decl := tc.visible_mutation_fn_decl(name, 'alpha') or { panic('missing method') }
		assert decl.idx == 2
	}
}

fn test_visible_lookup_incomplete_index_does_not_override_module_lookup() {
	mut tc := visible_lookup_checker()
	// A lookup made before collection registered anything finds nothing.
	assert tc.visible_mutation_fn_decl('helper', 'alpha') == none
	// One registration fills a name's module-less entry and, where another
	// module declared the name first, its module entry. Until the index is
	// complete, the entry of another module must not create a module-specific
	// match.
	tc.register_visible_mutation_fn_decl_keys(1, 'beta', 'beta.helper', 'helper', 'beta__helper',
		'helper')
	assert tc.visible_mutation_fn_decl('helper', 'alpha') == none
	// The module's own declaration is found once it is registered, although
	// the lookups above recorded a miss, and it does not replace the first
	// declaration of the name.
	tc.register_visible_mutation_fn_decl_keys(2, 'alpha', 'alpha.helper', 'helper', 'alpha__helper',
		'helper')
	decl := tc.visible_mutation_fn_decl('helper', 'alpha') or { panic('missing module entry') }
	assert decl.idx == 2
	first := tc.visible_mutation_fn_decl('helper', 'beta') or { panic('missing first module') }
	assert first.idx == 1
	// A module's first declaration of a name that no other module declared
	// before it has the module-less entry alone.
	tc.register_visible_mutation_fn_decl_keys(3, 'alpha', 'alpha.only', 'only', 'alpha__only',
		'only')
	only := tc.visible_mutation_fn_decl('only', 'alpha') or { panic('missing only entry') }
	assert only.idx == 3
	assert tc.visible_mutation_fn_decl('only', 'beta') == none
	tc.visible_mutation_cache.decl_index_ready = true
	ready_decl := tc.visible_mutation_fn_decl('helper', 'alpha') or { panic('missing complete') }
	assert ready_decl.idx == 2
	ready_first := tc.visible_mutation_fn_decl('helper', 'beta') or { panic('missing complete') }
	assert ready_first.idx == 1
	ready_global := tc.visible_mutation_fn_decl('helper', '') or { panic('missing global') }
	assert ready_global.idx == 1
	ready_only := tc.visible_mutation_fn_decl('only', 'alpha') or { panic('missing complete') }
	assert ready_only.idx == 3
	assert tc.visible_mutation_fn_decl('only', 'beta') == none
}

fn test_visible_lookup_child_cache_precedes_complete_base() {
	mut tc := visible_lookup_checker()
	visible_lookup_register(mut tc, 1, 'alpha', 'helper')
	tc.visible_mutation_cache.decl_index_ready = true
	base := tc.visible_mutation_cache
	tc.visible_mutation_cache = &VisibleMutationCache{ base: base }
	visible_lookup_register(mut tc, 2, 'alpha', 'helper')
	decl := tc.visible_mutation_fn_decl('helper', 'alpha') or { panic('missing child') }
	assert decl.idx == 2
	global_decl := tc.visible_mutation_fn_decl('helper', '') or { panic('missing child global') }
	assert global_decl.idx == 2
	tc.visible_mutation_cache = &VisibleMutationCache{ base: base }
	base_decl := tc.visible_mutation_fn_decl('helper', 'alpha') or { panic('missing base') }
	assert base_decl.idx == 1
	tc.visible_mutation_cache.decl_misses['alpha\x01helper'] = true
	assert tc.visible_mutation_fn_decl('helper', 'alpha') == none
}

fn test_visible_lookup_long_keys_and_c_linker_names_preserve_priority() {
	mut tc := visible_lookup_checker()
	long_name := 'function_' + 'x'.repeat(220)
	visible_lookup_register(mut tc, 1, 'alpha', long_name)
	tc.register_visible_mutation_fn_decl_with_lowered(2, 'alpha', 'C.accept', 'C.accept',
		'accept', 'accept')
	visible_lookup_register(mut tc, 3, 'alpha', 'accept')
	for ready in [false, true] {
		tc.visible_mutation_cache.decl_index_ready = ready
		decl := tc.visible_mutation_fn_decl(long_name, 'alpha') or { panic('missing long key') }
		assert decl.idx == 1
		v_decl := tc.visible_mutation_fn_decl('accept', 'alpha') or { panic('missing V function') }
		assert v_decl.idx == 3
		c_decl := tc.visible_mutation_fn_decl('C.accept', 'alpha') or { panic('missing C function') }
		assert c_decl.idx == 2
	}
}

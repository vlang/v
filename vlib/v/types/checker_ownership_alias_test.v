module types

fn test_long_pointer_index_alias_chain_is_borrowed() {
	aliases := {
		'p0': 'm[k]'
		'p1': 'p0'
		'p2': 'p1'
		'p3': 'p2'
		'p4': 'p3'
		'p5': 'p4'
		'p6': 'p5'
		'p7': 'p6'
		'p8': 'p7'
	}
	assert ownership_alias_chain_borrows_indexed_storage(aliases, 'p8')
	assert !ownership_alias_chain_borrows_indexed_storage(aliases, 'unrelated')

	cycle := {
		'left':  'right'
		'right': 'left'
	}
	assert ownership_alias_chain_borrows_indexed_storage(cycle, 'left')
}

fn test_return_param_call_source_keeps_acyclic_paths_exact() {
	mut suffix, mut is_prefix := ownership_return_param_call_source('.left', '.name', false,
		false)
	assert suffix == '.left.name'
	assert !is_prefix
	// Depth alone never widens a path.
	suffix, is_prefix = ownership_return_param_call_source('.a', '.b.c.d.e.f', false, false)
	assert suffix == '.a.b.c.d.e.f'
	assert !is_prefix
	// A prefix source stays a prefix source when it is composed further.
	suffix, is_prefix = ownership_return_param_call_source('.a', '.b', true, false)
	assert suffix == '.a.b'
	assert is_prefix
}

fn test_return_param_call_source_widens_call_cycles_to_prefixes() {
	mut suffix, mut is_prefix := ownership_return_param_call_source('.left', '.name', false,
		true)
	assert suffix == '.left'
	assert is_prefix
	// The returned path already went through the `.left` argument projection.
	suffix, is_prefix = ownership_return_param_call_source('.left', '.right.left.name', false,
		false)
	assert suffix == '.left'
	assert is_prefix
	suffix, is_prefix = ownership_return_param_call_source('[0]', '.items[0]', false, false)
	assert suffix == '[0]'
	assert is_prefix
}

fn test_storage_suffix_contains_projection() {
	assert ownership_storage_suffix_contains_projection('.left.next.name', '.next')
	assert ownership_storage_suffix_contains_projection('.next', '.next')
	assert ownership_storage_suffix_contains_projection('.items[0].next', '.items[0]')
	assert ownership_storage_suffix_contains_projection('.nextval.next', '.next')
	assert !ownership_storage_suffix_contains_projection('.nextval', '.next')
	assert !ownership_storage_suffix_contains_projection('.left.nextval.next2', '.next')
	assert !ownership_storage_suffix_contains_projection('', '.next')
	assert !ownership_storage_suffix_contains_projection('.next', '')
}

struct ReturnSourceCallSite {
	caller     string
	callee     string
	arg_suffix string
}

// Mutual recursion through two fields each: `a(p)` returns `b(p.left)`, `b(p.right)` or `p.name`,
// and `b(p)` returns `a(p.left)`, `a(p.right)` or `p.name`. Without widening, the returned paths
// of the return fixed point grow forever.
fn test_return_param_call_sources_reach_fixed_point_for_mutual_recursion() {
	sites := [
		ReturnSourceCallSite{'a', 'b', '.left'},
		ReturnSourceCallSite{'a', 'b', '.right'},
		ReturnSourceCallSite{'b', 'a', '.left'},
		ReturnSourceCallSite{'b', 'a', '.right'},
	]
	mut sources := {
		'a': {
			'.name': false
		}
		'b': {
			'.name': false
		}
	}
	mut rounds := 0
	for changed := true; changed; rounds++ {
		assert rounds < 20
		changed = false
		for site in sites {
			for callee_suffix, callee_is_prefix in sources[site.callee].clone() {
				suffix, is_prefix := ownership_return_param_call_source(site.arg_suffix,
					callee_suffix, callee_is_prefix, site.caller == site.callee)
				key := suffix + if is_prefix { '*' } else { '' }
				if key !in sources[site.caller] {
					sources[site.caller][key] = is_prefix
					changed = true
				}
			}
		}
	}
	assert sources['a'].len < 16
	assert sources['b'].len < 16
	// The acyclic parts of the paths stay exact.
	assert sources['a']['.left.name'] == false
	assert sources['a']['.right.left.name'] == false
}

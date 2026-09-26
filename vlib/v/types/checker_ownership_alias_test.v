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

fn test_return_param_call_source_keeps_acyclic_chains_exact() {
	// `inner(p)` returns `p.left.name`, and `outer(p)` returns `inner(p.left)`: the repeated
	// `.left` projection is not a cycle.
	mut suffix, mut is_prefix, mut via := ownership_return_param_call_source('outer', '.left',
		'.left.name', false, ['inner'])
	assert suffix == '.left.left.name'
	assert !is_prefix
	assert via == ['inner', 'outer']
	// Depth alone never widens a path.
	suffix, is_prefix, via = ownership_return_param_call_source('a', '.a', '.b.c.d.e.f', false,
		['b', 'c', 'd', 'e'])
	assert suffix == '.a.b.c.d.e.f'
	assert !is_prefix
	// A prefix source stays a prefix source when it is composed further.
	suffix, is_prefix, via = ownership_return_param_call_source('a', '.a', '.b', true, ['b'])
	assert suffix == '.a.b'
	assert is_prefix
	assert via == ['b', 'a']
}

fn test_return_param_call_source_widens_call_cycles_to_prefixes() {
	// Direct recursion: the callee's own path lists the caller.
	mut suffix, mut is_prefix, mut via := ownership_return_param_call_source('last', '.left',
		'.name', false, ['last'])
	assert suffix == '.left'
	assert is_prefix
	assert via == ['last']
	// Mutual recursion: `zig` composed the path that `zag` returns to it.
	suffix, is_prefix, via = ownership_return_param_call_source('zig', '[0]', '.alt.name', false,
		['zag', 'zig', 'zag'])
	assert suffix == '[0]'
	assert is_prefix
	assert via == ['zig']
}

fn test_return_param_via_lists_each_function_once() {
	assert ownership_return_param_via([], 'a') == ['a']
	assert ownership_return_param_via(['a'], 'b') == ['a', 'b']
	assert ownership_return_param_via(['a', 'b'], 'a') == ['a', 'b']
}

struct ReturnSourceCallSite {
	caller     string
	callee     string
	arg_suffix string
}

struct ReturnSource {
	suffix    string
	is_prefix bool
	via       []string
}

// return_sources_fixed_point runs the return alias fixed point of `sites` from base sources that
// return `.name` of the parameter, and returns the rounds it took with the sources per function.
fn return_sources_fixed_point(sites []ReturnSourceCallSite, fns []string) (int, map[string]map[string]ReturnSource) {
	mut sources := map[string]map[string]ReturnSource{}
	for f in fns {
		sources[f] = {
			'.name': ReturnSource{'.name', false, [f]}
		}
	}
	mut rounds := 0
	for changed := true; changed; rounds++ {
		assert rounds < 20
		changed = false
		for site in sites {
			for _, callee_source in sources[site.callee].clone() {
				suffix, is_prefix, via := ownership_return_param_call_source(site.caller,
					site.arg_suffix, callee_source.suffix, callee_source.is_prefix, callee_source.via)
				key := suffix + if is_prefix { '*' } else { '' }
				if key !in sources[site.caller] {
					sources[site.caller][key] = ReturnSource{suffix, is_prefix, via}
					changed = true
				}
			}
		}
	}
	return rounds, sources
}

// Mutual recursion through two fields each: `a(p)` returns `b(p.left)`, `b(p.right)` or `p.name`,
// and `b(p)` returns `a(p.left)`, `a(p.right)` or `p.name`. Without widening, the returned paths
// of the return fixed point grow forever.
fn test_return_param_call_sources_reach_fixed_point_for_mutual_recursion() {
	rounds, sources := return_sources_fixed_point([
		ReturnSourceCallSite{'a', 'b', '.left'},
		ReturnSourceCallSite{'a', 'b', '.right'},
		ReturnSourceCallSite{'b', 'a', '.left'},
		ReturnSourceCallSite{'b', 'a', '.right'},
	], ['a', 'b'])
	assert rounds < 10
	assert sources['a'].len < 16
	assert sources['b'].len < 16
	// The paths that did not go around the cycle stay exact.
	assert !sources['a']['.left.name'].is_prefix
	assert !sources['a']['.right.name'].is_prefix
	assert sources['a']['.left*'].is_prefix
}

// `outer(p)` returns `inner(p.left)`, `inner(p)` returns `leaf(p.left)`, and `leaf(p)` returns
// `leaf(p.next)` or `p.name`: only the recursion of `leaf` is widened.
fn test_return_param_call_sources_widen_only_the_cycle() {
	_, sources := return_sources_fixed_point([
		ReturnSourceCallSite{'outer', 'inner', '.left'},
		ReturnSourceCallSite{'inner', 'leaf', '.left'},
		ReturnSourceCallSite{'leaf', 'leaf', '.next'},
	], ['leaf'])
	assert sources['leaf'].keys().sorted() == ['.name', '.next*']
	assert sources['inner'].keys().sorted() == ['.left.name', '.left.next*']
	assert sources['outer'].keys().sorted() == ['.left.left.name', '.left.left.next*']
	assert sources['outer']['.left.left.name'].via == ['leaf', 'inner', 'outer']
}

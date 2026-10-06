module main

import semver

fn vd(version string, deps ...string) VersionedDeps {
	return VersionedDeps{
		version: version
		deps:    deps
	}
}

fn con(required_by string, rng string) Constraint {
	return Constraint{
		required_by: required_by
		range:       rng
	}
}

fn test_topo_sort_puts_dependencies_first() {
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0')]
		'b': [vd('1.0.0')]
	}
	order := topo_sort(candidates)
	assert order == ['b', 'a']
}

fn test_topo_sort_handles_a_diamond() {
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0', 'c@^1.0.0')]
		'b': [vd('1.0.0', 'd@^1.0.0')]
		'c': [vd('1.0.0', 'd@^1.0.0')]
		'd': [vd('1.0.0')]
	}
	order := topo_sort(candidates)
	// d must come before b and c, which must come before a.
	assert order.index('d') < order.index('b')
	assert order.index('d') < order.index('c')
	assert order.index('b') < order.index('a')
	assert order.index('c') < order.index('a')
}

fn test_resolve_with_backtracking_resolves_a_simple_chain() {
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0')]
		'b': [vd('1.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, map[string][]Constraint{}) or { panic(err) }
	assert resolved['a'] == '1.0.0'
	assert resolved['b'] == '1.0.0'
}

fn test_resolve_with_backtracking_picks_the_highest_satisfying_version() {
	candidates := {
		'a': [vd('1.0.0'), vd('2.0.0')]
	}
	constraints := {
		'a': [con('root', '^1.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, constraints) or { panic(err) }
	assert resolved['a'] == '1.0.0'
}

fn test_resolve_with_backtracks_when_the_highest_version_dead_ends() {
	// a@2.0.0 needs b@^2.0.0, but b is constrained to ^1.0.0. The highest a fails,
	// so the search backtracks to a@1.0.0, which needs b@^1.0.0 and works.
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0'), vd('2.0.0', 'b@^2.0.0')]
		'b': [vd('1.0.0'), vd('2.0.0')]
	}
	constraints := {
		'b': [con('root', '^1.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, constraints) or { panic(err) }
	assert resolved['a'] == '1.0.0'
	assert resolved['b'] == '1.0.0'
}

fn test_resolve_reports_a_conflict_when_no_version_works() {
	// a needs b@^1.0.0 and b needs a@^2.0.0. Whichever is resolved first, the other
	// cannot be satisfied, and the error has to say so.
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0')]
		'b': [vd('1.0.0', 'a@^2.0.0')]
	}
	mut msg := ''
	resolve_with_backtracking(candidates, map[string][]Constraint{}) or { msg = err.msg() }
	assert msg.contains('failed to resolve'), msg
}

fn test_resolve_backtracks_past_a_module_that_was_resolved_first() {
	// The ordering matters: b is resolved first and takes 2.0.0, then a@2.0.0 needs
	// b@^2.0.0 which works. But if a had been resolved first at 2.0.0 and then b
	// failed, the search has to go back and try a@1.0.0.
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0'), vd('2.0.0', 'b@^2.0.0')]
		'b': [vd('1.0.0'), vd('2.0.0')]
	}
	constraints := {
		'a': [con('root', '^1.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, constraints) or { panic(err) }
	assert resolved['a'] == '1.0.0'
	assert resolved['b'] == '1.0.0'
}

fn test_resolve_with_backtracking_takes_the_highest_version_when_unconstrained() {
	candidates := {
		'a': [vd('1.0.0'), vd('2.0.0'), vd('3.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, map[string][]Constraint{}) or { panic(err) }
	assert resolved['a'] == '3.0.0'
}

fn test_resolve_handles_a_module_with_no_dependencies() {
	candidates := {
		'a': [vd('1.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, map[string][]Constraint{}) or { panic(err) }
	assert resolved['a'] == '1.0.0'
}

fn test_resolve_reports_which_module_failed() {
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0')]
		'b': [vd('1.0.0')]
	}
	constraints := {
		'b': [con('root', '^2.0.0')]
	}
	mut msg2 := ''
	resolve_with_backtracking(candidates, constraints) or { msg2 = err.msg() }
	assert msg2.contains('b'), msg2
}

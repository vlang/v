module main

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

fn test_joint_tag_selection_requires_every_constraint_and_excludes_unrequested_prereleases() {
	tags := ['v1.0.0', 'v2.0.0', 'v10.0.0', 'v11.0.0-alpha', 'notes']
	assert select_version_tag_with_constraints(tags, []Constraint{})! == 'v10.0.0'
	assert select_version_tag_with_constraints(tags, [con('root', '')])! == 'v10.0.0'
	assert select_version_tag_with_constraints(tags, [con('root', '^2.0.0'),
		con('dependency', '<3.0.0')])! == 'v2.0.0'
	assert select_version_tag_with_constraints(tags, [con('root', '>=11.0.0-alpha <11.0.0')])! == 'v11.0.0-alpha'
	assert tags == ['v1.0.0', 'v2.0.0', 'v10.0.0', 'v11.0.0-alpha', 'notes']
	mut conflict := ''
	select_version_tag_with_constraints(tags, [con('root', '^1.0.0'), con('dependency',
		'^2.0.0')]) or { conflict = err.msg() }
	assert conflict.contains('root requires ^1.0.0'), conflict
	assert conflict.contains('dependency requires ^2.0.0'), conflict
	for invalid_tags in [[]string{}, tags] {
		mut invalid := ''
		select_version_tag_with_constraints(invalid_tags, [con('root', '^invalid')]) or {
			invalid = err.msg()
		}
		assert invalid.contains('invalid version range'), invalid
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

fn test_resolve_orders_multi_digit_versions_semantically() {
	resolved := resolve_with_backtracking({
		'a': [vd('2.0.0'), vd('10.0.0')]
	}, map[string][]Constraint{})!
	assert resolved['a'] == '10.0.0'
}

fn test_resolve_handles_satisfiable_cycles() {
	candidates := {
		'a': [vd('1.0.0', 'b@^1.0.0'), vd('2.0.0', 'b@^1.0.0')]
		'b': [vd('1.0.0', 'a@^2.0.0')]
	}
	resolved := resolve_with_backtracking(candidates, map[string][]Constraint{})!
	assert resolved['a'] == '2.0.0'
	assert resolved['b'] == '1.0.0'
}

fn test_resolve_ignores_unavailable_dependencies_of_rejected_candidates() {
	resolved := resolve_with_backtracking({
		'a': [vd('2.0.0', 'missing@^1.0.0'), vd('1.0.0')]
	}, map[string][]Constraint{})!
	assert resolved == {
		'a': '1.0.0'
	}
}

fn test_resolve_checks_self_dependencies() {
	resolved := resolve_with_backtracking({
		'a': [vd('1.0.0', 'a@^2.0.0'), vd('2.0.0', 'a@^2.0.0')]
	}, map[string][]Constraint{})!
	assert resolved['a'] == '2.0.0'
	mut rejected := false
	resolve_with_backtracking({
		'a': [vd('1.0.0', 'a@^2.0.0')]
	}, map[string][]Constraint{}) or { rejected = true }
	assert rejected
}

fn test_resolve_does_not_reorder_caller_candidates() {
	candidates := {
		'a': [vd('1.0.0'), vd('2.0.0')]
	}
	resolve_with_backtracking(candidates, map[string][]Constraint{})!
	assert candidates['a'][0].version == '1.0.0'
	assert candidates['a'][1].version == '2.0.0'
}

fn test_resolve_reports_invalid_input_ranges_and_missing_candidates() {
	mut range_msg := ''
	resolve_with_backtracking({
		'a': [vd('1.0.0')]
	}, {
		'a': [con('root', 'not-a-range')]
	}) or { range_msg = err.msg() }
	assert range_msg.contains('invalid version range'), range_msg
	assert range_msg.contains('root'), range_msg
	mut missing_msg := ''
	resolve_with_backtracking(map[string][]VersionedDeps{}, {
		'missing': [con('root', '*')]
	}) or { missing_msg = err.msg() }
	assert missing_msg.contains('missing'), missing_msg
	for candidates in [{
		'a': [vd('not-a-version')]
	}, {
		'a': [vd('1.0.0'), vd('1.0.0')]
	}, {
		'a': [vd('1.0.0', 'b@not-a-range')]
	}] {
		mut rejected := false
		resolve_with_backtracking(candidates, map[string][]Constraint{}) or { rejected = true }
		assert rejected
	}
}

fn test_pubgrub_entry_checks_dependencies_and_backtracks_incompatible_versions() {
	candidates := {
		'a': [VersionedDeps{'1.0.0', ['b@^1']}, VersionedDeps{'2.0.0', ['b@^2']}]
		'b': [VersionedDeps{'1.0.0', []}, VersionedDeps{'2.0.0', []}]
	}
	constraints := {
		'b': [Constraint{'root', '^1'}]
	}
	resolved := resolve_with_pubgrub(candidates, constraints)!
	assert resolved['a'] == '1.0.0'
	assert resolved['b'] == '1.0.0'
}

fn test_pubgrub_entry_orders_versions_semantically_and_rejects_empty_domains() {
	candidates := {
		'a': [VersionedDeps{'1.9.0', []}, VersionedDeps{'1.10.0', []}]
	}
	assert resolve_with_pubgrub(candidates, map[string][]Constraint{})!['a'] == '1.10.0'
	mut failed := false
	resolve_with_pubgrub(candidates, {
		'a': [Constraint{'root', '^2'}]
	}) or { failed = true }
	assert failed
	failed = false
	resolve_with_pubgrub({
		'a': [VersionedDeps{'1.0.0', ['missing@^1']}]
	}, map[string][]Constraint{}) or { failed = true }
	assert failed
}

fn test_pubgrub_entry_validates_cycle_edges_and_reports_unsatisfiable_cycles() {
	candidates := {
		'a': [VersionedDeps{'1.0.0', ['b@^1']}]
		'b': [VersionedDeps{'1.0.0', ['a@^2']}]
	}
	mut failed := false
	resolve_with_pubgrub(candidates, map[string][]Constraint{}) or { failed = true }
	assert failed
}

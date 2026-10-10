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

// topo_sort orders module names so that a module's dependencies come before it. The
// resolver prefers this order to check dependency versions early. Cycles are checked
// in both directions as their candidate versions become known.
fn topo_sort(candidates map[string][]VersionedDeps) []string {
	mut order := []string{}
	mut visited := map[string]bool{}
	mut visiting := map[string]bool{}
	for name in candidates.keys().sorted() {
		topo_visit(name, candidates, mut order, mut visited, mut visiting)
	}
	return order
}

fn topo_visit(node string, candidates map[string][]VersionedDeps, mut order []string, mut visited map[string]bool, mut visiting map[string]bool) {
	if node !in candidates {
		return
	}
	if visiting[node] {
		return
	}
	if visited[node] {
		return
	}
	visiting[node] = true
	for cand in candidates[node] {
		for dep_str in cand.deps {
			dep := dep_str.all_before('@').trim_space()
			if dep != '' {
				topo_visit(dep, candidates, mut order, mut visited, mut visiting)
			}
		}
	}
	visiting[node] = false
	visited[node] = true
	order << node
}

// version_ok reports whether a candidate version satisfies every constraint on its
// module and is compatible with the versions already resolved.
fn version_ok(cand VersionedDeps, name string, resolved map[string]VersionedDeps, candidates map[string][]VersionedDeps, constraints map[string][]Constraint) bool {
	v := semver.from(cand.version) or { return false }
	for c in constraints[name] {
		if !v.satisfies(c.range) {
			return false
		}
	}
	for dep_str in cand.deps {
		dep_name := dep_str.all_before('@').trim_space()
		dep_range := if dep_str.contains('@') { dep_str.all_after('@').trim_space() } else { '' }
		if dep_name !in candidates {
			return false
		}
		if dep_name == name {
			if !v.satisfies(dep_range) {
				return false
			}
		} else if dep_name in resolved {
			dep_v := semver.from(resolved[dep_name].version) or { return false }
			if !dep_v.satisfies(dep_range) {
				return false
			}
		}
	}
	// A cycle can place a requiring module first. Check its selected dependency
	// against the new candidate too, instead of accepting an unchecked edge.
	for other_name, other in resolved {
		if other_name == name {
			continue
		}
		for dep_str in other.deps {
			if dep_str.all_before('@').trim_space() == name {
				dep_range := if dep_str.contains('@') {
					dep_str.all_after('@').trim_space()
				} else {
					''
				}
				if !v.satisfies(dep_range) {
					return false
				}
			}
		}
	}
	return true
}

fn compare_candidate_versions(a &VersionedDeps, b &VersionedDeps) int {
	av := semver.from(a.version) or { return 0 }
	bv := semver.from(b.version) or { return 0 }
	if av > bv {
		return -1
	}
	if av < bv {
		return 1
	}
	return compare_strings(a.version, b.version)
}

// resolve_with_backtracking searches for a consistent assignment of versions. Modules
// are resolved in dependency order, each from its highest version down, and when a
// choice leads to a dead end the search backtracks to the previous module and tries
// its next version.
//
// The position map is what makes it backtracking rather than a single pass: when a
// module fails, the search steps back to the previous module and resumes it from
// where it left off, so a different earlier choice gets a different later outcome.
fn resolve_with_backtracking(candidates map[string][]VersionedDeps, constraints map[string][]Constraint) !map[string]string {
	for name, requirements in constraints {
		if name !in candidates {
			return error('failed to resolve `${name}`: no candidate versions were supplied')
		}
		for requirement in requirements {
			if !semver.is_valid_range(requirement.range) {
				return error('invalid version range `${requirement.range}` required by `${requirement.required_by}` for `${name}`')
			}
		}
	}
	for name, versions in candidates {
		mut seen_versions := map[string]bool{}
		for candidate in versions {
			_ := semver.from(candidate.version) or {
				return error('invalid candidate version `${candidate.version}` for `${name}`')
			}
			if candidate.version in seen_versions {
				return error('duplicate candidate version `${candidate.version}` for `${name}`')
			}
			seen_versions[candidate.version] = true
			for dep in candidate.deps {
				if dep.all_before('@').trim_space() == '' {
					return error('invalid dependency `${dep}` for `${name}`')
				}
				range := if dep.contains('@') { dep.all_after('@').trim_space() } else { '' }
				if !semver.is_valid_range(range) {
					return error('invalid version range `${range}` required by `${name}`')
				}
			}
		}
	}
	order := topo_sort(candidates)
	mut resolved := map[string]VersionedDeps{}
	mut pos := map[string]int{}
	for mod_name in order {
		pos[mod_name] = 0
	}

	mut idx := 0
	for idx < order.len {
		mod_name := order[idx]
		mut cands := candidates[mod_name].clone()
		cands.sort_with_compare(compare_candidate_versions)

		mut found := false
		for i in 0 .. cands.len {
			if i < pos[mod_name] {
				continue
			}
			cand := cands[i]
			if version_ok(cand, mod_name, resolved, candidates, constraints) {
				resolved[mod_name] = cand
				pos[mod_name] = i + 1
				found = true
				break
			}
		}

		if found {
			idx++
		} else {
			resolved.delete(mod_name)
			pos[mod_name] = 0
			if idx == 0 {
				return error('failed to resolve `${mod_name}`: no version satisfies every constraint and dependency')
			}
			idx--
		}
	}
	mut versions := map[string]string{}
	for name, candidate in resolved {
		versions[name] = candidate.version
	}
	return versions
}

// resolve_with_pubgrub preserves the candidate API while checking the complete graph.
// Context-dependent conflict learning is not implemented yet; use the consistent
// search instead of independently accepting incompatible dependency versions.
fn resolve_with_pubgrub(candidates map[string][]VersionedDeps, constraints map[string][]Constraint) !map[string]string {
	return resolve_with_backtracking(candidates, constraints)!
}

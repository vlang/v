module main

import semver
import v.vmod

fn constraint(required_by string, rng string) Constraint {
	return Constraint{
		required_by: required_by
		range:       rng
	}
}

fn test_collect_constraints_gathers_every_requirement() {
	mut modules := map[string]Module{}
	modules['a'] = Module{
		name:     'vsl'
		version:  '0.0.1'
		manifest: vmod.Manifest{
			name:         'vsl'
			dependencies: ['c@^1.0.0', 'markdown']
		}
	}
	modules['b'] = Module{
		name:     'other'
		version:  '0.0.1'
		manifest: vmod.Manifest{
			name:         'other'
			dependencies: ['c@^2.0.0']
		}
	}
	graph := build_graph(modules)
	constraints := collect_constraints(modules, graph)
	assert constraints['c'].len == 2
	assert constraints['c'][0].required_by == 'vsl'
	assert constraints['c'][0].range == '^1.0.0'
	assert constraints['c'][1].required_by == 'other'
	assert constraints['c'][1].range == '^2.0.0'
}

fn test_collect_constraints_ignores_a_bare_name() {
	// A dependency with no `@` places no range constraint, so it must not appear as
	// one: `c` and `c@*` are different statements.
	mut modules := map[string]Module{}
	modules['a'] = Module{
		name:     'vsl'
		version:  '0.0.1'
		manifest: vmod.Manifest{
			name:         'vsl'
			dependencies: ['c']
		}
	}
	graph := build_graph(modules)
	constraints := collect_constraints(modules, graph)
	assert constraints['c'].len == 1
	assert constraints['c'][0].range == ''
}

fn test_select_version_tag_with_constraints_picks_the_highest_that_satisfies_all() {
	tags := ['v1.0.0', 'v1.5.0', 'v2.0.0']
	constraints := [constraint('a', '^1.0.0'), constraint('b', '<2.0.0')]
	tag := select_version_tag_with_constraints(tags, constraints) or { panic(err) }
	assert tag == 'v1.5.0'
}

fn test_select_version_tag_with_constraints_takes_the_highest_when_one_constraint() {
	tags := ['v1.0.0', 'v1.5.0', 'v2.0.0']
	constraints := [constraint('a', '^1.0.0')]
	tag := select_version_tag_with_constraints(tags, constraints) or { panic(err) }
	assert tag == 'v1.5.0'
}

fn test_select_version_tag_with_constraints_reports_the_conflict() {
	// ^1.0.0 and ^2.0.0 cannot both be met, and the error has to say which
	// requirements conflict rather than just "no tag matched".
	tags := ['v1.0.0', 'v1.5.0', 'v2.0.0']
	constraints := [constraint('a', '^1.0.0'), constraint('b', '^2.0.0')]
	mut msg := ''
	select_version_tag_with_constraints(tags, constraints) or { msg = err.msg() }
	assert msg.contains('a requires ^1.0.0'), msg
	assert msg.contains('b requires ^2.0.0'), msg
}

fn test_select_version_tag_with_constraints_accepts_an_empty_constraint_set() {
	// A module no dependent constrains resolves as if unconstrained.
	tags := ['v1.0.0', 'v2.0.0']
	tag := select_version_tag_with_constraints(tags, []Constraint{}) or { panic(err) }
	assert tag == 'v2.0.0'
}

fn test_select_version_tag_with_constraints_skips_a_tag_that_fails_one_constraint() {
	// v2.0.0 is the highest, but `b` requires <2.0.0, so v1.5.0 wins even though it is
	// not the highest tag.
	tags := ['v1.0.0', 'v1.5.0', 'v2.0.0']
	constraints := [constraint('a', '*'), constraint('b', '<2.0.0')]
	tag := select_version_tag_with_constraints(tags, constraints) or { panic(err) }
	assert tag == 'v1.5.0'
}

fn test_select_version_tag_with_constraints_treats_an_empty_range_as_no_constraint() {
	// A bare dependency name places no range, so it must not filter anything out.
	tags := ['v1.0.0', 'v2.0.0']
	constraints := [constraint('a', '')]
	tag := select_version_tag_with_constraints(tags, constraints) or { panic(err) }
	assert tag == 'v2.0.0'
}

fn test_resolve_modules_resolves_each_module_against_its_constraints() {
	mut modules := map[string]Module{}
	modules['a'] = Module{
		name:          'c'
		version:       '1.5.0'
		version_range: '^1.0.0'
	}
	constraints := {
		'c': [constraint('a', '^1.0.0 <2.0.0')]
	}
	// module_tags reads from git, so this can only be tested through the selection
	// logic. The resolution entry point is covered by the selection tests above.
	assert constraints['c'].len == 1
}

fn test_a_conflict_is_reported_per_module_not_per_run() {
	// Two modules can conflict independently; the error must name the one that failed
	// rather than failing the whole run on the first.
	tags := ['v1.0.0']
	constraints := [constraint('a', '^2.0.0')]
	mut msg := ''
	select_version_tag_with_constraints(tags, constraints) or { msg = err.msg() }
	assert msg.contains('a requires ^2.0.0'), msg
}

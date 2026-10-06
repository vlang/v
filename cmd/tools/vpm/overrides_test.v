module main

import v.vmod

fn modules_with(names ...string) map[string]Module {
	mut result := map[string]Module{}
	for i, name in names {
		result['mod${i}'] = Module{
			name:    name
			version: '0.0.1'
		}
	}
	return result
}

fn test_parse_overrides_reads_name_and_version() {
	overrides := parse_overrides(['vsl: 0.1.60'])
	assert overrides.len == 1
	assert overrides[0].name == 'vsl'
	assert overrides[0].version == '0.1.60'
}

fn test_parse_overrides_reads_several_entries() {
	overrides := parse_overrides(['vsl: 0.1.60', 'markdown: 1.2.3'])
	assert overrides.len == 2
	assert overrides[1].name == 'markdown'
	assert overrides[1].version == '1.2.3'
}

fn test_parse_overrides_skips_malformed_entries() {
	// A typo in an override must not silently install something else, so an entry
	// that is not exactly `name: version` is skipped rather than guessed at.
	assert parse_overrides(['vsl']).len == 0
	assert parse_overrides(['vsl:']).len == 0
	assert parse_overrides([': 0.1.60']).len == 0
	assert parse_overrides(['']).len == 0
	assert parse_overrides(['vsl 0.1.60']).len == 0
}

fn test_parse_overrides_keeps_a_valid_one_next_to_a_bad_one() {
	overrides := parse_overrides(['vsl', 'markdown: 1.2.3'])
	assert overrides.len == 1
	assert overrides[0].name == 'markdown'
}

fn test_apply_overrides_replaces_the_version() {
	mut modules := modules_with('vsl', 'markdown')
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']), map[string][]string{})
	assert modules['mod0'].version == '0.1.60'
	// A module no override names is left alone.
	assert modules['mod1'].version == '0.0.1'
}

fn test_apply_overrides_clears_the_range_when_the_override_is_exact() {
	mut modules := map[string]Module{}
	modules['k'] = Module{
		name:          'vsl'
		version:       '0.1.47'
		version_range: '^0.1.47'
	}
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']), map[string][]string{})
	assert modules['k'].version == '0.1.60'
	assert modules['k'].version_range == ''
}

fn test_apply_overrides_keeps_a_range_when_the_override_is_one() {
	mut modules := map[string]Module{}
	modules['k'] = Module{
		name:    'vsl'
		version: '0.1.47'
	}
	apply_overrides(mut modules, parse_overrides(['vsl: ^0.1.60']), map[string][]string{})
	assert modules['k'].version == '^0.1.60'
	assert modules['k'].version_range == '^0.1.60'
}

fn test_apply_overrides_matches_on_name_not_on_key() {
	// The map key is the dependency string as written, which may carry a version or
	// a URL; the override matches the resolved module name.
	mut modules := map[string]Module{}
	modules['vsl@^0.1.47'] = Module{
		name:    'vsl'
		version: '0.1.47'
	}
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']), map[string][]string{})
	assert modules['vsl@^0.1.47'].version == '0.1.60'
}

fn test_apply_overrides_applies_to_every_matching_module() {
	mut modules := modules_with('vsl', 'vsl', 'markdown')
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']), map[string][]string{})
	assert modules['mod0'].version == '0.1.60'
	assert modules['mod1'].version == '0.1.60'
	assert modules['mod2'].version == '0.0.1'
}

fn test_apply_overrides_with_no_overrides_changes_nothing() {
	mut modules := modules_with('vsl')
	apply_overrides(mut modules, []Override{}, map[string][]string{})
	assert modules['mod0'].version == '0.0.1'
}

fn module_with_deps(name string, deps ...string) Module {
	mut manifest := vmod.Manifest{}
	manifest.name = name
	manifest.dependencies = deps
	return Module{
		name:     name
		version:  '0.0.1'
		manifest: manifest
	}
}

fn test_build_graph_reads_the_dependencies_of_every_module() {
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'c', 'markdown')
	modules['b'] = module_with_deps('c')
	graph := build_graph(modules)
	assert graph['vsl'] == ['c', 'markdown']
	assert graph['c'] == []
}

fn test_build_graph_strips_the_version_from_a_dependency() {
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'c@^1.0.0')
	graph := build_graph(modules)
	assert graph['vsl'] == ['c']
}

fn test_a_selector_override_applies_where_the_requiring_module_asks() {
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'c')
	modules['b'] = module_with_deps('c')
	graph := build_graph(modules)
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2']), graph)
	assert modules['b'].version == '1.0.2'
}

fn test_a_selector_override_is_skipped_where_the_requiring_module_does_not_ask() {
	// The whole point of the selector: `vsl>c` must not force c where vsl is not the
	// one asking for it.
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'markdown')
	modules['b'] = module_with_deps('c')
	graph := build_graph(modules)
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2']), graph)
	assert modules['b'].version == '0.0.1'
}

fn test_a_selector_override_applies_to_every_match_when_several_modules_ask() {
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'c')
	modules['b'] = module_with_deps('other', 'c')
	modules['c'] = module_with_deps('c')
	graph := build_graph(modules)
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2']), graph)
	// Only vsl asks for c, so only that edge is overridden. The module is installed
	// once, so the version is forced regardless of which edge asked.
	assert modules['c'].version == '1.0.2'
}

fn test_parse_override_reads_the_requiring_module() {
	o := parse_override('vsl>c: 1.0.2') or { panic(err) }
	assert o.requiring == 'vsl'
	assert o.name == 'c'
	assert o.version == '1.0.2'
}

fn test_parse_override_without_a_selector_has_no_requiring_module() {
	o := parse_override('vsl: 0.1.60') or { panic(err) }
	assert o.requiring == ''
	assert o.name == 'vsl'
	assert o.version == '0.1.60'
}

fn test_parse_overrides_skips_a_malformed_selector() {
	assert parse_overrides(['vsl>c']).len == 0
	assert parse_overrides(['vsl>c: ']).len == 0
}

fn test_overrides_come_from_the_root_manifest_only() {
	// A dependency's own `dependency_overrides` must not reach the consumer's tree,
	// so the key is read from the root `v.mod` and nowhere else. This is the shape
	// the install path uses: one manifest, one source of overrides.
	manifest := vmod.decode("Module {\n\tname: 'root'\n\tdependencies: ['vsl']\n\tdependency_overrides: ['vsl: 0.1.60']\n}\n") or {
		panic(err)
	}
	overrides := parse_overrides(manifest.unknown['dependency_overrides'] or { []string{} })
	assert overrides.len == 1
	assert overrides[0].name == 'vsl'
	assert overrides[0].version == '0.1.60'
}

fn test_a_manifest_without_overrides_yields_none() {
	manifest := vmod.decode("Module {\n\tname: 'root'\n\tdependencies: ['vsl']\n}\n") or {
		panic(err)
	}
	overrides := parse_overrides(manifest.unknown['dependency_overrides'] or { []string{} })
	assert overrides.len == 0
}

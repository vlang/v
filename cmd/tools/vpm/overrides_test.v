module main

import v.vmod

fn test_parse_overrides_accepts_names_refs_and_ranges() {
	overrides := parse_overrides(['vsl: 0.1.60', 'publisher.markdown: ^1.2.3', 'other: feature/topic'])!
	assert overrides.len == 3
	assert overrides[0].name == 'vsl'
	assert overrides[0].version == '0.1.60'
	assert overrides[1].name == 'publisher.markdown'
	assert overrides[1].version == '^1.2.3'
	assert overrides[2].version == 'feature/topic'
}

fn test_parse_overrides_rejects_malformed_or_duplicate_entries() {
	for raw in [['vsl'], ['vsl:'], [': 0.1.60'], [''], ['vsl 0.1.60'], ['vsl: 1', 'vsl: 2'],
		['../vsl: 1'], ['vsl: 1', 'malformed']] {
		mut rejected := false
		parse_overrides(raw) or { rejected = true }
		assert rejected, '${raw} must be rejected before any installation'
	}
}

fn test_overridden_request_keeps_the_source_and_supersedes_constraints() {
	overrides := parse_overrides(['vsl: v0.1.60'])!
	assert overridden_request('vsl@^0.1.47', ['vsl'], overrides) == 'vsl@v0.1.60'
	assert overridden_request('https://example.com/owner/vsl.git@^0.1.47', ['vsl'], overrides) == 'https://example.com/owner/vsl.git@v0.1.60'
	assert overridden_request('git@example.com:owner/vsl.git', ['vsl'], overrides) == 'git@example.com:owner/vsl.git@v0.1.60'
	assert overridden_request('markdown@1.0.0', ['markdown'], overrides) == 'markdown@1.0.0'
}

fn test_override_ranges_remain_constraints_for_checkout_and_locks() {
	overrides := parse_overrides(['vsl: ^0.1.60'])!
	request := overridden_request('vsl@old-tag', ['vsl'], overrides)
	assert request == 'vsl@^0.1.60'
	assert requirement_version(request) == '^0.1.60'
	entry := LockedModule{ requested: request, resolved: 'v0.1.59', url: 'https://example.com/vsl' }
	assert lock_mismatch(entry, request, entry.url).contains('outside')
}

fn test_changed_override_invalidates_the_locked_request() {
	entry := LockedModule{ requested: 'vsl@v0.1.60', resolved: 'v0.1.60', url: 'https://example.com/vsl' }
	request := overridden_request('vsl', ['vsl'], parse_overrides(['vsl: v0.1.61'])!)
	assert lock_mismatch(entry, request, entry.url).contains('records `vsl@v0.1.60`')
	assert lockfile_module_key(request) == lockfile_module_key(entry.requested)
}

fn test_overrides_come_from_root_manifest_data() {
	manifest := vmod.decode("Module {\n\tname: 'root'\n\tdependencies: ['vsl']\n\tdependency_overrides: ['vsl: 0.1.60']\n}\n")!
	overrides := parse_overrides(manifest.unknown['dependency_overrides'] or { []string{} })!
	assert overrides.len == 1
	assert overrides[0].name == 'vsl'
	assert parse_overrides([]string{})! == []
}

fn test_scoped_override_selects_only_the_requiring_edge_and_beats_global() {
	overrides := parse_overrides(['c: v3.0.0', 'parent>c: v1.0.0'])!
	assert overridden_request_for_module('c@v2.0.0', ['c'], 'parent', overrides) == 'c@v1.0.0'
	assert overridden_request_for_module('c@v2.0.0', ['c'], 'other', overrides) == 'c@v3.0.0'
	assert overridden_request('c@v2.0.0', ['c'], overrides) == 'c@v3.0.0'
	assert overridden_request_for_module('c@v2.0.0', ['c'], 'other', parse_overrides(['parent>c: v1.0.0'])!) == 'c@v2.0.0'
	assert parse_overrides(['parent>c: v1.0.0', 'other>c: v2.0.0'])!.len == 2
}

fn test_scoped_overrides_reject_malformed_and_duplicate_selectors() {
	for raw in [['>c: v1.0.0'], ['parent>: v1.0.0'], ['parent>c>extra: v1.0.0'],
		['parent>c: v1.0.0', 'parent>c: v2.0.0'], ['parent>c'], ['parent>c: ']] {
		mut rejected := false
		parse_overrides(raw) or { rejected = true }
		assert rejected, '${raw}'
	}
}

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

fn test_apply_overrides_replaces_the_version() {
	mut modules := modules_with('vsl', 'markdown')
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60'])!, map[string][]string{})
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
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60'])!, map[string][]string{})
	assert modules['k'].version == '0.1.60'
	assert modules['k'].version_range == ''
}

fn test_apply_overrides_keeps_a_range_when_the_override_is_one() {
	mut modules := map[string]Module{}
	modules['k'] = Module{
		name:    'vsl'
		version: '0.1.47'
	}
	apply_overrides(mut modules, parse_overrides(['vsl: ^0.1.60'])!, map[string][]string{})
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
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60'])!, map[string][]string{})
	assert modules['vsl@^0.1.47'].version == '0.1.60'
}

fn test_apply_overrides_applies_to_every_matching_module() {
	mut modules := modules_with('vsl', 'vsl', 'markdown')
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60'])!, map[string][]string{})
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

// build_graph reads the dependencies of every module and returns a map from a module
// name to the names it depends on. This is the graph the selector form of an override
// needs: `vsl>c: 1.0.2` can only be applied where vsl actually asks for c.
fn build_graph(modules map[string]Module) map[string][]string {
	mut graph := map[string][]string{}
	for _, m in modules {
		mut deps := []string{}
		for dep in m.manifest.dependencies {
			name := dep.all_before('@').trim_space()
			if name != '' {
				deps << name
			}
		}
		graph[m.name] = deps
	}
	return graph
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
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2'])!, graph)
	assert modules['b'].version == '1.0.2'
}

fn test_a_selector_override_is_skipped_where_the_requiring_module_does_not_ask() {
	// The whole point of the selector: `vsl>c` must not force c where vsl is not the
	// one asking for it.
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'markdown')
	modules['b'] = module_with_deps('c')
	graph := build_graph(modules)
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2'])!, graph)
	assert modules['b'].version == '0.0.1'
}

fn test_a_selector_override_applies_to_every_match_when_several_modules_ask() {
	mut modules := map[string]Module{}
	modules['a'] = module_with_deps('vsl', 'c')
	modules['b'] = module_with_deps('other', 'c')
	modules['c'] = module_with_deps('c')
	graph := build_graph(modules)
	apply_overrides(mut modules, parse_overrides(['vsl>c: 1.0.2'])!, graph)
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

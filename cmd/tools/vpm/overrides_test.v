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
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']))
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
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']))
	assert modules['k'].version == '0.1.60'
	assert modules['k'].version_range == ''
}

fn test_apply_overrides_keeps_a_range_when_the_override_is_one() {
	mut modules := map[string]Module{}
	modules['k'] = Module{
		name:    'vsl'
		version: '0.1.47'
	}
	apply_overrides(mut modules, parse_overrides(['vsl: ^0.1.60']))
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
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']))
	assert modules['vsl@^0.1.47'].version == '0.1.60'
}

fn test_apply_overrides_applies_to_every_matching_module() {
	mut modules := modules_with('vsl', 'vsl', 'markdown')
	apply_overrides(mut modules, parse_overrides(['vsl: 0.1.60']))
	assert modules['mod0'].version == '0.1.60'
	assert modules['mod1'].version == '0.1.60'
	assert modules['mod2'].version == '0.0.1'
}

fn test_apply_overrides_with_no_overrides_changes_nothing() {
	mut modules := modules_with('vsl')
	apply_overrides(mut modules, []Override{})
	assert modules['mod0'].version == '0.0.1'
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

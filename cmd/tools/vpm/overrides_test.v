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
	assert dependency_request_version(request) == '^0.1.60'
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

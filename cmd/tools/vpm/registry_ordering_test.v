module main

import json2

fn test_opaque_version_order_is_independent_of_publication_order() {
	for insertion in [
		['release_2025', 'release_2026', 'release_2024'],
		['release_2026', 'release_2024', 'release_2025'],
		['release_2024', 'release_2025', 'release_2026'],
	] {
		mut r := new_registry()
		for version in insertion {
			r.add_module(ModuleInfo{ name: 'opaque', version: version })
		}
		assert r.list_versions('opaque') == ['release_2026', 'release_2025', 'release_2024']
		latest_info := r.get_latest('opaque') or { panic('missing latest opaque version') }
		assert latest_info.version == 'release_2026'
		listed := serve(r, 'GET', '/opaque/@v/list', map[string]string{}, map[string]string{})
		assert json2.decode[[]string](listed.body)! == r.list_versions('opaque')
		latest := serve(r, 'GET', '/opaque/@latest', map[string]string{}, map[string]string{})
		assert json2.decode[ModuleInfo](latest.body)!.version == 'release_2026'
		assert r.yank('opaque', 'release_2026')
		after_yank := r.get_latest('opaque') or { panic('missing latest after yank') }
		assert after_yank.version == 'release_2025'
	}
}

fn test_mixed_versions_use_semver_precedence_and_deterministic_ties() {
	expected := ['2.0.0', '2.0.0-rc.1', '1.0.0+build.8', '1.0.0+build.7', '1.0.0', '0.0.0',
		'release_2026', 'release_2025']
	for insertion in [expected, expected.reverse()] {
		mut r := new_registry()
		for version in insertion {
			r.add_module(ModuleInfo{ name: 'mixed', version: version })
		}
		assert r.list_versions('mixed') == expected
		latest := r.get_latest('mixed') or { panic('missing latest mixed version') }
		assert latest.version == '2.0.0'
		for version in expected[..6] {
			assert r.yank('mixed', version)
		}
		after_yank := r.get_latest('mixed') or { panic('missing opaque fallback after yanks') }
		assert after_yank.version == 'release_2026'
		assert r.list_versions('mixed') == expected
		assert r.yank('mixed', 'release_2026')
		assert r.yank('mixed', 'release_2025')
		assert r.get_latest('mixed') == none
	}
}

fn test_opaque_version_order_is_lexical_rather_than_a_numeric_guess() {
	mut r := new_registry()
	for version in ['release_10', 'release_9'] {
		r.add_module(ModuleInfo{ name: 'lexical', version: version })
	}
	assert r.list_versions('lexical') == ['release_9', 'release_10']
	latest := r.get_latest('lexical') or { panic('missing lexical latest version') }
	assert latest.version == 'release_9'
}

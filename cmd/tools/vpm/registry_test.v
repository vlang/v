module main

import json2

fn test_new_registry_is_empty() {
	r := new_registry()
	assert r.modules.len == 0
}

fn test_add_module() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'abc123'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
	assert r.modules.len == 1
	assert r.modules['testmod'].versions.len == 1
	assert r.modules['testmod'].versions[0].version == '1.0.0'
}

fn test_list_versions() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '2.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.5.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	versions := r.list_versions('testmod')
	assert versions.len == 3
	assert versions[0] == '2.0.0'
	assert versions[1] == '1.5.0'
	assert versions[2] == '1.0.0'
}

fn test_get_info() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'abc123'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
	info := r.get_info('testmod', '1.0.0') or {
		assert false
		return
	}
	assert info.name == 'testmod'
	assert info.version == '1.0.0'
	assert info.description == 'A test module'
	assert info.license == 'MIT'
	assert info.checksum == 'abc123'
}

fn test_get_info_not_found() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	assert r.get_info('testmod', '2.0.0') == none
	assert r.get_info('othermod', '1.0.0') == none
}

fn test_get_latest() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '2.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	latest := r.get_latest('testmod') or {
		assert false
		return
	}
	assert latest.version == '2.0.0'
}

fn test_get_latest_skips_yanked() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '2.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.yank('testmod', '2.0.0')
	latest := r.get_latest('testmod') or {
		assert false
		return
	}
	assert latest.version == '1.0.0'
}

fn test_yank_and_unyank() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	assert r.yank('testmod', '1.0.0') == true
	info := r.get_info('testmod', '1.0.0') or {
		assert false
		return
	}
	assert info.yanked == true
	assert r.unyank('testmod', '1.0.0') == true
	info2 := r.get_info('testmod', '1.0.0') or {
		assert false
		return
	}
	assert info2.yanked == false
}

fn test_yank_not_found() {
	mut r := new_registry()
	assert r.yank('testmod', '1.0.0') == false
	assert r.unyank('testmod', '1.0.0') == false
}

fn test_search() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'othermod'
		version:      '1.0.0'
		description:  'Another module'
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	results := r.search('test')
	assert results.len == 1
	assert results[0].name == 'testmod'
}

fn test_search_no_match() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	results := r.search('nonexistent')
	assert results.len == 0
}

fn test_export_import_json() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'abc123'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
	data := r.export_json()
	r2 := import_json(data) or {
		assert false
		return
	}
	assert r2.modules.len == 1
	assert r2.modules['testmod'].versions.len == 1
	assert r2.modules['testmod'].versions[0].version == '1.0.0'
}

fn test_handle_request_list_versions() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	result := handle_request(r, 'GET', '/testmod/@v/list', map[string]string{})
	versions := json2.decode[[]string](result) or {
		assert false
		return
	}
	assert versions.len == 1
	assert versions[0] == '1.0.0'
}

fn test_handle_request_get_info() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'abc123'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
	result := handle_request(r, 'GET', '/testmod/@v/1.0.0.info', map[string]string{})
	info := json2.decode[ModuleInfo](result) or {
		assert false
		return
	}
	assert info.name == 'testmod'
	assert info.version == '1.0.0'
}

fn test_handle_request_get_latest() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '2.0.0'
		description:  ''
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	result := handle_request(r, 'GET', '/testmod/@latest', map[string]string{})
	info := json2.decode[ModuleInfo](result) or {
		assert false
		return
	}
	assert info.version == '2.0.0'
}

fn test_handle_request_unknown_module_lists_empty() {
	r := new_registry()
	result := handle_request(r, 'GET', '/nonexistent/@v/list', map[string]string{})
	versions := json2.decode[[]string](result) or {
		assert false
		return
	}
	assert versions.len == 0
}

fn test_handle_request_not_found() {
	r := new_registry()
	result := handle_request(r, 'GET', '/nonexistent/@v/1.0.0.info', map[string]string{})
	assert result.contains('not found')
}

fn test_handle_request_config() {
	r := new_registry()
	result := handle_request(r, 'GET', '/config.json', map[string]string{})
	config := json2.decode[RegistryConfig](result) or {
		assert false
		return
	}
	assert config.dl != ''
	assert config.api != ''
}

fn test_handle_request_search() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      ''
		dependencies: map[string]string{}
		checksum:     ''
		published_at: ''
		features:     map[string][]string{}
	})
	result := handle_request(r, 'GET', '/api/search', map[string]string{
		'q': 'test'
	})
	results := json2.decode[[]RegistryEntry](result) or {
		assert false
		return
	}
	assert results.len == 1
	assert results[0].name == 'testmod'
}

fn test_handle_request_module_metadata() {
	mut r := new_registry()
	r.add_module(ModuleInfo{
		name:         'testmod'
		version:      '1.0.0'
		description:  'A test module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'abc123'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
	result := handle_request(r, 'GET', '/api/modules/testmod', map[string]string{})
	entry := json2.decode[RegistryEntry](result) or {
		assert false
		return
	}
	assert entry.name == 'testmod'
	assert entry.versions.len == 1
}

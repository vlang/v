module toolchain

import os

fn tmp_root() string {
	return os.join_path(os.vtmp_dir(), 'v_toolchain_module_test_${os.getpid()}')
}

fn test_normalize_version() {
	assert (normalize_version('stable') or { '' }) == 'master'
	assert (normalize_version('latest') or { '' }) == 'master'
	assert (normalize_version('nightly') or { '' }) == 'nightly'
	assert (normalize_version('0.4.11') or { '' }) == 'v0.4.11'
	assert (normalize_version('v0.4.11') or { '' }) == 'v0.4.11'
	assert normalize_version('not-a-version') == none
	assert normalize_version('') == none
}

fn test_nightly_branch_and_manifest_path() ! {
	assert branch_for('nightly') == 'master'
	assert branch_for('v0.4.11') == 'v0.4.11'
	custom := os.join_path(tmp_root(), 'versions.json')
	os.setenv(manifest_env, custom, true)
	defer {
		os.setenv(manifest_env, '', true)
	}
	assert manifest_path() == custom
}

fn test_set_and_read_key_preserves_other_keys() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'settings.toml')
	os.write_file(path, 'other = "keep me"\n')!
	set_key_in_file(path, settings_key, 'v0.4.11')!
	first := read_key(path, settings_key) or { '' }
	assert first == 'v0.4.11'
	other := read_key(path, 'other') or { '' }
	assert other == 'keep me'
	set_key_in_file(path, settings_key, 'master')!
	second := read_key(path, settings_key) or { '' }
	assert second == 'master'
}

fn test_select_precedence() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(project, pin_file), 'toolchain = "v1.2.3"\n')!
	settings := os.join_path(root, 'settings.toml')
	os.write_file(settings, 'default_toolchain = "v9.9.9"\n')!
	os.setenv(settings_env, settings, true)
	os.setenv(project_env, project, true)
	defer {
		os.setenv(settings_env, '', true)
		os.setenv(project_env, '', true)
		os.setenv(version_env, '', true)
	}
	// The environment overrides the project pin.
	os.setenv(version_env, '0.4.7', true)
	assert select().version == 'v0.4.7'
	// The project pin overrides the user default.
	os.setenv(version_env, '', true)
	selection := select()
	assert selection.version == 'v1.2.3'
	assert selection.source == os.join_path(project, pin_file)
}

fn test_select_reads_vmod_pin() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'demo'\n\ttoolchain: 'v7.7.7'\n}\n")!
	os.setenv(project_env, project, true)
	os.setenv(version_env, '', true)
	os.setenv(settings_env, os.join_path(root, 'missing.toml'), true)
	defer {
		os.setenv(project_env, '', true)
		os.setenv(version_env, '', true)
		os.setenv(settings_env, '', true)
	}
	selection := select()
	assert selection.version == 'v7.7.7'
}

fn test_select_reads_vmod_pin_with_commas() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := "Module {\n\tname: 'demo',\n\ttoolchain: 'v6.6.6',\n\tversion: '0.0.0'\n}\n"
	os.write_file(os.join_path(project, 'v.mod'), manifest)!
	os.setenv(project_env, project, true)
	os.setenv(version_env, '', true)
	os.setenv(settings_env, os.join_path(root, 'missing.toml'), true)
	defer {
		os.setenv(project_env, '', true)
		os.setenv(version_env, '', true)
		os.setenv(settings_env, '', true)
	}
	assert select().version == 'v6.6.6'
	// A single-line Module block with commas is valid too.
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'demo', toolchain: 'v5.5.5' }\n")!
	assert select().version == 'v5.5.5'
}

fn test_set_key_stays_at_top_level() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'settings.toml')
	os.write_file(path, '[other]\nfoo = "bar"\ndefault_toolchain = "v9.9.9"\n')!
	// A key inside a table is not a top-level default.
	assert read_key(path, settings_key) == none
	set_key_in_file(path, settings_key, 'v0.4.11')!
	text := os.read_file(path)!
	assert text.starts_with('default_toolchain = "v0.4.11"\n'), text
	assert (read_key(path, settings_key) or { '' }) == 'v0.4.11'
	assert (read_key(path, 'foo') or { '' }) == '' // foo is inside a table
}

fn test_select_falls_back_to_settings_then_default() ! {
	root := tmp_root()
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	settings := os.join_path(root, 'settings.toml')
	os.write_file(settings, 'default_toolchain = "v8.8.8"\n')!
	os.setenv(settings_env, settings, true)
	os.setenv(project_env, root, true)
	os.setenv(version_env, '', true)
	defer {
		os.setenv(settings_env, '', true)
		os.setenv(project_env, '', true)
		os.setenv(version_env, '', true)
	}
	assert select().version == 'v8.8.8'
	os.rm(settings)!
	fallback := select()
	assert fallback.version == 'master'
	assert fallback.source == 'default'
}

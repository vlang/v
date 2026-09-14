module main

import os

fn test_compiler_selection_flags_are_not_forwarded() {
	assert clean_compiler_selection_flags(['-silent', '-new-compiler', 'main.v']) == [
		'-silent',
		'main.v',
	]
	assert clean_compiler_selection_flags(['-old-compiler', 'run', 'main.v']) == [
		'run',
		'main.v',
	]
}

fn test_launcher_finds_the_source_root() {
	root := find_vroot(@FILE) or { panic(err) }
	assert os.is_file(os.join_path(root, 'GNUmakefile'))
	assert os.is_dir(os.join_path(root, 'vlib', 'v'))
	directory_root := find_vroot(root) or { panic(err) }
	assert directory_root == root
}

fn test_launcher_finds_external_commands() {
	index, command := find_command(['-cc', 'clang', 'fmt', '-w', 'main.v'])
	assert index == 2
	assert command == 'fmt'
	missing_index, missing := find_command(['-silent', 'main.v'])
	assert missing_index == -1
	assert missing == ''
	program_arg_index, program_arg := find_command(['run', 'main.v', 'fmt'])
	assert program_arg_index == -1
	assert program_arg == ''
	option_value_index, option_value := find_command(['-o', 'fmt', 'main.v'])
	assert option_value_index == -1
	assert option_value == ''
}

fn test_json_quote_escapes_report_content() {
	assert json_quote('a\n"b"\\c\t') == '"a\\n\\"b\\"\\\\c\\t"'
}

fn test_v1_fallback_installer_exposes_crypto_subtle() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	compatibility := source.all_after('install_crypto_subtle_compatibility() {').all_before('\n}')
	assert compatibility.contains('vlib/crypto/internal/subtle')
	assert compatibility.contains('vlib/crypto/subtle')
	assert compatibility.contains('aliasing.v')
	assert compatibility.contains('comparison.v')
	assert source.count('install_crypto_subtle_compatibility "$cache_root" || return 1') == 2
	assert source.contains('mkdir -p ./vlib/crypto/subtle')
	assert source.contains('mkdir .\\vlib\\crypto\\subtle')
}

fn test_v1_fallback_resolution_requires_crypto_subtle_compatibility() {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_crypto_subtle_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	fallback := os.join_path(root, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	fallback_root := os.join_path(root, 'release')
	cached_fallback := os.join_path(fallback_root, 'v' + $if windows { '.exe' } $else { '' })
	os.mkdir_all(fallback_root)!
	os.cp(@VEXE, fallback)!
	os.cp(@VEXE, cached_fallback)!
	os.chmod(fallback, 0o755)!
	os.chmod(cached_fallback, 0o755)!
	os.write_file(fallback + '.vroot', fallback_root)!
	assert resolve_v1_fallback(fallback) == none
	module_dir := os.join_path(fallback_root, 'vlib', 'crypto', 'subtle')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'aliasing.v'), 'module subtle\n')!
	assert resolve_v1_fallback(fallback) == none
	os.write_file(os.join_path(module_dir, 'comparison.v'), 'module subtle\n')!
	assert resolve_v1_fallback(fallback) or { panic(err) } == cached_fallback
}

fn test_cached_fallback_root_is_preferred_when_installed() {
	root := find_vroot(@FILE) or { panic(err) }
	fallback := os.join_path(root, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	root_file := fallback + '.vroot'
	if !os.is_executable(fallback) || !os.is_file(root_file) {
		return
	}
	resolved := ensure_v1_fallback('test') or { panic(err) }
	assert os.dir(resolved) == os.read_file(root_file)!.trim_space()
}

fn test_fallback_failure_notes_name_the_stage_v_stopped_in() {
	notes := v1_fallback_failure_notes('compiler_error\nsemantic checking')
	assert notes.len == 2
	assert notes[0].contains('compatibility compiler failed too')
	assert notes[1].contains('V stopped during semantic checking')
	assert notes[1].contains('-new-compiler')
	stageless := v1_fallback_failure_notes('inline_asm')
	assert stageless[1].contains('V stopped and kept its diagnostics quiet')
}

fn fake_v1_fallback_tree(tag string) string {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_${tag}_${os.getpid()}')
	os.rmdir_all(root) or {}
	previous := os.join_path(root, 'vlib', 'x', 'json2')
	os.mkdir_all(os.join_path(previous, 'decoder2')) or { panic(err) }
	os.write_file(os.join_path(previous, 'json2.v'), 'module json2\n') or { panic(err) }
	os.write_file(os.join_path(previous, 'decoder2', 'decoder.v'), 'module decoder2\n') or {
		panic(err)
	}
	return root
}

fn test_moved_modules_are_staged_into_a_writable_overlay() {
	root := fake_v1_fallback_tree('overlay')
	overlay_home := os.join_path(root, 'writable_home')
	os.mkdir_all(overlay_home)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.setenv('XDG_CACHE_HOME', overlay_home, true)
	defer {
		os.unsetenv('XDG_CACHE_HOME')
	}

	overlay := v1_fallback_module_overlay(root) or { panic('expected an overlay') }
	assert overlay.starts_with(overlay_home)
	assert os.read_file(os.join_path(overlay, 'json2', 'json2.v'))! == 'module json2\n'
	assert os.read_file(os.join_path(overlay, 'json2', 'decoder2', 'decoder.v'))! == 'module decoder2\n'
	// The fallback tree itself must stay exactly as it was found.
	assert os.ls(os.join_path(root, 'vlib'))! == ['x']
	// Staging directories must not survive as importable modules.
	assert os.ls(overlay)!.filter(it.starts_with('.')).len == 0

	// A staged overlay is reused as is, rather than copied over every launch.
	os.write_file(os.join_path(overlay, 'json2', 'json2.v'), 'module json2 // kept\n')!
	reused := v1_fallback_module_overlay(root) or { panic('expected an overlay') }
	assert reused == overlay
	assert os.read_file(os.join_path(overlay, 'json2', 'json2.v'))! == 'module json2 // kept\n'
}

fn test_a_read_only_fallback_tree_still_gets_its_moved_modules() {
	root := fake_v1_fallback_tree('readonly')
	overlay_home := os.join_path(os.vtmp_dir(), 'v1_fallback_readonly_home_${os.getpid()}')
	os.rmdir_all(overlay_home) or {}
	os.mkdir_all(overlay_home)!
	vlib_dir := os.join_path(root, 'vlib')
	os.chmod(vlib_dir, 0o500)!
	defer {
		os.chmod(vlib_dir, 0o700) or {}
		os.rmdir_all(root) or {}
		os.rmdir_all(overlay_home) or {}
	}
	os.setenv('XDG_CACHE_HOME', overlay_home, true)
	defer {
		os.unsetenv('XDG_CACHE_HOME')
	}

	overlay := v1_fallback_module_overlay(root) or { panic('expected an overlay') }
	assert os.read_file(os.join_path(overlay, 'json2', 'json2.v'))! == 'module json2\n'
	assert os.ls(vlib_dir)! == ['x']
}

fn test_a_fallback_tree_that_already_carries_the_module_needs_no_overlay() {
	root := fake_v1_fallback_tree('current')
	os.mkdir_all(os.join_path(root, 'vlib', 'json2'))!
	defer {
		os.rmdir_all(root) or {}
	}
	if _ := v1_fallback_module_overlay(root) {
		assert false, 'a tree that carries `json2` needs no overlay'
	}
}

fn test_staging_skips_a_fallback_tree_without_the_previous_location() {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_nostage_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'vlib'))!
	if _ := v1_fallback_module_overlay(root) {
		assert false, 'a tree without `x/json2` has nothing to stage'
	}
}

fn test_a_host_with_nowhere_to_stage_reports_it_instead_of_going_quiet() {
	if os.getuid() == 0 {
		// root writes through the mode bits this case is built from.
		return
	}
	root := fake_v1_fallback_tree('nowhere')
	blocked := os.join_path(os.vtmp_dir(), 'v1_fallback_blocked_${os.getpid()}')
	os.rmdir_all(blocked) or {}
	os.mkdir_all(blocked)!
	os.chmod(blocked, 0o500)!
	defer {
		os.chmod(blocked, 0o700) or {}
		os.rmdir_all(blocked) or {}
		os.rmdir_all(root) or {}
	}

	if _ := v1_fallback_module_overlay_in(root, [os.join_path(blocked, 'overlay')]) {
		assert false, 'nothing is writable, so there is no overlay to return'
	}
	// A later candidate that does accept the copy is still used.
	usable := os.join_path(root, 'usable_overlay')
	assert v1_fallback_module_overlay_in(root, [os.join_path(blocked, 'overlay'), usable])? == usable
	assert os.read_file(os.join_path(usable, 'json2', 'json2.v'))! == 'module json2\n'
}

fn test_the_overlay_is_looked_for_in_the_cache_before_the_temporary_directory() {
	cache := os.join_path(os.vtmp_dir(), 'v1_fallback_xdg_${os.getpid()}')
	os.setenv('XDG_CACHE_HOME', cache, true)
	defer {
		os.unsetenv('XDG_CACHE_HOME')
	}
	dirs := v1_fallback_overlay_dirs()
	assert dirs.len >= 2
	assert dirs[0] == os.join_path(cache, 'v', 'v1-fallback-modules', v_version)
	assert dirs.last().starts_with(os.temp_dir())
}

fn test_the_overlay_is_searched_after_the_module_paths_the_user_has() {
	os.setenv('VMODULES', ['/one', '/two'].join(os.path_delimiter), true)
	defer {
		os.unsetenv('VMODULES')
	}
	assert v1_fallback_vmodules_env('/overlay') == ['/one', '/two', '/overlay'].join(os.path_delimiter)
	// An overlay that is already on the list must not be repeated.
	assert v1_fallback_vmodules_env('/two') == ['/one', '/two'].join(os.path_delimiter)
}

fn test_fallback_installer_writes_a_native_windows_root() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	writer := source.all_after('write_candidate_root() {').all_before('\n}\n\nsha256_of()')
	assert writer.contains('MSYS*|MINGW*')
	assert writer.contains('cygpath -w')
	assert writer.contains('pwd -W')
	assert source.count('write_candidate_root || return 1') == 2
}

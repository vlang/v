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

fn test_windows_makev_keeps_the_v3_tcc_root_absolute() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'makev.bat'))!
	assert source.contains('set V_FALLBACK_CC_ARGS=-cc "!tcc_exe!"')
	assert source.contains('"%V_BOOTSTRAP%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc -cc "!tcc_exe!" -o "%V_STAGE%" cmd/v')
	assert source.contains('if !ERRORLEVEL! EQU 0 set stage_vflags=-cc "!tcc_exe!"')
	assert !source.contains('-cflags -Bthirdparty/tcc')
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

fn test_v1_fallback_installer_exposes_compatibility_modules() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	crypto := source.all_after('install_crypto_subtle_compatibility() {').all_before('\n}')
	assert crypto.contains('vlib/crypto/internal/subtle')
	assert crypto.contains('vlib/crypto/subtle')
	assert crypto.contains('aliasing.v')
	assert crypto.contains('comparison.v')
	assert source.contains('install_crypto_subtle_compatibility "$1" || return 1')
	assert source.contains('mkdir -p ./vlib/crypto/subtle')
	assert source.contains('mkdir .\\vlib\\crypto\\subtle')
	moved := source.all_after('install_moved_module_compatibility() {').all_before('\n}')
	assert moved.contains('vlib/x/json2')
	assert moved.contains('vlib/json2')
	assert source.contains('install_moved_module_compatibility "$1" || return 1')
	assert source.contains(r'bootstrap_v=${V1_FALLBACK_BOOTSTRAP:-$1}')
	assert source.contains(r'fallback_output=${V1_FALLBACK_OUTPUT:-$2}')
	assert source.contains('install_fallback_compatibility "$cache_root" || return 1')
	assert source.contains('install_fallback_compatibility "$staged_cache" || return 1')
	assert source.contains('cp -R ./vlib/x/json2 ./vlib/json2')
	assert source.contains('xcopy /E /I /Y .\\vlib\\x\\json2 .\\vlib\\json2')
	assert source.contains('acquire_cache_lock || exit 1')
	lock_index := source.index('acquire_cache_lock || exit 1') or { -1 }
	install_index := source.index('if use_cached_release') or { -1 }
	assert lock_index >= 0
	assert lock_index < install_index
	assert source.contains('cache_lock_owner=$(mktemp')
	assert source.contains('cache_lock_probe=$cache_lock_owner.probe')
	assert source.contains('kill -0 "$existing_pid"')
	assert source.contains('process_identity "$existing_pid"')
	assert source.contains('if [ ! -e "$cache_lock" ]')
	assert source.contains('$cache_lock.reclaim-$stale_owner')
	assert source.contains('ln "$cache_lock_owner" "$reclaim"')
	assert source.contains('reclaim_pid=$(sed -n')
	assert source.contains('fallback_compatibility_is_installed "$cache_root"')
}

fn test_v1_fallback_resolution_requires_compatibility_modules() {
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
	assert resolve_v1_fallback(fallback) == none
	json2_dir := os.join_path(fallback_root, 'vlib', 'json2')
	os.mkdir_all(json2_dir)!
	os.write_file(os.join_path(json2_dir, 'json2.v'), 'module json2\n')!
	assert resolve_v1_fallback(fallback) or { panic(err) } == cached_fallback
	legacy := os.join_path(root, 'legacy_' + v1_fallback_binary + $if windows { '.exe' } $else { '' })
	legacy_root := os.join_path(root, 'legacy_release')
	os.mkdir_all(legacy_root)!
	os.cp(@VEXE, legacy)!
	os.chmod(legacy, 0o755)!
	os.write_file(legacy + '.vroot', legacy_root)!
	assert resolve_installed_v1_fallback(legacy, fallback) or { panic(err) } == cached_fallback
}

fn test_v1_fallback_cached_launcher_uses_the_configured_cache() {
	configured := 'configured_v1_cache_${os.getpid()}'
	previous := os.getenv_opt('V1_FALLBACK_CACHE_DIR')
	os.setenv('V1_FALLBACK_CACHE_DIR', configured, true)
	defer {
		if value := previous {
			os.setenv('V1_FALLBACK_CACHE_DIR', value, true)
		} else {
			os.unsetenv('V1_FALLBACK_CACHE_DIR')
		}
	}
	cache_parent := v1_fallback_cache_parent()!
	assert v1_fallback_cached_launcher(cache_parent) == os.join_path(os.abs_path(configured), v_version, v1_fallback_binary + $if windows { '.exe' } $else { '' })
}

fn test_v1_fallback_make_environment_preserves_special_path_characters() {
	bootstrap := os.join_path(os.vtmp_dir(), "o'connor", r'$compiler')
	cache_parent := os.join_path(os.vtmp_dir(), "o'connor", r'$cache')
	output := os.join_path(cache_parent, v_version, r'$fallback')
	environment := v1_fallback_make_environment(bootstrap, cache_parent, output)
	assert environment['VEXE'] == './v'
	assert environment['V1_FALLBACK_BOOTSTRAP'] == bootstrap
	assert environment['V1_FALLBACK_CACHE_DIR'] == cache_parent
	assert environment['V1_FALLBACK_OUTPUT'] == output
}

fn test_v1_fallback_cache_without_a_home_is_private() {
	previous_cache := os.getenv_opt('V1_FALLBACK_CACHE_DIR')
	previous_xdg := os.getenv_opt('XDG_CACHE_HOME')
	previous_home := os.getenv_opt('HOME')
	os.unsetenv('V1_FALLBACK_CACHE_DIR')
	os.unsetenv('XDG_CACHE_HOME')
	os.unsetenv('HOME')
	defer {
		restore_environment('V1_FALLBACK_CACHE_DIR', previous_cache)
		restore_environment('XDG_CACHE_HOME', previous_xdg)
		restore_environment('HOME', previous_home)
	}
	first := v1_fallback_cache_parent()!
	second := v1_fallback_cache_parent()!
	assert first == second
	assert first == os.join_path(os.temp_dir(), 'v1-fallback-cache-${os.geteuid()}')
	assert os.is_dir(first)
	assert !os.is_link(first)
	$if !windows {
		attributes := os.lstat(first)!
		assert attributes.uid == u32(os.geteuid())
		assert attributes.get_mode().bitmask() == 0o700
	}
}

fn test_v1_fallback_private_temp_cache_rejects_a_symlink() {
	$if !windows {
		base := os.join_path(os.vtmp_dir(), 'v1_fallback_unsafe_cache_${os.getpid()}')
		target := os.join_path(base, 'target')
		candidate := os.join_path(base, 'v1-fallback-cache-${os.geteuid()}')
		os.rmdir_all(base) or {}
		defer {
			os.rmdir_all(base) or {}
		}
		os.mkdir_all(target)!
		os.symlink(target, candidate)!
		if unsafe_cache := v1_fallback_private_temp_cache_parent(base) {
			assert false, 'accepted unsafe fallback cache `${unsafe_cache}`'
		} else {
			assert err.msg().contains('expected a real directory')
		}
		os.rm(candidate)!
		os.mkdir(candidate)!
		os.chmod(candidate, 0o755)!
		if permissive_cache := v1_fallback_private_temp_cache_parent(base) {
			assert false, 'accepted unsafe fallback cache `${permissive_cache}`'
		} else {
			assert err.msg().contains('expected user-owned mode 0700')
		}
	}
}

fn restore_environment(name string, previous ?string) {
	if original := previous {
		os.setenv(name, original, true)
	} else {
		os.unsetenv(name)
	}
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

fn test_fallback_failure_notes_are_only_reported_for_compile_only_commands() {
	assert v1_fallback_exit_identifies_compiler_failure(['main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-prod', '-o', 'app', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['build', 'main.v'])

	assert !v1_fallback_exit_identifies_compiler_failure(['run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-prod', 'run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['test', 'vlib/context'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-profile', 'run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-profile', 'test', 'vlib/context'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-profile', 'trace.out', 'run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['example_test.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['example_test.c.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-b', 'js', 'example_test.js.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-b', 'js_node', 'example_test.js.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-backend=js_browser',
		'example_test.js.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-backend=wasm', 'example_test.wasm.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['script.vsh'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-e', 'exit(1)'])
	assert !v1_fallback_exit_identifies_compiler_failure(['fmt', '-verify', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-'])

	assert v1_fallback_exit_identifies_compiler_failure(['-profile', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-profile', 'trace.out', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-b', 'js', 'example_test.c.v'])
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

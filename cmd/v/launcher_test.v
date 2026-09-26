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

fn test_external_tool_build_args_drop_non_binary_modes() {
	assert external_tool_build_args('vfmt', ['-silent', '-N', '-W', '-check']) == [
		'-silent',
		'-N',
		'-W',
	]
	assert external_tool_build_args('vfmt', ['-new-compiler', '-c', '-cc', 'clang']) == [
		'-cc',
		'clang',
	]
}

fn test_diagnostic_external_tool_build_args_disable_gc() {
	for tool_name in ['vself', 'vup', 'vdoctor', 'vsymlink'] {
		assert external_tool_compile_args(tool_name, ['-prod']) == ['-prod', '-g', '-gc', 'none']
		assert external_tool_compile_args(tool_name, ['-gc', 'boehm', '-prod']) == [
			'-prod',
			'-g',
			'-gc',
			'none',
		]
		assert external_tool_compile_args(tool_name, ['-gc=boehm', '-prod']) == [
			'-prod',
			'-g',
			'-gc',
			'none',
		]
	}
	assert external_tool_compile_args('vfmt', ['-prod']) == ['-prod']
}

fn test_tools_that_consume_prefix_compiler_options_receive_them() {
	prefix := ['-silent', '-N', '-W', '-check']
	assert external_tool_runtime_args('build-tools', prefix, ['build-tools']) == [
		'-silent',
		'-N',
		'-W',
		'-check',
		'build-tools',
	]
	assert external_tool_runtime_args('build-examples', ['-no-memory-limit'], ['build-examples']) == [
		'-no-memory-limit',
		'build-examples',
	]
	assert external_tool_runtime_args('self', ['-prod'], ['self']) == ['-prod', 'self']
	assert external_tool_runtime_args('test-self', ['-no-memory-limit', '-silent'], [
		'test-self',
		'vlib',
	]) == ['-no-memory-limit', '-silent', 'test-self', 'vlib']
	assert external_tool_runtime_args('fmt', prefix, ['fmt', '-verify', 'file.v']) == [
		'fmt',
		'-verify',
		'file.v',
	]
}

fn test_ownership_compiler_is_selected_only_for_explicit_modes() {
	assert ownership_compiler_is_required(['-autofree', 'main.v'])
	assert ownership_compiler_is_required(['-ownership', 'main.v'])
	assert ownership_compiler_is_required(['--ownership', 'main.v'])
	assert ownership_compiler_is_required(['-d', 'ownership', 'main.v'])
	assert ownership_compiler_is_required(['-define', 'ownership=on', 'main.v'])
	assert ownership_compiler_is_required(['-downership', 'main.v'])
	assert !ownership_compiler_is_required(['main.v'])
	assert !ownership_compiler_is_required(['-d', 'autofree', 'main.v'])
	assert !ownership_compiler_is_required(['run', 'ownership'])
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
	test_index, test_command := find_command(['-silent', 'test', 'vlib/builtin', 'vlib/os'])
	assert test_index == 1
	assert test_command == 'test'
}

fn test_external_tool_source_prefers_an_executable_file() {
	root := os.join_path(os.vtmp_dir(), 'external_tool_source_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	base := os.join_path(root, 'vshare')
	os.mkdir_all(base)!
	os.write_file(base + '.v', 'module main\n')!
	assert find_external_tool_source(base)? == base + '.v'
	os.rm(base + '.v')!
	assert find_external_tool_source(base)? == base
	assert find_external_tool_source(os.join_path(root, 'missing')) == none
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
	assert source.contains('compatibility_marker=.v1-fallback-complete')
	assert source.contains('rm -f "$marker" || return 1')
	assert source.contains('> "$marker" || return 1')
	assert source.contains('validate_trusted_temp_root "$temp_root" || exit 1')
	assert source.contains('validate_private_directory "$work_dir" || exit 1')
	assert source.contains('cache_parent=$(private_temp_cache_parent "$temp_root") || exit 1')
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
	assert resolve_v1_fallback(fallback) == none
	os.write_file(os.join_path(json2_dir, v1_fallback_compatibility_marker), '${v_version}\n')!
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
	$if windows {
		defer {
			os.rmdir(first) or {}
			os.rmdir(second) or {}
		}
		assert first != second
		assert os.is_dir(first)
		assert os.is_dir(second)
		windows_source := os.read_file(os.join_path(find_vroot(@FILE)!, 'cmd', 'v', 'v1_fallback_cache_windows.c.v'))!
		assert windows_source.contains('rand.bytes(16)')
		assert windows_source.contains('os.mkdir(candidate, mode: 0o700)')
	} $else {
		assert first == second
		assert first == os.join_path(os.temp_dir(), 'v1-fallback-cache-${os.geteuid()}')
		assert os.is_dir(first)
		assert !os.is_link(first)
		attributes := os.lstat(first)!
		assert attributes.uid == u32(os.geteuid())
		assert attributes.get_mode().bitmask() == 0o700
	}
}

fn test_v1_fallback_temp_root_requires_a_trusted_owner() {
	assert v1_fallback_temp_root_owner_is_trusted(0, 501)
	assert v1_fallback_temp_root_owner_is_trusted(501, 501)
	assert !v1_fallback_temp_root_owner_is_trusted(502, 501)
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

fn test_fallback_exit_notes_name_the_stage_v_stopped_in() {
	notes := v1_fallback_exit_notes('compiler_error\nsemantic checking', true)
	assert notes.len == 2
	assert notes[0].contains('compatibility compiler failed too')
	assert notes[1].contains('V stopped during semantic checking')
	assert notes[1].contains('-new-compiler')
	stageless := v1_fallback_exit_notes('inline_asm', true)
	assert stageless[1].contains('V stopped and kept its diagnostics quiet')
	ambiguous := v1_fallback_exit_notes('compiler_error\nsemantic checking', false)
	assert ambiguous[0].contains('compatibility retry exited unsuccessfully')
	assert ambiguous[0].contains('any errors above are its own')
	assert ambiguous[0].contains('exit status may instead come from the program')
	assert !ambiguous[0].contains('compiler failed too')
	assert ambiguous[1] == notes[1]
}

fn run_launcher_test_process(executable string, args []string, work_dir string,
	environment map[string]string) os.Result {
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	result := os.Result{
		exit_code: process.code
		output:    process.stdout_slurp() + process.stderr_slurp()
	}
	process.close()
	return result
}

fn test_external_tools_do_not_use_the_v1_fallback() {
	$if !windows {
		false_executable := os.find_abs_path_of_executable('false') or { return }
		dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
			os.join_path(os.dir(@VEXE), 'v')
		} else {
			@VEXE
		}
		if !os.is_executable(dispatcher) {
			return
		}
		cache := os.join_path(os.vtmp_dir(), 'v3_external_tool_failure_${os.getpid()}')
		os.rmdir_all(cache) or {}
		os.mkdir_all(cache)!
		defer {
			os.rmdir_all(cache) or {}
		}
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['VTOOLS_CACHE_DIR'] = cache
		environment['VTOOLS_NO_CACHE'] = ''
		environment['V_MACOS_V3_NO_FALLBACK'] = ''
		result := run_launcher_test_process(dispatcher, ['-cc', false_executable, 'timeout', '1',
			dispatcher, 'version'], os.dir(dispatcher), environment)
		assert result.exit_code != 0, result.output
		assert !result.output.contains('retrying with'), result.output
	}
}

fn test_failed_run_retry_explains_how_to_show_v3_diagnostics() {
	dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	fallback := os.join_path(os.dir(dispatcher), v1_fallback_binary + $if windows { '.exe' } $else { '' })
	if !os.is_executable(dispatcher) || !os.is_executable(fallback) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_failed_run_retry_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import time\n\nfn main() {\n\ttimer := time.new_timer(time.nanosecond)\n\t_ = timer\n\tmissing_v3_failure()\n}\n')!
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	environment['V_MACOS_V3_NO_FALLBACK'] = ''
	retried := run_launcher_test_process(dispatcher, ['-nocache', '-no-parallel', 'run', source], os.dir(dispatcher), environment)
	assert retried.exit_code == 1, retried.output
	assert retried.output.contains('unknown function: time.new_timer'), retried.output
	assert retried.output.contains('compatibility retry exited unsuccessfully'), retried.output
	assert retried.output.contains('any errors above are its own'), retried.output
	assert retried.output.contains('exit status may instead come from the program'), retried.output
	assert retried.output.contains('V stopped during semantic checking'), retried.output
	assert retried.output.contains('re-run with `-new-compiler`'), retried.output

	strict := run_launcher_test_process(dispatcher, ['-new-compiler', '-nocache', '-no-parallel',
		'run', source], os.dir(dispatcher), environment)
	assert strict.exit_code == 1, strict.output
	assert strict.output.contains('unknown function `missing_v3_failure`'), strict.output
	assert !strict.output.contains('unknown function: time.new_timer'), strict.output
	assert !strict.output.contains('compatibility retry'), strict.output
}

fn test_fallback_exit_classifies_compile_only_commands() {
	previous_norun := os.getenv_opt('VNORUN')
	os.unsetenv('VNORUN')
	defer {
		restore_environment('VNORUN', previous_norun)
	}
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
	assert !v1_fallback_exit_identifies_compiler_failure(['-backend=js_browser', 'example_test.js.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-backend=wasm', 'example_test.wasm.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['script.vsh'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-e', 'exit(1)'])
	assert !v1_fallback_exit_identifies_compiler_failure(['fmt', '-verify', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-'])

	assert v1_fallback_exit_identifies_compiler_failure(['-profile', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-profile', 'trace.out', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-b', 'js', 'example_test.c.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-skip-running', 'example_test.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-skip-running', 'script.vsh'])
	assert v1_fallback_exit_identifies_compiler_failure(['-skip-running', 'run', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-skip-running', 'crun', 'main.c'])
	assert v1_fallback_exit_identifies_compiler_failure(['-check', 'example_test.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-check', 'run', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-check-syntax', 'script.vsh'])
	assert v1_fallback_exit_identifies_compiler_failure(['-generate-c-project', 'generated', 'run',
		'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-o', '-', 'run', 'main.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['-o', 'generated.c', 'run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-o', 'generated.js', 'run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-b', 'js', '-o', 'generated.c', 'run',
		'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['run', 'main.v', '-skip-running'])
	assert !v1_fallback_exit_identifies_compiler_failure(['run', 'main.v', '-generate-c-project',
		'generated'])
	assert v1_fallback_exit_identifies_compiler_failure(['-o', 'generated.c', 'run', 'main.v',
		'-b', 'js'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-o', 'generated.js', 'run', 'main.v',
		'-b', 'c'])
	// V 0.5.2 runs direct tests with explicit executable outputs.
	assert !v1_fallback_exit_identifies_compiler_failure(['-o', 'test-bin', 'example_test.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-output', 'test-bin', 'example_test.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-stats', '-o', 'test-bin', 'example_test.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-checker-fixture', '-output', 'test-bin',
		'example_test.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['-o', 'test-bin', 'example_test.v', '-stats'])
	assert !v1_fallback_exit_identifies_compiler_failure(['example_test.v', '-output', 'test-bin',
		'-checker-fixture'])
	assert v1_fallback_exit_identifies_compiler_failure(['-o', 'test-bin', 'example_test.v',
		'-skip-running', '-stats'])
	assert !v1_fallback_exit_identifies_compiler_failure(['example_test.js.v', '-b', 'js'])
	os.setenv('VNORUN', 'yes', true)
	assert v1_fallback_exit_identifies_compiler_failure(['example_test.v'])
	assert v1_fallback_exit_identifies_compiler_failure(['script.vsh'])
	assert v1_fallback_exit_identifies_compiler_failure(['run', 'main.v'])
	assert !v1_fallback_exit_identifies_compiler_failure(['test', 'vlib/context'])
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

fn test_install_external_tool_modules_leaves_a_build_with_a_path_to_the_compiler() {
	$if windows {
		return
	}
	base := os.join_path(os.vtmp_dir(), 'launcher_tool_modules_${os.getpid()}')
	os.rmdir_all(base) or {}
	vmodules := os.join_path(base, 'vmodules')
	path_root := os.join_path(base, 'path')
	tool_source := os.join_path(base, 'vdoc')
	os.mkdir_all(vmodules)!
	os.mkdir_all(path_root)!
	os.mkdir_all(tool_source)!
	// This stands in for `v retry -- git clone ...`, and records every install attempt. It
	// fails, so an attempt would also make install_external_tool_modules exit.
	attempts := os.join_path(base, 'install_attempts')
	fake_vexe := os.join_path(base, 'fake_v')
	os.write_file(fake_vexe, '#!/bin/sh\necho "\$*" >> ${os.quoted_path(attempts)}\nexit 1\n')!
	os.chmod(fake_vexe, 0o755)!
	previous_vmodules := os.getenv_opt('VMODULES')
	previous_vexe := os.getenv_opt('VEXE')
	previous_sandboxed := os.getenv_opt('VTEST_SANDBOXED_PACKAGING')
	os.setenv('VMODULES', vmodules, true)
	os.setenv('VEXE', fake_vexe, true)
	os.unsetenv('VTEST_SANDBOXED_PACKAGING')
	defer {
		restore_environment('VMODULES', previous_vmodules)
		restore_environment('VEXE', previous_vexe)
		restore_environment('VTEST_SANDBOXED_PACKAGING', previous_sandboxed)
		os.rmdir_all(base) or {}
	}
	// `markdown` is in none of the roots, but a `-path` replaces VMODULES, so installing it
	// there would not help. The compiler reports the missing module instead.
	install_external_tool_modules('vdoc', tool_source, ['-path', path_root])
	assert !os.exists(attempts)
}

module main

import os
import v.pref

fn test_native_backend_aliases_select_the_host_backend() {
	expected := $if amd64 { 'amd64' } $else $if arm64 { 'arm64' } $else { 'native' }
	assert pref.native_backend() == expected
	for flags in [['-native'], ['-b', 'native'], ['-backend', 'native'], ['-b=native'],
		['-backend=native']] {
		for suffix in [['main.v'], ['run', 'main.v'], ['test', '.'], ['self']] {
			mut args := flags.clone()
			args << suffix
			assert selected_backend(compiler_args_before_program(args)) == expected
			assert full_compiler_is_required(compiler_args_before_program(args)) ==
				full_compiler_is_required(['-b', expected])
		}
	}
	assert selected_backend(['-native', '-b', 'c', 'main.v']) == 'c'
	assert selected_backend(['-b', 'c', '-native', 'main.v']) == expected
}

fn test_native_backend_aliases_select_the_requested_architecture() {
	for arch, expected in {
		'amd64':   'amd64'
		'arm64':   'arm64'
		'riscv64': 'native'
	} {
		for flags in [['-native'], ['-b', 'native'], ['-backend', 'native'], ['-b=native'],
			['-backend=native']] {
			mut arch_first := ['-arch', arch]
			arch_first << flags
			mut arch_last := flags.clone()
			arch_last << ['-arch', arch]
			for args in [arch_first, arch_last] {
				assert selected_backend(args) == expected, args.str()
				if expected != 'native' {
					assert full_compiler_is_required(args) == full_compiler_is_required([
						'-b',
						expected,
					])
				} else {
					assert !full_compiler_is_required(args)
				}
			}
		}
	}
	assert selected_backend(['-native', '-arch', 'amd64', '-arch', 'arm64']) == 'arm64'
	assert selected_backend(['-arch', 'arm64', '-native', '-arch', 'amd64']) == 'amd64'
	assert selected_backend(['-arch', 'arm64', '-native', '-b', 'c']) == 'c'
	assert selected_backend(['-arch', 'arm64', '-b', 'x64']) == 'x64'
	assert selected_backend(['-arch', 'amd64', '-b', 'arm64']) == 'arm64'
}

fn test_native_backend_architecture_preserves_argument_boundaries() {
	expected := pref.native_backend()
	for option in ['-cflags', '-ldflags', '-o', '-d', '-compile-backend'] {
		assert selected_backend(['-native', option, '-arch', 'main.v']) == expected
	}
	for prefix in [['run', 'main.v'], ['crun', 'main.v'], ['script.vsh'], ['-e', 'println(1)'],
		['-e=println(1)']] {
		mut args := ['-native']
		args << prefix
		args << ['-arch', 'arm64']
		assert selected_backend(compiler_args_before_program(args)) == expected
	}
}

fn test_native_backend_aliases_preserve_argument_boundaries() {
	for literal in ['-native', '-b=native', '-backend=native', 'native'] {
		for option in ['-cflags', '-ldflags', '-o', '-d', '-compile-backend'] {
			args := [option, literal, 'main.v']
			assert selected_backend(args) == 'c'
		}
		for prefix in [['run', 'main.v'], ['crun', 'main.v'], ['script.vsh'], ['-e', 'println(1)'],
			['-e=println(1)']] {
			mut args := prefix.clone()
			args << literal
			assert selected_backend(compiler_args_before_program(args)) == 'c'
		}
	}
}

fn test_native_backend_aliases_preserve_formatter_preferences() {
	for flags in [['-native'], ['-b', 'native'], ['-backend', 'native'], ['-b=native'],
		['-backend=native']] {
		mut args := flags.clone()
		args << ['fmt', 'main.v']
		index, _ := find_command(args)
		prefix := args[..index].clone()
		assert external_tool_compile_args('vfmt', prefix) == []string{}
		expected := if flags == ['-native'] { ['-b', 'native', 'fmt', 'main.v'] } else { args }
		assert external_tool_runtime_args('fmt', prefix, args[index..]) == expected
	}
}

fn test_native_backend_aliases_reach_self_and_test_without_targeting_the_helper() {
	for command in ['self', 'test'] {
		args := ['-native', command]
		index, _ := find_command(args)
		prefix := args[..index]
		assert selected_backend(external_tool_compile_args('v${command}', prefix)) == 'c'
		assert external_tool_runtime_args(command, prefix, args[index..]) == args
	}
}

fn test_external_tool_build_args_drop_non_binary_modes() {
	assert external_tool_build_args('vfmt', ['-cross', '-os', 'windows', '-arch', 'x64']) == []string{}
	assert external_tool_build_args('vfmt', ['-silent', '-N', '-W', '-check', '-check-syntax']) == [
		'-silent',
		'-N',
		'-W',
	]
	assert external_tool_build_args('vfmt', ['-c', '-cc', 'clang']) == [
		'-cc',
		'clang',
	]
}

fn test_vfmt_build_args_leave_module_paths_to_the_tool() {
	// vfmt reads `-path` from VFLAGS at run time; it is no reason to rebuild the tool.
	assert external_tool_build_args('vfmt', ['-path', '/tmp/mods|@vlib', '-cc', 'clang']) == [
		'-cc',
		'clang',
	]
	assert external_tool_build_args('vfmt', ['-cflags', '-path', '-path', '@vlib']) == [
		'-cflags',
		'-path',
	]
	assert external_tool_build_args('vvet', ['-path', '@vlib']) == ['-path', '@vlib']
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
	assert external_tool_runtime_args('watch', ['-d', 'veb_livereload'], ['watch', 'run', '.']) == [
		'-d',
		'veb_livereload',
		'watch',
		'run',
		'.',
	]
}

fn test_self_launcher_keeps_replacement_backend_options_out_of_helper_build() {
	prefix := ['-b', 'fastc', '-os', 'linux', '-arch', 'arm64', '-silent']
	assert external_tool_compile_args('vself', prefix) == ['-silent', '-g', '-gc', 'none']
	assert external_tool_runtime_args('self', prefix, ['self']) == [
		'-b',
		'fastc',
		'-os',
		'linux',
		'-arch',
		'arm64',
		'-silent',
		'self',
	]
}

fn test_retry_runtime_args_preserve_a_command_named_retry() {
	assert external_tool_runtime_args('retry', []string{}, ['retry', '-r', '1', '--', 'retry',
		'argument']) == ['-r', '1', '--', 'retry', 'argument']
	assert external_tool_runtime_args('retry', []string{}, ['retry', 'retry', 'argument']) == [
		'retry',
		'argument',
	]
	assert external_tool_runtime_args('retry', []string{}, ['retry']) == []string{}
}

fn test_tools_that_own_their_name_receive_only_their_arguments() {
	for name in ['cover', 'diff', 'download', 'gret', 'repeat', 'time', 'timeout'] {
		assert external_tool_runtime_args(name, []string{}, [name, '--version']) == ['--version'], name
	}
}

fn test_formatter_backend_options_are_runtime_preferences() {
	for flag in ['-b', '-backend'] {
		prefix := ['-cc', 'clang', flag, 'js', '-gc', 'none']
		assert external_tool_compile_args('vfmt', prefix) == ['-cc', 'clang', '-gc', 'none']
		assert external_tool_runtime_args('fmt', prefix, ['fmt', 'source.v']) == [
			flag,
			'js',
			'fmt',
			'source.v',
		]
		assert external_tool_compile_args('vtest', prefix) == ['-cc', 'clang', '-gc', 'none']
		joined_prefix := ['-cc', 'clang', '${flag}=js', '-gc', 'none']
		assert external_tool_compile_args('vfmt', joined_prefix) == ['-cc', 'clang', '-gc', 'none']
		assert external_tool_runtime_args('fmt', joined_prefix, ['fmt', 'source.v']) == [
			'${flag}=js',
			'fmt',
			'source.v',
		]
		assert external_tool_compile_args('vtest', joined_prefix) == ['-cc', 'clang', '-gc', 'none']
	}
}

fn test_formatter_target_options_do_not_target_its_executable() {
	assert external_tool_compile_args('vfmt', ['-b', 'wasm', '-os', 'browser', '-arch', 'wasm32',
		'-cc', 'clang']) == [
		'-cc',
		'clang',
	]
	assert external_tool_compile_args('vfmt', ['-os=browser', '-arch=wasm32', '-prod']) == [
		'-prod',
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

fn test_ownership_self_build_dispatches_analysis_modes() {
	root := find_vroot(@FILE) or { panic('missing vroot') }
	for compiler_source in [os.join_path(root, 'cmd', 'v'), os.join_path(root, 'cmd', 'v', 'v.v'),
		os.join_path(root, 'vlib', 'v', 'v.v')] {
		for define_args in [
			['-d', 'ownership'],
			['-define', 'ownership=on'],
			['-downership'],
		] {
			mut args := define_args.clone()
			args << compiler_source
			assert ownership_bootstrap_can_use_current_compiler(args, root)
			for mode in ['-autofree', '-ownership', '--ownership'] {
				assert !ownership_bootstrap_can_use_current_compiler([mode, compiler_source], root)
				assert !ownership_bootstrap_can_use_current_compiler([mode, ...args], root)
			}
		}
	}
}

fn test_ownership_compiler_bootstrap_recognizes_each_compiler_entry() {
	root := find_vroot(@FILE) or { panic('missing vroot') }
	for entry in [
		os.join_path(root, 'cmd', 'v'),
		os.join_path(root, 'cmd', 'v', 'v.v'),
		os.join_path(root, 'vlib', 'v', 'v.v'),
	] {
		assert ownership_compiler_bootstrap_input(['-gc', 'none', '-d', 'ownership', entry],
			root), entry
	}
	assert !ownership_compiler_bootstrap_input(['-d', 'ownership', os.join_path(root,
		'vlib', 'v', 'parser', 'parser.v')], root)
	assert !ownership_compiler_bootstrap_input(['-d', 'ownership', 'ordinary.v'], root)
	assert ownership_compiler_bootstrap_input(['-d', 'ownership', os.join_path(root,
		'cmd', 'v', '..', 'v', 'v.v')], root)

	dir := os.join_path(os.vtmp_dir(), 'ownership_bootstrap_entries_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'cmd', 'v'))!
	os.mkdir_all(os.join_path(dir, 'vlib', 'v'))!
	defer { os.rmdir_all(dir) or {} }
	for entry in [
		os.join_path(dir, 'v.v'),
		os.join_path(dir, 'cmd', 'v', 'v.v'),
		os.join_path(dir, 'vlib', 'v', 'v.v'),
	] {
		os.write_file(entry, 'fn main() {}\n')!
		assert !ownership_compiler_bootstrap_input(['-d', 'ownership', entry], root), entry
	}
	assert !ownership_compiler_bootstrap_input(['-d', 'ownership', os.join_path(dir,
		'cmd', 'v')], root)
	assert !ownership_compiler_bootstrap_input(['-d', 'ownership', os.join_path(dir,
		'vlib', 'v')], root)
}

fn test_launcher_finds_the_source_root() {
	root := find_vroot(@FILE) or { panic('missing vroot') }
	assert os.is_file(os.join_path(root, 'GNUmakefile'))
	assert os.is_dir(os.join_path(root, 'vlib', 'v'))
	directory_root := find_vroot(root) or { panic('missing vroot') }
	assert directory_root == root
}

fn test_windows_makev_keeps_the_v3_tcc_root_absolute() {
	root := find_vroot(@FILE) or { panic('missing vroot') }
	source := os.read_file(os.join_path(root, 'makev.bat'))!
	assert source.contains('set tcc_dir=%~dp0thirdparty\\tcc')
	assert source.contains('set tcc_exe=%tcc_dir%\\tcc.exe')
	// The bootstrap compiler emits the stage C, which this script then links.
	// The absolute tcc path reaches it through `!stage_vflags!`, asserted below.
	assert source.contains('"%V_BOOTSTRAP%" %V_BOOTSTRAP_VFLAGS% -gc none -g !stage_vflags! -o "%V_STAGE_C%" cmd/v')
	assert source.contains('set stage_vflags=-cc "!tcc_exe!"')
	// The stage compiler, and the self-hosted fallback, build the updated compiler
	// with `!updated_vflags!`; each TCC route sets it to the absolute tcc path first.
	assert_makev_routine_sets_tcc_before(source, 'build_fresh_v_with_tcc',
		'"%V_STAGE%" %V_BOOTSTRAP_VFLAGS% -keepc -g -showcc !updated_vflags! -o "%V_UPDATED%" cmd/v')
	assert_makev_routine_sets_tcc_before(source, 'build_self_hosted_with_tcc',
		'"%V_EXE%" %V_BOOTSTRAP_VFLAGS% -g !updated_vflags! -o "%V_UPDATED%" cmd/v')
	assert !source.contains('-cflags -Bthirdparty/tcc')
}

// assert_makev_routine_sets_tcc_before checks that the makev.bat subroutine `:name`
// sets `updated_vflags` to the bundled tcc before it runs `command`.
fn assert_makev_routine_sets_tcc_before(source string, name string, command string) {
	text := source.replace('\r\n', '\n')
	start := text.index('\n:${name}\n') or { panic('makev.bat has no `:${name}` subroutine') }
	routine_end := text.index_after('\n:', start + 1) or { text.len }
	routine := text[start..routine_end]
	set_index := routine.index('\nset updated_vflags=-cc "!tcc_exe!"\n') or { -1 }
	command_index := routine.index('\n${command}\n') or { -1 }
	assert set_index >= 0, name
	assert command_index > set_index, name
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

// The agent-facing tools take a subcommand, so `find_command` has to stop at the
// tool name and let the tool read the rest itself.
fn test_launcher_finds_the_agent_facing_commands() {
	mcp_index, mcp_command := find_command(['mcp', 'serve', '--http', ':9090'])
	assert mcp_index == 0
	assert mcp_command == 'mcp'
	skills_index, skills_command := find_command(['-silent', 'skills', 'add', '--all'])
	assert skills_index == 1
	assert skills_command == 'skills'
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

fn restore_environment(name string, previous ?string) {
	if original := previous {
		os.setenv(name, original, true)
	} else {
		os.unsetenv(name)
	}
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

fn test_prod_with_tinycc_is_refused_before_any_compiler_runs() {
	dir := os.join_path(os.vtmp_dir(), 'v_prod_tinycc_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'fn main() {\n\tprintln(1)\n}\n') or { panic(err) }
	output := os.join_path(dir, 'main')
	for args in [['-prod', '-cc', 'tcc'], ['-cc=tinyc', '-prod']] {
		result := os.exec([@VEXE, ...args, '-o', output, source])
		assert result.exit_code == 1, '${args}: ${result.output}'
		assert result.output.contains('tcc is not allowed with -prod'), '${args}: ${result.output}'
	}
}

fn test_prod_refuses_tinycc_under_another_name_once_it_resolves() {
	bundled := os.join_path(os.dir(@VEXE), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(bundled) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_prod_tinycc_alias_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	// The name has no `tcc`, so only the resolved compiler gives it away.
	alias_path := os.join_path(dir, 'cc_alias.exe')
	os.cp(bundled, alias_path) or { panic(err) }
	// Shared TinyCC builds need their adjacent DLL even to print the version.
	libtcc := os.join_path(os.dir(bundled), 'libtcc.dll')
	if os.is_file(libtcc) {
		os.cp(libtcc, os.join_path(dir, 'libtcc.dll')) or { panic(err) }
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'fn main() {\n\tprintln(1)\n}\n') or { panic(err) }
	result := os.exec([@VEXE, '-prod', '-cc', alias_path, '-o', os.join_path(dir, 'main'), source])
	assert result.exit_code == 1, result.output
	assert result.output.contains('tcc is not allowed with -prod'), result.output
}

fn test_prod_with_tinycc_in_vflags_does_not_block_version_or_help() {
	mut environment := os.environ()
	environment['VFLAGS'] = '-prod -cc tcc'
	for args in [['version'], ['help']] {
		result := run_launcher_test_process(@VEXE, args, os.getwd(), environment)
		assert result.exit_code == 0, '${args}: ${result.output}'
		assert !result.output.contains('tcc is not allowed with -prod'), '${args}: ${result.output}'
	}
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

fn test_ownership_bootstrap_recognizes_compiler_inputs_only() {
	root := os.join_path(os.temp_dir(), 'v3_bootstrap_inputs_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	for entry in ['cmd/v', 'vlib/v'] {
		directory := os.join_path(root, entry)
		os.mkdir_all(directory)!
		os.write_file(os.join_path(directory, 'v.v'), 'module main')!
	}
	for entry in ['cmd/v', 'cmd/v/v.v', 'vlib/v', 'vlib/v/v.v'] {
		input := os.join_path(root, entry)
		args := ['-gc', 'none', '-d', 'ownership', '-o', 'compiler', input]
		assert compiler_bootstrap_requested(args, root)
	}
	entry := os.join_path(root, 'vlib/v/v.v')
	assert compiler_bootstrap_requested(['run', entry], root)
	assert compiler_bootstrap_requested(['crun', entry], root)
	input := os.join_path(root, 'v.v')
	os.write_file(input, 'module main')!
	assert !compiler_bootstrap_requested(['-d', 'ownership', input], root)
	assert !compiler_bootstrap_requested(['-o', entry, input], root)
	assert !compiler_bootstrap_requested(['-path', entry, input], root)
}

fn test_private_tool_builds_keep_tools_available_with_tinycc_production_flags() {
	assert external_tool_compile_args('vfmt', ['-prod', '-cc', 'tcc']) == ['-cc', 'tcc']
	assert external_tool_args_without_prod(['-prod', '-o', '-prod', '-cc', 'tcc']) == [
		'-o',
		'-prod',
		'-cc',
		'tcc',
	]
	assert external_tool_runtime_args('self', ['-prod', '-cc', 'tcc'], ['self']) == [
		'-prod',
		'-cc',
		'tcc',
		'self',
	]
}

fn test_cold_formatter_cache_accepts_tinycc_production_environment() {
	if !os.is_executable(os.join_path(os.dir(@VEXE), 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'launcher_prod_tcc_tool_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut environment := os.environ()
	environment['VFLAGS'] = '-prod -cc tcc -gc none'
	environment['VTOOLS_CACHE_DIR'] = os.join_path(root, 'tools')

	result := run_launcher_test_process(@VEXE, ['fmt', '--help'], os.getwd(), environment)
	assert result.exit_code == 0, result.output
	assert !result.output.contains('tcc is not allowed with -prod'), result.output
}

fn test_obsolete_skip_unused_cleanup_preserves_option_values_and_execution_boundaries() {
	for command in ['run', 'crun'] {
		for input in ['run', 'crun', 'build', 'test'] {
			args := [command, input, '-skip-unused']
			assert remove_obsolete_skip_unused_flag(args) == args
			assert remove_obsolete_skip_unused_flag(['-skip-unused', ...args]) == args
		}
	}
	assert remove_obsolete_skip_unused_flag(['-raw-vsh-tmp-prefix', 'prefix', 'run', '-skip-unused']) == [
		'-raw-vsh-tmp-prefix',
		'prefix',
		'run',
		'-skip-unused',
	]
	for args in [['-o', '-skip-unused', 'main.v'], ['-cflags', '-skip-unused', 'main.v'],
		['run', 'main.v', '-skip-unused'], ['crun', 'main.v', '-skip-unused'],
		['main.vsh', '-skip-unused'], ['-e', 'println("-skip-unused")', '-skip-unused'],
		['complete', 'bash', 'v', '-skip-unused']] {
		assert remove_obsolete_skip_unused_flag(args) == args
	}
	assert remove_obsolete_skip_unused_flag(['-skip-unused', '-o', '-skip-unused', 'main.v']) == [
		'-o',
		'-skip-unused',
		'main.v',
	]
	assert remove_obsolete_skip_unused_flag(['-skip-unused', '-skip-unused', 'run', 'main.v',
		'-skip-unused']) == ['run', 'main.v', '-skip-unused']
}

fn test_test_launcher_builds_host_helper_and_preserves_test_target() {
	for backend in ['arm64', 'amd64', 'wasm', 'js'] {
		for flag in ['-b', '-backend'] {
			for prefix in [[flag, backend, '-os', 'linux', '-arch', 'arm64'],
				['${flag}=${backend}', '-os=linux', '-arch=arm64']] {
				assert external_tool_compile_args('vtest', prefix) == []string{}
				mut expected := prefix.clone()
				expected << ['test', 'one_test.v']
				assert external_tool_runtime_args('test', prefix, ['test', 'one_test.v']) == expected
			}
		}
	}
}

fn test_host_tool_filters_preserve_other_option_operands() {
	for value in ['-arch', '-os', '-cross', '-arch=amd64', '-os=linux', '-b', '-backend=wasm'] {
		for option in ['-cflags', '-ldflags', '-d', '-o', '-cf'] {
			prefix := [option, value]
			assert external_tool_compile_args('vtest', prefix) == prefix
			assert external_tool_build_args('vtest', prefix) == prefix
		}
	}
	prefix := ['-cc', 'clang', '-cflags', '-arch', '-cflags', 'arm64', '-arch', 'amd64']
	assert external_tool_compile_args('vtest', prefix) == ['-cc', 'clang', '-cflags', '-arch',
		'-cflags', 'arm64']
}

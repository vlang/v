import v.pref
import v.vmod
import os

const vexe = @VEXE
const vroot = os.dir(vexe)

fn test_check_parametes() {
	// reproducing issue https://github.com/vlang/v/issues/13983
	_, cmd := pref.parse_args_and_show_errors(['help'], [''], true)
	// no command found from args
	assert cmd == ''
}

fn test_version_flag() {
	v_ver := vmod.from_file(os.join_path(vroot, 'v.mod'))!.version
	v_ver_cmd_res := os.execute_opt('${vexe} --version')!.output
	assert v_ver_cmd_res.starts_with('V ${v_ver}'), v_ver_cmd_res

	v_retry_ver_cmd_res := os.execute_opt('${vexe} retry --version')!.output
	assert v_retry_ver_cmd_res != v_ver_cmd_res

	v_git_ver_subcmd_res := os.execute_opt('${vexe} retry -- git --version')!.output
	assert v_git_ver_subcmd_res !in [v_ver_cmd_res, v_retry_ver_cmd_res]

	// Test version / verbosity toggle.
	assert os.execute_opt('${vexe} -v')!.output == v_ver_cmd_res
	assert os.execute_opt('${vexe} -cc tcc -v')!.output == v_ver_cmd_res

	example_path := os.join_path(vroot, 'examples', 'hello_world.v')
	v_verbose_cmd_res := os.execute_opt('${vexe} -v run ${example_path}')!.output
	assert v_verbose_cmd_res != v_ver_cmd_res
	assert v_verbose_cmd_res.contains('v.pref.lookup_path:')

	v_verbose_cmd_with_additional_args_res := os.execute_opt('${vexe} -g -v run ${example_path}')!.output
	assert v_verbose_cmd_with_additional_args_res != v_ver_cmd_res
	assert v_verbose_cmd_with_additional_args_res.contains('v.pref.lookup_path:')
}

fn test_cross_compile_keeps_explicit_cc() {
	target_os := if pref.get_host_os() == .linux { 'macos' } else { 'linux' }
	custom_cc := 'cosmocc'

	first, _ := pref.parse_args_and_show_errors(['help'], ['', '-cc', custom_cc, '-os', target_os],
		false)
	assert first.ccompiler_set_by_flag
	assert first.ccompiler == custom_cc

	second, _ := pref.parse_args_and_show_errors(['help'],
		['', '-os', target_os, '-cc', custom_cc], false)
	assert second.ccompiler_set_by_flag
	assert second.ccompiler == custom_cc
}

fn test_disable_explicit_mutability_flag() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['-disable-explicit-mutability', target], false)
	assert prefs.disable_explicit_mutability
	assert prefs.build_options.contains('-disable-explicit-mutability')

	prefs2, _ := pref.parse_args_and_show_errors([], ['--disable-explicit-mutability', target],
		false)
	assert prefs2.disable_explicit_mutability
	assert prefs2.build_options.contains('--disable-explicit-mutability')
}

fn new_wasm_preferences() pref.Preferences {
	return pref.Preferences{
		backend: .wasm
		os:      .browser
		arch:    .wasm32
	}
}

fn test_wasm_backend_filters_backend_specific_files() {
	prefs := new_wasm_preferences()
	dir := os.join_path(os.vtmp_dir(), 'wasm_backend_filters')
	filtered := prefs.should_compile_filtered_files(dir, [
		'mod.c.v',
		'mod.js.v',
		'mod.v',
		'mod.wasm.v',
	])
	assert filtered == [
		os.join_path(dir, 'mod.v'),
		os.join_path(dir, 'mod.wasm.v'),
	]
}

fn test_wasm_backend_skips_modules_with_only_c_and_js_variants() {
	prefs := new_wasm_preferences()
	filtered := prefs.should_compile_filtered_files('sus', ['sus.c.v', 'sus.js.v'])
	assert filtered.len == 0
}

fn test_explicit_gc_mode_is_forwarded_to_build_module() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	for gc_mode in ['none', 'boehm', 'boehm_full', 'boehm_incr', 'boehm_full_opt', 'boehm_incr_opt',
		'boehm_leak'] {
		prefs, _ := pref.parse_args_and_show_errors([], ['-usecache', '-gc', gc_mode, target],
			false)
		assert prefs.build_options.contains('-gc ${gc_mode}')
	}
}

fn test_cross_compile_defaults_windows_to_the_cross_compiler_arch() {
	if pref.get_host_os() == .windows {
		return
	}
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'windows', target], false)
	assert prefs.arch == .amd64
	assert prefs.ccompiler == 'x86_64-w64-mingw32-gcc'
}

fn test_cross_compile_infers_android_arch_from_vcross_compiler_name() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	old_cross_compiler := os.getenv('VCROSS_COMPILER_NAME')
	defer {
		os.setenv('VCROSS_COMPILER_NAME', old_cross_compiler, true)
	}
	for compiler_name, expected_arch in {
		'aarch64-linux-android21-clang':    pref.Arch.arm64
		'armv7a-linux-androideabi21-clang': pref.Arch.arm32
		'i686-linux-android21-clang':       pref.Arch.i386
		'x86_64-linux-android21-clang':     pref.Arch.amd64
	} {
		os.setenv('VCROSS_COMPILER_NAME', compiler_name, true)
		prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'android', target], false)
		assert prefs.arch == expected_arch
		assert prefs.ccompiler == compiler_name
	}
}

fn test_musl_defaults_to_no_gc() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-musl', target], false)
	assert prefs.is_musl
	assert prefs.gc_mode == .no_gc
}

fn test_musl_keeps_explicit_gc_selection() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-musl', '-gc', 'boehm', target], false)
	assert prefs.is_musl
	assert prefs.gc_mode == .boehm_full_opt
}

fn test_v_cmds_and_flags() {
	build_cmd_res := os.execute('${vexe} build ${vroot}/examples/hello_world.v')
	assert build_cmd_res.output.trim_space() == 'Use `v ${vroot}/examples/hello_world.v` instead.'

	too_many_targets_res :=
		os.execute('${vexe} ${vroot}/examples/hello_world.v ${vroot}/examples/fizz_buzz.v')
	assert too_many_targets_res.output.trim_space() == 'Too many targets. Specify just one target: <target.v|target_directory>.'

	unknown_arg_res := os.execute('${vexe} -xyz')
	assert unknown_arg_res.output.trim_space() == 'Unknown argument `-xyz`'

	unknown_arg_for_cmd_res := os.execute('${vexe} build-module -xyz ${vroot}/vlib/math')
	assert unknown_arg_for_cmd_res.output.trim_space() == 'Unknown argument `-xyz` for command `build-module`'

	no_run_files_res := os.execute('${vexe} run')
	assert no_run_files_res.output.trim_space() == 'v run: no v files listed'

	no_bm_files_res := os.execute('${vexe} build-module')
	assert no_bm_files_res.output.trim_space() == 'v build-module: no module specified'
}

const tfile = os.join_path(os.vtmp_dir(), 'unknown_options_output.c')

fn test_unknown_option_flags_no_run() {
	os.chdir(os.dir(@VEXE))!
	os.rm(tfile) or {}

	res1 :=
		os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} examples/hello_world.v --an-unknown-option')
	assert res1.exit_code == 1, res1.output
	assert res1.output.starts_with('Unknown argument')
	assert res1.output.contains('--an-unknown-option')
	assert !os.exists(tfile)

	res2 :=
		os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} --an-unknown-option examples/hello_world.v')
	assert res2.exit_code == 1, res2.output
	assert res2.output.starts_with('Unknown argument')
	assert res2.output.contains('--an-unknown-option')
	assert !os.exists(tfile)
}

fn test_unknown_option_flags_with_run() {
	res_run_o :=
		os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(tfile)} run examples/hello_world.v --an-unknown-option')
	assert res_run_o.exit_code == 0, res_run_o.output
	assert res_run_o.output == '' // because of -o, there should not be an actual run, since compilation stopped after generating the .c file
	assert os.exists(tfile)
	os.rm(tfile) or {}

	res_run_no_o_unknown_before_run :=
		os.execute('${os.quoted_path(@VEXE)} --an-unknown-option run examples/hello_world.v ')
	assert res_run_no_o_unknown_before_run.exit_code == 1, res_run_no_o_unknown_before_run.output
	assert res_run_no_o_unknown_before_run.output.starts_with('Unknown argument')
	assert res_run_no_o_unknown_before_run.output.contains('--an-unknown-option')
	assert !os.exists(tfile)

	res_run_no_o :=
		os.execute('${os.quoted_path(@VEXE)} run examples/hello_world.v --an-unknown-option')
	assert res_run_no_o.exit_code == 0, res_run_no_o.output
	assert res_run_no_o.output.trim_space() == 'Hello, World!'
	assert !os.exists(tfile)
}

fn test_missing_explicit_ccompiler_reports_error() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	missing_cc := 'missing_compiler_17126_for_pref_test'
	output := os.join_path(os.vtmp_dir(), 'missing_explicit_ccompiler_output')
	mut expected_output := output
	$if windows {
		expected_output += '.exe'
	}
	os.rm(expected_output) or {}
	res :=
		os.execute('${os.quoted_path(@VEXE)} -cc ${missing_cc} -o ${os.quoted_path(output)} ${os.quoted_path(target)}')
	assert res.exit_code != 0
	assert res.output.contains(missing_cc), res.output
	assert res.output.to_lower().contains('not found') || res.output.to_lower().contains('missing'), res.output

	assert !os.exists(expected_output)
}

fn test_generate_c_project_flag_parsing() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['-generate-c-project', 'cproj', target], false)
	assert prefs.generate_c_project == 'cproj'
	assert prefs.use_cache == false
}

fn test_generate_c_project_creates_build_files() {
	output_dir := os.join_path(os.vtmp_dir(), 'v_generate_c_project_json')
	os.rmdir_all(output_dir) or {}
	defer {
		os.rmdir_all(output_dir) or {}
	}
	target := os.join_path(vroot, 'examples', 'json.v')
	cmd := '${os.quoted_path(vexe)} -generate-c-project ${os.quoted_path(output_dir)} ${os.quoted_path(target)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	for rel_path in ['json.c', 'build_command.txt', 'build.sh', 'build.bat', 'Makefile'] {
		assert os.is_file(os.join_path(output_dir, rel_path))
	}
	build_command := os.read_file(os.join_path(output_dir, 'build_command.txt')) or { panic(err) }
	assert build_command.contains(os.join_path(output_dir, 'json.c'))
	assert build_command.contains('cJSON.c')
	assert !build_command.contains('.tmp.c')
	assert !build_command.contains('.module.')
}

fn test_output_flag_accepts_directory_path() {
	output_dir := os.join_path(os.vtmp_dir(), 'v_output_flag_directory')
	os.rmdir_all(output_dir) or {}
	defer {
		os.rmdir_all(output_dir) or {}
	}
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	output_arg := output_dir + os.path_separator
	res :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(output_arg)} ${os.quoted_path(target)}')
	assert res.exit_code == 0, res.output
	assert os.is_dir(output_dir)
	mut expected_output := os.join_path(output_dir, 'hello_world')
	$if windows {
		expected_output += '.exe'
	}
	assert os.is_file(expected_output)
}

fn test_tcc_shared_builds_disable_backtraces() {
	mut shared_prefs := &pref.Preferences{
		path:      'libfoo.v'
		is_shared: true
		ccompiler: 'tinyc'
	}
	shared_prefs.fill_with_defaults()
	assert 'no_backtrace' in shared_prefs.compile_defines_all

	mut regular_prefs := &pref.Preferences{
		path:      'main.v'
		ccompiler: 'tinyc'
	}
	regular_prefs.fill_with_defaults()
	assert 'no_backtrace' !in regular_prefs.compile_defines_all
}

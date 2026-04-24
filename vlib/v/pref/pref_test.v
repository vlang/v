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

fn test_mac_is_alias_for_macos() {
	os_kind := pref.os_from_string('mac') or {
		assert false, err.msg()
		return
	}
	assert os_kind == .macos
	assert pref.OS.macos.is_target_of('mac')
	assert !pref.OS.linux.is_target_of('mac')
}

fn test_bsd_target_matches_macos_and_bsd_systems() {
	for os_kind in [pref.OS.macos, .freebsd, .openbsd, .netbsd, .dragonfly] {
		assert os_kind.is_target_of('bsd')
	}
	assert !pref.OS.linux.is_target_of('bsd')
	assert !pref.OS.windows.is_target_of('bsd')
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

fn test_profile_flag_does_not_consume_direct_compile_target() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, command := pref.parse_args_and_show_errors([], ['-profile', target], false)
	assert command == target
	assert prefs.path == target
	assert prefs.is_prof
	assert prefs.profile_file == '-'
}

fn test_profile_flag_does_not_consume_run_command() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, command := pref.parse_args_and_show_errors([], ['-profile', 'run', target], false)
	assert command == 'run'
	assert prefs.path == target
	assert prefs.is_run
	assert prefs.is_prof
	assert prefs.profile_file == '-'
}

fn test_profile_flag_still_accepts_explicit_output_file() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, command := pref.parse_args_and_show_errors([], ['-profile', 'profile.txt', target],
		false)
	assert command == target
	assert prefs.path == target
	assert prefs.is_prof
	assert prefs.profile_file == 'profile.txt'
}

fn new_wasm_preferences() pref.Preferences {
	return pref.Preferences{
		backend: .wasm
		os:      .browser
		arch:    .wasm32
	}
}

fn new_c_preferences() pref.Preferences {
	return pref.Preferences{
		backend: .c
		os:      .linux
		arch:    .amd64
	}
}

fn test_c_backend_filters_backend_specific_files() {
	prefs := new_c_preferences()
	dir := os.join_path(os.vtmp_dir(), 'c_backend_filters')
	filtered := prefs.should_compile_filtered_files(dir, [
		'mod.c.v',
		'mod.js.v',
		'mod.v',
		'mod.wasm.v',
	])
	assert filtered == [
		os.join_path(dir, 'mod.c.v'),
		os.join_path(dir, 'mod.v'),
	]
}

fn test_c_backend_skips_modules_with_only_non_c_variants() {
	prefs := new_c_preferences()
	filtered := prefs.should_compile_filtered_files('sus', ['sus.js.v', 'sus.wasm.v'])
	assert filtered.len == 0
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

fn filtered_file_names_for_os(os_kind pref.OS, files []string) []string {
	prefs := pref.Preferences{
		os: os_kind
	}
	dir := os.join_path(os.vtmp_dir(), 'environment_specific_files')
	mut res := []string{}
	for file in prefs.should_compile_filtered_files(dir, files) {
		res << os.base(file)
	}
	return res
}

fn test_bsd_specific_files_are_filtered_by_target_os() {
	for os_kind in [pref.OS.macos, .freebsd, .openbsd, .netbsd, .dragonfly] {
		assert filtered_file_names_for_os(os_kind, ['mod_bsd.c.v', 'mod_bsd.v']) == [
			'mod_bsd.c.v',
			'mod_bsd.v',
		]
	}
	assert filtered_file_names_for_os(.linux, ['mod_bsd.c.v', 'mod_bsd.v']).len == 0
	assert filtered_file_names_for_os(.windows, ['mod_bsd.c.v', 'mod_bsd.v']).len == 0
}

fn test_bsd_specific_files_prefer_more_specific_variants() {
	mut files := [
		'main.v',
		'something_default.c.v',
		'something_windows.c.v',
	]
	assert filtered_file_names_for_os(.freebsd, files) == ['main.v', 'something_default.c.v']

	files << 'something_nix.c.v'
	assert filtered_file_names_for_os(.freebsd, files) == ['main.v', 'something_nix.c.v']

	files << 'something_bsd.c.v'
	assert filtered_file_names_for_os(.freebsd, files) == ['main.v', 'something_bsd.c.v']

	files << 'something_freebsd.c.v'
	assert filtered_file_names_for_os(.freebsd, files) == ['main.v', 'something_freebsd.c.v']
}

fn test_bsd_specific_files_prefer_darwin_on_macos() {
	files := [
		'main.v',
		'something_nix.c.v',
		'something_bsd.v',
		'something_darwin.v',
	]
	assert filtered_file_names_for_os(.macos, files) == ['main.v', 'something_darwin.v']
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

fn test_cross_compile_windows_m32_uses_i386_arch_and_compiler() {
	if pref.get_host_os() == .windows {
		return
	}
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'windows', '-m32', target], false)
	assert !prefs.m64
	assert prefs.arch == .i386
	assert prefs.ccompiler == 'i686-w64-mingw32-gcc'
	assert prefs.build_options.contains('-m32')
}

fn test_cross_compile_defaults_linux_to_amd64() {
	if pref.get_host_os() == .linux {
		return
	}
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-os', 'linux', target], false)
	assert prefs.arch == .amd64
	assert prefs.ccompiler == 'clang'
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

fn stale_windows_gc_prefs(gc_set_by_flag bool) pref.Preferences {
	mut prefs := pref.Preferences{
		os:                  .windows
		ccompiler_type:      .msvc
		gc_mode:             .boehm_full_opt
		gc_set_by_flag:      gc_set_by_flag
		compile_defines:     ['gcboehm', 'gcboehm_full', 'gcboehm_opt', 'custom']
		compile_defines_all: ['gcboehm', 'gcboehm_full', 'gcboehm_opt', 'custom']
		compile_values:      map[string]string{}
		build_options:       ['-prod', '-d gcboehm', '-d gcboehm_full', '-d gcboehm_opt']
	}
	prefs.compile_values['gcboehm'] = 'true'
	prefs.compile_values['gcboehm_full'] = 'true'
	prefs.compile_values['gcboehm_opt'] = 'true'
	prefs.compile_values['custom'] = 'true'
	return prefs
}

fn test_windows_msvc_gc_defaults_are_cleared_after_compiler_resolution() {
	mut prefs := stale_windows_gc_prefs(false)

	prefs.normalize_gc_defaults_for_resolved_ccompiler()

	assert prefs.gc_mode == .no_gc
	assert prefs.build_options == ['-prod', '-gc', 'none']
	assert prefs.compile_defines == ['custom']
	assert prefs.compile_defines_all == ['custom']
	assert prefs.compile_values == {
		'custom': 'true'
	}
}

fn test_windows_msvc_gc_defaults_keep_explicit_gc_selection() {
	mut prefs := stale_windows_gc_prefs(true)
	prefs.build_options = ['-prod', '-gc', 'boehm', '-d gcboehm', '-d gcboehm_full', '-d gcboehm_opt']

	prefs.normalize_gc_defaults_for_resolved_ccompiler()

	assert prefs.gc_mode == .boehm_full_opt
	assert prefs.build_options == ['-prod', '-gc', 'boehm', '-d gcboehm', '-d gcboehm_full',
		'-d gcboehm_opt']
	assert prefs.compile_defines == ['gcboehm', 'gcboehm_full', 'gcboehm_opt', 'custom']
	assert prefs.compile_defines_all == ['gcboehm', 'gcboehm_full', 'gcboehm_opt', 'custom']
	assert prefs.compile_values == {
		'custom':       'true'
		'gcboehm':      'true'
		'gcboehm_full': 'true'
		'gcboehm_opt':  'true'
	}
}

fn test_m32_sets_i386_arch_when_not_explicitly_set() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-m32', target], false)
	assert !prefs.m64
	assert prefs.arch == .i386
	assert prefs.build_options.contains('-m32')
}

fn test_m32_does_not_override_explicit_arch() {
	target := os.join_path(vroot, 'examples', 'hello_world.v')
	prefs, _ := pref.parse_args_and_show_errors([], ['', '-arch', 'amd64', '-m32', target], false)
	assert !prefs.m64
	assert prefs.arch == .amd64
	assert prefs.build_options.contains('-m32')
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

fn test_build_command_compiles_vsh_without_running_it() {
	test_dir := os.join_path(os.vtmp_dir(), 'v_pref_build_vsh_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	script_path := os.join_path(test_dir, 'build_only.vsh')
	marker_path := os.join_path(test_dir, 'marker.txt')
	mut exe_path := os.join_path(test_dir, 'build_only')
	$if windows {
		exe_path += '.exe'
	}
	os.write_file(script_path, "import os

fn main() {
	marker_path := os.join_path(@DIR, 'marker.txt')
	os.write_file(marker_path, 'ran') or { panic(err) }
	println('ran')
}
")!
	build_res := os.execute('${os.quoted_path(vexe)} -silent build ${os.quoted_path(script_path)}')
	assert build_res.exit_code == 0, build_res.output
	assert build_res.output == ''
	assert !os.exists(marker_path)
	assert os.is_file(exe_path)

	run_res := os.execute(os.quoted_path(exe_path))
	assert run_res.exit_code == 0, run_res.output
	assert run_res.output.trim_space() == 'ran'
	assert os.read_file(marker_path)! == 'ran'
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

fn test_late_resolved_tcc_shared_builds_disable_backtraces() {
	mut shared_prefs := &pref.Preferences{
		path:      'libfoo.v'
		is_shared: true
		ccompiler: 'gcc'
	}
	shared_prefs.fill_with_defaults()
	assert 'no_backtrace' !in shared_prefs.compile_defines_all

	shared_prefs.ccompiler_type = .tinyc
	shared_prefs.normalize_gc_defaults_for_resolved_ccompiler()

	assert 'no_backtrace' in shared_prefs.compile_defines_all
	assert shared_prefs.build_options.contains('-d no_backtrace')
}

fn test_wayland_only_linux_session_surfaces_a_v_error_for_gg() {
	if os.user_os() == 'windows' {
		return
	}
	pid := os.getpid()
	test_dir := os.join_path(os.vtmp_dir(), 'v_issue_18030_gg_wayland_${pid}')
	source_path := os.join_path(test_dir, 'main.v')
	exe_path := os.join_path(test_dir, 'app')
	source := 'import gg as _\n\nfn main() {}\n'
	os.mkdir_all(test_dir) or { panic(err) }
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	cmd := 'DISPLAY= WAYLAND_DISPLAY=wayland-0 XDG_SESSION_TYPE=wayland ${os.quoted_path(vexe)} -os linux -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	output := res.output.replace('\r', '')
	if res.exit_code == 0 {
		eprintln('> failed command: ${cmd}')
	}
	assert res.exit_code != 0
	assert output.contains('Wayland-only Linux session without `-d sokol_wayland`')
	assert !output.contains('C error. This should never happen.')
}

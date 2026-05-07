module main

import os
import v.builder
import v.pref

const vexe = @VEXE
const test_path = os.join_path(os.vtmp_dir(), 'run_check')

fn testsuite_begin() {
	os.mkdir_all(test_path) or {}
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn run_v_ok(command string) string {
	res := os.execute(command)
	if res.exit_code != 0 {
		eprintln('> failing cmd: ${command}')
		eprintln('> output:\n${res.output}')
		assert res.exit_code == 0
	}
	return res.output
}

fn test_conditional_executable_removal() {
	os.chdir(test_path)!
	os.write_file('main.v', 'fn main(){\n\tprintln("Hello World!")\n}\n')!

	mut executable := 'run_check'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path)!
	dump(original_file_list_)
	assert executable !in original_file_list_

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_run_file_list := os.ls(test_path)!.filter(os.exists(it))
	dump(after_run_file_list)
	assert executable !in after_run_file_list

	assert os.execute('${os.quoted_path(vexe)} .').exit_code == 0
	assert os.execute('./${executable}').output.trim_space() == 'Hello World!'
	after_compilation__ := os.ls(test_path)!
	dump(after_compilation__)
	assert executable in after_compilation__

	assert os.execute('${os.quoted_path(vexe)} run .').output.trim_space() == 'Hello World!'
	after_second_run___ := os.ls(test_path)!
	dump(after_second_run___)
	assert executable in after_second_run___
}

fn test_run_from_workdir_with_spaces() {
	project_dir := os.join_path(test_path, 'issue 16501 path with spaces')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir)!
	defer {
		os.chdir(test_path) or {}
		os.rmdir_all(project_dir) or {}
	}
	os.write_file(os.join_path(project_dir, 'main.v'),
		'fn main(){\n\tprintln("Hello from a spaced path")\n}\n')!
	os.write_file(os.join_path(project_dir, 'Test.vsh'),
		"println('Hello from a spaced script path')\n")!
	os.chdir(project_dir)!

	run_file_res := os.execute('${os.quoted_path(vexe)} run Test.vsh')
	assert run_file_res.exit_code == 0, run_file_res.output
	assert run_file_res.output.trim_space() == 'Hello from a spaced script path'

	run_dir_res := os.execute('${os.quoted_path(vexe)} run .')
	assert run_dir_res.exit_code == 0, run_dir_res.output
	assert run_dir_res.output.trim_space() == 'Hello from a spaced path'
}

fn test_existing_vsh_executable_uses_cache_until_source_is_newer() {
	project_dir := os.join_path(test_path, 'existing vsh executable')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir)!
	defer {
		os.chdir(test_path) or {}
		os.rmdir_all(project_dir) or {}
	}
	script_path := os.join_path(project_dir, 'script.vsh')
	os.write_file(script_path, "println('Hello from cached vsh')\n")!
	os.chdir(project_dir)!

	cmd := '${os.quoted_path(vexe)} script.vsh'
	first_res := os.execute(cmd)
	assert first_res.exit_code == 0, first_res.output
	assert first_res.output.trim_space() == 'Hello from cached vsh'

	mut executable := os.join_path(project_dir, 'script')
	$if windows {
		executable += '.exe'
	}
	assert os.is_file(executable)

	warm_cache_res := os.execute(cmd)
	assert warm_cache_res.exit_code == 0, warm_cache_res.output
	assert warm_cache_res.output.trim_space() == 'Hello from cached vsh'

	cache_stamp := os.file_last_mod_unix(executable) + 3600
	os.utime(executable, cache_stamp, cache_stamp)!

	os.write_file(script_path, "println('Hello from rebuilt vsh')\n")!
	os.utime(script_path, cache_stamp - 1, cache_stamp - 1)!
	assert os.file_last_mod_unix(script_path) < cache_stamp

	cached_res := os.execute(cmd)
	assert cached_res.exit_code == 0, cached_res.output
	assert cached_res.output.trim_space() == 'Hello from cached vsh'

	os.utime(script_path, cache_stamp + 60, cache_stamp + 60)!
	assert os.file_last_mod_unix(script_path) > cache_stamp

	rebuilt_res := os.execute(cmd)
	assert rebuilt_res.exit_code == 0, rebuilt_res.output
	assert rebuilt_res.output.trim_space() == 'Hello from rebuilt vsh'
}

fn test_file_list() {
	os.chdir(test_path)!
	os.mkdir_all('filelist')!
	os.write_file('filelist/main.v', 'module main
fn main() {
        part_a := AS{}
        part_b := BS{}
        println("\${part_a}=>\${part_b}")
}')!
	os.write_file('filelist/part_a.v', 'module main
pub struct AS{}
')!

	os.write_file('filelist/part_b.v', 'module main
pub struct BS{}
')!

	// `part_c.v` is not included in the compilation
	// or there will be a conflit definition of `struct BS`
	os.write_file('filelist/part_c.v', 'module main
pub struct BS{}
')!
	mut executable := 'filelist_check'
	$if windows {
		executable += '.exe'
	}

	original_file_list_ := os.ls(test_path)!
	dump(original_file_list_)
	assert executable !in original_file_list_

	cmd := '${os.quoted_path(vexe)} -o ${executable} filelist/main.v -file-list "filelist/part_a.v,filelist/part_b.v,"'
	os.execute(cmd)
	after_compile_file_list := os.ls(test_path)!.filter(os.exists(it))
	dump(after_compile_file_list)
	assert executable in after_compile_file_list
	assert os.execute('./${executable}').output.trim_space() == 'AS{}=>BS{}'
}

fn test_run_custom_base_url_uses_project_root_lookup() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_base_url_project')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'source', 'foo'))!
	os.mkdir_all(os.join_path(project_dir, 'source', 'modules', 'dep'))!
	os.write_file(os.join_path(project_dir, 'v.mod'),
		"Module {\n\tname: 'run_base_url_project'\n\tbase_url: 'source'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")!
	os.write_file(os.join_path(project_dir, 'source', 'main.v'), 'module main
import foo
import dep

fn main() {
	println(foo.name() + "+" + dep.name())
}
')!
	os.write_file(os.join_path(project_dir, 'source', 'foo', 'foo.v'), 'module foo

pub fn name() string {
	return "foo"
}
')!
	os.write_file(os.join_path(project_dir, 'source', 'modules', 'dep', 'dep.v'), 'module dep

pub fn name() string {
	return "dep"
}
')!
	os.chdir(project_dir)!
	assert run_v_ok('${os.quoted_path(vexe)} run .').trim_space() == 'foo+dep'
	assert run_v_ok('${os.quoted_path(vexe)} run source').trim_space() == 'foo+dep'
	assert run_v_ok('${os.quoted_path(vexe)} run ./source').trim_space() == 'foo+dep'
}

fn test_empty_local_dir_does_not_shadow_vlib_module() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_empty_local_module_dir')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'os'))!
	os.write_file(os.join_path(project_dir, 'main.v'),
		"module main\n\nimport os\n\nfn main() {\n\tprintln(os.is_dir('.'))\n}\n")!
	os.chdir(project_dir)!

	res := os.execute('${os.quoted_path(vexe)} run main.v')

	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'true'
}

fn test_removed_src_layout_error_mentions_vmod_subdirs() {
	os.chdir(test_path)!
	project_dir := os.join_path(test_path, 'run_removed_src_project')
	defer {
		os.chdir(test_path) or {}
	}
	os.mkdir_all(os.join_path(project_dir, 'src'))!
	os.write_file(os.join_path(project_dir, 'src', 'main.v'),
		'fn main() {\n\tprintln("Hello from src")\n}\n')!
	os.chdir(project_dir)!

	res := os.execute('${os.quoted_path(vexe)} run .')
	normalized_output := res.output.replace('\r\n', '\n')

	assert res.exit_code != 0
	assert normalized_output.contains('the virtual `src/` module directory is no longer supported.')
	assert !normalized_output.contains('base_url')
	assert normalized_output.contains('add `subdirs` to v.mod')
	assert normalized_output.contains("subdirs: ['admin', 'repo', 'commit', 'ci', 'security', 'ssh', 'user']")
}

fn test_thirdparty_object_build_with_multiline_cflags() {
	mut env := os.environ()
	existing_cflags := if 'CFLAGS' in env { env['CFLAGS'] } else { '' }
	env['CFLAGS'] = if existing_cflags == '' {
		'-DTHIRDPARTY_MULTILINE_1=1\n-DTHIRDPARTY_MULTILINE_2=1'
	} else {
		'${existing_cflags}\n-DTHIRDPARTY_MULTILINE_1=1\n-DTHIRDPARTY_MULTILINE_2=1'
	}
	mut p := os.new_process(vexe)
	p.set_work_folder(@VEXEROOT)
	p.set_args(['run', os.join_path(@VEXEROOT, 'vlib', 'v', 'tests', 'project_with_c_code', 'main.v')])
	p.set_environment(env)
	p.set_redirect_stdio()
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	p.close()
	assert p.code == 0, 'stdout:\n${stdout}\nstderr:\n${stderr}'
}

fn test_missing_library_is_reported_without_compiler_bug_hint() {
	os.chdir(test_path)!
	os.mkdir_all('missing_library')!
	lib_name := 'v_missing_lib_25499'
	src_file := os.join_path('missing_library', 'main.v')
	os.write_file(src_file, '#flag -l${lib_name}\nfn main() {}\n')!

	res := os.execute('${os.quoted_path(vexe)} ${os.quoted_path(src_file)}')
	normalized_output := res.output.replace('\r\n', '\n')

	assert res.exit_code != 0
	assert normalized_output.contains('builder error:')
	assert normalized_output.contains('C library `${lib_name}` was not found while linking the generated program.')
	assert normalized_output.contains('Please install the corresponding development package/libraries')
	assert !normalized_output.contains('This is a V compiler bug')
}

fn test_windows_host_c_compiler_probe_is_skipped_for_non_windows_targets() {
	assert builder.should_find_windows_host_c_compiler(&pref.Preferences{
		backend: .c
		os:      .windows
	})
	assert !builder.should_find_windows_host_c_compiler(&pref.Preferences{
		backend: .c
		os:      .linux
	})
	assert !builder.should_find_windows_host_c_compiler(&pref.Preferences{
		backend:        .c
		os:             .windows
		output_cross_c: true
	})
	assert !builder.should_find_windows_host_c_compiler(&pref.Preferences{
		backend: .js_browser
		os:      .windows
	})
}

fn test_message_limit_notices_do_not_fail_build() {
	os.chdir(test_path)!
	src_file := os.join_path(test_path, 'message_limit_notices.v')
	mut exe_file := os.join_path(test_path, 'message_limit_notices')
	$if windows {
		exe_file += '.exe'
	}

	mut lines := []string{}
	for i in 0 .. 10 {
		lines << '@[deprecated]'
		lines << "@[deprecated_after: '3000-12-30']"
		lines << 'fn future_${i}() {}'
	}
	lines << 'fn main() {'
	for i in 0 .. 10 {
		lines << '\tfuture_${i}()'
	}
	lines << '}'
	os.write_file(src_file, lines.join('\n') + '\n')!

	res :=
		os.execute('${os.quoted_path(vexe)} -stats -message-limit 5 -o ${os.quoted_path(exe_file)} ${os.quoted_path(src_file)}')
	normalized_output := res.output.replace('\r\n', '\n')

	assert res.exit_code == 0, normalized_output
	assert os.exists(exe_file), normalized_output
	assert !normalized_output.contains('builder error: too many errors/warnings/notices')
	assert normalized_output.contains('checker summary: 0 V errors, 0 V warnings, 10 V notices'), normalized_output
}

fn test_run_with_obscure_source_filenames() {
	if os.user_os() == 'windows' {
		return
	}
	obscure_dir := os.join_path(test_path, 'obscure_filenames')
	os.rmdir_all(obscure_dir) or {}
	os.mkdir_all(obscure_dir)!
	source := "println('hi')\n"
	for idx, file_name in [
		"quote's.v",
		'"hi".v',
		"'.v",
		'".v',
		'.v',
		'..v',
		'...v',
		'-.v',
		'.c.v',
		'line\nfeed.v',
	] {
		src_file := os.join_path(obscure_dir, file_name)
		os.write_file(src_file, source)!
		out_file := os.join_path(obscure_dir, 'obscure_output_${idx}')
		display_name := file_name.replace('\n', '\\n')
		res :=
			os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out_file)} run ${os.quoted_path(src_file)}')
		assert res.exit_code == 0, '${display_name}: ${res.output}'
		assert res.output.trim_space() == 'hi', '${display_name}: ${res.output}'
		assert os.read_file(src_file)! == source
		os.rm(out_file) or {}
	}
}

fn test_macos_2048_build_does_not_force_deployment_target() {
	$if !macos {
		return
	}
	game_path := os.join_path(@VEXEROOT, 'examples', '2048', '2048.v')
	flags_output := run_v_ok('${os.quoted_path(vexe)} -dump-c-flags - ${os.quoted_path(game_path)}')
	assert flags_output.contains('-fobjc-arc')
	assert !flags_output.contains('-mmacosx-version-min=')
}

fn test_macos_arch_flag_is_forwarded_to_c_compiler() {
	$if !macos {
		return
	}
	src_file := os.join_path(test_path, 'macos_arch_forwarding.v')
	out_file := os.join_path(test_path, 'macos_arch_forwarding')
	os.write_file(src_file, 'fn main() {\n\tprintln("hello")\n}\n')!
	flags_output :=
		run_v_ok('${os.quoted_path(vexe)} -cc clang -gc none -showcc -skip-running -arch amd64 -o ${os.quoted_path(out_file)} ${os.quoted_path(src_file)}')
	assert flags_output.contains('-arch x86_64')
}

import os
import rand
import v.cmdexec

fn test_new_compiler_shared_library_exports_only_tagged_functions() {
	if os.user_os() != 'linux' {
		return
	}
	// The test runner can be the legacy fallback, but the library under test
	// must be built by the new compiler whose flag handling regressed in #28749.
	vexe := if os.base(@VEXE) == 'v1_fallback' {
		os.join_path(os.dir(@VEXE), 'v')
	} else {
		@VEXE
	}
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_new_shared_visibility_${rand.ulid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	dep_src := os.join_path(workdir, 'dep.c')
	dep_obj := os.join_path(workdir, 'dep.o')
	os.write_file(dep_src, 'int mylib_direct_object_symbol(void) { return 7; }\n')!
	dep_build := cmdexec.run('cc', ['-fPIC', '-c', dep_src, '-o', dep_obj])
	assert dep_build.exit_code == 0, dep_build.output
	lib_src := os.join_path(workdir, 'mylib.v')
	os.write_file(lib_src, [
		'module mylib',
		'',
		'#flag ${dep_obj}',
		'',
		"@[export: 'mylib_counter']",
		'__global counter = 7',
		'',
		'@[noinline]',
		'fn helper(x int) int {',
		'\treturn x * 2',
		'}',
		'',
		'@[noinline]',
		'pub fn public_helper(x int) int {',
		'\treturn helper(x) + 1',
		'}',
		'',
		"@[export: 'mylib_compute']",
		'pub fn compute(x int) int {',
		'\treturn public_helper(x)',
		'}',
		'',
		"@[export: 'mylib_private_export']",
		'fn private_export(x int) int {',
		'\treturn helper(x)',
		'}',
		'',
	].join('\n'))!
	host_src := os.join_path(workdir, 'host.c')
	os.write_file(host_src, [
		'int mylib_compute(int);',
		'int mylib_private_export(int);',
		'extern long long mylib_counter;',
		'int main(void) {',
		'\treturn mylib_compute(21) != 43 || mylib_private_export(21) != 42 || mylib_counter != 7;',
		'}',
		'',
	].join('\n'))!
	for is_prod in [false, true] {
		mode := if is_prod { 'prod' } else { 'debug' }
		lib_out := os.join_path(workdir, 'libmylib_${mode}')
		lib_so := '${lib_out}.so'
		// Exercise the default compiler choice and a directly linked native object.
		mut args := ['-new-compiler', '-nocache', '-enable-globals', '-shared']
		if is_prod {
			args << '-prod'
		}
		args << ['-o', lib_out, lib_src]
		build := cmdexec.run(vexe, args)
		assert build.exit_code == 0, build.output
		assert os.is_file(lib_so)
		for name in os.ls(workdir)! {
			assert !name.starts_with('.libmylib_${mode}.so.v3cc.'), name
		}
		nm := cmdexec.run('nm', ['-D', '--defined-only', '--format=posix', lib_so])
		assert nm.exit_code == 0, nm.output
		mut symbols := []string{}
		for line in nm.output.split_into_lines() {
			fields := line.fields()
			if fields.len > 0 {
				symbols << fields[0]
			}
		}
		symbols.sort()
		assert symbols == ['mylib_compute', 'mylib_counter', 'mylib_private_export'], '${mode}:\n${nm.output}'
		// Hidden implementation symbols must remain callable from the exported
		// wrappers; checking nm alone would not verify the library's behavior.
		host_bin := os.join_path(workdir, 'host_${mode}')
		link := cmdexec.run('cc', [host_src, lib_so, '-Wl,-rpath,${workdir}', '-o', host_bin])
		assert link.exit_code == 0, link.output
		run := cmdexec.run(host_bin, []string{})
		assert run.exit_code == 0, run.output
	}
	project_dir := os.join_path(workdir, 'generated')
	generated := cmdexec.run(vexe, ['-new-compiler', '-nocache', '-enable-globals', '-shared',
		'-generate-c-project', project_dir, lib_src])
	assert generated.exit_code == 0, generated.output
	assert os.is_file(os.join_path(project_dir, 'exports.map'))
	build_command := os.read_file(os.join_path(project_dir, 'build_command.txt'))!
	assert build_command.contains('--version-script')
	project_build := cmdexec.run('sh', [os.join_path(project_dir, 'build.sh')])
	assert project_build.exit_code == 0, project_build.output
	project_nm := cmdexec.run('nm', ['-D', '--defined-only', '--format=posix',
		os.join_path(project_dir, 'mylib')])
	assert project_nm.exit_code == 0, project_nm.output
	assert project_nm.output.contains('mylib_compute'), project_nm.output
	assert !project_nm.output.contains('mylib_direct_object_symbol'), project_nm.output
	user_script := os.join_path(workdir, 'user.map')
	os.write_file(user_script, 'V1 { global: mylib_compute; local: *; };\n')!
	user_out := os.join_path(workdir, 'libmylib_user')
	user_build := cmdexec.run(vexe, ['-new-compiler', '-nocache', '-enable-globals', '-shared',
		'-ldflags', '-Wl,--version-script,${user_script}', '-o', user_out, lib_src])
	assert user_build.exit_code == 0, user_build.output
	user_nm := cmdexec.run('nm', ['-D', '--defined-only', '--format=posix', '${user_out}.so'])
	assert user_nm.exit_code == 0, user_nm.output
	assert user_nm.output.contains('mylib_compute'), user_nm.output
	assert !user_nm.output.contains('mylib_counter'), user_nm.output
	old_cflags := os.getenv_opt('CFLAGS')
	os.setenv('CFLAGS', '-Wl,--version-script,${user_script}', true)
	defer {
		if value := old_cflags {
			os.setenv('CFLAGS', value, true)
		} else {
			os.unsetenv('CFLAGS')
		}
	}
	env_out := os.join_path(workdir, 'libmylib_cflags')
	env_build := cmdexec.run(vexe, ['-new-compiler', '-nocache', '-enable-globals', '-shared',
		'-o', env_out, lib_src])
	assert env_build.exit_code == 0, env_build.output
	env_nm := cmdexec.run('nm', ['-D', '--defined-only', '--format=posix', '${env_out}.so'])
	assert env_nm.exit_code == 0, env_nm.output
	assert env_nm.output.contains('mylib_compute'), env_nm.output
	assert !env_nm.output.contains('mylib_counter'), env_nm.output
}

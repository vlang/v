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
	lib_src := os.join_path(workdir, 'mylib.v')
	os.write_file(lib_src, [
		'module mylib',
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
		'int main(void) {',
		'\treturn mylib_compute(21) != 43 || mylib_private_export(21) != 42;',
		'}',
		'',
	].join('\n'))!
	for is_prod in [false, true] {
		mode := if is_prod { 'prod' } else { 'debug' }
		lib_out := os.join_path(workdir, 'libmylib_${mode}')
		lib_so := '${lib_out}.so'
		// Isolate the generated V symbols from any platform GC archive exports.
		// TCC does not honor hidden visibility; exercise the system C compiler.
		mut args := ['-new-compiler', '-nocache', '-cc', 'cc', '-gc', 'none', '-shared']
		if is_prod {
			args << '-prod'
		}
		args << ['-o', lib_out, lib_src]
		build := cmdexec.run(vexe, args)
		assert build.exit_code == 0, build.output
		assert os.is_file(lib_so)
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
		assert symbols == ['mylib_compute', 'mylib_private_export'], '${mode}:\n${nm.output}'
		// Hidden implementation symbols must remain callable from the exported
		// wrappers; checking nm alone would not verify the library's behavior.
		host_bin := os.join_path(workdir, 'host_${mode}')
		link := cmdexec.run('cc', [host_src, lib_so, '-Wl,-rpath,${workdir}', '-o', host_bin])
		assert link.exit_code == 0, link.output
		run := cmdexec.run(host_bin, []string{})
		assert run.exit_code == 0, run.output
	}
}

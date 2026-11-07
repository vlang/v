module main

import os
import v.pref

fn project_backend_fixture(name string, manifest string) string {
	root := os.join_path(os.vtmp_dir(), 'project_backend_test_${os.getpid()}', name)
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'sub')) or { panic(err) }
	// Keep the walk from reaching a v.mod above the temp directory.
	os.write_file(os.join_path(root, pref.module_search_stop_marker), '') or { panic(err) }
	if manifest != '' {
		os.write_file(os.join_path(root, 'v.mod'), manifest) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), 'fn main() {}\n') or { panic(err) }
	os.write_file(os.join_path(root, 'sub', 'a_test.v'), 'fn test_a() {}\n') or { panic(err) }
	return root
}

fn testsuite_end() {
	os.rmdir_all(os.join_path(os.vtmp_dir(), 'project_backend_test_${os.getpid()}')) or {}
}

fn test_compiler_input_path_skips_options_and_commands() {
	assert compiler_input_path(['main.v']) == 'main.v'
	assert compiler_input_path(['-o', 'out', '-path', 'a|b', '.']) == '.'
	assert compiler_input_path(['-g', 'run', 'main.v']) == 'main.v'
	assert compiler_input_path(['test', 'sub/']) == 'sub/'
	assert compiler_input_path(['build', 'build']) == 'build'
	assert compiler_input_path(['-e', 'println(1)']) == ''
	assert compiler_input_path(['run', '-']) == ''
	assert compiler_input_path(['-silent']) == ''
}

fn test_backend_option_given_reads_every_spelling() {
	assert backend_option_given(['-b', 'c', 'main.v'])
	assert backend_option_given(['-backend', 'fastc', 'main.v'])
	assert backend_option_given(['-b=c', 'main.v'])
	assert !backend_option_given(['-compile-backend', 'fastc', 'main.v'])
	assert !backend_option_given(['-o', '-b', 'main.v'])
	assert !backend_option_given(['main.v'])
}

fn test_project_backend_is_the_default_for_the_project() {
	root := project_backend_fixture('fastc', "Module {\n\tname: 'app'\n\tbackend: 'fastc'\n}\n")
	main_file := os.join_path(root, 'main.v')
	test_dir := os.join_path(root, 'sub')
	assert with_project_backend([root]) == ['-b', 'fastc', root]
	assert with_project_backend(['-g', 'run', main_file, 'x']) == ['-b', 'fastc', '-g', 'run',
		main_file, 'x']
	assert with_project_backend(['test', test_dir]) == ['-b', 'fastc', 'test', test_dir]
	assert with_project_backend([os.join_path(test_dir, 'a_test.v')])[..2] == ['-b', 'fastc']
	// The command line wins over v.mod.
	assert with_project_backend(['-b', 'c', root]) == ['-b', 'c', root]
	// Arguments of the program that `run` starts are not compiler options.
	assert with_project_backend(['run', main_file, '-b', 'c']) == ['-b', 'fastc', 'run', main_file,
		'-b', 'c']
	// Inline source and stdin have no project.
	assert with_project_backend(['-e', 'println(1)']) == ['-e', 'println(1)']
	assert with_project_backend(['run', '-']) == ['run', '-']
}

fn test_project_without_backend_keeps_the_command_line() {
	plain := project_backend_fixture('plain', "Module {\n\tname: 'app'\n}\n")
	assert with_project_backend([plain]) == [plain]
	no_manifest := project_backend_fixture('none', '')
	assert with_project_backend([no_manifest]) == [no_manifest]
	invalid := project_backend_fixture('invalid', 'not a manifest')
	assert with_project_backend([invalid]) == [invalid]
}

fn test_unknown_project_backend_is_reported_with_its_manifest() {
	root := project_backend_fixture('unknown', "Module {\n\tname: 'app'\n\tbackend: 'pascal'\n}\n")
	res := os.exec([@VEXE, root])
	assert res.exit_code == 1
	assert res.output.contains(os.join_path(os.real_path(root), 'v.mod'))
	assert res.output.contains('unknown backend `pascal`; expected c, fastc, arm64, x64, wasm')
	// An explicit backend does not read the manifest's choice.
	assert with_project_backend(['-b', 'c', root]) == ['-b', 'c', root]
}

// run_launcher_in runs this launcher from `dir`, the way a shell in that folder would.
fn run_launcher_in(dir string, args []string, stdin_path string) os.Result {
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment[v3_no_fallback_env] = '1'
	mut process := os.new_process(@VEXE)
	process.set_args(args)
	process.set_work_folder(dir)
	process.set_environment(environment)
	if stdin_path != '' {
		process.set_stdin_path(stdin_path)
	}
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

fn test_inline_source_and_stdin_belong_to_no_project() {
	// The launcher rejects this manifest as soon as it applies it, so success proves that the
	// temporary sources that `-e` and `run -` write into the folder skip it.
	root := project_backend_fixture('inline', "Module {\n\tname: 'app'\n\tbackend: 'pascal'\n}\n")
	inline := run_launcher_in(root, ['-e', 'println(41 + 1)'], '')
	assert inline.exit_code == 0, inline.output
	assert inline.output.contains('42'), inline.output
	stdin_source := os.join_path(os.dir(root), 'inline_stdin.v')
	os.write_file(stdin_source, 'println(43)\n')!
	piped := run_launcher_in(root, ['run', '-'], stdin_source)
	assert piped.exit_code == 0, piped.output
	assert piped.output.contains('43'), piped.output
	// The folder's own sources still use the manifest.
	project := run_launcher_in(root, ['run', '.'], '')
	assert project.exit_code == 1, project.output
	assert project.output.contains('unknown backend `pascal`'), project.output
}

fn test_test_command_hands_the_project_backend_to_the_test_runner() {
	// The runner selects files and evaluates `// vtest build:` constraints for the backend in
	// the options before `test`; its reproduction command shows those options.
	root := project_backend_fixture('vtest', "Module {\n\tname: 'app'\n\tbackend: 'c'\n}\n")
	os.write_file(os.join_path(root, 'sub', 'a_test.v'), 'fn test_a() {\n\tassert false\n}\n')!
	tested := run_launcher_in(root, ['test', 'sub'], '')
	assert tested.exit_code == 1, tested.output
	assert tested.output.contains(' -b c '), tested.output
	// An unknown manifest backend stops `v test` before the runner starts.
	unknown := project_backend_fixture('vtest_unknown', "Module {\n\tname: 'app'\n\tbackend: 'pascal'\n}\n")
	rejected := run_launcher_in(unknown, ['test', '.'], '')
	assert rejected.exit_code == 1, rejected.output
	assert rejected.output.contains('unknown backend `pascal`'), rejected.output
	assert !rejected.output.contains('Summary for all V _test.v files'), rejected.output
}

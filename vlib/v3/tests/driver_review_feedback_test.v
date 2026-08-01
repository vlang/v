import os
import v3.cmdexec

const driver_review_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const driver_review_v3_dir = os.dir(os.dir(@FILE))
const driver_review_v3_src = os.join_path(driver_review_v3_dir, 'v3.v')

fn driver_review_environment() map[string]string {
	mut environment := os.environ()
	environment['CFLAGS'] = ''
	environment['LDFLAGS'] = ''
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	return environment
}

fn run_driver_review_process(program string, args []string, environment map[string]string) os.Result {
	mut process := os.new_process(program)
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	result := os.Result{
		exit_code: process.code
		output:    output
	}
	process.close()
	return result
}

fn build_driver_review_v3(root string) string {
	v3_bin := os.join_path(root, 'v3_review_driver')
	result := run_driver_review_process(@VEXE, ['-old-compiler', '-gc', 'none', '-path',
		'${driver_review_vlib_dir}|@vlib|@vmodules', '-o', v3_bin, driver_review_v3_src],
		driver_review_environment())
	assert result.exit_code == 0, result.output
	return v3_bin
}

fn test_driver_preserves_delegated_cli_modes() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_review_feedback_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_review_v3(root)

	include_dir := os.join_path(root, 'review headers')
	lib_dir := os.join_path(root, 'review libraries')
	os.mkdir_all(include_dir)!
	os.mkdir_all(lib_dir)!
	os.write_file(os.join_path(include_dir, 'review_env.h'), '#ifndef V3_REVIEW_ENV
#error CFLAGS define was not preserved
#endif

int review_env_value(void);
')!
	lib_source := os.join_path(root, 'review_env.c')
	lib_object := os.join_path(root, 'review_env.o')
	os.write_file(lib_source, 'int review_env_value(void) { return 73; }\n')!
	lib_compile := cmdexec.run('cc', ['-c', lib_source, '-o', lib_object])
	assert lib_compile.exit_code == 0, lib_compile.output
	archive := os.join_path(lib_dir, 'libreview_env.a')
	archive_build := cmdexec.run('ar', ['rcs', archive, lib_object])
	assert archive_build.exit_code == 0, archive_build.output
	environment_source := os.join_path(root, 'environment.v')
	os.write_file(environment_source, 'module main

#include "review_env.h"

fn C.review_env_value() int

fn main() {
	println(C.review_env_value())
}
')!
	environment_output := os.join_path(root, 'environment_program')
	mut environment := driver_review_environment()
	environment['CFLAGS'] = '-I "${include_dir}" -DV3_REVIEW_ENV=1'
	environment['LDFLAGS'] = '-L "${lib_dir}" -lreview_env'
	environment_compile := run_driver_review_process(v3_bin, ['-nocache', '-cc', 'cc', '-o',
		environment_output, environment_source], environment)
	assert environment_compile.exit_code == 0, environment_compile.output
	environment_run := cmdexec.run(environment_output, [])
	assert environment_run.exit_code == 0, environment_run.output
	assert environment_run.output == '73\n', environment_run.output

	object_source := os.join_path(root, 'unit.v')
	os.write_file(object_source, "module main

@[export: 'review_answer']
pub fn answer() int {
	return 42
}
")!
	object_output := os.join_path(root, 'unit.o')
	object_compile := run_driver_review_process(v3_bin, ['-nocache', '-cc', 'cc', '-o', object_output,
		object_source], driver_review_environment())
	assert object_compile.exit_code == 0, object_compile.output
	assert os.is_file(object_output)
	probe_source := os.join_path(root, 'object_probe.c')
	probe_output := os.join_path(root, 'object_probe')
	os.write_file(probe_source, 'int review_answer(void);

int main(void) {
	return review_answer() == 42 ? 0 : 1;
}
')!
	probe_compile := cmdexec.run('cc', [probe_source, object_output, '-o', probe_output])
	assert probe_compile.exit_code == 0, probe_compile.output
	probe_run := cmdexec.run(probe_output, [])
	assert probe_run.exit_code == 0, probe_run.output

	first_root := os.join_path(root, 'modules_first')
	second_root := os.join_path(root, 'modules_second')
	os.mkdir_all(os.join_path(first_root, 'chosen'))!
	os.mkdir_all(os.join_path(second_root, 'chosen'))!
	os.write_file(os.join_path(first_root, 'chosen', 'chosen.v'), 'module chosen

pub fn value() int {
	return 11
}
')!
	os.write_file(os.join_path(second_root, 'chosen', 'chosen.v'), 'module chosen

pub fn value() int {
	return 82
}
')!
	path_source := os.join_path(root, 'path_order.v')
	os.write_file(path_source, 'module main

import chosen

fn main() {
	println(chosen.value())
}
')!
	path_output := os.join_path(root, 'path_order')
	path_compile := run_driver_review_process(v3_bin, ['-nocache', '-path',
		'${second_root}|${first_root}|@vlib', '-o', path_output, path_source],
		driver_review_environment())
	assert path_compile.exit_code == 0, path_compile.output
	path_run := cmdexec.run(path_output, [])
	assert path_run.exit_code == 0, path_run.output
	assert path_run.output == '82\n', path_run.output

	printfn_source := os.join_path(root, 'printfn.v')
	os.write_file(printfn_source, 'module main

fn selected() int {
	return 3
}

fn ignored() int {
	return 4
}

fn main() {
	println(selected() + ignored())
}
')!
	printfn_output := os.join_path(root, 'printfn.c')
	printfn_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-printfn',
		'main__selected', '-o', printfn_output, printfn_source], driver_review_environment())
	assert printfn_compile.exit_code == 0, printfn_compile.output
	assert printfn_compile.output.contains('selected('), printfn_compile.output
	assert !printfn_compile.output.contains('ignored('), printfn_compile.output
	generated_c := os.read_file(printfn_output)!
	assert generated_c.contains('selected(')
	assert generated_c.contains('ignored(')
	main_printfn_output := os.join_path(root, 'printfn_main.c')
	main_printfn_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-printfn',
		'main__main', '-o', main_printfn_output, printfn_source], driver_review_environment())
	assert main_printfn_compile.exit_code == 0, main_printfn_compile.output
	assert main_printfn_compile.output.contains('int main(int argc, char** argv)'), main_printfn_compile.output

	js_source := os.join_path(root, 'alias.v')
	os.write_file(js_source, "fn main() {\n\tprintln('js alias')\n}\n")!
	js_output := os.join_path(root, 'alias.js')
	js_compile := run_driver_review_process(v3_bin, ['-b', 'js_node', '-o', js_output, js_source],
		driver_review_environment())
	assert js_compile.exit_code == 0, js_compile.output
	assert os.is_file(js_output)
}

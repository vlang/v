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

	cross_c_source := os.join_path(root, 'cross_target.v')
	cross_c_output := os.join_path(root, 'cross_target.c')
	os.write_file(cross_c_source, 'fn main() {}\n')!
	cross_target_os := if os.user_os() == 'windows' { 'linux' } else { 'windows' }
	cross_c_compile := run_driver_review_process(v3_bin, ['-nocache', '-os', cross_target_os, '-o',
		cross_c_output, cross_c_source], driver_review_environment())
	assert cross_c_compile.exit_code == 0, cross_c_compile.output
	assert os.is_file(cross_c_output)
	assert os.read_file(cross_c_output)!.contains('int main(int argc, char** argv)')

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

	multi_extension_source := os.join_path(root, 'multi_extension.c.v')
	multi_extension_output := os.join_path(root, 'multi_extension')
	os.write_file(multi_extension_source, 'fn main() {}\n')!
	multi_extension_compile := run_driver_review_process(v3_bin,
		['-nocache', multi_extension_source], driver_review_environment())
	assert multi_extension_compile.exit_code == 0, multi_extension_compile.output
	assert os.is_file(multi_extension_output)
	assert !os.exists(os.join_path(root, 'multi_extension.c'))

	bounds_source := os.join_path(root, 'forced_bounds.v')
	bounds_output := os.join_path(root, 'forced_bounds')
	os.write_file(bounds_source, '@[direct_array_access]
fn unchecked_at(values []int, index int) int {
	return values[index]
}

fn main() {
	println(unchecked_at([1], 1))
}
')!
	bounds_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache',
		'-force-bounds-checking', '-o', bounds_output, bounds_source], driver_review_environment())
	assert bounds_compile.exit_code == 0, bounds_compile.output
	bounds_run := cmdexec.run(bounds_output, [])
	assert bounds_run.exit_code != 0, bounds_run.output
	assert bounds_run.output.contains('index out of range'), bounds_run.output

	overflow_cases := {
		'add': 'fn checked(a i8, b i8) i8 { return a + b }\nfn main() { println(checked(i8(127), i8(1))) }\n'
		'sub': 'fn checked(a i8, b i8) i8 { return a - b }\nfn main() { println(checked(i8(-128), i8(1))) }\n'
		'mul': 'fn checked(a i8, b i8) i8 { return a * b }\nfn main() { println(checked(i8(64), i8(2))) }\n'
	}
	for operation, source in overflow_cases {
		overflow_source := os.join_path(root, 'overflow_${operation}.v')
		overflow_output := os.join_path(root, 'overflow_${operation}')
		os.write_file(overflow_source, source)!
		overflow_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache',
			'-check-overflow', '-o', overflow_output, overflow_source], driver_review_environment())
		assert overflow_compile.exit_code == 0, overflow_compile.output
		overflow_run := cmdexec.run(overflow_output, [])
		assert overflow_run.exit_code != 0, '${operation}: ${overflow_run.output}'
		assert overflow_run.output.contains('integer overflow'), '${operation}: ${overflow_run.output}'
	}
	ignored_overflow_source := os.join_path(root, 'ignored_overflow.v')
	ignored_overflow_output := os.join_path(root, 'ignored_overflow')
	os.write_file(ignored_overflow_source, '@[ignore_overflow]
fn wrapping_add(value u32) u32 {
	return value + 1
}

fn main() {
	println(wrapping_add(u32(0xffffffff)))
}
')!
	ignored_overflow_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache',
		'-check-overflow', '-o', ignored_overflow_output, ignored_overflow_source],
		driver_review_environment())
	assert ignored_overflow_compile.exit_code == 0, ignored_overflow_compile.output
	ignored_overflow_run := cmdexec.run(ignored_overflow_output, [])
	assert ignored_overflow_run.exit_code == 0, ignored_overflow_run.output
	assert ignored_overflow_run.output.trim_space() == '0', ignored_overflow_run.output

	warning_source := os.join_path(root, 'warning.v')
	os.write_file(warning_source, '@[deprecated]\nfn old() {}\n\nfn main() {\n\told()\n}\n')!
	warning_output := os.join_path(root, 'warning')
	warning_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-o', warning_output,
		warning_source], driver_review_environment())
	assert warning_compile.exit_code == 0, warning_compile.output
	assert warning_compile.output.contains('warning:'), warning_compile.output
	warning_error := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-W', '-o',
		warning_output, warning_source], driver_review_environment())
	assert warning_error.exit_code != 0, warning_error.output
	assert warning_error.output.contains('error:'), warning_error.output
	assert warning_error.output.contains('has been deprecated'), warning_error.output

	parser_warning_source := os.join_path(root, 'parser_warning.v')
	os.write_file(parser_warning_source, 'fn main() {\n\t_ := typeof(1)\n}\n')!
	parser_warning_output := os.join_path(root, 'parser_warning')
	parser_warning := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-o',
		parser_warning_output, parser_warning_source], driver_review_environment())
	assert parser_warning.exit_code == 0, parser_warning.output
	assert parser_warning.output.contains('warning:'), parser_warning.output
	parser_warning_error := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-W', '-o',
		parser_warning_output, parser_warning_source], driver_review_environment())
	assert parser_warning_error.exit_code != 0, parser_warning_error.output
	assert parser_warning_error.output.contains('error:'), parser_warning_error.output
	assert parser_warning_error.output.contains('use e.g. `typeof(expr).name`'), parser_warning_error.output
	parser_warning_prod := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-prod', '-o',
		parser_warning_output, parser_warning_source], driver_review_environment())
	assert parser_warning_prod.exit_code != 0, parser_warning_prod.output
	assert parser_warning_prod.output.contains('error:'), parser_warning_prod.output
	assert parser_warning_prod.output.contains('use e.g. `typeof(expr).name`'), parser_warning_prod.output

	clean_impure_text_source := os.join_path(root, 'clean_impure_text.v')
	clean_impure_text_output := os.join_path(root, 'clean_impure_text')
	os.write_file(clean_impure_text_source,
		"// C.comment() and JS.comment()\nfn main() { println('C.foo JS.bar') }\n")!
	clean_impure_text := run_driver_review_process(v3_bin, ['-silent', '-Wimpure-v', '-W', '-o',
		clean_impure_text_output, clean_impure_text_source], driver_review_environment())
	assert clean_impure_text.exit_code == 0, clean_impure_text.output

	directory_project := os.join_path(root, 'impure_directory')
	os.mkdir_all(directory_project)!
	os.write_file(os.join_path(directory_project, 'main.v'),
		'module main\n\nfn main() { call_c() }\n')!
	directory_interop_file := os.join_path(directory_project, 'interop.v')
	os.write_file(directory_interop_file,
		"module main\n\nfn C.puts(&char) int\n\nfn call_c() { C.puts(c'impure') }\n")!
	directory_impure := run_driver_review_process(v3_bin, ['-silent', '-Wimpure-v', '-W', '-o',
		os.join_path(root, 'impure_directory_output'), directory_project],
		driver_review_environment())
	assert directory_impure.exit_code != 0, directory_impure.output
	assert directory_impure.output.contains('C code will not be allowed in pure .v files'), directory_impure.output

	assert directory_impure.output.contains(directory_interop_file), directory_impure.output

	import_project := os.join_path(root, 'impure_import')
	import_module := os.join_path(import_project, 'impurejs')
	os.mkdir_all(import_module)!
	os.write_file(os.join_path(import_project, 'main.v'),
		'module main\n\nimport impurejs\n\nfn main() { impurejs.call_js() }\n')!
	import_interop_file := os.join_path(import_module, 'impurejs.v')
	os.write_file(import_interop_file,
		'module impurejs\n\nfn JS.do_work()\n\npub fn call_js() { JS.do_work() }\n')!
	import_impure := run_driver_review_process(v3_bin, ['-silent', '-Wimpure-v', '-W', '-o',
		os.join_path(root, 'impure_import_output'), os.join_path(import_project, 'main.v')],
		driver_review_environment())
	assert import_impure.exit_code != 0, import_impure.output
	assert import_impure.output.contains('JS code will not be allowed in pure .v files'), import_impure.output

	assert import_impure.output.contains(import_interop_file), import_impure.output

	notice_source := os.join_path(root, 'notice.v')
	os.write_file(notice_source, 'fn unused() {}\n\nfn main() {}\n')!
	notice_output := os.join_path(root, 'notice')
	notice_compile := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-o', notice_output,
		notice_source], driver_review_environment())
	assert notice_compile.exit_code == 0, notice_compile.output
	assert notice_compile.output.contains('notice:'), notice_compile.output
	notice_error := run_driver_review_process(v3_bin, ['-silent', '-nocache', '-N', '-o',
		notice_output, notice_source], driver_review_environment())
	assert notice_error.exit_code != 0, notice_error.output
	assert notice_error.output.contains('error:'), notice_error.output
	assert notice_error.output.contains('unused function'), notice_error.output
}

fn test_driver_cache_separates_check_and_semantic_modes() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_cache_review_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_review_v3(root)
	environment := driver_review_environment()

	check_source := os.join_path(root, 'check_only.v')
	check_output := os.join_path(root, 'check_only')
	os.write_file(check_source, "fn main() {\n\tprintln('cached')\n}\n")!
	warm_check_cache := run_driver_review_process(v3_bin, ['-silent', '-o', check_output,
		check_source], environment)
	assert warm_check_cache.exit_code == 0, warm_check_cache.output
	os.write_file(check_output, 'check-only-sentinel')!
	check_only := run_driver_review_process(v3_bin, ['-silent', '-check', '-o', check_output,
		check_source], environment)
	assert check_only.exit_code == 0, check_only.output
	assert os.read_file(check_output)! == 'check-only-sentinel'

	globals_source := os.join_path(root, 'globals.v')
	globals_output := os.join_path(root, 'globals')
	os.write_file(globals_source,
		'module main\n\n__global cached_global int\n\nfn main() {\n\tcached_global = 42\n\tprintln(cached_global)\n}\n')!
	uncached_strict_globals := run_driver_review_process(v3_bin, ['-silent', '-no-parallel',
		'-nocache', '-o', globals_output, globals_source], environment)
	assert uncached_strict_globals.exit_code != 0, uncached_strict_globals.output
	warm_globals_cache := run_driver_review_process(v3_bin, ['-silent', '-no-parallel',
		'-enable-globals', '-o', globals_output, globals_source], environment)
	assert warm_globals_cache.exit_code == 0, warm_globals_cache.output
	strict_globals := run_driver_review_process(v3_bin, ['-silent', '-no-parallel', '-o',
		globals_output, globals_source], environment)
	assert strict_globals.exit_code != 0, strict_globals.output
	assert strict_globals.output.contains('use `v -enable-globals ...` to enable globals'), strict_globals.output

	translated_source := os.join_path(root, 'translated.v')
	translated_output := os.join_path(root, 'translated')
	os.write_file(translated_source,
		'module main\n\nfn next() int {\n\tmut static value := 0\n\tvalue++\n\treturn value\n}\n\nfn main() {\n\tprintln(next())\n}\n')!
	warm_translated_cache := run_driver_review_process(v3_bin, ['-silent', '-translated', '-o',
		translated_output, translated_source], environment)
	assert warm_translated_cache.exit_code == 0, warm_translated_cache.output
	strict_translated := run_driver_review_process(v3_bin, ['-silent', '-o', translated_output,
		translated_source], environment)
	assert strict_translated.exit_code != 0, strict_translated.output
	assert strict_translated.output.contains('static variables are supported only in -translated mode'), strict_translated.output
}

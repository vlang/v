module main

import os

const parallel_cc_test_path = os.join_path(os.vtmp_dir(), 'parallel_cc_failure_test')
const parallel_cc_vexe = @VEXE

fn run_parallel_cc_case(case_name string, files map[string]string) os.Result {
	return run_parallel_cc_case_with_env(case_name, files, {})
}

fn run_parallel_cc_case_with_env(case_name string, files map[string]string, env_overrides map[string]string) os.Result {
	return run_parallel_cc_case_with_args(case_name, ['-parallel-cc', 'main.v'], files,
		env_overrides)
}

fn run_parallel_cc_case_with_args(case_name string, args []string, files map[string]string, env_overrides map[string]string) os.Result {
	case_dir := os.join_path(parallel_cc_test_path, case_name)
	os.rmdir_all(case_dir) or {}
	os.mkdir_all(case_dir) or { panic(err) }
	defer {
		os.rmdir_all(case_dir) or {}
	}
	for rel_path, contents in files {
		fpath := os.join_path(case_dir, rel_path)
		os.mkdir_all(os.dir(fpath)) or { panic(err) }
		os.write_file(fpath, contents) or { panic(err) }
	}
	mut env := os.environ()
	for key, value in env_overrides {
		env[key] = value
	}
	mut p := os.new_process(parallel_cc_vexe)
	p.set_work_folder(case_dir)
	p.set_args(args)
	p.set_environment(env)
	p.set_redirect_stdio()
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	exit_code := p.code
	p.close()
	return os.Result{
		exit_code: exit_code
		output:    '${stdout}${stderr}'
	}
}

fn run_parallel_cc_failure_case(case_name string, files map[string]string) os.Result {
	return run_parallel_cc_case(case_name, files)
}

fn test_parallel_cc_fails_when_c_compilation_fails() {
	res := run_parallel_cc_failure_case('compile_fail', {
		'main.v': 'module main
	#include "definitely_missing_parallel_cc_header.h"
fn main() {}
'
	})
	assert res.exit_code == 1, res.output
	assert res.output.contains('failed parallel C compilation'), res.output
}

fn test_parallel_cc_fails_when_final_linking_fails() {
	res := run_parallel_cc_failure_case('link_fail', {
		'parallel_cc_duplicate_symbol.h': 'int parallel_cc_duplicate_symbol(void) { return 42; }
'
		'main.v':                         'module main
#include "@DIR/parallel_cc_duplicate_symbol.h"
fn C.parallel_cc_duplicate_symbol() int
fn main() {
	println(C.parallel_cc_duplicate_symbol())
}
'
	})
	assert res.exit_code == 1, res.output
	assert res.output.contains('failed to link after parallel C compilation'), res.output
}

fn test_parallel_cc_succeeds_with_array_sort_compare_helper() {
	res := run_parallel_cc_case('array_sort_compare_helper', {
		'main.v': 'module main
	fn main() {
		mut xs := [3, 1, 2]
		xs.sort(a < b)
		println(xs)
	}
	'
	})
	assert res.exit_code == 0, res.output
}

fn test_parallel_cc_uses_resolved_compiler_even_when_cc_env_is_invalid() {
	res := run_parallel_cc_case_with_env('ignore_invalid_cc_env', {
		'main.v': 'module main
fn main() {
	println("hello")
}
'
	}, {
		'CC': 'definitely_missing_parallel_cc_compiler_23174'
	})
	assert res.exit_code == 0, res.output
}

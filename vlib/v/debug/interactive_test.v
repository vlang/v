import os
import term
import time

const vexe = @VEXE
const expect_tests_path = os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests')
const test_module_path = os.join_path(os.vtmp_dir(), 'test_vdbg_input')
const bar = term.yellow('-'.repeat(100))

const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})

fn testsuite_begin() {
	os.chdir(@VEXEROOT) or {}
	os.rmdir_all(test_module_path) or {}
	os.mkdir_all(test_module_path) or {}
	dump(test_module_path)
}

fn gprintln(msg string) {
	println(term.gray(msg))
}

fn test_debugger() {
	os.chdir(test_module_path)!
	all_expect_files := os.walk_ext(expect_tests_path, '.expect')
	assert all_expect_files.len > 0, 'no .expect files found in ${expect_tests_path}'
	for eidx, efile in all_expect_files {
		vfile := efile.replace('.expect', '.vv')
		output_file := os.join_path(test_module_path, os.file_name(efile).replace('.expect',
			'.exe'))

		println(bar)
		gprintln('>>>> running test [${eidx + 1}/${all_expect_files.len}], ${term.magenta(efile)} ...')

		compile_sw := time.new_stopwatch()
		comp_res := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(output_file)} ${os.quoted_path(vfile)}')
		gprintln('>>>>>>>>>>> compilation took ${compile_sw.elapsed().milliseconds()} ms, comp_res: ${comp_res}')

		expect_cmd := '${expect_exe} -c "set timeout 15" ${os.quoted_path(efile)} ${os.quoted_path(output_file)} ${os.quoted_path(vfile)}'
		println(term.cyan(expect_cmd))
		flush_stdout()
		sw := time.new_stopwatch()
		res := os.system(expect_cmd)
		gprintln('>>>>>>>>>>> expect took: ${sw.elapsed().milliseconds()} ms, res: ${res}')

		if res != 0 {
			assert false, term.red('failed expect cmd: ${expect_cmd}')
		}
		assert true
	}
	os.chdir(@VEXEROOT) or {}
	os.rmdir_all(test_module_path) or {}

	println(bar)
	gprintln(term.bold('A total of ${all_expect_files.len} tests for the debugger are OK'))
}

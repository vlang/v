import os
import term
import time

const vexe = @VEXE
const expect_tests_path = os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests')
const test_module_path = os.join_path(os.vtmp_dir(), 'test_vdbg_input')
const bar = term.yellow('-'.repeat(107))
const be_verbose = os.getenv('GITHUB_JOB') != '' || os.getenv('VERBOSE') != ''

const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})

fn testsuite_begin() {
	os.chdir(@VEXEROOT) or {}
	os.rmdir_all(test_module_path) or {}
	os.mkdir_all(test_module_path) or {}
}

fn gprintln(msg string) {
	println(term.green(msg))
	flush_stdout()
}

fn gprint(msg string) {
	print(term.green(msg))
	flush_stdout()
}

fn test_debugger() {
	all_expect_files := os.walk_ext(expect_tests_path, '.expect')
	assert all_expect_files.len > 0, 'no .expect files found in ${expect_tests_path}'
	mut oks := 0
	for eidx, efile in all_expect_files.sorted() {
		// if !efile.contains('sumtype') { gprintln('skipping $efile') continue }
		vfile := efile.replace('.expect', '.vv')
		output_file := os.join_path(test_module_path, os.file_name(efile).replace('.expect',
			'.exe'))

		if be_verbose {
			println(bar)
		}
		gprint('>>>> Running [${eidx + 1:2}/${all_expect_files.len:-2}] ${term.magenta(efile):-68} ... ')
		if be_verbose {
			println('')
		}

		compile_sw := time.new_stopwatch()
		comp_res := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(output_file)} ${os.quoted_path(vfile)}')
		cdur_ms := compile_sw.elapsed().milliseconds()
		if be_verbose {
			gprintln('>>>>>>>>>>> compilation took ${cdur_ms} ms, comp_res: ${comp_res}')
		}

		verbose_options := if be_verbose { '-d' } else { '' }
		expect_cmd := '${expect_exe} ${verbose_options} -c "cd ${expect_tests_path}" ${os.quoted_path(efile)} ${os.quoted_path(output_file)} ${os.quoted_path(vfile)}'
		if be_verbose {
			gprintln(term.cyan(expect_cmd))
		}
		sw := time.new_stopwatch()
		mut res := 0
		if be_verbose {
			res = os.system(expect_cmd)
		} else {
			result := os.execute(expect_cmd)
			res = result.exit_code
			if res != 0 {
				eprintln(result.output)
			}
		}
		edur_ms := sw.elapsed().milliseconds()
		if be_verbose {
			gprintln('>>>>>>>>>>> expect took: ${edur_ms} ms, res: ${res}')
		}
		if res != 0 {
			assert false, term.red('failed expect cmd: ${expect_cmd}')
		}
		assert true
		oks++
		if !be_verbose {
			gprintln('    c: ${cdur_ms} ms, e: ${edur_ms} ms')
		}
	}
	os.chdir(@VEXEROOT) or {}
	os.rmdir_all(test_module_path) or {}

	println(bar)
	gprintln('Passed debugger tests: ${term.bold(oks.str())} of ${all_expect_files.len} total.')
}

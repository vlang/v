import os
import term
import time

const vexe = @VEXE
const trace_tests_path = os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests', 'trace')
const bar = term.yellow('-'.repeat(105))
const be_verbose = os.getenv('GITHUB_JOB') != '' || os.getenv('VERBOSE') != ''

const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})

fn testsuite_begin() {
	os.chdir(@VEXEROOT) or {}
}

fn gprintln(msg string) {
	println(term.green(msg))
	flush_stdout()
}

fn gprint(msg string) {
	print(term.green(msg))
	flush_stdout()
}

fn test_trace() {
	all_expect_files := os.walk_ext(trace_tests_path, '.v')
	assert all_expect_files.len > 0, 'no .v files found in ${trace_tests_path}'
	mut oks := 0
	for eidx, efile in all_expect_files.sorted() {
		// if !efile.contains('sumtype') { gprintln('skipping $efile') continue }
		vfile := efile

		if be_verbose {
			println(bar)
		}
		gprint('>>>> Running [${eidx + 1}/${all_expect_files.len}] ${term.magenta(efile):-68} ... ')
		if be_verbose {
			println('')
		}

		compile_sw := time.new_stopwatch()
		comp_res := os.system('${os.quoted_path(vexe)} -d trace test ${os.quoted_path(vfile)}')
		cdur_ms := compile_sw.elapsed().milliseconds()
		if be_verbose {
			gprintln('>>>>>>>>>>> compilation took ${cdur_ms} ms, comp_res: ${comp_res}')
		}
		if comp_res != 0 {
			assert false, term.red('failed test cmd: ${vfile}')
		}
		assert true
		oks++
	}
	os.chdir(@VEXEROOT) or {}

	println(bar)
	gprintln('Passed debugger tests: ${term.bold(oks.str())} of ${all_expect_files.len} total.')
}

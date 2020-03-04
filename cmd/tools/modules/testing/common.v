module testing

import (
	os
	term
	benchmark
	filepath
	sync
	v.pref
)

pub struct TestSession {
pub mut:
	files         []string
	vexe          string
	vargs         string
	failed        bool
	benchmark     benchmark.Benchmark
	show_ok_tests bool
}

pub fn new_test_session(vargs string) TestSession {
	return TestSession{
		vexe: pref.vexe_path()
		vargs: vargs
		show_ok_tests: !vargs.contains('-silent')
	}
}

pub fn (ts mut TestSession) init() {
	ts.benchmark = benchmark.new_benchmark()
}

pub fn (ts mut TestSession) test() {
	ts.init()
	mut remaining_files := []string
	for dot_relative_file in ts.files {
		relative_file := dot_relative_file.replace('./', '')
		file := os.realpath(relative_file)
		$if windows {
			if file.contains('sqlite') || file.contains('httpbin') {
				continue
			}
		}
		$if !macos {
			if file.contains('customer') {
				continue
			}
		}
		$if msvc {
			if file.contains('asm') {
				continue
			}
		}
		$if tinyc {
			if file.contains('asm') {
				continue
			}
		}
		remaining_files << dot_relative_file
	}
	ts.files = remaining_files
	ts.benchmark.set_total_expected_steps(remaining_files.len)
	mut pool_of_test_runners := sync.new_pool_processor({
		callback: worker_trunner
	})
	pool_of_test_runners.set_shared_context(ts)
	$if msvc {
		// NB: MSVC can not be launched in parallel, without giving it
		// the option /FS because it uses a shared PDB file, which should
		// be locked, but that makes writing slower...
		// See: https://docs.microsoft.com/en-us/cpp/build/reference/fs-force-synchronous-pdb-writes?view=vs-2019
		// Instead, just run tests on 1 core for now.
		pool_of_test_runners.set_max_jobs(1)
	}
	pool_of_test_runners.work_on_pointers(remaining_files.pointers())
	ts.benchmark.stop()
	eprintln(term.h_divider('-'))
}

fn worker_trunner(p mut sync.PoolProcessor, idx int, thread_id int) voidptr {
	mut ts := &TestSession(p.get_shared_context())
	tmpd := os.tmpdir()
	show_stats := '-stats' in ts.vargs.split(' ')
	// tls_bench is used to format the step messages/timings
	mut tls_bench := &benchmark.Benchmark(p.get_thread_context(idx))
	if isnil(tls_bench) {
		tls_bench = benchmark.new_benchmark_pointer()
		tls_bench.set_total_expected_steps(ts.benchmark.nexpected_steps)
		p.set_thread_context(idx, tls_bench)
	}
	tls_bench.cstep = idx
	dot_relative_file := p.get_string_item(idx)
	relative_file := dot_relative_file.replace('./', '')
	file := os.realpath(relative_file)
	// Ensure that the generated binaries will be stored in the temporary folder.
	// Remove them after a test passes/fails.
	fname := filepath.filename(file)
	generated_binary_fname := if os.user_os() == 'windows' { fname.replace('.v', '.exe') } else { fname.replace('.v', '') }
	generated_binary_fpath := filepath.join(tmpd,generated_binary_fname)
	if os.exists(generated_binary_fpath) {
		os.rm(generated_binary_fpath)
	}
	mut cmd_options := [ts.vargs]
	if !ts.vargs.contains('fmt') {
		cmd_options << ' -o "$generated_binary_fpath"'
	}
	cmd := '"${ts.vexe}" ' + cmd_options.join(' ') + ' "${file}"'
	// eprintln('>>> v cmd: $cmd')
	ts.benchmark.step()
	tls_bench.step()
	if show_stats {
		eprintln(term.h_divider('-'))
		status := os.system(cmd)
		if status == 0 {
			ts.benchmark.ok()
			tls_bench.ok()
		}
		else {
			ts.failed = true
			ts.benchmark.fail()
			tls_bench.fail()
			return sync.no_result
		}
	}
	else {
		r := os.exec(cmd) or {
			ts.failed = true
			ts.benchmark.fail()
			tls_bench.fail()
			eprintln(tls_bench.step_message_fail(relative_file))
			return sync.no_result
		}
		if r.exit_code != 0 {
			ts.failed = true
			ts.benchmark.fail()
			tls_bench.fail()
			eprintln(tls_bench.step_message_fail('${relative_file}\n`$file`\n (\n$r.output\n)'))
		}
		else {
			ts.benchmark.ok()
			tls_bench.ok()
			if ts.show_ok_tests {
				eprintln(tls_bench.step_message_ok(relative_file))
			}
		}
	}
	if os.exists(generated_binary_fpath) {
		os.rm(generated_binary_fpath)
	}
	return sync.no_result
}

pub fn vlib_should_be_present(parent_dir string) {
	vlib_dir := filepath.join(parent_dir,'vlib')
	if !os.is_dir(vlib_dir) {
		eprintln('$vlib_dir is missing, it must be next to the V executable')
		exit(1)
	}
}

pub fn v_build_failing(zargs string, folder string) bool {
	main_label := 'Building $folder ...'
	finish_label := 'building $folder'
	vexe := pref.vexe_path()
	parent_dir := filepath.dir(vexe)
	vlib_should_be_present(parent_dir)
	vargs := zargs.replace(vexe, '')
	eheader(main_label)
	eprintln('v compiler args: "$vargs"')
	mut session := new_test_session(vargs)
	files := os.walk_ext(filepath.join(parent_dir,folder), '.v')
	mut mains := []string
	for f in files {
		if !f.contains('modules') && !f.contains('preludes') {
			$if windows {
				// skip pico example on windows
				if f.ends_with('examples\\pico\\pico.v') {
					continue
				}
			}
			mains << f
		}
	}
	session.files << mains
	session.test()
	eprintln(session.benchmark.total_message(finish_label))
	return session.failed
}

pub fn build_v_cmd_failed(cmd string) bool {
	res := os.exec(cmd) or {
		return true
	}
	if res.exit_code != 0 {
		eprintln('')
		eprintln(res.output)
		return true
	}
	return false
}

pub fn building_any_v_binaries_failed() bool {
	eheader('Building V binaries...')
	eprintln('VFLAGS is: "' + os.getenv('VFLAGS') + '"')
	vexe := pref.vexe_path()
	parent_dir := filepath.dir(vexe)
	testing.vlib_should_be_present(parent_dir)
	os.chdir(parent_dir)
	mut failed := false
	v_build_commands := ['$vexe -o v_g             -g  cmd/v',
	'$vexe -o v_prod_g  -prod -g  cmd/v',
	'$vexe -o v_cg            -cg cmd/v',
	'$vexe -o v_prod_cg -prod -cg cmd/v',
	'$vexe -o v_prod    -prod     cmd/v',
	]
	mut bmark := benchmark.new_benchmark()
	for cmd in v_build_commands {
		bmark.step()
		if build_v_cmd_failed(cmd) {
			bmark.fail()
			failed = true
			eprintln(bmark.step_message_fail('command: ${cmd} . See details above ^^^^^^^'))
			eprintln('')
			continue
		}
		bmark.ok()
		eprintln(bmark.step_message_ok('command: ${cmd}'))
	}
	bmark.stop()
	eprintln(term.h_divider('-'))
	eprintln(bmark.total_message('building v binaries'))
	return failed
}

pub fn eheader(msg string) {
	eprintln(term.header(msg, '-'))
}

pub fn header(msg string) {
	println(term.header(msg, '-'))
}

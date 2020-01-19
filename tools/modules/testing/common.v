module testing

import (
	os
	term
	benchmark
	filepath
	runtime
	time
)

pub struct TestSession {
pub mut:
	files     []string
	vexe      string
	vargs     string
	failed    bool
	current   int
	benchmark benchmark.ConcurrentBenchmark
}

pub fn new_test_session(vargs string) TestSession {
	return TestSession{
		vexe: vexe_path()
		vargs: vargs
	}
}

pub fn vexe_path() string {
	// NB: tools extracted from v require that the VEXE
	// environment variable contains the path to the v executable location.
	// They are usually launched by vlib/compiler/vtools.v,
	// launch_tool/1 , which provides it.
	return os.getenv('VEXE')
}

pub fn (ts mut TestSession) init() {
	ts.benchmark = benchmark.new_concurrent_benchmark()
}

pub fn (ts mut TestSession) test() {
	tmpd := os.tmpdir()
	ts.init()
	show_stats := '-stats' in ts.vargs.split(' ')
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
	ts.benchmark.set_total_expected_steps(remaining_files.len)
	ts.files = remaining_files
	for i in 1..runtime.nr_cpus() {
		go ts.build_instance(tmpd, show_stats)
	}
	ts.build_instance(tmpd, show_stats)
	for ts.benchmark.ntotal < remaining_files.len {
		time.sleep_ms(500)
	}
	ts.benchmark.stop()
	eprintln(term.h_divider())
}

fn (ts mut TestSession) build_instance(tmpd string, show_stats bool) {
	for ts.current < ts.files.len {
		//TODO Currently has race condition. Add lock when that's available
		self := ts.current
		ts.current++

		relative_file := ts.files[self].replace('./', '')
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
		mut step := ts.benchmark.step()
		if show_stats {
			eprintln('-------------------------------------------------')
			status := os.system(cmd)
			if status == 0 {
				step.ok()
			}
			else {
				step.fail()
				ts.failed = true
				continue
			}
		}
		else {
			r := os.exec(cmd) or {
				step.fail()
				ts.failed = true
				eprintln(step.step_message_fail(relative_file))
				continue
			}
			if r.exit_code != 0 {
				step.fail()
				ts.failed = true
				eprintln(step.step_message_fail('${relative_file}\n`$file`\n (\n$r.output\n)'))
			}
			else {
				step.ok()
				eprintln(step.step_message_ok(relative_file))
			}
		}
		if os.exists(generated_binary_fpath) {
			os.rm(generated_binary_fpath)
		}
	}
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
	vexe := vexe_path()
	parent_dir := filepath.dir(vexe)
	vlib_should_be_present(parent_dir)
	vargs := zargs.replace(vexe, '')
	eprintln(main_label)
	eprintln('   v compiler args: "$vargs"')
	mut session := new_test_session(vargs)
	files := os.walk_ext(filepath.join(parent_dir,folder), '.v')
	mains := files.filter(!it.contains('modules') && !it.contains('preludes'))
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
	eprintln('Building V binaries...')
	eprintln('VFLAGS is: "' + os.getenv('VFLAGS') + '"')
	vexe := testing.vexe_path()
	parent_dir := filepath.dir(vexe)
	testing.vlib_should_be_present(parent_dir)
	os.chdir(parent_dir)
	mut failed := false
	v_build_commands := ['$vexe -o v_g             -g  v.v',
	'$vexe -o v_prod_g  -prod -g  v.v',
	'$vexe -o v_cg            -cg v.v',
	'$vexe -o v_prod_cg -prod -cg v.v',
	'$vexe -o v_prod    -prod     v.v',
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
	eprintln(term.h_divider())
	eprintln(bmark.total_message('building v binaries'))
	return failed
}

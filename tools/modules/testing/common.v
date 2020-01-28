module testing

import (
	os
	term
	benchmark
	filepath
	runtime
	sync
)

pub struct TestSession {
pub mut:
	files     []string
	vexe      string
	vargs     string
	failed    bool
	benchmark benchmark.Benchmark
	
	ntask     int // writing to this should be locked by mu.
	ntask_mtx &sync.Mutex
	waitgroup &sync.WaitGroup
	show_ok_tests bool
}

pub fn new_test_session(vargs string) TestSession {
	return TestSession{
		vexe: vexe_path()
		vargs: vargs
		
		ntask: 0
		ntask_mtx: sync.new_mutex()
		waitgroup: sync.new_waitgroup()
		
		show_ok_tests: !vargs.contains('-silent')
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

	mut ncpus := runtime.nr_cpus()
	$if msvc {
		// NB: MSVC can not be launched in parallel, without giving it
		// the option /FS because it uses a shared PDB file, which should
		// be locked, but that makes writing slower...
		// See: https://docs.microsoft.com/en-us/cpp/build/reference/fs-force-synchronous-pdb-writes?view=vs-2019
		// Instead, just run tests on 1 core for now.
		ncpus = 1
	}	
	ts.waitgroup.add( ncpus )
	for i:=0; i < ncpus; i++ {
		go process_in_thread(ts)
	}
	ts.waitgroup.wait()
	ts.benchmark.stop()
	eprintln(term.h_divider('-'))
}


fn process_in_thread(ts mut TestSession){
	ts.process_files()
	ts.waitgroup.done()
}

fn (ts mut TestSession) process_files() {
	tmpd := os.tmpdir()
	show_stats := '-stats' in ts.vargs.split(' ')
	
	mut tls_bench := benchmark.new_benchmark() // tls_bench is used to format the step messages/timings
	tls_bench.set_total_expected_steps( ts.benchmark.nexpected_steps )
	for {

		ts.ntask_mtx.lock()
		ts.ntask++
		idx := ts.ntask-1
		ts.ntask_mtx.unlock()
		
		if idx >= ts.files.len { break }
		tls_bench.cstep = idx
		
		dot_relative_file := ts.files[ idx ]			
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
			eprintln('-------------------------------------------------')
			status := os.system(cmd)
			if status == 0 {
				ts.benchmark.ok()
				tls_bench.ok()
			}
			else {
				ts.failed = true
				ts.benchmark.fail()
				tls_bench.fail()
				continue
			}
		}
		else {
			r := os.exec(cmd) or {
				ts.failed = true
				ts.benchmark.fail()
				tls_bench.fail()
				eprintln(tls_bench.step_message_fail(relative_file))
				continue
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
	mut mains := files.filter(!it.contains('modules') && !it.contains('preludes'))
	$if windows {
		// skip pico example on windows
		// there was a bug using filter here
		mut mains_filtered := []string
		for file in mains {
			if !file.ends_with('examples\\pico\\pico.v') {
				mains_filtered << file
			}
		}
		mains = mains_filtered
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
	eprintln(term.h_divider('-'))
	eprintln(bmark.total_message('building v binaries'))
	return failed
}

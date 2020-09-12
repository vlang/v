module testing

import os
import time
import term
import benchmark
import sync
import v.pref
import v.util.vtest

pub struct TestMessageHandler {
mut:
	messages []string
pub mut:
	message_idx int
	mtx &sync.Mutex
}

pub struct TestSession {
pub mut:
	files         []string
	skip_files    []string
	vexe          string
	vroot         string
	vargs         string
	failed        bool
	benchmark     benchmark.Benchmark
	show_ok_tests bool
	message_handler &TestMessageHandler
}

pub fn (mut mh TestMessageHandler) append_message(msg string) {
	mh.mtx.m_lock()
	mh.messages << msg
	mh.mtx.unlock()
}

pub fn new_test_session(_vargs string) TestSession {
	mut skip_files := []string{}
	skip_files << '_non_existing_'
	$if solaris {
		skip_files << "examples/gg/gg2.v"
		skip_files << "examples/pico/pico.v"
		skip_files << "examples/sokol/fonts.v"
		skip_files << "examples/sokol/drawing.v"
	}
	vargs := _vargs.replace('-silent', '')
	vexe := pref.vexe_path()
	return TestSession{
		vexe: vexe
		vroot: os.dir(vexe)
		skip_files: skip_files
		vargs: vargs
		show_ok_tests: !_vargs.contains('-silent')
		message_handler: &TestMessageHandler(0)
	}
}

pub fn (mut ts TestSession) init() {
	ts.benchmark = benchmark.new_benchmark_no_cstep()
}

pub fn (mut ts TestSession) test() {
	// Ensure that .tmp.c files generated from compiling _test.v files,
	// are easy to delete at the end, *without* affecting the existing ones.
	now := time.sys_mono_now()
	new_vtmp_dir := os.join_path(os.temp_dir(), 'v', 'test_session_$now')
	os.mkdir_all(new_vtmp_dir)
	os.setenv('VTMP', new_vtmp_dir, true)
	//
	ts.init()
	mut remaining_files := []string{}
	for dot_relative_file in ts.files {
		relative_file := dot_relative_file.replace('./', '')
		file := os.real_path(relative_file)
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
	remaining_files = vtest.filter_vtest_only(remaining_files, fix_slashes: false)
	ts.files = remaining_files
	ts.benchmark.set_total_expected_steps(remaining_files.len)
	mut pool_of_test_runners := sync.new_pool_processor({
		callback: worker_trunner
	})
	// for handling messages across threads
	ts.message_handler = &TestMessageHandler{
		mtx: sync.new_mutex()
	}
	pool_of_test_runners.set_shared_context(ts)
	pool_of_test_runners.work_on_pointers(remaining_files.pointers())
	ts.benchmark.stop()
	eprintln(term.h_divider('-'))
	// cleanup generated .tmp.c files after successfull tests:
	if ts.benchmark.nfail == 0 {
		os.rmdir_all(new_vtmp_dir)
	}
}

pub fn (mut m TestMessageHandler) display_message() {
	m.mtx.m_lock()
	defer {
		m.messages.clear()
		m.mtx.unlock()
	}
	for msg in m.messages {
		m.message_idx++
		eprintln(msg.
			replace("TMP1", "${m.message_idx:1d}").
			replace("TMP2", "${m.message_idx:2d}").
			replace("TMP3", "${m.message_idx:3d}")
		)
	}
}

fn worker_trunner(mut p sync.PoolProcessor, idx int, thread_id int) voidptr {
	mut ts := &TestSession(p.get_shared_context())
	defer { ts.message_handler.display_message() }
	tmpd := os.temp_dir()
	show_stats := '-stats' in ts.vargs.split(' ')
	// tls_bench is used to format the step messages/timings
	mut tls_bench := &benchmark.Benchmark(p.get_thread_context(idx))
	if isnil(tls_bench) {
		tls_bench = benchmark.new_benchmark_pointer()
		tls_bench.set_total_expected_steps(ts.benchmark.nexpected_steps)
		p.set_thread_context(idx, tls_bench)
	}
	tls_bench.no_cstep = true
	dot_relative_file := p.get_string_item(idx)
	relative_file := dot_relative_file.replace(ts.vroot + os.path_separator, '').replace('./', '')
	file := os.real_path(relative_file)
	// Ensure that the generated binaries will be stored in the temporary folder.
	// Remove them after a test passes/fails.
	fname := os.file_name(file)
	generated_binary_fname := if os.user_os() == 'windows' { fname.replace('.v', '.exe') } else { fname.replace('.v', '') }
	generated_binary_fpath := os.join_path(tmpd, generated_binary_fname)
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
	if relative_file.replace('\\', '/') in ts.skip_files {
	   ts.benchmark.skip()
	   tls_bench.skip()
	   ts.message_handler.append_message(tls_bench.step_message_skip(relative_file))
	   return sync.no_result
	}
	if show_stats {
		ts.message_handler.append_message(term.h_divider('-'))
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
			ts.message_handler.append_message(tls_bench.step_message_fail(relative_file))
			return sync.no_result
		}
		if r.exit_code != 0 {
			ts.failed = true
			ts.benchmark.fail()
			tls_bench.fail()
			ts.message_handler.append_message(tls_bench.step_message_fail('${relative_file}\n$r.output\n'))
		}
		else {
			ts.benchmark.ok()
			tls_bench.ok()
			if ts.show_ok_tests {
				ts.message_handler.append_message(tls_bench.step_message_ok(relative_file))
			}
		}
	}
	if os.exists(generated_binary_fpath) {
		os.rm(generated_binary_fpath)
	}
	return sync.no_result
}

pub fn vlib_should_be_present(parent_dir string) {
	vlib_dir := os.join_path(parent_dir,'vlib')
	if !os.is_dir(vlib_dir) {
		eprintln('$vlib_dir is missing, it must be next to the V executable')
		exit(1)
	}
}

pub fn v_build_failing(zargs string, folder string) bool {
	return v_build_failing_skipped(zargs, folder, [])
}

pub fn v_build_failing_skipped(zargs string, folder string, oskipped []string) bool {
	main_label := 'Building $folder ...'
	finish_label := 'building $folder'
	vexe := pref.vexe_path()
	parent_dir := os.dir(vexe)
	vlib_should_be_present(parent_dir)
	vargs := zargs.replace(vexe, '')
	eheader(main_label)
	eprintln('v compiler args: "$vargs"')
	mut session := new_test_session(vargs)
	files := os.walk_ext(os.join_path(parent_dir, folder), '.v')
	mut mains := []string{}
	mut skipped := oskipped
	for f in files {
		if !f.contains('modules') && !f.contains('preludes') {
			//$if !linux {
				// run pg example only on linux
				if f.contains('/pg/') {
					continue
				}
			//}

			if f.contains('life_gg') || f.contains('/graph.v') || f.contains('rune.v') {
				continue

			}
			$if windows {
				// skip pico example on windows
				if f.ends_with('examples\\pico\\pico.v') {
					continue
				}
			}
			c := os.read_file(f) or { panic(err) }
			maxc := if c.len > 300 { 300 } else { c.len }
			start := c[0..maxc]
			if start.contains('module ') && !start.contains('module main') {
				skipped_f := f.replace(os.join_path(parent_dir,''), '')
				skipped << skipped_f
			}
			mains << f
		}
	}
	session.files << mains
	session.skip_files << skipped
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
	parent_dir := os.dir(vexe)
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

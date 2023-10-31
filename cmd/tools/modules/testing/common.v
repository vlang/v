module testing

import os
import os.cmdline
import time
import term
import benchmark
import sync
import sync.pool
import v.pref
import v.util.vtest
import runtime

pub const github_job = os.getenv('GITHUB_JOB')

pub const runner_os = os.getenv('RUNNER_OS') // GitHub runner OS

pub const show_start = os.getenv('VTEST_SHOW_START') == '1'

pub const hide_skips = os.getenv('VTEST_HIDE_SKIP') == '1'

pub const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'

pub const fail_fast = os.getenv('VTEST_FAIL_FAST') == '1'

pub const fail_flaky = os.getenv('VTEST_FAIL_FLAKY') == '1'

pub const test_only = os.getenv('VTEST_ONLY').split_any(',')

pub const test_only_fn = os.getenv('VTEST_ONLY_FN').split_any(',')

pub const is_node_present = os.execute('node --version').exit_code == 0

pub const all_processes = get_all_processes()

fn get_all_processes() []string {
	$if windows {
		// TODO
		return []
	} $else {
		return os.execute('ps ax').output.split_any('\r\n')
	}
}

pub struct TestSession {
pub mut:
	files         []string
	skip_files    []string
	vexe          string
	vroot         string
	vtmp_dir      string
	vargs         string
	fail_fast     bool
	benchmark     benchmark.Benchmark
	rm_binaries   bool = true
	silent_mode   bool
	show_stats    bool
	progress_mode bool
	root_relative bool // used by CI runs, so that the output is stable everywhere
	nmessages     chan LogMessage // many publishers, single consumer/printer
	nmessage_idx  int // currently printed message index
	failed_cmds   shared []string
	reporter      Reporter = Reporter(NormalReporter{})
	hash          string // used during testing in temporary directory and file names to prevent collisions when files and directories are created in a test file.
}

pub fn (mut ts TestSession) add_failed_cmd(cmd string) {
	lock ts.failed_cmds {
		ts.failed_cmds << cmd
	}
}

pub fn (mut ts TestSession) show_list_of_failed_tests() {
	rlock ts.failed_cmds {
		ts.reporter.list_of_failed_commands(ts.failed_cmds)
	}
}

struct MessageThreadContext {
mut:
	file    string
	flow_id string
}

fn (mut ts TestSession) append_message(kind MessageKind, msg string, mtc MessageThreadContext) {
	ts.nmessages <- LogMessage{
		file: mtc.file
		flow_id: mtc.flow_id
		message: msg
		kind: kind
		when: time.now()
	}
}

fn (mut ts TestSession) append_message_with_duration(kind MessageKind, msg string, d time.Duration, mtc MessageThreadContext) {
	ts.nmessages <- LogMessage{
		file: mtc.file
		flow_id: mtc.flow_id
		message: msg
		kind: kind
		when: time.now()
		took: d
	}
}

pub fn (mut ts TestSession) session_start(message string) {
	ts.reporter.session_start(message, mut ts)
}

pub fn (mut ts TestSession) session_stop(message string) {
	ts.reporter.session_stop(message, mut ts)
}

pub fn (mut ts TestSession) print_messages() {
	mut test_idx := 0
	mut print_msg_time := time.new_stopwatch()
	for {
		// get a message from the channel of messages to be printed:
		mut rmessage := <-ts.nmessages
		ts.nmessage_idx++

		// first sent *all events* to the output reporter, so it can then process them however it wants:
		ts.reporter.report(ts.nmessage_idx, rmessage)

		if rmessage.kind in [.cmd_begin, .cmd_end] {
			// The following events, are sent before the test framework has determined,
			// what the full completion status is. They can also be repeated multiple times,
			// for tests that are flaky and need repeating.
			continue
		}
		if rmessage.kind == .sentinel {
			// a sentinel for stopping the printing thread
			if !ts.silent_mode && ts.progress_mode {
				ts.reporter.report_stop()
			}
			return
		}
		if rmessage.kind != .info {
			// info events can also be repeated, and should be ignored when determining
			// the total order of the current test file, in the following replacements:
			test_idx++
		}
		msg := rmessage.message.replace_each([
			'TMP1',
			'${test_idx:1d}',
			'TMP2',
			'${test_idx:2d}',
			'TMP3',
			'${test_idx:3d}',
			'TMP4',
			'${test_idx:4d}',
		])
		is_ok := rmessage.kind == .ok
		//
		time_passed := print_msg_time.elapsed().seconds()
		if time_passed > 10 && ts.silent_mode && is_ok {
			// Even if OK tests are suppressed,
			// show *at least* 1 result every 10 seconds,
			// otherwise the CI can seem stuck ...
			ts.reporter.progress(ts.nmessage_idx, msg)
			print_msg_time.restart()
			continue
		}
		if ts.progress_mode {
			if is_ok && !ts.silent_mode {
				ts.reporter.update_last_line(ts.nmessage_idx, msg)
			} else {
				ts.reporter.update_last_line_and_move_to_next(ts.nmessage_idx, msg)
			}
			continue
		}
		if !ts.silent_mode || !is_ok {
			// normal expanded mode, or failures in -silent mode
			ts.reporter.message(ts.nmessage_idx, msg)
			continue
		}
	}
}

pub fn new_test_session(_vargs string, will_compile bool) TestSession {
	mut skip_files := []string{}
	if will_compile {
		// Skip the call_v_from_c files. They need special instructions for compilation.
		// Check the README.md for detailed information.
		skip_files << 'examples/call_v_from_c/v_test_print.v'
		skip_files << 'examples/call_v_from_c/v_test_math.v'
		// Skip the compilation of the coroutines example for now, since the Photon wrapper
		// is only available on macos for now, and it is not yet trivial enough to
		// build/install on the CI:
		skip_files << 'examples/coroutines/simple_coroutines.v'
		skip_files << 'examples/coroutines/coroutines_bench.v'
		$if msvc {
			skip_files << 'vlib/v/tests/const_comptime_eval_before_vinit_test.v' // _constructor used
			skip_files << 'vlib/v/tests/project_with_cpp_code/compiling_cpp_files_with_a_cplusplus_compiler_test.v'
		}
		$if solaris {
			skip_files << 'examples/gg/gg2.v'
			skip_files << 'examples/pico/pico.v'
			skip_files << 'examples/sokol/fonts.v'
			skip_files << 'examples/sokol/drawing.v'
		}
		$if macos {
			skip_files << 'examples/database/mysql.v'
			skip_files << 'examples/database/orm.v'
			skip_files << 'examples/database/psql/customer.v'
		}
		$if windows {
			skip_files << 'examples/database/mysql.v'
			skip_files << 'examples/database/orm.v'
			skip_files << 'examples/smtp/mail.v' // requires OpenSSL
			skip_files << 'examples/websocket/ping.v' // requires OpenSSL
			skip_files << 'examples/websocket/client-server/client.v' // requires OpenSSL
			skip_files << 'examples/websocket/client-server/server.v' // requires OpenSSL
			skip_files << 'vlib/v/tests/websocket_logger_interface_should_compile_test.v' // requires OpenSSL
			$if tinyc {
				skip_files << 'examples/database/orm.v' // try fix it
			}
		}
		$if windows {
			// TODO: remove when closures on windows are supported...
			skip_files << 'examples/pendulum-simulation/animation.v'
			skip_files << 'examples/pendulum-simulation/full.v'
			skip_files << 'examples/pendulum-simulation/parallel.v'
			skip_files << 'examples/pendulum-simulation/parallel_with_iw.v'
			skip_files << 'examples/pendulum-simulation/sequential.v'
			if testing.github_job == 'tcc' {
				// TODO: fix these by adding declarations for the missing functions in the prebuilt tcc
				skip_files << 'vlib/net/mbedtls/mbedtls_compiles_test.v'
				skip_files << 'vlib/net/ssl/ssl_compiles_test.v'
			}
		}
		if testing.runner_os != 'Linux' || testing.github_job != 'tcc' {
			skip_files << 'examples/c_interop_wkhtmltopdf.v' // needs installation of wkhtmltopdf from https://github.com/wkhtmltopdf/packaging/releases
			skip_files << 'examples/call_v_from_python/test.v' // the example only makes sense to be compiled, when python is installed
			skip_files << 'examples/call_v_from_ruby/test.v' // the example only makes sense to be compiled, when ruby is installed
			skip_files << 'vlib/vweb/vweb_app_test.v' // imports the `sqlite` module, which in turn includes sqlite3.h
		}
		$if !macos {
			skip_files << 'examples/macos_tray/tray.v'
		}
		if testing.github_job == 'ubuntu-docker-musl' {
			skip_files << 'vlib/net/openssl/openssl_compiles_test.v'
			skip_files << 'vlib/x/ttf/ttf_test.v'
		}
		if testing.github_job == 'tests-sanitize-memory-clang' {
			skip_files << 'vlib/net/openssl/openssl_compiles_test.v'
		}
		if testing.github_job != 'misc-tooling' {
			// These examples need .h files that are produced from the supplied .glsl files,
			// using by the shader compiler tools in https://github.com/floooh/sokol-tools-bin/archive/pre-feb2021-api-changes.tar.gz
			skip_files << 'examples/sokol/simple_shader_glsl/simple_shader.v'
			skip_files << 'examples/sokol/02_cubes_glsl/cube_glsl.v'
			skip_files << 'examples/sokol/03_march_tracing_glsl/rt_glsl.v'
			skip_files << 'examples/sokol/04_multi_shader_glsl/rt_glsl.v'
			skip_files << 'examples/sokol/05_instancing_glsl/rt_glsl.v'
			// Skip obj_viewer code in the CI
			skip_files << 'examples/sokol/06_obj_viewer/show_obj.v'
			// skip the audio examples too on most CI jobs
			skip_files << 'examples/sokol/sounds/melody.v'
			skip_files << 'examples/sokol/sounds/wav_player.v'
			skip_files << 'examples/sokol/sounds/simple_sin_tones.v'
		}
		// examples/wasm/mandelbrot/mandelbrot.v requires special compilation flags: `-b wasm -os browser`, skip it for now:
		skip_files << 'examples/wasm/mandelbrot/mandelbrot.v'

		// TODO: always build the wasm_builder in the future, not just when it was build manually before:
		wasm_builder_executable := $if !windows {
			'cmd/tools/builders/wasm_builder'
		} $else {
			'cmd/tools/builders/wasm_builder.exe'
		}
		if !os.exists(wasm_builder_executable) {
			skip_files << os.join_path('cmd/tools/builders/wasm_builder.v')
		}
	}
	vargs := _vargs.replace('-progress', '')
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	hash := '${sync.thread_id().hex()}_${time.sys_mono_now()}'
	new_vtmp_dir := setup_new_vtmp_folder(hash)
	if term.can_show_color_on_stderr() {
		os.setenv('VCOLORS', 'always', true)
	}
	mut ts := TestSession{
		vexe: vexe
		vroot: vroot
		skip_files: skip_files
		fail_fast: testing.fail_fast
		show_stats: '-stats' in vargs.split(' ')
		vargs: vargs
		vtmp_dir: new_vtmp_dir
		hash: hash
		silent_mode: _vargs.contains('-silent')
		progress_mode: _vargs.contains('-progress')
	}
	ts.handle_test_runner_option()
	return ts
}

fn (mut ts TestSession) handle_test_runner_option() {
	test_runner := cmdline.option(os.args, '-test-runner', 'normal')
	if test_runner !in pref.supported_test_runners {
		eprintln('v test: `-test-runner ${test_runner}` is not using one of the supported test runners: ${pref.supported_test_runners_list()}')
	}
	test_runner_implementation_file := os.join_path(ts.vroot, 'cmd/tools/modules/testing/output_${test_runner}.v')
	if !os.exists(test_runner_implementation_file) {
		eprintln('v test: using `-test-runner ${test_runner}` needs ${test_runner_implementation_file} to exist, and contain a valid testing.Reporter implementation for that runner. See `cmd/tools/modules/testing/output_dump.v` for an example.')
		exit(1)
	}
	match test_runner {
		'normal' {
			// default, nothing to do
		}
		'dump' {
			ts.reporter = DumpReporter{}
		}
		'teamcity' {
			ts.reporter = TeamcityReporter{}
		}
		else {
			dump('just set ts.reporter to an instance of your own struct here')
		}
	}
}

pub fn (mut ts TestSession) init() {
	ts.files.sort()
	ts.benchmark = benchmark.new_benchmark_no_cstep()
}

pub fn (mut ts TestSession) add(file string) {
	ts.files << file
}

pub fn (mut ts TestSession) test() {
	// Ensure that .tmp.c files generated from compiling _test.v files,
	// are easy to delete at the end, *without* affecting the existing ones.
	current_wd := os.getwd()
	if current_wd == os.wd_at_startup && current_wd == ts.vroot {
		ts.root_relative = true
	}
	//
	ts.init()
	mut remaining_files := []string{}
	for dot_relative_file in ts.files {
		file := os.real_path(dot_relative_file)
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
		remaining_files << dot_relative_file
	}
	remaining_files = vtest.filter_vtest_only(remaining_files, fix_slashes: false)
	ts.files = remaining_files
	ts.benchmark.set_total_expected_steps(remaining_files.len)
	mut njobs := runtime.nr_jobs()
	if remaining_files.len < njobs {
		njobs = remaining_files.len
	}
	ts.benchmark.njobs = njobs
	mut pool_of_test_runners := pool.new_pool_processor(callback: worker_trunner)
	// ensure that the nmessages queue/channel, has enough capacity for handling many messages across threads, without blocking
	ts.nmessages = chan LogMessage{cap: 10000}
	ts.nmessage_idx = 0
	printing_thread := spawn ts.print_messages()
	pool_of_test_runners.set_shared_context(ts)
	ts.reporter.worker_threads_start(remaining_files, mut ts)
	// all the testing happens here:
	pool_of_test_runners.work_on_pointers(unsafe { remaining_files.pointers() })
	//
	ts.benchmark.stop()
	ts.append_message(.sentinel, '', MessageThreadContext{ flow_id: '-1' }) // send the sentinel
	printing_thread.wait()
	ts.reporter.worker_threads_finish(mut ts)
	ts.reporter.divider()
	ts.show_list_of_failed_tests()
	// cleanup generated .tmp.c files after successful tests:
	if ts.benchmark.nfail == 0 {
		if ts.rm_binaries {
			os.rmdir_all(ts.vtmp_dir) or {}
		}
	}
	// remove empty session folders:
	if os.ls(ts.vtmp_dir) or { [] }.len == 0 {
		os.rmdir_all(ts.vtmp_dir) or {}
	}
}

fn worker_trunner(mut p pool.PoolProcessor, idx int, thread_id int) voidptr {
	mut ts := unsafe { &TestSession(p.get_shared_context()) }
	if ts.fail_fast {
		if ts.failed_cmds.len > 0 {
			return pool.no_result
		}
	}
	// tls_bench is used to format the step messages/timings
	mut tls_bench := unsafe { &benchmark.Benchmark(p.get_thread_context(idx)) }
	if isnil(tls_bench) {
		tls_bench = benchmark.new_benchmark_pointer()
		tls_bench.set_total_expected_steps(ts.benchmark.nexpected_steps)
		p.set_thread_context(idx, tls_bench)
	}
	tls_bench.no_cstep = true
	tls_bench.njobs = ts.benchmark.njobs
	mut relative_file := os.real_path(p.get_item[string](idx))
	mut cmd_options := [ts.vargs]
	mut run_js := false

	is_fmt := ts.vargs.contains('fmt')
	is_vet := ts.vargs.contains('vet')
	produces_file_output := !(is_fmt || is_vet)

	if relative_file.ends_with('js.v') {
		if produces_file_output {
			cmd_options << ' -b js'
			run_js = true
		}
	}

	if relative_file.contains('global') && !is_fmt {
		cmd_options << ' -enable-globals'
	}
	if ts.root_relative {
		relative_file = relative_file.replace(ts.vroot + os.path_separator, '')
	}
	file := os.real_path(relative_file)
	mtc := MessageThreadContext{
		file: file
		flow_id: thread_id.str()
	}
	normalised_relative_file := relative_file.replace('\\', '/')
	// Ensure that the generated binaries will be stored in the temporary folder.
	// Remove them after a test passes/fails.
	fname := os.file_name(file)
	generated_binary_fname := if os.user_os() == 'windows' && !run_js {
		'${fname.all_before_last('.v')}_${ts.hash}.exe'
	} else {
		'${fname.all_before_last('.v')}_${ts.hash}'
	}
	generated_binary_fpath := os.join_path_single(ts.vtmp_dir, generated_binary_fname)
	if produces_file_output {
		if ts.rm_binaries {
			os.rm(generated_binary_fpath) or {}
		}

		cmd_options << ' -o ${os.quoted_path(generated_binary_fpath)}'
	}
	cmd := '${os.quoted_path(ts.vexe)} ${cmd_options.join(' ')} ${os.quoted_path(file)}'
	ts.benchmark.step()
	tls_bench.step()
	if relative_file.replace('\\', '/') in ts.skip_files {
		ts.benchmark.skip()
		tls_bench.skip()
		if !testing.hide_skips {
			ts.append_message(.skip, tls_bench.step_message_skip(normalised_relative_file),
				mtc)
		}
		return pool.no_result
	}
	if ts.show_stats {
		ts.reporter.divider()

		ts.append_message(.cmd_begin, cmd, mtc)
		d_cmd := time.new_stopwatch()

		mut res := os.execute(cmd)
		if res.exit_code != 0 {
			eprintln(res.output)
		} else {
			println(res.output)
		}
		mut status := res.exit_code

		mut cmd_duration := d_cmd.elapsed()
		ts.append_message_with_duration(.cmd_end, '', cmd_duration, mtc)

		if status != 0 {
			details := get_test_details(file)
			os.setenv('VTEST_RETRY_MAX', '${details.retry}', true)
			for retry := 1; retry <= details.retry; retry++ {
				ts.append_message(.info, '  [stats]        retrying ${retry}/${details.retry} of ${relative_file} ; known flaky: ${details.flaky} ...',
					mtc)
				os.setenv('VTEST_RETRY', '${retry}', true)

				ts.append_message(.cmd_begin, cmd, mtc)
				d_cmd_2 := time.new_stopwatch()
				status = os.system(cmd)
				cmd_duration = d_cmd_2.elapsed()
				ts.append_message_with_duration(.cmd_end, '', cmd_duration, mtc)

				if status == 0 {
					unsafe {
						goto test_passed_system
					}
				}
				time.sleep(500 * time.millisecond)
			}
			if details.flaky && !testing.fail_flaky {
				ts.append_message(.info, '   *FAILURE* of the known flaky test file ${relative_file} is ignored, since VTEST_FAIL_FLAKY is 0 . Retry count: ${details.retry} .',
					mtc)
				unsafe {
					goto test_passed_system
				}
			}

			// most probably compiler error
			if res.output.contains(': error: ') {
				ts.append_message(.cannot_compile, 'Cannot compile file ${file}', mtc)
			}

			ts.benchmark.fail()
			tls_bench.fail()
			ts.add_failed_cmd(cmd)
			return pool.no_result
		} else {
			test_passed_system:
			ts.benchmark.ok()
			tls_bench.ok()
		}
	} else {
		if testing.show_start {
			ts.append_message(.info, '                 starting ${relative_file} ...',
				mtc)
		}
		ts.append_message(.cmd_begin, cmd, mtc)
		d_cmd := time.new_stopwatch()
		mut r := os.execute(cmd)
		mut cmd_duration := d_cmd.elapsed()
		ts.append_message_with_duration(.cmd_end, r.output, cmd_duration, mtc)

		if r.exit_code < 0 {
			ts.benchmark.fail()
			tls_bench.fail()
			ts.append_message_with_duration(.fail, tls_bench.step_message_fail(normalised_relative_file),
				cmd_duration, mtc)
			ts.add_failed_cmd(cmd)
			return pool.no_result
		}
		if r.exit_code != 0 {
			details := get_test_details(file)
			os.setenv('VTEST_RETRY_MAX', '${details.retry}', true)
			for retry := 1; retry <= details.retry; retry++ {
				ts.append_message(.info, '                 retrying ${retry}/${details.retry} of ${relative_file} ; known flaky: ${details.flaky} ...',
					mtc)
				os.setenv('VTEST_RETRY', '${retry}', true)

				ts.append_message(.cmd_begin, cmd, mtc)
				d_cmd_2 := time.new_stopwatch()
				r = os.execute(cmd)
				cmd_duration = d_cmd_2.elapsed()
				ts.append_message_with_duration(.cmd_end, r.output, cmd_duration, mtc)

				if r.exit_code == 0 {
					unsafe {
						goto test_passed_execute
					}
				}
			}
			if details.flaky && !testing.fail_flaky {
				ts.append_message(.info, '   *FAILURE* of the known flaky test file ${relative_file} is ignored, since VTEST_FAIL_FLAKY is 0 . Retry count: ${details.retry} .',
					mtc)
				unsafe {
					goto test_passed_execute
				}
			}
			ts.benchmark.fail()
			tls_bench.fail()
			ending_newline := if r.output.ends_with('\n') { '\n' } else { '' }
			ts.append_message_with_duration(.fail, tls_bench.step_message_fail('${normalised_relative_file}\n${r.output.trim_space()}${ending_newline}'),
				cmd_duration, mtc)
			ts.add_failed_cmd(cmd)
		} else {
			test_passed_execute:
			ts.benchmark.ok()
			tls_bench.ok()
			if !testing.hide_oks {
				ts.append_message_with_duration(.ok, tls_bench.step_message_ok(normalised_relative_file),
					cmd_duration, mtc)
			}
		}
	}
	if produces_file_output && ts.rm_binaries {
		os.rm(generated_binary_fpath) or {}
	}
	return pool.no_result
}

pub fn vlib_should_be_present(parent_dir string) {
	vlib_dir := os.join_path_single(parent_dir, 'vlib')
	if !os.is_dir(vlib_dir) {
		eprintln('${vlib_dir} is missing, it must be next to the V executable')
		exit(1)
	}
}

pub fn prepare_test_session(zargs string, folder string, oskipped []string, main_label string) TestSession {
	vexe := pref.vexe_path()
	parent_dir := os.dir(vexe)
	vlib_should_be_present(parent_dir)
	vargs := zargs.replace(vexe, '')
	eheader(main_label)
	if vargs.len > 0 {
		eprintln('v compiler args: "${vargs}"')
	}
	mut session := new_test_session(vargs, true)
	files := os.walk_ext(os.join_path_single(parent_dir, folder), '.v')
	mut mains := []string{}
	mut skipped := oskipped.clone()
	next_file: for f in files {
		fnormalised := f.replace('\\', '/')
		// Note: a `testdata` folder, is the preferred name of a folder, containing V code,
		// that you *do not want* the test framework to find incidentally for various reasons,
		// for example module import tests, or subtests, that are compiled/run by other parent tests
		// in specific configurations, etc.
		if fnormalised.contains('testdata/') || fnormalised.contains('modules/')
			|| fnormalised.contains('preludes/') {
			continue
		}
		$if windows {
			// skip process/command examples on windows
			if fnormalised.ends_with('examples/process/command.v') {
				continue
			}
		}
		c := os.read_file(f) or { panic(err) }
		maxc := if c.len > 500 { 500 } else { c.len }
		start := c[0..maxc]
		if start.contains('module ') && !start.contains('module main') {
			skipped_f := f.replace(os.join_path_single(parent_dir, ''), '')
			skipped << skipped_f
		}
		for skip_prefix in oskipped {
			skip_folder := skip_prefix + '/'
			if fnormalised.starts_with(skip_folder) {
				continue next_file
			}
		}
		mains << f
	}
	session.files << mains
	session.skip_files << skipped
	return session
}

pub type FnTestSetupCb = fn (mut session TestSession)

pub fn v_build_failing_skipped(zargs string, folder string, oskipped []string, cb FnTestSetupCb) bool {
	main_label := 'Building ${folder} ...'
	finish_label := 'building ${folder}'
	mut session := prepare_test_session(zargs, folder, oskipped, main_label)
	cb(mut session)
	session.test()
	eprintln(session.benchmark.total_message(finish_label))
	return session.failed_cmds.len > 0
}

pub fn build_v_cmd_failed(cmd string) bool {
	res := os.execute(cmd)
	if res.exit_code < 0 {
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
	vlib_should_be_present(parent_dir)
	os.chdir(parent_dir) or { panic(err) }
	mut failed := false
	v_build_commands := ['${vexe} -o v_g             -g  cmd/v',
		'${vexe} -o v_prod_g  -prod -g  cmd/v', '${vexe} -o v_cg            -cg cmd/v',
		'${vexe} -o v_prod_cg -prod -cg cmd/v', '${vexe} -o v_prod    -prod     cmd/v']
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
		if !testing.hide_oks {
			eprintln(bmark.step_message_ok('command: ${cmd}'))
		}
	}
	bmark.stop()
	eprintln(term.h_divider('-'))
	eprintln(bmark.total_message('building v binaries'))
	return failed
}

// setup_new_vtmp_folder creates a new nested folder inside VTMP, then resets VTMP to it,
// so that V programs/tests will write their temporary files to new location.
// The new nested folder, and its contents, will get removed after all tests/programs succeed.
pub fn setup_new_vtmp_folder(hash string) string {
	new_vtmp_dir := os.join_path(os.vtmp_dir(), 'tsession_${hash}')
	os.mkdir_all(new_vtmp_dir) or { panic(err) }
	os.setenv('VTMP', new_vtmp_dir, true)
	return new_vtmp_dir
}

pub struct TestDetails {
pub mut:
	retry int
	flaky bool // when flaky tests fail, the whole run is still considered successful, unless VTEST_FAIL_FLAKY is 1
}

pub fn get_test_details(file string) TestDetails {
	mut res := TestDetails{}
	lines := os.read_lines(file) or { [] }
	for line in lines {
		if line.starts_with('// vtest retry:') {
			res.retry = line.all_after(':').trim_space().int()
		}
		if line.starts_with('// vtest flaky:') {
			res.flaky = line.all_after(':').trim_space().bool()
		}
	}
	return res
}

pub fn find_started_process(pname string) !string {
	for line in testing.all_processes {
		if line.contains(pname) {
			return line
		}
	}
	return error('could not find process matching ${pname}')
}

pub fn eheader(msg string) {
	eprintln(term.header_left(msg, '-'))
}

pub fn header(msg string) {
	println(term.header_left(msg, '-'))
	flush_stdout()
}

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
import rand
import strings
import v.build_constraint

pub const max_header_len = get_max_header_len()

pub const host_os = pref.get_host_os()

pub const github_job = os.getenv('GITHUB_JOB')

pub const runner_os = os.getenv('RUNNER_OS') // GitHub runner OS

pub const show_start = os.getenv('VTEST_SHOW_START') == '1'

pub const hide_skips = os.getenv('VTEST_HIDE_SKIP') == '1'

pub const hide_oks = os.getenv('VTEST_HIDE_OK') == '1'

pub const fail_fast = os.getenv('VTEST_FAIL_FAST') == '1'

pub const fail_flaky = os.getenv('VTEST_FAIL_FLAKY') == '1'

pub const test_only = os.getenv('VTEST_ONLY').split_any(',')

pub const test_only_fn = os.getenv('VTEST_ONLY_FN').split_any(',')

// TODO: this !!!*reliably*!!! fails compilation of `v cmd/tools/vbuild-examples.v` with a cgen error, without `-no-parallel`:
// pub const fail_retry_delay_ms = os.getenv_opt('VTEST_FAIL_RETRY_DELAY_MS') or { '500' }.int() * time.millisecond
// Note, it works with `-no-parallel`, and it works when that whole expr is inside a function, like below:
pub const fail_retry_delay_ms = get_fail_retry_delay_ms()

pub const is_node_present = os.execute('node --version').exit_code == 0

pub const is_go_present = os.execute('go version').exit_code == 0

pub const is_ruby_present = os.execute('ruby --version').exit_code == 0
	&& os.execute('pkg-config ruby --libs').exit_code == 0

pub const is_python_present = os.execute('python --version').exit_code == 0
	&& os.execute('pkg-config python3 --libs').exit_code == 0

pub const is_sqlite3_present = os.execute('sqlite3 --version').exit_code == 0
	&& os.execute('pkg-config sqlite3 --libs').exit_code == 0

pub const all_processes = get_all_processes()

pub const header_bytes_to_search_for_module_main = 500

pub const separator = '-'.repeat(max_header_len) + '\n'

pub const max_compilation_retries = get_max_compilation_retries()

fn get_max_compilation_retries() int {
	return os.getenv_opt('VTEST_MAX_COMPILATION_RETRIES') or { '3' }.int()
}

fn get_fail_retry_delay_ms() time.Duration {
	return os.getenv_opt('VTEST_FAIL_RETRY_DELAY_MS') or { '500' }.int() * time.millisecond
}

fn get_all_processes() []string {
	$if windows {
		// TODO
		return []
	} $else {
		return os.execute('ps ax').output.split_any('\r\n')
	}
}

pub enum ActionMode {
	compile
	compile_and_run
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
	build_tools   bool // builds only executables in cmd/tools; used by `v build-tools'
	silent_mode   bool
	show_stats    bool
	show_asserts  bool
	progress_mode bool
	root_relative bool            // used by CI runs, so that the output is stable everywhere
	nmessages     chan LogMessage // many publishers, single consumer/printer
	nmessage_idx  int             // currently printed message index
	failed_cmds   shared []string
	reporter      Reporter = Reporter(NormalReporter{})
	hash          string // used as part of the name of the temporary directory created for tests, to ease cleanup

	exec_mode ActionMode = .compile // .compile_and_run only for `v test`

	build_environment build_constraint.Environment // see the documentation in v.build_constraint
	custom_defines    []string                     // for adding custom defines, known only to the individual runners
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
		file:    mtc.file
		flow_id: mtc.flow_id
		message: msg
		kind:    kind
		when:    time.now()
	}
}

fn (mut ts TestSession) append_message_with_duration(kind MessageKind, msg string, d time.Duration, mtc MessageThreadContext) {
	ts.nmessages <- LogMessage{
		file:    mtc.file
		flow_id: mtc.flow_id
		message: msg
		kind:    kind
		when:    time.now()
		took:    d
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

		if rmessage.kind in [.cmd_begin, .cmd_end, .compile_begin, .compile_end] {
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
		$if windows {
			skip_files << 'examples/vanilla_http_server' // requires epoll // TODO: find a way to support `// vtest build:` for project folders too...
		}
		if runner_os != 'Linux' || !github_job.starts_with('tcc-') {
			if !os.exists('/usr/local/include/wkhtmltox/pdf.h') {
				skip_files << 'examples/c_interop_wkhtmltopdf.v' // needs installation of wkhtmltopdf from https://github.com/wkhtmltopdf/packaging/releases
			}
		}
	}
	skip_files = skip_files.map(os.abs_path)
	vargs := _vargs.replace('-progress', '')
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	hash := '${sync.thread_id().hex()}_${rand.ulid()}'
	new_vtmp_dir := setup_new_vtmp_folder(hash)
	if term.can_show_color_on_stderr() {
		os.setenv('VCOLORS', 'always', true)
	}
	mut ts := TestSession{
		vexe:          vexe
		vroot:         vroot
		skip_files:    skip_files
		fail_fast:     fail_fast
		show_stats:    '-stats' in vargs.split(' ')
		show_asserts:  '-show-asserts' in vargs.split(' ')
		vargs:         vargs
		vtmp_dir:      new_vtmp_dir
		hash:          hash
		silent_mode:   _vargs.contains('-silent')
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

	ts.init()
	mut remaining_files := []string{}
	for dot_relative_file in ts.files {
		file := os.real_path(dot_relative_file)
		if ts.build_tools && dot_relative_file.ends_with('_test.v') {
			continue
		}
		// Skip OS-specific tests if we are not running that OS
		// Special case for android_outside_termux because of its
		// underscores
		if file.ends_with('_android_outside_termux_test.v') {
			if !host_os.is_target_of('android_outside_termux') {
				remaining_files << dot_relative_file
				ts.skip_files << file
				continue
			}
		}
		os_target := file.all_before_last('_test.v').all_after_last('_')
		if !host_os.is_target_of(os_target) {
			remaining_files << dot_relative_file
			ts.skip_files << file
			continue
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

	ts.setup_build_environment()

	// all the testing happens here:
	pool_of_test_runners.work_on_pointers(unsafe { remaining_files.pointers() })

	ts.benchmark.stop()
	ts.append_message(.sentinel, '', MessageThreadContext{ flow_id: '-1' }) // send the sentinel
	printing_thread.wait()
	ts.reporter.worker_threads_finish(mut ts)
	ts.reporter.divider()
	ts.show_list_of_failed_tests()

	// cleanup the session folder, if everything was ok:
	if ts.benchmark.nfail == 0 {
		if ts.rm_binaries {
			os.rmdir_all(ts.vtmp_dir) or {}
		}
	}
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
	abs_path := os.real_path(p.get_item[string](idx))
	mut relative_file := abs_path
	mut cmd_options := [ts.vargs]
	mut run_js := false

	is_fmt := ts.vargs.contains('fmt')
	is_vet := ts.vargs.contains('vet')
	produces_file_output := !(is_fmt || is_vet)

	if relative_file.ends_with('.js.v') {
		if produces_file_output {
			cmd_options << ' -b js'
			run_js = true
		}
	}

	if relative_file.ends_with('.c.v') {
		if produces_file_output {
			cmd_options << ' -b c'
			run_js = false
		}
	}

	if relative_file.contains('global') && !is_fmt {
		cmd_options << ' -enable-globals'
	}
	if ts.root_relative {
		relative_file = relative_file.replace_once(ts.vroot + os.path_separator, '')
	}
	file := os.real_path(relative_file)
	mtc := MessageThreadContext{
		file:    file
		flow_id: thread_id.str()
	}
	normalised_relative_file := relative_file.replace('\\', '/')

	// Ensure that the generated binaries will be stored in an *unique*, fresh, and per test folder,
	// inside the common session temporary folder, used for all the tests.
	// This is done to provide a clean working environment, for each test, that will not contain
	// files from other tests, and will make sure that tests with the same name, can be compiled
	// inside their own folders, without name conflicts (and without locking issues on windows,
	// where an executable is not writable, if it is running).
	// Note, that the common session temporary folder ts.vtmp_dir,
	// will be removed after all tests are done.
	test_id := '${idx}_${thread_id}'
	mut test_folder_path := os.join_path(ts.vtmp_dir, test_id)
	if ts.build_tools {
		// `v build-tools`, produce all executables in the same session folder, so that they can be copied later:
		test_folder_path = ts.vtmp_dir
	} else {
		os.mkdir_all(test_folder_path) or {}
	}
	fname := os.file_name(file)
	// There are test files ending with `_test.v`, `_test.c.v` and `_test.js.v`.
	mut fname_without_extension := fname.all_before_last('.v')
	if fname_without_extension.ends_with('.c') {
		fname_without_extension = fname_without_extension.all_before_last('.c')
	}
	generated_binary_fname := if os.user_os() == 'windows' && !run_js {
		fname_without_extension + '.exe'
	} else {
		fname_without_extension
	}
	mut details := get_test_details(file)
	if details.vflags != '' && !is_fmt {
		cmd_options << details.vflags
	}

	reproduce_options := cmd_options.clone()
	generated_binary_fpath := os.join_path_single(test_folder_path, generated_binary_fname)
	if produces_file_output {
		if ts.rm_binaries {
			os.rm(generated_binary_fpath) or {}
		}
		cmd_options << ' -o ${os.quoted_path(generated_binary_fpath)}'
	}
	defer {
		if produces_file_output && ts.rm_binaries {
			os.rmdir_all(test_folder_path) or {}
		}
	}

	mut skip_running := '-skip-running'
	if ts.show_stats {
		skip_running = ''
	}
	reproduce_cmd := '${os.quoted_path(ts.vexe)} ${reproduce_options.join(' ')} ${os.quoted_path(file)}'
	cmd := '${os.quoted_path(ts.vexe)} ${skip_running} ${cmd_options.join(' ')} ${os.quoted_path(file)}'
	run_cmd := if run_js {
		'node ${os.quoted_path(generated_binary_fpath)}'
	} else {
		os.quoted_path(generated_binary_fpath)
	}
	mut should_be_built := true
	if details.vbuild != '' {
		should_be_built = ts.build_environment.eval(details.vbuild) or {
			eprintln('${file}:${details.vbuild_line}:17: error during parsing the `// v test build` expression `${details.vbuild}`: ${err}')
			false
		}
		$if trace_should_be_built ? {
			eprintln('${file} has specific build constraint: `${details.vbuild}` => should_be_built: `${should_be_built}`')
			eprintln('>          env   facts: ${ts.build_environment.facts}')
			eprintln('>          env defines: ${ts.build_environment.defines}')
		}
	}

	ts.benchmark.step()
	tls_bench.step()
	if !ts.build_tools && (!should_be_built || abs_path in ts.skip_files) {
		ts.benchmark.skip()
		tls_bench.skip()
		if !hide_skips {
			ts.append_message(.skip, tls_bench.step_message_with_label_and_duration(benchmark.b_skip,
				normalised_relative_file, 0,
				preparation: 1 * time.microsecond
			), mtc)
		}
		return pool.no_result
	}
	mut compile_cmd_duration := time.Duration(0)
	mut cmd_duration := time.Duration(0)
	if ts.show_stats {
		ts.append_message(.cmd_begin, cmd, mtc)
		d_cmd := time.new_stopwatch()

		mut res := os.execute(cmd)
		if res.exit_code != 0 {
			eprintln(res.output)
		} else {
			println(res.output)
		}
		mut status := res.exit_code

		cmd_duration = d_cmd.elapsed()
		ts.append_message_with_duration(.cmd_end, '', cmd_duration, mtc)

		if status != 0 {
			os.setenv('VTEST_RETRY_MAX', '${details.retry}', true)
			for retry := 1; retry <= details.retry; retry++ {
				if !details.hide_retries {
					ts.append_message(.info, '  [stats]        retrying ${retry}/${details.retry} of ${relative_file} ; known flaky: ${details.flaky} ...',
						mtc)
				}
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
				time.sleep(fail_retry_delay_ms)
			}
			if details.flaky && !fail_flaky {
				ts.append_message(.info, '   *FAILURE* of the known flaky test file ${relative_file} is ignored, since VTEST_FAIL_FLAKY is 0 . Retry count: ${details.retry} .\ncmd: ${cmd}',
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
			ts.add_failed_cmd(reproduce_cmd)
			return pool.no_result
		}
	} else {
		if show_start {
			ts.append_message(.info, '                 starting ${relative_file} ...',
				mtc)
		}
		ts.append_message(.compile_begin, cmd, mtc)
		compile_d_cmd := time.new_stopwatch()
		mut compile_r := os.Result{}
		for cretry in 0 .. max_compilation_retries {
			compile_r = os.execute(cmd)
			compile_cmd_duration = compile_d_cmd.elapsed()
			// eprintln('>>>> cretry: $cretry | compile_r.exit_code: $compile_r.exit_code | compile_cmd_duration: ${compile_cmd_duration:8} | file: $normalised_relative_file')
			if compile_r.exit_code == 0 {
				break
			}
			random_sleep_ms(50, 100 * cretry)
		}
		ts.append_message_with_duration(.compile_end, compile_r.output, compile_cmd_duration,
			mtc)
		if compile_r.exit_code != 0 {
			ts.benchmark.fail()
			tls_bench.fail()
			ts.append_message_with_duration(.fail, tls_bench.step_message_with_label_and_duration(benchmark.b_fail,
				'${normalised_relative_file}\n>> compilation failed:\n${compile_r.output}',
				cmd_duration,
				preparation: compile_cmd_duration
			), cmd_duration, mtc)
			ts.add_failed_cmd(reproduce_cmd)
			return pool.no_result
		}
		tls_bench.step_restart()
		ts.benchmark.step_restart()
		if ts.exec_mode == .compile {
			unsafe {
				goto test_passed_execute
			}
		}
		//
		mut retry := 1
		ts.append_message(.cmd_begin, run_cmd, mtc)
		mut failure_output := strings.new_builder(1024)
		d_cmd := time.new_stopwatch()
		mut r := os.execute(run_cmd)
		cmd_duration = d_cmd.elapsed()
		ts.append_message_with_duration(.cmd_end, r.output, cmd_duration, mtc)
		if ts.show_asserts && r.exit_code == 0 {
			println(r.output.split_into_lines().filter(it.contains(' assert')).join('\n'))
		}
		if r.exit_code != 0 {
			mut trimmed_output := r.output.trim_space()
			if trimmed_output.len == 0 {
				// retry running at least 1 more time, to avoid CI false positives as much as possible
				details.retry++
			}
			if details.retry != 0 {
				failure_output.write_string(separator)
				failure_output.writeln(' retry: 0 ; max_retry: ${details.retry} ; r.exit_code: ${r.exit_code} ; trimmed_output.len: ${trimmed_output.len}')
			}
			failure_output.writeln(trimmed_output)
			os.setenv('VTEST_RETRY_MAX', '${details.retry}', true)
			for retry = 1; retry <= details.retry; retry++ {
				if !details.hide_retries {
					ts.append_message(.info, '                 retrying ${retry}/${details.retry} of ${relative_file} ; known flaky: ${details.flaky} ...',
						mtc)
				}
				os.setenv('VTEST_RETRY', '${retry}', true)

				ts.append_message(.cmd_begin, run_cmd, mtc)
				d_cmd_2 := time.new_stopwatch()
				r = os.execute(run_cmd)
				cmd_duration = d_cmd_2.elapsed()
				ts.append_message_with_duration(.cmd_end, r.output, cmd_duration, mtc)

				if r.exit_code == 0 {
					unsafe {
						goto test_passed_execute
					}
				}
				trimmed_output = r.output.trim_space()
				failure_output.write_string(separator)
				failure_output.writeln(' retry: ${retry} ; max_retry: ${details.retry} ; r.exit_code: ${r.exit_code} ; trimmed_output.len: ${trimmed_output.len}')
				failure_output.writeln(trimmed_output)
				time.sleep(fail_retry_delay_ms)
			}
			full_failure_output := failure_output.str().trim_space()
			if details.flaky && !fail_flaky {
				ts.append_message(.info, '>>> flaky failures so far:', mtc)
				for line in full_failure_output.split_into_lines() {
					ts.append_message(.info, '>>>>>> ${line}', mtc)
				}
				ts.append_message(.info, '   *FAILURE* of the known flaky test file ${relative_file} is ignored, since VTEST_FAIL_FLAKY is 0 . Retry count: ${details.retry} .\n    comp_cmd: ${cmd}\n     run_cmd: ${run_cmd}',
					mtc)
				unsafe {
					goto test_passed_execute
				}
			}
			ts.benchmark.fail()
			tls_bench.fail()
			cmd_duration = d_cmd.elapsed() - (fail_retry_delay_ms * details.retry)
			ts.append_message_with_duration(.fail, tls_bench.step_message_with_label_and_duration(benchmark.b_fail,
				'${normalised_relative_file}\n${full_failure_output}', cmd_duration,
				preparation: compile_cmd_duration
			), cmd_duration, mtc)
			ts.add_failed_cmd(reproduce_cmd)
			return pool.no_result
		}
	}
	test_passed_system:
	test_passed_execute:
	ts.benchmark.ok()
	tls_bench.ok()
	if !hide_oks {
		ts.append_message_with_duration(.ok, tls_bench.step_message_with_label_and_duration(benchmark.b_ok,
			normalised_relative_file, cmd_duration,
			preparation: compile_cmd_duration
		), cmd_duration, mtc)
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
	nparent_dir := parent_dir.replace('\\', '/')
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
			// skip process/command examples on windows. TODO: remove the need for this, fix os.Command
			if fnormalised.ends_with('examples/process/command.v') {
				skipped << fnormalised.replace(nparent_dir + '/', '')
				continue
			}
		}
		c := os.read_file(fnormalised) or { panic(err) }
		start := c#[0..header_bytes_to_search_for_module_main]
		if start.contains('module ') {
			modname := start.all_after('module ').all_before('\n')
			if modname !in ['main', 'no_main'] {
				skipped << fnormalised.replace(nparent_dir + '/', '')
				continue next_file
			}
		}
		for skip_prefix in oskipped {
			skip_folder := skip_prefix + '/'
			if fnormalised.starts_with(skip_folder) {
				continue next_file
			}
		}
		mains << fnormalised
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
		if !hide_oks {
			eprintln(bmark.step_message_ok('command: ${cmd}'))
		}
	}
	bmark.stop()
	h_divider()
	eprintln(bmark.total_message('building v binaries'))
	return failed
}

pub fn h_divider() {
	eprintln(term.h_divider('-')#[..max_header_len])
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
	//
	hide_retries bool   // when true, all retry tries are silent; used by `vlib/v/tests/retry_test.v`
	vbuild       string // could be `!(windows && tinyc)`
	vbuild_line  int    // for more precise error reporting, if the `vbuild` expression is incorrect
	vflags       string // custom compilation flags for the test (enables for example: `// vtest vflags: -w`, for tests that have known warnings, but should still pass with -W)
}

pub fn get_test_details(file string) TestDetails {
	mut res := TestDetails{}
	if !os.is_file(file) {
		return res
	}
	lines := os.read_lines(file) or { [] }
	for idx, line in lines {
		if line.starts_with('// vtest retry:') {
			res.retry = line.all_after(':').trim_space().int()
		}
		if line.starts_with('// vtest flaky:') {
			res.flaky = line.all_after(':').trim_space().bool()
		}
		if line.starts_with('// vtest build:') {
			res.vbuild = line.all_after(':').trim_space()
			res.vbuild_line = idx + 1
		}
		if line.starts_with('// vtest vflags:') {
			res.vflags = line.all_after(':').trim_space()
		}
		if line.starts_with('// vtest hide_retries') {
			res.hide_retries = true
		}
	}
	return res
}

pub fn find_started_process(pname string) !string {
	for line in all_processes {
		if line.contains(pname) {
			return line
		}
	}
	return error('could not find process matching ${pname}')
}

fn limited_header(msg string) string {
	return term.header_left(msg, '-')#[..max_header_len]
}

pub fn eheader(msg string) {
	eprintln(limited_header(msg))
}

pub fn header(msg string) {
	println(limited_header(msg))
	flush_stdout()
}

fn random_sleep_ms(min_ms int, random_add_ms int) {
	time.sleep((50 + rand.intn(50) or { 0 }) * time.millisecond)
}

fn get_max_header_len() int {
	maximum := 140
	cols, _ := term.get_terminal_size()
	if cols > maximum {
		return maximum
	}
	return cols
}

fn check_openssl_present() bool {
	$if openbsd {
		return os.execute('eopenssl34 --version').exit_code == 0
			&& os.execute('pkg-config eopenssl34 --libs').exit_code == 0
	} $else {
		return os.execute('openssl --version').exit_code == 0
			&& os.execute('pkg-config openssl --libs').exit_code == 0
	}
}

pub const is_openssl_present = check_openssl_present()

// is_started_mysqld is true, when the test runner determines that there is a running mysql server
pub const is_started_mysqld = find_started_process('mysqld') or { '' }

// is_started_postgres is true, when the test runner determines that there is a running postgres server
pub const is_started_postgres = find_started_process('postgres') or { '' }

pub fn (mut ts TestSession) setup_build_environment() {
	facts, mut defines := pref.get_build_facts_and_defines()
	// add the runtime information, that the test runner has already determined by checking once:
	if is_started_mysqld != '' {
		defines << 'started_mysqld'
	}
	if is_started_postgres != '' {
		defines << 'started_postgres'
	}
	if is_node_present {
		defines << 'present_node'
	}
	if is_python_present {
		defines << 'present_python'
	}
	if is_ruby_present {
		defines << 'present_ruby'
	}
	if is_go_present {
		defines << 'present_go'
	}
	if is_sqlite3_present {
		defines << 'present_sqlite3'
	}
	if is_openssl_present {
		defines << 'present_openssl'
	}

	// detect the linux distribution as well when possible:
	if os.is_file('/etc/os-release') {
		mut distro_kind := ''
		if lines := os.read_lines('/etc/os-release') {
			for line in lines {
				if line.starts_with('ID=') {
					distro_kind = line.all_after('ID=')
					break
				}
			}
		}
		if distro_kind != '' {
			defines << 'os_id_${distro_kind}' // os_id_alpine, os_id_freebsd, os_id_ubuntu, os_id_debian etc
		}
	}

	defines << ts.custom_defines
	$if trace_vbuild ? {
		eprintln('>>> testing.get_build_environment facts: ${facts}')
		eprintln('>>> testing.get_build_environment defines: ${defines}')
	}
	ts.build_environment = build_constraint.new_environment(facts, defines)
}

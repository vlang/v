import os
import term
import v.util.diff
import v.util.vtest
import time
import runtime
import benchmark

const skip_files = [
	'non_existing.vv', // minimize commit diff churn, do not remove
]

const skip_on_cstrict = [
	'vlib/v/checker/tests/missing_c_lib_header_1.vv',
	'vlib/v/checker/tests/missing_c_lib_header_with_explanation_2.vv',
]

const skip_on_ubuntu_musl = [
	'vlib/v/checker/tests/vweb_tmpl_used_var.vv',
	'vlib/v/checker/tests/vweb_routing_checks.vv',
	'vlib/v/checker/tests/orm_op_with_option_and_none.vv',
	'vlib/v/tests/skip_unused/gg_code.vv',
]

const skip_on_ci_musl = [
	'vlib/v/tests/skip_unused/gg_code.vv',
]

const vexe = os.getenv('VEXE')

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const show_cmd = os.getenv('VTEST_SHOW_CMD') != ''

// This is needed, because some of the .vv files are tests, and we do need stable
// output from them, that can be compared against their .out files:
const turn_on_normal_test_runner = os.setenv('VTEST_RUNNER', 'normal', true)

const should_autofix = os.getenv('VAUTOFIX') != ''

const github_job = os.getenv('GITHUB_JOB')

const v_ci_ubuntu_musl = os.getenv('V_CI_UBUNTU_MUSL').len > 0

const v_ci_musl = os.getenv('V_CI_MUSL').len > 0

const v_ci_cstrict = os.getenv('V_CI_CSTRICT').len > 0

struct TaskDescription {
	vexe             string
	evars            string
	dir              string
	voptions         string
	result_extension string
	path             string
mut:
	is_error          bool
	is_skipped        bool
	is_module         bool
	expected          string
	expected_out_path string
	found___          string
	took              time.Duration
	cli_cmd           string
	ntries            int
	max_ntries        int = 1
}

struct Tasks {
	vexe          string
	parallel_jobs int // 0 is using VJOBS, anything else is an override
	label         string
mut:
	show_cmd bool
	all      []TaskDescription
}

fn test_all() {
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	checker_dir := 'vlib/v/checker/tests'
	checker_with_check_option_dir := 'vlib/v/checker/tests/with_check_option'
	parser_dir := 'vlib/v/parser/tests'
	scanner_dir := 'vlib/v/scanner/tests'
	module_dir := '${checker_dir}/modules'
	global_dir := '${checker_dir}/globals'
	global_run_dir := '${checker_dir}/globals_run'
	run_dir := '${checker_dir}/run'
	skip_unused_dir := 'vlib/v/tests/skip_unused'

	checker_tests := get_tests_in_dir(checker_dir, false).filter(!it.contains('with_check_option'))
	parser_tests := get_tests_in_dir(parser_dir, false)
	scanner_tests := get_tests_in_dir(scanner_dir, false)
	global_tests := get_tests_in_dir(global_dir, false)
	global_run_tests := get_tests_in_dir(global_run_dir, false)
	module_tests := get_tests_in_dir(module_dir, true)
	run_tests := get_tests_in_dir(run_dir, false)
	skip_unused_dir_tests := get_tests_in_dir(skip_unused_dir, false)
	checker_with_check_option_tests := get_tests_in_dir(checker_with_check_option_dir,
		false)
	mut tasks := Tasks{
		vexe:  vexe
		label: 'all tests'
	}
	tasks.add('', parser_dir, '', '.out', parser_tests, false)
	tasks.add('', checker_dir, '', '.out', checker_tests, false)
	tasks.add('', scanner_dir, '', '.out', scanner_tests, false)
	tasks.add('', checker_dir, '-enable-globals run', '.run.out', ['globals_error.vv'],
		false)
	tasks.add('', global_run_dir, '-enable-globals run', '.run.out', global_run_tests,
		false)
	tasks.add('', global_dir, '-enable-globals', '.out', global_tests, false)
	tasks.add('', module_dir, '-prod run', '.out', module_tests, true)
	tasks.add('', run_dir, 'run', '.run.out', run_tests, false)
	tasks.add('', checker_with_check_option_dir, '-check', '.out', checker_with_check_option_tests,
		false)
	tasks.run()

	if os.user_os() == 'linux' {
		mut skip_unused_tasks := Tasks{
			vexe:          vexe
			parallel_jobs: 1
			label:         '-skip-unused tests'
		}
		skip_unused_tasks.add('', skip_unused_dir, 'run', '.run.out', skip_unused_dir_tests,
			false)
		skip_unused_tasks.add('', skip_unused_dir, '-d no_backtrace -skip-unused run',
			'.skip_unused.run.out', skip_unused_dir_tests, false)
		skip_unused_tasks.run()
	}

	if github_job == 'ubuntu-tcc' {
		// This is done with tcc only, because the error output is compiler specific.
		// Note: the tasks should be run serially, since they depend on
		// setting and using environment variables.
		mut cte_tasks := Tasks{
			vexe:          vexe
			parallel_jobs: 1
			label:         'comptime env tests'
		}
		cte_dir := '${checker_dir}/comptime_env'
		files := get_tests_in_dir(cte_dir, false)
		cte_tasks.add('', cte_dir, '-no-retry-compilation run', '.run.out', files, false)
		cte_tasks.add_evars('VAR=/usr/include', '', cte_dir, '-no-retry-compilation run',
			'.var.run.out', ['using_comptime_env.vv'], false)
		cte_tasks.add_evars('VAR=/opt/invalid/path', '', cte_dir, '-no-retry-compilation run',
			'.var_invalid.run.out', ['using_comptime_env.vv'], false)
		cte_tasks.run()
	}
	mut ct_tasks := Tasks{
		vexe:          vexe
		parallel_jobs: 1
		label:         'comptime define tests'
	}
	ct_tasks.add_checked_run('-d mysymbol run', '.mysymbol.run.out', [
		'custom_comptime_define_error.vv',
	])
	ct_tasks.add_checked_run('-d mydebug run', '.mydebug.run.out', [
		'custom_comptime_define_if_flag.vv',
	])
	ct_tasks.add_checked_run('-d nodebug run', '.nodebug.run.out', [
		'custom_comptime_define_if_flag.vv',
	])
	ct_tasks.add_checked_run('run', '.run.out', ['custom_comptime_define_if_debug.vv'])
	ct_tasks.add_checked_run('-g run', '.g.run.out', [
		'custom_comptime_define_if_debug.vv',
	])
	ct_tasks.add_checked_run('-cg run', '.cg.run.out', [
		'custom_comptime_define_if_debug.vv',
	])
	ct_tasks.add_checked_run('-d debug run', '.debug.run.out', [
		'custom_comptime_define_if_debug.vv',
	])
	ct_tasks.add_checked_run('-d debug -d bar run', '.debug.bar.run.out', [
		'custom_comptime_define_if_debug.vv',
	])
	ct_tasks.run()
}

fn (mut tasks Tasks) add_checked_run(voptions string, result_extension string, tests []string) {
	checker_dir := 'vlib/v/checker/tests'
	tasks.add('', checker_dir, voptions, result_extension, tests, false)
}

fn (mut tasks Tasks) add(custom_vexe string, dir string, voptions string, result_extension string, tests []string,
	is_module bool) {
	tasks.add_evars('', custom_vexe, dir, voptions, result_extension, tests, is_module)
}

fn (mut tasks Tasks) add_evars(evars string, custom_vexe string, dir string, voptions string, result_extension string,
	tests []string, is_module bool) {
	max_ntries := get_max_ntries()
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	for path in paths {
		tasks.all << TaskDescription{
			evars:            evars
			vexe:             if custom_vexe != '' { custom_vexe } else { tasks.vexe }
			dir:              dir
			voptions:         voptions
			result_extension: result_extension
			path:             path
			is_module:        is_module
			max_ntries:       max_ntries
		}
	}
}

fn bstep_message(mut bench benchmark.Benchmark, label string, msg string, sduration time.Duration) string {
	return bench.step_message_with_label_and_duration(label, msg, sduration)
}

// process an array of tasks in parallel, using no more than vjobs worker threads
fn (mut tasks Tasks) run() {
	if tasks.all.len == 0 {
		return
	}
	tasks.show_cmd = show_cmd
	vjobs := if tasks.parallel_jobs > 0 { tasks.parallel_jobs } else { runtime.nr_jobs() }
	mut bench := benchmark.new_benchmark()
	bench.set_total_expected_steps(tasks.all.len)
	mut work := chan TaskDescription{cap: tasks.all.len}
	mut results := chan TaskDescription{cap: tasks.all.len}
	mut m_skip_files := skip_files.clone()
	if v_ci_ubuntu_musl {
		m_skip_files << skip_on_ubuntu_musl
	}
	if v_ci_musl {
		m_skip_files << skip_on_ci_musl
	}
	if v_ci_cstrict {
		m_skip_files << skip_on_cstrict
	}
	$if noskip ? {
		m_skip_files = []
	}
	$if tinyc {
		// Note: tcc does not support __has_include, so the detection mechanism
		// used for the other compilers does not work. It still provides a
		// cleaner error message, than a generic C error, but without the explanation.
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_1.vv'
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_with_explanation_2.vv'
		m_skip_files << 'vlib/v/checker/tests/comptime_value_d_in_include_errors.vv'
	}
	$if msvc {
		m_skip_files << 'vlib/v/checker/tests/asm_alias_does_not_exist.vv'
		m_skip_files << 'vlib/v/checker/tests/asm_immutable_err.vv'
		// TODO: investigate why MSVC regressed
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_1.vv'
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_with_explanation_2.vv'
		m_skip_files << 'vlib/v/checker/tests/comptime_value_d_in_include_errors.vv'
	}
	$if windows {
		m_skip_files << 'vlib/v/checker/tests/modules/deprecated_module'
	}
	for i in 0 .. tasks.all.len {
		if tasks.all[i].path in m_skip_files {
			tasks.all[i].is_skipped = true
		}
		work <- tasks.all[i]
	}
	work.close()
	for _ in 0 .. vjobs {
		spawn work_processor(work, results)
	}
	if github_job == '' {
		println('')
	}
	mut line_can_be_erased := true
	mut total_errors := 0
	for _ in 0 .. tasks.all.len {
		mut task := TaskDescription{}
		task = <-results
		bench.step()
		if task.is_skipped {
			bench.skip()
			eprintln(bstep_message(mut bench, benchmark.b_skip, task.path, task.took))
			line_can_be_erased = false
			continue
		}
		if task.is_error {
			total_errors++
			bench.fail()
			eprintln(bstep_message(mut bench, benchmark.b_fail, task.path, task.took))
			println('============')
			println('failed cmd: ${task.cli_cmd}')
			println('expected_out_path: ${task.expected_out_path}')
			println('============')
			println('expected (len: ${task.expected.len:5}, hash: ${task.expected.hash()}):')
			println(task.expected)
			println('============')
			println('found    (len: ${task.found___.len:5}, hash: ${task.found___.hash()}):')
			println(task.found___)
			println('============\n')
			diff_content(task.expected, task.found___)
			line_can_be_erased = false
		} else {
			bench.ok()
			assert true
			if tasks.show_cmd {
				eprintln(bstep_message(mut bench, benchmark.b_ok, '${task.cli_cmd}', task.took))
			} else {
				if github_job == '' {
					// local mode:
					if line_can_be_erased {
						term.clear_previous_line()
					}
					println(bstep_message(mut bench, benchmark.b_ok, task.path, task.took))
				}
			}
			line_can_be_erased = true
		}
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message(tasks.label))
	if total_errors != 0 {
		exit(1)
	}
}

// a single worker thread spends its time getting work from the `work` channel,
// processing the task, and then putting the task in the `results` channel
fn work_processor(work chan TaskDescription, results chan TaskDescription) {
	for {
		mut task := <-work or { break }
		mut i := 0
		for i = 1; i <= task.max_ntries; i++ {
			// reset the .is_error flag, from the potential previous retries, otherwise it can
			// be set on the first retry, all the next retries can succeed, and the task will
			// be still considered failed, with a very puzzling non difference reported.
			task.is_error = false
			sw := time.new_stopwatch()
			task.execute()
			task.took = sw.elapsed()
			cli_cmd := task.get_cli_cmd()
			if !task.is_error {
				if i > 1 {
					eprintln('>    succeeded after ${i:3}/${task.max_ntries} retries, doing `${cli_cmd}`')
				}
				break
			}
			eprintln('>    failed ${i:3}/${task.max_ntries} times, doing `${cli_cmd}`')
			if i <= task.max_ntries {
				time.sleep(100 * time.millisecond)
			}
		}
		task.ntries = i
		results <- task
	}
}

fn (mut task TaskDescription) get_cli_cmd() string {
	program := task.path
	cmd_prefix := if task.evars.len > 0 { '${task.evars} ' } else { '' }
	cli_cmd := '${cmd_prefix}${os.quoted_path(task.vexe)} ${task.voptions} ${os.quoted_path(program)}'
	return cli_cmd
}

// actual processing; Note: no output is done here at all
fn (mut task TaskDescription) execute() {
	if task.is_skipped {
		return
	}
	cli_cmd := task.get_cli_cmd()
	res := os.execute(cli_cmd)
	expected_out_path := task.path.replace('.vv', '') + task.result_extension
	task.expected_out_path = expected_out_path
	task.cli_cmd = cli_cmd
	if should_autofix && !os.exists(expected_out_path) {
		os.create(expected_out_path) or { panic(err) }
	}
	mut expected := os.read_file(expected_out_path) or { panic(err) }
	task.expected = clean_line_endings(expected)
	task.found___ = clean_line_endings(res.output)
	$if windows {
		if task.is_module {
			task.found___ = task.found___.replace_once('\\', '/')
		}
	}
	if task.expected != task.found___ {
		task.is_error = true
		if should_autofix {
			os.write_file(expected_out_path, res.output) or { panic(err) }
		}
	}
}

fn clean_line_endings(s string) string {
	mut res := s.trim_space()
	res = res.replace(' \n', '\n')
	res = res.replace(' \r\n', '\n')
	res = res.replace('\r\n', '\n')
	res = res.trim('\n')
	return res
}

fn chunks(s string, chunk_size int) string {
	mut res := []string{}
	for i := 0; i < s.len; i += chunk_size {
		res << s#[i..i + chunk_size]
	}
	return res.join('\n')
}

fn chunka(s []u8, chunk_size int) string {
	mut res := []string{}
	for i := 0; i < s.len; i += chunk_size {
		res << s#[i..i + chunk_size].str()
	}
	return res.join('\n')
}

fn diff_content(expected string, found string) {
	println(term.bold(term.yellow('diff: ')))
	if diff_ := diff.compare_text(expected, found) {
		println(diff_)
	} else {
		println('>>>> `${err}`; dumping bytes instead...')
		println('expected bytes:\n${chunka(expected.bytes(), 25)}')
		println('   found bytes:\n${chunka(found.bytes(), 25)}')
		println('============')
		println('  expected hex:\n${chunks(expected.bytes().hex(), 80)}')
		println('     found hex:\n${chunks(found.bytes().hex(), 80)}')
	}
	println('============\n')
}

fn get_tests_in_dir(dir string, is_module bool) []string {
	files := os.ls(dir) or { panic(err) }
	mut tests := files.clone()
	if !is_module {
		tests = files.filter(it.ends_with('.vv'))
	} else {
		tests = files.filter(!it.ends_with('.out'))
	}
	tests.sort()
	return tests
}

fn get_max_ntries() int {
	return if v_ci_musl { 3 } else { 1 }
}

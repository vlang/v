import os
import term
import v.util
import v.util.vtest
import time
import sync
import runtime
import benchmark

const (
	skip_files = [
		'vlib/v/checker/tests/custom_comptime_define_if_flag.vv',
	]
)

struct TaskDescription {
	vexe             string
	dir              string
	voptions         string
	result_extension string
	path             string
mut:
	is_error         bool
	is_skipped       bool
	is_module        bool
	expected         string
	found___         string
	took             time.Duration
}

fn test_all() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	checker_dir := 'vlib/v/checker/tests'
	parser_dir := 'vlib/v/parser/tests'
	module_dir := '$checker_dir/modules'
	global_dir := '$checker_dir/globals'
	run_dir := '$checker_dir/run'
	//
	checker_tests := get_tests_in_dir(checker_dir, false)
	parser_tests := get_tests_in_dir(parser_dir, false)
	global_tests := get_tests_in_dir(global_dir, false)
	module_tests := get_tests_in_dir(module_dir, true)
	run_tests := get_tests_in_dir(run_dir, false)
	// -prod is used for the parser and checker tests, so that warns are errors
	mut tasks := []TaskDescription{}
	tasks.add(vexe, parser_dir, '-prod', '.out', parser_tests, false)
	tasks.add(vexe, checker_dir, '-prod', '.out', checker_tests, false)
	tasks.add(vexe, checker_dir, '-d mysymbol run', '.mysymbol.run.out', ['custom_comptime_define_error.vv'],
		false)
	tasks.add(vexe, checker_dir, '-d mydebug run', '.mydebug.run.out', ['custom_comptime_define_if_flag.vv'],
		false)
	tasks.add(vexe, checker_dir, '-d nodebug run', '.nodebug.run.out', ['custom_comptime_define_if_flag.vv'],
		false)
	tasks.add(vexe, checker_dir, '--enable-globals run', '.run.out', ['globals_error.vv'],
		false)
	tasks.add(vexe, global_dir, '--enable-globals', '.out', global_tests, false)
	tasks.add(vexe, module_dir, '-prod run', '.out', module_tests, true)
	tasks.add(vexe, run_dir, 'run', '.run.out', run_tests, false)
	tasks.run()
}

fn (mut tasks []TaskDescription) add(vexe string, dir string, voptions string, result_extension string, tests []string, is_module bool) {
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	for path in paths {
		tasks << TaskDescription{
			vexe: vexe
			dir: dir
			voptions: voptions
			result_extension: result_extension
			path: path
			is_module: is_module
		}
	}
}

fn bstep_message(mut bench benchmark.Benchmark, label string, msg string, sduration time.Duration) string {
	return bench.step_message_with_label_and_duration(label, msg, sduration)
}

// process an array of tasks in parallel, using no more than vjobs worker threads
fn (mut tasks []TaskDescription) run() {
	vjobs := runtime.nr_jobs()
	mut bench := benchmark.new_benchmark()
	bench.set_total_expected_steps(tasks.len)
	mut work := sync.new_channel<TaskDescription>(tasks.len)
	mut results := sync.new_channel<TaskDescription>(tasks.len)
	mut m_skip_files := skip_files.clone()
	$if noskip ? {
		m_skip_files = []
	}
	$if tinyc {
		// NB: tcc does not support __has_include, so the detection mechanism
		// used for the other compilers does not work. It still provides a
		// cleaner error message, than a generic C error, but without the explanation.
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_1.vv'
		m_skip_files << 'vlib/v/checker/tests/missing_c_lib_header_with_explanation_2.vv'
	}
	for i in 0 .. tasks.len {
		if tasks[i].path in m_skip_files {
			tasks[i].is_skipped = true
		}
		unsafe {work.push(&tasks[i])}
	}
	work.close()
	for _ in 0 .. vjobs {
		go work_processor(mut work, mut results)
	}
	mut total_errors := 0
	for _ in 0 .. tasks.len {
		mut task := TaskDescription{}
		results.pop(&task)
		bench.step()
		if task.is_skipped {
			bench.skip()
			eprintln(bstep_message(mut bench, benchmark.b_skip, task.path, task.took))
			continue
		}
		if task.is_error {
			total_errors++
			bench.fail()
			eprintln(bstep_message(mut bench, benchmark.b_fail, task.path, task.took))
			println('============')
			println('expected:')
			println(task.expected)
			println('============')
			println('found:')
			println(task.found___)
			println('============\n')
			diff_content(task.expected, task.found___)
		} else {
			bench.ok()
			eprintln(bstep_message(mut bench, benchmark.b_ok, task.path, task.took))
		}
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('all tests'))
	assert total_errors == 0
}

// a single worker thread spends its time getting work from the `work` channel,
// processing the task, and then putting the task in the `results` channel
fn work_processor(mut work sync.Channel, mut results sync.Channel) {
	for {
		mut task := TaskDescription{}
		if !work.pop(&task) {
			break
		}
		sw := time.new_stopwatch({})
		task.execute()
		task.took = sw.elapsed()
		results.push(&task)
	}
}

// actual processing; NB: no output is done here at all
fn (mut task TaskDescription) execute() {
	if task.is_skipped {
		return
	}
	program := task.path
	cli_cmd := '$task.vexe $task.voptions $program'
	res := os.exec(cli_cmd) or { panic(err) }
	mut expected := os.read_file(program.replace('.vv', '') + task.result_extension) or {
		panic(err)
	}
	task.expected = clean_line_endings(expected)
	task.found___ = clean_line_endings(res.output)
	$if windows {
		if task.is_module {
			task.found___ = task.found___.replace_once('\\', '/')
		}
	}
	if task.expected != task.found___ {
		task.is_error = true
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

fn diff_content(s1 string, s2 string) {
	diff_cmd := util.find_working_diff_command() or { return }
	println('diff: ')
	println(util.color_compare_strings(diff_cmd, s1, s2))
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

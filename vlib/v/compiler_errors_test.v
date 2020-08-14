import os
import term
import v.util
import v.util.vtest
import time
import sync
import runtime
import benchmark

struct TaskDescription {
	vexe             string
	dir              string
	voptions         string
	result_extension string
	path             string
mut:
	is_error         bool
	expected         string
	found___         string
	took             time.Duration
}

fn test_all() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	classic_dir := 'vlib/v/checker/tests'
	classic_tests := get_tests_in_dir(classic_dir)
	global_dir := '$classic_dir/globals'
	global_tests := get_tests_in_dir(global_dir)
	run_dir := '$classic_dir/run'
	run_tests := get_tests_in_dir(run_dir)
	parser_dir := 'vlib/v/parser/tests'
	parser_tests := get_tests_in_dir(parser_dir)
	// -prod so that warns are errors
	mut tasks := []TaskDescription{}
	tasks << new_tasks(vexe, classic_dir, '-prod', '.out', classic_tests)
	tasks << new_tasks(vexe, global_dir, '--enable-globals', '.out', global_tests)
	tasks <<
		new_tasks(vexe, classic_dir, '--enable-globals run', '.run.out', ['globals_error.vv'])
	tasks << new_tasks(vexe, run_dir, 'run', '.run.out', run_tests)
	tasks << new_tasks(vexe, parser_dir, '-prod', '.out', parser_tests)
	tasks.run()
}

fn new_tasks(vexe, dir, voptions, result_extension string, tests []string) []TaskDescription {
	paths := vtest.filter_vtest_only(tests, {
		basepath: dir
	})
	mut res := []TaskDescription{}
	for path in paths {
		res << TaskDescription{
			vexe: vexe
			dir: dir
			voptions: voptions
			result_extension: result_extension
			path: path
		}
	}
	return res
}

// process an array of tasks in parallel, using no more than vjobs worker threads
fn (mut tasks []TaskDescription) run() {
	vjobs := runtime.nr_jobs()
	mut bench := benchmark.new_benchmark()
	bench.set_total_expected_steps(tasks.len)
	mut work := sync.new_channel<TaskDescription>(tasks.len)
	mut results := sync.new_channel<TaskDescription>(tasks.len)
	for i in 0 .. tasks.len {
		work.push(&tasks[i])
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
		if task.is_error {
			total_errors++
			bench.fail()
			eprintln(bench.step_message_with_label_and_duration(benchmark.b_fail, task.path,
				task.took))
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
			eprintln(bench.step_message_with_label_and_duration(benchmark.b_ok, task.path,
				task.took))
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
	program := task.path.replace('.vv', '.v')
	os.cp(task.path, program) or {
		panic(err)
	}
	res := os.exec('$task.vexe $task.voptions $program') or {
		panic(err)
	}
	mut expected := os.read_file(program.replace('.v', '') + task.result_extension) or {
		panic(err)
	}
	task.expected = clean_line_endings(expected)
	task.found___ = clean_line_endings(res.output)
	if task.expected != task.found___ {
		task.is_error = true
	} else {
		os.rm(program)
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

fn diff_content(s1, s2 string) {
	diff_cmd := util.find_working_diff_command() or {
		return
	}
	println('diff: ')
	println(util.color_compare_strings(diff_cmd, s1, s2))
	println('============\n')
}

fn get_tests_in_dir(dir string) []string {
	files := os.ls(dir) or {
		panic(err)
	}
	mut tests := files.filter(it.ends_with('.vv'))
	tests.sort()
	return tests
}

// vtest build: !windows
// go_race_test.v runs the V translation of Go's race detector test suite
// (src/runtime/race/testdata) the way Go's src/runtime/race/race_test.go does: every
// program in testdata/ is built with `-race`, its tests run one after another, and a test
// passes when the race detector reported a data race exactly for the tests whose names
// start with `test_race_`, and for none of the `test_no_race_` ones. See README.md.
//
// Go's harness runs the tests with GOMAXPROCS=1, because some of them only race in the
// execution order that gives (a goroutine starts when the test blocks), and because
// ThreadSanitizer can miss a race whose two accesses happen at the very same time. V threads
// run in parallel, so when a test missed its race, the program runs again, up to
// `max_runs` times: a `test_race_` test passes when a race was reported in any run, a
// `test_no_race_` test fails when a race was reported in any run.
//
// `VRACE_GO_TEST_ONLY=chan,mop` runs only the programs whose names contain one of the
// comma separated parts, `VRACE_GO_TEST_VERBOSE=1` also prints the race reports of the
// tests that failed.
import os

const vexe = os.quoted_path(@VEXE)
const testdata_dir = os.join_path(os.dir(@FILE), 'testdata')
const tdir = os.join_path(os.vtmp_dir(), 'go_race_test_${os.getpid()}')
const run_marker = '=== RUN   '
const done_marker = '=== DONE'
const skip_marker = '--- SKIP: '
const max_runs = 3
// gcc's ThreadSanitizer instrumentation does not see the reads and writes of whole struct
// values in call arguments and results (a struct passed by value, a struct result stored
// through the return slot), which V uses for strings, arrays and maps. `v -race` prefers
// clang for that reason; these tests only pass with it.
const gcc_known_misses = ['issues.test_race_issue12664', 'map.test_race_map_variable',
	'map.test_race_map_variable2', 'map.test_race_map_variable3', 'mop2.test_race_complex128_ww',
	'mop3.test_race_as_func1', 'mop3.test_race_panic_arg', 'mop3.test_race_slice_slice',
	'mop3.test_race_slice_string', 'mop3.test_race_method_thunk2', 'mop3.test_race_method_thunk4',
	'slice.test_race_slice_write_slice', 'slice.test_race_slice_var_write',
	'slice.test_race_slice_var_read', 'slice.test_race_slice_var_range',
	'slice.test_race_slice_var_append', 'slice.test_race_slice_var_copy',
	'slice.test_race_slice_var_copy2', 'slice.test_race_concat_string', 'slice.test_race_compare_string']
// The tests contain many races on the same addresses and code (memory is constantly
// reused), so turn off the heuristics that suppress seemingly identical reports, as Go's
// harness does. The exit status of a program that reported races does not matter here.
const vrace_options = 'suppress_equal_stacks=0 suppress_equal_addresses=0 atexit_sleep_ms=0 exitcode=0'

struct TestResult {
	name     string
	expected bool // a race is expected
	got      bool // a race was reported
	skipped  bool
	log      []string
	got_run  int // the run in which the race was first reported
}

fn testsuite_begin() {
	os.mkdir_all(tdir) or {}
}

fn testsuite_end() {
	os.rmdir_all(tdir) or {}
}

// race_c_compiler returns the C compiler that `v -race` uses here: the one that VFLAGS
// selects with `-cc`, or else clang when it is installed, or else the platform default.
fn race_c_compiler() string {
	flags := os.getenv('VFLAGS').fields()
	i := flags.index('-cc')
	if i >= 0 && i + 1 < flags.len {
		return flags[i + 1]
	}
	$if !macos {
		if _ := os.find_abs_path_of_executable('clang') {
			return 'clang'
		}
	}
	return 'cc'
}

// thread_sanitizer_runs reports whether the C compiler that `-race` uses can build and run
// a ThreadSanitizer program here.
fn thread_sanitizer_runs() bool {
	probe_c := os.join_path(tdir, 'tsan_probe.c')
	probe_exe := os.join_path(tdir, 'tsan_probe')
	os.write_file(probe_c, 'int main(void) { return 0; }\n') or { return false }
	cc := race_c_compiler()
	compiled := os.exec([cc, '-fsanitize=thread', '${probe_c}', '-o', probe_exe])
	if compiled.exit_code != 0 {
		eprintln('skipping: `${cc} -fsanitize=thread` does not work here:\n${compiled.output}')
		return false
	}
	ran := os.exec([probe_exe])
	if ran.exit_code != 0 {
		eprintln('skipping: ThreadSanitizer programs do not run here:\n${ran.output}')
		return false
	}
	return true
}

fn selected_programs() []string {
	mut files := os.ls(testdata_dir) or { panic(err) }
	files = files.filter(it.ends_with('.v')).map(os.join_path(testdata_dir, it))
	files.sort()
	only := os.getenv('VRACE_GO_TEST_ONLY')
	if only == '' {
		return files
	}
	parts := only.split(',').map(it.trim_space()).filter(it != '')
	return files.filter(fn [parts] (file string) bool {
		name := os.file_name(file)
		return parts.any(name.contains(it))
	})
}

// TestFn is a test function of a translated program, and the lines that it spans.
struct TestFn {
	name  string
	first int
	last  int
}

// program_test_fns returns the test functions of a translated program.
fn program_test_fns(source string) []TestFn {
	lines := source.split_into_lines()
	mut fns := []TestFn{}
	mut name := ''
	mut first := 0
	for i, line in lines {
		if !line.starts_with('fn ') {
			continue
		}
		if name != '' {
			fns << TestFn{name, first, i}
		}
		name = if line.starts_with('fn test_') { line.all_after('fn ').all_before('(') } else { '' }
		first = i + 1
	}
	if name != '' {
		fns << TestFn{name, first, lines.len}
	}
	return fns
}

// frame_test returns the test whose code a stack frame line of a race report is in, like
// `    #0 __anon_fn_3 /path/chan.v:121:8 (chan+0x1234)`, if it is in the program itself.
fn frame_test(frame string, program_name string, fns []TestFn) ?string {
	for word in frame.fields() {
		parts := word.split(':')
		if parts.len < 2 || os.file_name(parts[0]) != program_name {
			continue
		}
		line := parts[1].int()
		for f in fns {
			if line >= f.first && line <= f.last {
				return f.name
			}
		}
	}
	return none
}

// report_test returns the test in whose code the race of a report happened: the first
// frame of the report's stacks (the second access first) that is in the program itself. A
// race is reported by the thread that makes the second access, which can print it after
// its test returned, while the next tests run.
fn report_test(report []string, program_name string, fns []TestFn) ?string {
	for line in report {
		if line.trim_space().starts_with('#') {
			if test := frame_test(line, program_name, fns) {
				return test
			}
		}
	}
	return none
}

// parse_output splits the output of a program into the logs of its tests, like Go's
// harness does with the output of `go test -v`, and attributes each race report to the
// test in whose code it happened, or else to the test that was running when it appeared.
fn parse_output(output string, program_name string, fns []TestFn, attempt int) ([]TestResult, bool) {
	mut names := []string{}
	mut logs := map[string][]string{}
	mut races := map[string]bool{}
	mut name := ''
	mut done := false
	mut report := []string{}
	mut report_start_test := ''
	mut in_report := false
	for raw_line in output.split_into_lines() {
		mut line := raw_line
		// The markers of the main thread can end up in the middle of a race report that
		// another thread prints.
		if line.contains(run_marker) {
			name = line.all_after(run_marker)
			names << name
			logs[name] = []string{}
			line = line.all_before(run_marker)
		} else if line.contains(done_marker) {
			done = true
			line = line.all_before(done_marker)
		}
		if name != '' && line != '' {
			logs[name] << line
		}
		if line.contains('WARNING: ThreadSanitizer: data race') {
			in_report = true
			report = []string{}
			report_start_test = name
			continue
		}
		if !in_report {
			continue
		}
		report << line
		if line.contains('SUMMARY: ThreadSanitizer: data race') {
			in_report = false
			races[report_test(report, program_name, fns) or { report_start_test }] = true
		}
	}
	mut results := []TestResult{cap: names.len}
	for test in names {
		log := logs[test]
		results << TestResult{
			name:     test
			expected: test.starts_with('test_race_')
			got:      races[test]
			skipped:  log.any(it.starts_with(skip_marker))
			log:      log
			got_run:  if races[test] { attempt } else { 0 }
		}
	}
	return results, done
}

// merge_runs combines the results of one more run of a program with those of the previous
// runs: a race counts when it was reported in any of them.
fn merge_runs(previous []TestResult, current []TestResult) []TestResult {
	if previous.len == 0 {
		return current
	}
	mut merged := []TestResult{cap: previous.len}
	for i, r in previous {
		c := current[i] or { r }
		merged << TestResult{
			...r
			got:     r.got || c.got
			log:     if r.got || !c.got { r.log } else { c.log }
			got_run: if r.got { r.got_run } else { c.got_run }
		}
	}
	return merged
}

// c_compiler_is_gcc reports whether a C compiler command is gcc (`cc` often is).
fn c_compiler_is_gcc(cc string) bool {
	version := os.exec([cc, '--version']).output
	return version.contains('Free Software Foundation') && !version.contains('clang')
}

fn test_go_race_suite() {
	if !thread_sanitizer_runs() {
		return
	}
	verbose := os.getenv('VRACE_GO_TEST_VERBOSE') != ''
	known_misses := if c_compiler_is_gcc(race_c_compiler()) { gcc_known_misses } else { [] }
	mut known_failures := 0
	mut total := 0
	mut passed := 0
	mut skipped := 0
	mut false_pos := 0
	mut false_neg := 0
	mut problems := []string{}
	for program in selected_programs() {
		name := os.file_name(program).all_before_last('.v')
		exe := os.join_path(tdir, name)
		build := os.exec([@VEXE, '-race', '-o', exe, '${program}'])
		if build.exit_code != 0 {
			problems << '${name}.v does not compile with -race:\n${build.output}'
			continue
		}
		source := os.read_file(program) or { panic(err) }
		fns := program_test_fns(source)
		mut results := []TestResult{}
		for attempt in 1 .. max_runs + 1 {
			run := os.exec(['env', 'VRACE=${vrace_options}', exe])
			run_results, done := parse_output(run.output, os.file_name(program), fns, attempt)
			if !done {
				problems << '${name}.v did not run to completion (exit code ${run.exit_code}):\n${run.output#[-3000..]}'
				results = run_results.clone()
				break
			}
			results = merge_runs(results, run_results)
			if attempt == 1 {
				ran := run_results.map(it.name)
				for test in fns.map(it.name) {
					if test !in ran {
						problems << '${name}.v: ${test} is defined, but was not run'
					}
				}
			}
			if !results.any(it.expected && !it.got && !it.skipped) {
				break
			}
		}
		for r in results {
			if !r.name.starts_with('test_race_') && !r.name.starts_with('test_no_race_') {
				continue
			}
			if r.skipped {
				skipped++
				println('${name + '.' + r.name:-72} SKIPPED')
				continue
			}
			total++
			if r.expected == r.got {
				passed++
				rerun_note := if r.got_run > 1 { ' (in run ${r.got_run})' } else { '' }
				println('${name + '.' + r.name:-72} .${rerun_note}')
				continue
			}
			if r.expected && '${name}.${r.name}' in known_misses {
				known_failures++
				println('${name + '.' + r.name:-72} FAILED (known gcc limitation)')
				continue
			}
			if r.expected {
				false_neg++
			} else {
				false_pos++
			}
			println('${name + '.' + r.name:-72} FAILED${if r.expected { '' } else { '+' }}')
			if verbose {
				println(r.log.join('\n'))
			}
		}
	}
	println('\nPassed ${passed} of ${total} tests (${false_pos}+, ${false_neg}-), ${skipped} skipped, ${known_failures} known gcc limitations')
	for problem in problems {
		eprintln(problem)
	}
	assert problems.len == 0
	assert total > 0
	assert passed + known_failures == total
}

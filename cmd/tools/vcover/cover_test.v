// vtest retry: 2
import os

const vexe = @VEXE
const vroot = os.dir(vexe)
const tfolder = os.join_path(os.vtmp_dir(), 'cover_test')

const t1 = np(os.join_path(tfolder, 't1'))
const t2 = np(os.join_path(tfolder, 't2'))
const t3 = np(os.join_path(tfolder, 't3'))
const gen_folder = np(os.join_path(tfolder, 'gen'))

// fresh_testdata is the example1 fixture copied to a writable directory, so
// `--write-tests` can be tested without touching the fixture itself.
const fresh_testdata = 'vcover/testdata/../fresh/example1/'

fn np(path string) string {
	return path.replace('\\', '/')
}

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	os.rmdir_all(tfolder) or {}
	os.mkdir(tfolder) or {}
	// The fixture copy is made once per test process, so a retry of this file
	// starts from exactly the state the first attempt started from.
	copy_example1()
}

fn testsuite_end() {
	os.rmdir_all(np(os.join_path(vroot, 'cmd/tools/vcover/testdata/generated'))) or {}
	os.rmdir_all(tfolder) or {}
}

fn copy_example1() {
	// The copy lives inside the tree, because `--write-tests` writes a test file
	// beside its source and refuses to reach outside the working directory.
	target := np(os.join_path(vroot, 'cmd/tools/vcover/testdata/generated', 'example1'))
	os.rmdir_all(target) or {}
	os.mkdir_all(target) or { panic(err) }
	os.cp('cmd/tools/vcover/testdata/example1/abc.v', np(os.join_path(target, 'abc.v'))) or {
		panic(err)
	}
	for test in [
		'internal_abc01_test.v',
		'internal_abc10_abc30_test.v',
		'internal_abc20_abc25_test.v',
	] {
		os.cp('cmd/tools/vcover/testdata/example1/${test}', np(os.join_path(target, test))) or {
			panic(err)
		}
	}
	// The compiler needs a v.mod in the directory to resolve the tests.
	os.write_file(np(os.join_path(target, 'v.mod')), '') or { panic(err) }
}

// filter_fresh is the --filter value that selects only the copied fixture. Paths
// are normalized to `/` before being matched, so the filter uses that too.
fn filter_fresh() string {
	return 'testdata/generated/example1'
}

fn test_help() {
	res := execute('${os.quoted_path(vexe)} cover -h')
	assert res.exit_code == 0
	assert res.output.contains('Usage: v cover')
	assert res.output.contains('Description: Analyze & make reports')
	assert res.output.contains('Options:')
	assert res.output.contains('-h, --help                Show this help text.')
	assert res.output.contains('-v, --verbose             Be more verbose while processing the coverages.')
	assert res.output.contains('-H, --hotspots            Show most frequently executed covered lines.')
	assert res.output.contains('-P, --percentages         Show coverage percentage per file.')
	assert res.output.contains('--lcov <string>           Write an LCOV line coverage report')
	assert res.output.contains('-S, --show_test_files     Show `_test.v` files as well (normally filtered).')
	assert res.output.contains('-A, --absolute            Use absolute paths for all files')
}

fn test_simple() {
	assert !os.exists(t1), t1
	assert !os.exists(t2), t2
	assert !os.exists(t3), t3

	r1 :=
		execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t1)} cmd/tools/vcover/testdata/simple/t1_test.v')
	assert r1.exit_code == 0, r1.str()
	assert r1.output.trim_space() == '10', r1.str()
	assert os.exists(t1), t1
	cmd := '${os.quoted_path(vexe)} cover ${os.quoted_path(t1)} --filter vcover/testdata/simple/'
	filter1 := execute(cmd)
	assert filter1.exit_code == 0, filter1.output
	assert filter1.output.contains('cmd/tools/vcover/testdata/simple/simple.v'), filter1.output
	assert filter1.output.trim_space().ends_with('|      4 |      9 |  44.44%'), filter1.output
	lcov_file := np(os.join_path(tfolder, 'coverage', 'simple.lcov'))
	os.rm(lcov_file) or {}
	lcov :=
		execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t1)} --filter vcover/testdata/simple/ --lcov ${os.quoted_path(lcov_file)} -P false')
	assert lcov.exit_code == 0, lcov.str()
	assert os.exists(lcov_file), lcov_file
	lcov_content := os.read_file(lcov_file) or { panic(err) }
	assert lcov_content.contains('SF:${np(os.join_path(vroot, 'cmd/tools/vcover/testdata/simple/simple.v'))}')
	assert lcov_content.contains('DA:4,1')
	assert lcov_content.contains('DA:12,0')
	assert lcov_content.contains('LF:9')
	assert lcov_content.contains('LH:4')
	assert lcov_content.contains('end_of_record')
	hfilter1 :=
		execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t1)} --filter vcover/testdata/simple/ -H -P false')
	assert hfilter1.exit_code == 0, hfilter1.output
	assert !hfilter1.output.contains('%'), hfilter1.output
	houtput1 := hfilter1.output.trim_space().split_into_lines()
	zeros1 := houtput1.filter(it.starts_with('0 '))
	nzeros1 := houtput1.filter(!it.starts_with('0 '))
	assert zeros1.len > 0
	assert zeros1.any(it.contains('simple.v:12')), zeros1.str()
	assert zeros1.any(it.contains('simple.v:14')), zeros1.str()
	assert zeros1.any(it.contains('simple.v:17')), zeros1.str()
	assert zeros1.any(it.contains('simple.v:18')), zeros1.str()
	assert zeros1.any(it.contains('simple.v:19')), zeros1.str()
	assert nzeros1.len > 0
	assert nzeros1.any(it.contains('simple.v:4')), nzeros1.str()
	assert nzeros1.any(it.contains('simple.v:6')), nzeros1.str()
	assert nzeros1.any(it.contains('simple.v:8')), nzeros1.str()
	assert nzeros1.any(it.contains('simple.v:25')), nzeros1.str()

	r2 :=
		execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t2)} cmd/tools/vcover/testdata/simple/t2_test.v')
	assert r2.exit_code == 0, r2.str()
	assert r2.output.trim_space() == '24', r2.str()
	assert os.exists(t2), t2
	filter2 :=
		execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t2)} --filter vcover/testdata/simple')
	assert filter2.exit_code == 0, filter2.output
	assert filter2.output.contains('cmd/tools/vcover/testdata/simple/simple.v')
	assert filter2.output.trim_space().ends_with('|      6 |      9 |  66.67%'), filter2.output
	hfilter2 :=
		execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t2)} --filter testdata/simple -H -P false')
	assert hfilter2.exit_code == 0, hfilter2.output
	assert !hfilter2.output.contains('%'), hfilter2.output
	houtput2 := hfilter2.output.trim_space().split_into_lines()
	zeros2 := houtput2.filter(it.starts_with('0 '))
	nzeros2 := houtput2.filter(!it.starts_with('0 '))
	assert zeros2.len > 0
	assert zeros2.any(it.contains('simple.v:4')), zeros2.str()
	assert zeros2.any(it.contains('simple.v:6')), zeros2.str()
	assert zeros2.any(it.contains('simple.v:8')), zeros2.str()
	assert nzeros2.len > 0
	assert nzeros2.any(it.contains('simple.v:17')), nzeros2.str()
	assert nzeros2.any(it.contains('simple.v:18')), nzeros2.str()
	assert nzeros2.any(it.contains('simple.v:19')), nzeros2.str()
	assert nzeros2.any(it.contains('simple.v:25')), nzeros2.str()

	// Run both tests. The coverage should be combined and == 100%
	r3 :=
		execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t3)} test cmd/tools/vcover/testdata/simple/')
	assert r3.exit_code == 0, r3.str()
	assert r3.output.trim_space().contains('Summary for all V _test.v files: '), r3.str()
	assert os.exists(t3), t3
	filter3 := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t3)} --filter simple/')
	assert filter3.exit_code == 0, filter3.str()
	assert filter3.output.contains('cmd/tools/vcover/testdata/simple/simple.v'), filter3.str()
	assert filter3.output.trim_space().match_glob('*cmd/tools/vcover/testdata/simple/simple.v *|      9 |      9 | 100.00%'), filter3.str()
}

// example1 calls abc01, abc10 through abc19, and abc20 through abc30, so the
// functions the three test files never call are the ones `--uncovered` reports.
const called_functions = [
	'abc01',
	'abc10',
	'abc11',
	'abc12',
	'abc13',
	'abc14',
	'abc15',
	'abc16',
	'abc17',
	'abc18',
	'abc19',
	'abc20',
	'abc21',
	'abc22',
	'abc23',
	'abc24',
	'abc25',
	'abc26',
	'abc27',
	'abc28',
	'abc29',
	'abc30',
]

fn test_functions_and_uncovered_report_the_functions_no_run_entered() {
	ex := np(os.join_path(tfolder, 'example1'))
	assert !os.exists(ex), ex
	source := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(ex)} test cmd/tools/vcover/testdata/example1/')
	assert source.exit_code == 0, source.str()
	assert os.exists(ex), ex

	functions := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(ex)} --filter example1 -F -P false')
	assert functions.exit_code == 0, functions.str()
	// The fixture has 39 functions of abc.v, 22 of which a test calls.
	assert functions.output.contains('abc.v                                         |     22 |     39 |  56.41%'), functions.output
	assert functions.output.contains('    covered  abc01'), functions.output
	assert functions.output.contains('    MISSING  abc02'), functions.output
	// A function called by only one of the three test files is covered once and
	// reported once, not once per meta file that recorded it.
	assert functions.output.split('abc01').len == 2, functions.output

	uncovered := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(ex)} --filter example1 -U -P false')
	assert uncovered.exit_code == 0, uncovered.str()
	lines := uncovered.output.trim_space().split_into_lines()
	// `--uncovered` prints only the list, so the number of lines is the number
	// of uncalled functions: 39 - 22.
	assert lines.len == 17, lines.str()
	for line in lines {
		location, name := line.all_before_last(' '), line.all_after_last(' ')
		assert location.starts_with('cmd/tools/vcover/testdata/example1/abc.v:'), location
		assert !called_functions.contains(name), '${name} was reported but a test calls it'
	}
	// The line numbers point at the declarations, in source order.
	assert lines[0].starts_with('cmd/tools/vcover/testdata/example1/abc.v:11 '), lines[0]
}

fn test_json_report_carries_both_lines_and_functions() {
	ex := np(os.join_path(tfolder, 'example1'))
	assert os.exists(ex), ex
	report_path := np(os.join_path(tfolder, 'report.json'))
	os.rm(report_path) or {}
	report := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(ex)} --filter example1 --json ${os.quoted_path(report_path)} -P false')
	assert report.exit_code == 0, report.str()
	assert os.exists(report_path), report_path
	content := os.read_file(report_path) or { panic(err) }
	assert content.contains('"function_points": 39'), content
	assert content.contains('"function_covered": 22'), content
	assert content.contains('"name": "abc01"'), content
	assert content.contains('"name": "abc02"'), content
	assert content.contains('"covered": true'), content
	assert content.contains('"covered": false'), content
	assert content.contains('"line": 3'), content
	assert content.contains('"line": 11'), content
}

fn test_write_tests_generates_only_for_untested_code() {
	ex := np(os.join_path(tfolder, 'example1'))
	assert os.exists(ex), ex
	// The copy of the fixture, made by `testsuite_begin`, is where the generated
	// file is written: `--write-tests` puts a test file beside its source, and
	// that has to be a file the tool was allowed to reach.
	fresh := np(os.join_path(vroot, 'cmd/tools/vcover/testdata/generated/example1'))
	assert os.is_dir(fresh), fresh
	test_path := np(os.join_path(fresh, 'abc_test.v'))
	assert !os.exists(test_path), test_path

	// The copy is compiled rather than filtered, because a coverage directory
	// only holds what a run recorded. The filter then names the copy, so a
	// generated test is written beside it rather than beside a fixture.
	fresh_cov := np(os.join_path(tfolder, 'fresh_cov_${os.getpid()}'))
	os.rmdir_all(fresh_cov) or {}
	r4 := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(fresh_cov)} test ${os.quoted_path(fresh)}')
	assert r4.exit_code == 0, r4.str()
	assert os.exists(fresh_cov), fresh_cov

	dry := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(fresh_cov)} --filter ${filter_fresh()} -U -P false --write-tests --dry-run')
	assert dry.exit_code == 0, dry.str()
	// The dry run prints the file it would write with a `---` header.
	assert dry.output.contains('--- '), dry.output
	assert dry.output.contains('fn test_abc02() {'), dry.output
	assert dry.output.contains('fn test_abc31() {'), dry.output
	assert !dry.output.contains('fn test_abc01() {'), dry.output
	assert !dry.output.contains('fn test_abc10() {'), dry.output
	// The signature of the function is written for the author, with its zero
	// value call beside it, and the placeholder assertion below.
	assert dry.output.contains('// signature: fn abc02() int'), dry.output
	assert dry.output.contains('_ = abc02()'), dry.output
	assert dry.output.contains('assert true'), dry.output
	assert dry.output.contains('module example1'), dry.output
	// A dry run writes nothing.
	assert !os.exists(test_path), test_path

	written := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(fresh_cov)} --filter ${filter_fresh()} -U -P false --write-tests')
	assert written.exit_code == 0, written.str()
	assert os.exists(test_path), test_path
	content := os.read_file(test_path) or { panic(err) }
	assert content.contains('fn test_abc02() {'), content
	assert content.contains('fn test_abc39() {'), content
	assert !content.contains('fn test_abc01() {'), content
	assert !content.contains('fn test_abc30() {'), content
	// The generated test file compiles and passes.
	check := execute('${os.quoted_path(vexe)} test ${os.quoted_path(test_path)}')
	assert check.exit_code == 0, check.str()
	// A second run changes nothing: the functions are still reported uncovered,
	// because the coverage data still says so, but every one of them already has
	// a test function in the file, so nothing is written a second time.
	again := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(fresh_cov)} --filter ${filter_fresh()} -U -P false --write-tests')
	assert again.exit_code == 0, again.str()
	assert again.output.contains('abc02'), again.output
	unchanged := os.read_file(test_path) or { panic(err) }
	assert unchanged == content, 'a second run changed the generated file'

	// Without a filter the tool refuses, rather than writing a test beside every
	// file the run happened to reach.
	unfiltered := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(fresh_cov)} -U -P false --write-tests')
	assert unfiltered.exit_code == 0, unfiltered.str()
	assert unfiltered.output.contains('--write-tests needs --filter'), unfiltered.output
	assert os.read_file(test_path) or { panic(err) } == content, 'a run without a filter changed the generated file'
}

fn test_the_generated_tests_leave_a_comment_where_no_call_can_be_built() {
	// `params.v` calls `needs_a_map` from its test and leaves `takes_plain`
	// untested. Its parameters are all types a generated test can spell, so it
	// gets a call; a parameter such as a map or a generic does not, and gets a
	// comment instead, because a test that does not compile is worse than one
	// that does not assert anything yet.
	cov := np(os.join_path(tfolder, 'params_cov'))
	assert !os.exists(cov), cov
	r := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(cov)} test cmd/tools/vcover/testdata/params/')
	assert r.exit_code == 0, r.str()

	uncovered := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(cov)} -U -P false --filter testdata/params')
	assert uncovered.exit_code == 0, uncovered.str()
	assert uncovered.output.contains('params.v:7 takes_plain'), uncovered.output
	has_other := uncovered.output.contains('needs_a_map')
	assert !has_other, uncovered.output

	dry := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(cov)} -U -P false --filter testdata/params --write-tests --dry-run')
	assert dry.exit_code == 0, dry.str()
	assert dry.output.contains('fn test_takes_plain() {'), dry.output
	assert dry.output.contains('// signature: fn takes_plain(a int, b string, c bool) bool'), dry.output
	// The test file imports its module, so the call is spelled through it.
	assert dry.output.contains("_ = params.takes_plain(0, '', false)"), dry.output
	// The report echoes the test file it already has, which holds the test for
	// the covered function; what is generated is `takes_plain` only.
	assert dry.output.contains('fn test_takes_plain() {'), dry.output
}

fn execute(cmd string) os.Result {
	eprintln('Executing: ${cmd}')
	return os.exec(os.split_args(cmd) or { panic(err) })
}

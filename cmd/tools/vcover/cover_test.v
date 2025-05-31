// vtest retry: 2
import os

const vexe = @VEXE
const vroot = os.dir(vexe)
const tfolder = os.join_path(os.vtmp_dir(), 'cover_test')

const t1 = np(os.join_path(tfolder, 't1'))
const t2 = np(os.join_path(tfolder, 't2'))
const t3 = np(os.join_path(tfolder, 't3'))

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	os.rmdir_all(tfolder) or {}
	os.mkdir(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
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
	assert res.output.contains('-S, --show_test_files     Show `_test.v` files as well (normally filtered).')
	assert res.output.contains('-A, --absolute            Use absolute paths for all files')
}

fn np(path string) string {
	return path.replace('\\', '/')
}

fn test_simple() {
	assert !os.exists(t1), t1
	assert !os.exists(t2), t2
	assert !os.exists(t3), t3

	r1 := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t1)} cmd/tools/vcover/testdata/simple/t1_test.v')
	assert r1.exit_code == 0, r1.str()
	assert r1.output.trim_space() == '10', r1.str()
	assert os.exists(t1), t1
	cmd := '${os.quoted_path(vexe)} cover ${os.quoted_path(t1)} --filter vcover/testdata/simple/'
	filter1 := execute(cmd)
	assert filter1.exit_code == 0, filter1.output
	assert filter1.output.contains('cmd/tools/vcover/testdata/simple/simple.v'), filter1.output
	assert filter1.output.trim_space().ends_with('|      4 |      9 |  44.44%'), filter1.output
	hfilter1 := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t1)} --filter vcover/testdata/simple/ -H -P false')
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

	r2 := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t2)} cmd/tools/vcover/testdata/simple/t2_test.v')
	assert r2.exit_code == 0, r2.str()
	assert r2.output.trim_space() == '24', r2.str()
	assert os.exists(t2), t2
	filter2 := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t2)} --filter vcover/testdata/simple')
	assert filter2.exit_code == 0, filter2.output
	assert filter2.output.contains('cmd/tools/vcover/testdata/simple/simple.v')
	assert filter2.output.trim_space().ends_with('|      6 |      9 |  66.67%'), filter2.output
	hfilter2 := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t2)} --filter testdata/simple -H -P false')
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
	r3 := execute('${os.quoted_path(vexe)} -no-skip-unused -coverage ${os.quoted_path(t3)} test cmd/tools/vcover/testdata/simple/')
	assert r3.exit_code == 0, r3.str()
	assert r3.output.trim_space().contains('Summary for all V _test.v files: '), r3.str()
	assert os.exists(t3), t3
	filter3 := execute('${os.quoted_path(vexe)} cover ${os.quoted_path(t3)} --filter simple/')
	assert filter3.exit_code == 0, filter3.str()
	assert filter3.output.contains('cmd/tools/vcover/testdata/simple/simple.v'), filter3.str()
	assert filter3.output.trim_space().match_glob('*cmd/tools/vcover/testdata/simple/simple.v *|      9 |      9 | 100.00%'), filter3.str()
}

fn execute(cmd string) os.Result {
	eprintln('Executing: ${cmd}')
	return os.execute(cmd)
}

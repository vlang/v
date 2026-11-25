// vtest build: !self_sandboxed_packaging? && !sanitized_job?
// .out file:
// To test a panic, remove everything after the long `===` line
// You can also remove the line with 'line:' e.g. for a builtin fn
import os
import rand
import runtime
import term
import v.util.diff
import v.util.vtest

$if windows {
	import v.fixturetest
}

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const v_ci_ubuntu_musl = os.getenv('V_CI_UBUNTU_MUSL').len > 0

const skip_files = [
	'do_not_remove_this',
	'tmpl_parse_html.vv', // skipped, due to a V template compilation problem after b42c824
]

// InoutResult is the outcome of one program of the folder, reported in the order of the files.
struct InoutResult {
	idx    int
	path   string
	status InoutStatus
	label  string // why the file was skipped
	report string // the difference, for a failed file
}

enum InoutStatus {
	ok
	ok_panic
	skip
	fail
}

fn test_all() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	dir := 'vlib/v/slow_tests/inout'
	mut files := os.ls(dir) or { panic(err) }
	files.sort()
	tests := files.filter(it.ends_with('.vv') || it.ends_with('.vsh') || it.ends_with('.v3.v'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	println('Found ${paths.len} .vv/.vsh/.v3.v files in ${dir} ...')
	// Each program compiles and runs on its own, so they are done side by side. Their
	// compilers share the CPUs, like the compilers of `v test`.
	mut jobs := runtime.nr_jobs()
	if jobs > paths.len {
		jobs = paths.len
	}
	if jobs < 1 {
		jobs = 1
	}
	compiler_jobs := runtime.nr_cpus() / jobs
	previous_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', (if compiler_jobs < 1 { 1 } else { compiler_jobs }).str(), true)
	defer {
		if value := previous_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	work := chan int{cap: paths.len}
	for idx in 0 .. paths.len {
		work <- idx
	}
	work.close()
	results := chan InoutResult{cap: paths.len}
	mut workers := []thread{}
	for _ in 0 .. jobs {
		workers << spawn inout_worker(work, results, paths, vexe, vroot)
	}
	workers.wait()
	mut ordered := []InoutResult{len: paths.len}
	for _ in 0 .. paths.len {
		r := <-results
		ordered[r.idx] = r
	}
	mut total_oks := 0
	mut total_oks_panic := 0
	mut total_skips := 0
	mut total_errors := 0
	for r in ordered {
		vprint('${r.idx + 1:3}/${paths.len:-3} ${r.path} ')
		match r.status {
			.skip {
				vprintln(term.bright_yellow(r.label))
				total_skips++
			}
			.ok {
				vprintln(term.green('OK'))
				total_oks++
			}
			.ok_panic {
				vprintln(term.green('OK (panic)'))
				total_oks_panic++
			}
			.fail {
				println(term.red('FAIL'))
				println(r.report)
				total_errors++
			}
		}
	}
	println('>>> Summary: files: ${paths.len}, ok: ${total_oks}, ok panics: ${total_oks_panic}, skipped: ${total_skips}, errors: ${total_errors} .')
	assert total_errors == 0
}

fn inout_worker(work chan int, results chan InoutResult, paths []string, vexe string, vroot string) {
	for {
		idx := <-work or { break }
		mut r := run_inout_program(paths[idx], vexe, vroot)
		r = InoutResult{
			...r
			idx:  idx
			path: paths[idx]
		}
		results <- r
	}
}

// run_inout_program compiles and runs `path`, and compares its output with the `.out` file.
fn run_inout_program(path string, vexe string, vroot string) InoutResult {
	fname := os.file_name(path)
	if fname in skip_files {
		return InoutResult{
			status: .skip
			label:  'SKIP'
		}
	}
	if fname.ends_with('.v3.v') && os.user_os() != 'macos' {
		return InoutResult{
			status: .skip
			label:  'SKIP on non-macOS'
		}
	}
	if v_ci_ubuntu_musl && fname.contains('orm_') {
		// the ORM programs use db.sqlite, which is not easy to install in a way usable by ubuntu-musl, so just skip them:
		return InoutResult{
			status: .skip
			label:  'SKIP on ubuntu musl'
		}
	}
	program := path
	tname := rand.ulid()
	tbase := os.join_path(os.vtmp_dir(), tname)
	texe := if os.user_os() == 'windows' { '${tbase}.exe' } else { tbase }
	compilation := os.exec([vexe, '-o', '${tbase}', '-cflags', '-w', '-cg', '${program}'])
	if compilation.exit_code != 0 {
		return InoutResult{
			status: .fail
			report: 'compilation failed: ${compilation.output}'
		}
	}
	res := os.exec([texe])
	$if windows {
		os.rm(texe) or {}
		$if msvc {
			os.rm('${tbase}.ilk') or {}
			os.rm('${tbase}.pdb') or {}
		}
	} $else {
		os.rm(texe) or {}
	}
	if res.exit_code < 0 {
		return InoutResult{
			status: .fail
			report: 'could not run the program: ${res.output}'
		}
	}
	mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
	$if windows {
		found = fixturetest.slash_diagnostic_paths(found)
	}
	expected_path := if program.ends_with('.v3.v') {
		program.trim_string_right('.v3.v') + '.out'
	} else {
		program.replace('.vv', '').replace('.vsh', '') + '.out'
	}
	mut expected := os.read_file(expected_path) or {
		return InoutResult{
			status: .fail
			report: 'cannot read ${expected_path}: ${err}'
		}
	}
	expected = expected.trim_right('\r\n').replace('\r\n', '\n')
	if expected.contains('================ V panic ================') {
		// panic include backtraces and absolute file paths, so can't do char by char comparison
		n_found := normalize_panic_message(found, vroot)
		n_expected := normalize_panic_message(expected, vroot)
		if found.contains('================ V panic ================') {
			if n_found.starts_with(n_expected) {
				return InoutResult{
					status: .ok_panic
				}
			}
			// Both have panics, but there was a difference...
			// Pass the normalized strings for further reporting.
			// There is no point in comparing the backtraces too.
			found = n_found
			expected = n_expected
		}
	}
	if expected == found {
		return InoutResult{
			status: .ok
		}
	}
	mut report := ''
	if diff_ := diff.compare_text(expected, found) {
		report = term.header('difference:', '-') + '\n' + diff_
	} else {
		report = '${err}\n' + term.header('expected:', '-') + '\n' + expected + '\n' +
			term.header('found:', '-') + '\n' + found
	}
	return InoutResult{
		status: .fail
		report: report + '\n' + term.h_divider('-')
	}
}

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	// remove vroot
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}

@[if !silent ?]
fn vprint(msg string) {
	print(msg)
}

@[if !silent ?]
fn vprintln(msg string) {
	println(msg)
}

import os
import time

// vtest flaky: true
// vtest retry: 4

/*
The goal of this test, is to simulate a developer, that has run a program, compiled with -live flag.

It does so by writing a new generated program containing a [live] fn pmessage() string {...} function,
(that program is in `vlib/v/live/live_test_template.vv`)
then runs the generated program at the start *in the background*,
waits some time, so that the program could run a few iterations, then modifies its source
(simulates a developer that has saved a new version of the program source),
then it waits some more, modifies it again and saves it once more.

On each modification, the running program, should detect that its source code has changed,
and recompile a shared library, which it then it should load, and thus modify its own
behavior at runtime (the pmessage function).

If everything works fine, the output of the generated program would have changed at least 1-2 times,
which then is detected by the test program (the histogram checks).

Since this test program is sensitive to coordination (or lack of) of several processes,
it tries to sidestep the coordination issue by polling the file system for the existance
of files, ORIGINAL.txt ... STOP.txt , which are appended to by the generated program.

Note: That approach of monitoring the state of the running generated program, is clearly not ideal,
but sidesteps the issue of coordinating processes through IPC or stdin/stdout in hopefully
not very flaky way.

TODO: Cleanup this when/if v has better process control/communication primitives.
*/
const (
	vexe                = os.getenv('VEXE')
	tmp_file            = os.join_path(os.temp_dir(), 'generated_live_program.tmp.v')
	source_file         = os.join_path(os.temp_dir(), 'generated_live_program.v')
	genexe_file         = os.join_path(os.temp_dir(), 'generated_live_program')
	output_file         = os.join_path(os.temp_dir(), 'generated_live_program.output.txt')
	res_original_file   = os.join_path(os.temp_dir(), 'ORIGINAL.txt')
	res_changed_file    = os.join_path(os.temp_dir(), 'CHANGED.txt')
	res_another_file    = os.join_path(os.temp_dir(), 'ANOTHER.txt')
	res_stop_file       = os.join_path(os.temp_dir(), 'STOP.txt')
	cleanup_files       = [tmp_file, source_file, genexe_file, output_file, res_original_file,
		res_changed_file, res_another_file, res_stop_file]
	live_program_source = get_source_template()
)

fn get_source_template() string {
	src := os.read_file(os.join_path(os.dir(@FILE), 'live_test_template.vv')) or { panic(err) }
	return src.replace('#OUTPUT_FILE#', output_file)
}

fn edefault(name string, default string) string {
	res := os.getenv(name)
	if res == '' {
		return default
	}
	return res
}

fn atomic_write_source(source string) {
	// Note: here wrtiting is done in 2 steps, since os.write_file can take some time,
	// during which the file will be modified, but it will still be not completely written.
	// The os.mv after that, guarantees that the reloader will see a complete valid V program.
	os.write_file(tmp_file, source) or { panic(err) }
	os.mv(tmp_file, source_file) or { panic(err) }
}

//
fn testsuite_begin() {
	if os.user_os() !in ['linux', 'solaris'] && os.getenv('FORCE_LIVE_TEST').len == 0 {
		eprintln('Testing the runtime behaviour of -live mode,')
		eprintln('is reliable only on Linux/macOS for now.')
		eprintln('You can still do it by setting FORCE_LIVE_TEST=1 .')
		exit(0)
	}
	for f in [tmp_file, source_file, output_file, res_original_file, res_changed_file,
		res_another_file, res_stop_file] {
		os.rm(f) or {}
	}
	atomic_write_source(live_program_source)
}

[debuglivetest]
fn vprintln(s string) {
	eprintln(s)
}

fn testsuite_end() {
	vprintln('source: $source_file')
	vprintln('output: $output_file')
	vprintln('---------------------------------------------------------------------------')
	output_lines := os.read_lines(output_file) or {
		panic('could not read $output_file, error: $err')
	}
	mut histogram := map[string]int{}
	for line in output_lines {
		histogram[line] = histogram[line] + 1
	}
	for k, v in histogram {
		eprintln('> found ${v:5d} times: $k')
	}
	vprintln('---------------------------------------------------------------------------')
	assert histogram['START'] > 0
	assert histogram['ORIGINAL'] > 0
	assert histogram['CHANGED'] + histogram['ANOTHER'] > 0
	// assert histogram['END'] > 0
	for tfile in cleanup_files {
		os.rm(tfile) or {}
	}
}

fn change_source(new string) {
	time.sleep(100 * time.millisecond)
	vprintln('> change ORIGINAL to: $new')
	atomic_write_source(live_program_source.replace('ORIGINAL', new))
	wait_for_file(new)
}

fn wait_for_file(new string) {
	time.sleep(100 * time.millisecond)
	expected_file := os.join_path(os.temp_dir(), new + '.txt')
	eprintln('waiting for $expected_file ...')
	max_wait_cycles := edefault('WAIT_CYCLES', '1').int()
	for i := 0; i <= max_wait_cycles; i++ {
		if i % 25 == 0 {
			vprintln('   checking ${i:-10d} for $expected_file ...')
		}
		if os.exists(expected_file) {
			assert true
			vprintln('> done.')
			time.sleep(100 * time.millisecond)
			break
		}
		time.sleep(5 * time.millisecond)
	}
}

fn setup_cycles_environment() {
	mut max_live_cycles := 1000
	mut max_wait_cycles := 400
	if os.user_os() == 'macos' {
		//		max_live_cycles *= 5
		//		max_wait_cycles *= 5
	}
	os.setenv('LIVE_CYCLES', '$max_live_cycles', true)
	os.setenv('WAIT_CYCLES', '$max_wait_cycles', true)
}

//
fn test_live_program_can_be_compiled() {
	setup_cycles_environment()
	eprintln('Compiling...')
	os.system('${os.quoted_path(vexe)} -nocolor -live -o ${os.quoted_path(genexe_file)} ${os.quoted_path(source_file)}')
	//
	cmd := '${os.quoted_path(genexe_file)} > /dev/null &'
	eprintln('Running with: $cmd')
	res := os.system(cmd)
	assert res == 0
	eprintln('... running in the background')
	wait_for_file('ORIGINAL')
}

fn test_live_program_can_be_changed_1() {
	change_source('CHANGED')
	assert true
}

fn test_live_program_can_be_changed_2() {
	change_source('ANOTHER')
	assert true
}

fn test_live_program_can_be_changed_3() {
	change_source('STOP')
	change_source('STOP')
	change_source('STOP')
	assert true
}

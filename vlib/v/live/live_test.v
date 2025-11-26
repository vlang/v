// vtest retry: 4
import os
import log
import time

/*
The goal of this test, is to simulate a developer, that has run a program, compiled with -live flag.

It does so by writing a new generated program containing a @[live] fn pmessage() string {...} function,
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
it tries to sidestep the coordination issue by polling the file system for the existence
of files, ORIGINAL.txt ... STOP.txt , which are appended to by the generated program.

Note: That approach of monitoring the state of the running generated program, is clearly not ideal,
but sidesteps the issue of coordinating processes through IPC or stdin/stdout in hopefully
not very flaky way.

TODO: Cleanup this when/if v has better process control/communication primitives.
*/
const vexe = os.getenv('VEXE')
const vtmp_folder = os.join_path(os.vtmp_dir(), 'live_tests')
const main_source_file = os.join_path(vtmp_folder, 'main.v')
const tmp_file = os.join_path(vtmp_folder, 'mymodule', 'generated_live_module.tmp')
const source_file = os.join_path(vtmp_folder, 'mymodule', 'mymodule.v')
const genexe_file = os.join_path(vtmp_folder, 'generated_live_program.exe')
const output_file = os.join_path(vtmp_folder, 'generated_live_program.output.txt')
const res_original_file = os.join_path(vtmp_folder, 'ORIGINAL.txt')
const res_changed_file = os.join_path(vtmp_folder, 'CHANGED.txt')
const res_another_file = os.join_path(vtmp_folder, 'ANOTHER.txt')
const res_stop_file = os.join_path(vtmp_folder, 'STOP.txt')
const live_program_source = get_source_template()

fn get_source_template() string {
	src := os.read_file(os.join_path(os.dir(@FILE), 'live_test_template.vv')) or { panic(err) }
	return src.replace('#OUTPUT_FILE#', output_file.replace('\\', '\\\\'))
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
	os.rmdir_all(vtmp_folder) or {}
	os.mkdir_all(vtmp_folder) or {}
	os.mkdir_all(os.join_path(vtmp_folder, 'mymodule'))!
	os.write_file(os.join_path(vtmp_folder, 'v.mod'), '')!
	os.cp(os.join_path(os.dir(@FILE), 'live_test_template_main.vv'), os.join_path(vtmp_folder,
		'main.v'))!
	if os.user_os() !in ['linux', 'solaris'] && os.getenv('FORCE_LIVE_TEST').len == 0 {
		eprintln('Testing the runtime behaviour of -live mode,')
		eprintln('is reliable only on Linux/macOS for now.')
		eprintln('You can still do it by setting FORCE_LIVE_TEST=1 .')
		exit(0)
	}
	atomic_write_source(live_program_source)
	// os.system('tree $vtmp_folder') exit(1)
	spawn watchdog()
}

fn watchdog() {
	// This thread will automatically exit the live_test.v process, if it gets stuck.
	// On the Github CI, especially on the sanitized jobs, that are super slow, this allows
	// the job as a whole to continue, because the V test framework will restart live_test.v
	// a few times, and then it will stop.
	// Previusly, it could not do that, because if the process itself takes say a few hours,
	// when the CI job gets reprioritized, the whole Github job will get cancelled, when it
	// reaches its own timeout (which is 3 hours).
	// Note, that usually `v vlib/v/live/live_test.v` does not take too long - it takes
	// ~4 seconds, even on an i3, with tcc, ~12 seconds with clang, and ~15 seconds with gcc,
	// so the *5 minutes* period, allows plenty of time for the process to finish normally.
	sw := time.new_stopwatch()
	for {
		elapsed_time_in_seconds := sw.elapsed().seconds()
		$if print_watchdog_time ? {
			log.warn('> dt: ${elapsed_time_in_seconds:6.3f}s')
		}
		if elapsed_time_in_seconds > 5 * 60 {
			log.warn('> watchdog triggered, elapsed time: ${elapsed_time_in_seconds:6.3f}s')
			exit(3)
		}
		time.sleep(1 * time.second)
	}
}

@[if debuglivetest ?]
fn vprintln(s string) {
	eprintln(s)
}

fn testsuite_end() {
	// os.system('tree $vtmp_folder') exit(1)
	vprintln('source: ${source_file}')
	vprintln('output: ${output_file}')
	vprintln('---------------------------------------------------------------------------')
	output_lines := os.read_lines(output_file) or {
		panic('could not read ${output_file}, error: ${err}')
	}
	mut histogram := map[string]int{}
	for oline in output_lines {
		line := oline.all_after('|| ')
		histogram[line]++
	}
	for k, v in histogram {
		eprintln('> found ${v:5d} times: ${k}')
	}
	vprintln('---------------------------------------------------------------------------')
	assert histogram['START'] > 0
	assert histogram['ORIGINAL'] > 0
	assert histogram['CHANGED'] + histogram['ANOTHER'] > 0
	// assert histogram['END'] > 0
	$if !keep_results ? {
		os.rmdir_all(vtmp_folder) or {}
		log.info('Removed ${vtmp_folder} . Use `-d keep_results` to override.')
	}
}

fn change_source(new string) {
	log.info('> change ORIGINAL to: ${new} ...')
	atomic_write_source(live_program_source.replace('ORIGINAL', new))
	wait_for_file(new)
}

fn wait_for_file(new string) {
	expected_file := os.join_path(vtmp_folder, new + '.txt')
	max_wait_cycles := os.getenv_opt('WAIT_CYCLES') or { '1' }.int()
	log.info('waiting max_wait_cycles: ${max_wait_cycles} for file: ${expected_file} ...')
	mut sw := time.new_stopwatch()
	for i := 0; i <= max_wait_cycles; i++ {
		if i > 0 && i % 500 == 0 {
			log.info('   checking ${i:3d}/${max_wait_cycles:-3d}, waited for: ${sw.elapsed().seconds():6.3f}s, for ${expected_file} ...')
		}
		if os.exists(expected_file) {
			assert true
			log.info('> done waiting for ${expected_file}, iteration: ${i:3d}, waited for: ${sw.elapsed().seconds():6.3f}s')
			time.sleep(80 * time.millisecond)
			break
		}
		time.sleep(1 * time.millisecond)
	}
}

fn setup_cycles_environment() {
	mut max_live_cycles := 1000 // read by live_test_template.vv
	mut max_wait_cycles := 5000
	os.setenv('LIVE_CYCLES', '${max_live_cycles}', true)
	os.setenv('WAIT_CYCLES', '${max_wait_cycles}', true)
}

fn run_in_background(cmd string) {
	log.warn('running in background: ${cmd} ...')
	spawn fn (cmd string) {
		res := os.execute(cmd)
		log.warn('Background cmd ended. res.exit_code: ${res.exit_code} | res.output.len: ${res.output.len}')
		if res.exit_code != 0 {
			eprintln('----------------------- background command failed: --------------------------')
			eprintln('----- exit_code: ${res.exit_code}, cmd: ${cmd}, output:')
			eprintln(res.output)
			eprintln('-----------------------------------------------------------------------------')
		}
		assert res.exit_code == 0
	}(cmd)
	log.warn('the live program should be running in the background now')
}

fn test_live_program_can_be_compiled() {
	setup_cycles_environment()
	compile_cmd := '${os.quoted_path(vexe)} -cg -keepc -nocolor -live -o ${os.quoted_path(genexe_file)} ${os.quoted_path(main_source_file)}'
	log.info('Compiling with compile_cmd:')
	eprintln('> ${compile_cmd}')
	compile_res := os.system(compile_cmd)
	log.info('> DONE')
	assert compile_res == 0
	run_in_background('${os.quoted_path(genexe_file)}')
	wait_for_file('ORIGINAL')
}

fn test_live_program_can_be_changed_1() {
	change_source('CHANGED')
	time.sleep(250 * time.millisecond)
	assert true
}

fn test_live_program_can_be_changed_2() {
	change_source('ANOTHER')
	time.sleep(250 * time.millisecond)
	assert true
}

fn test_live_program_can_be_changed_3() {
	time.sleep(500 * time.millisecond)
	change_source('STOP')
	time.sleep(250 * time.millisecond)
	change_source('STOP')
	change_source('STOP')
	assert true
}

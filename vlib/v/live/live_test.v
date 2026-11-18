// vtest retry: 4
import os
import log
import time

/*
The generated -live program writes a marker for each version of pmessage() that it runs.
Wait for each marker before editing the source again. The STOP version lets the program
exit so the test can check its complete output.
*/
const vexe = os.getenv('VEXE')
const vtmp_folder = os.join_path(os.vtmp_dir(), 'live_tests_${os.getpid()}')
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
	previous_mtime := os.file_last_mod_unix(source_file)
	os.write_file(tmp_file, source) or { panic(err) }
	// Stamp the staged source before publishing it. The reloader needs a newer whole-second mtime.
	if os.file_last_mod_unix(tmp_file) <= previous_mtime {
		next_mtime := previous_mtime + 1
		os.utime(tmp_file, next_mtime, next_mtime) or { panic(err) }
	}
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
	if os.user_os() !in ['linux', 'macos', 'solaris'] && os.getenv('FORCE_LIVE_TEST').len == 0 {
		eprintln('Testing the runtime behaviour of -live mode,')
		eprintln('is reliable only on Linux/macOS for now.')
		eprintln('You can still do it by setting FORCE_LIVE_TEST=1 .')
		exit(0)
	}
	atomic_write_source(live_program_source)
	// os.system('tree ${vtmp_folder}') exit(1)
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
	// os.system('tree ${vtmp_folder}') exit(1)
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
	assert histogram['END'] > 0
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

fn remove_live_attr_from_source() {
	log.info('> remove @[live] attr from pmessage() while program is running ...')
	source_without_live :=
		live_program_source.replace('ORIGINAL', 'NO_LIVE').replace('@[live]\n', '')
	// Wait for a fresh marker from the original executable's fallback implementation.
	os.rm(res_original_file) or { panic(err) }
	atomic_write_source(source_without_live)
	wait_for_file('ORIGINAL')
}

fn wait_for_file(new string) {
	expected_file := os.join_path(vtmp_folder, new + '.txt')
	max_wait_cycles := os.getenv_opt('WAIT_CYCLES') or { '60000' }.int()
	log.info('waiting max_wait_cycles: ${max_wait_cycles} for file: ${expected_file} ...')
	mut sw := time.new_stopwatch()
	for i := 0; i <= max_wait_cycles; i++ {
		if i > 0 && i % 500 == 0 {
			log.info('   checking ${i:3d}/${max_wait_cycles:-3d}, waited for: ${sw.elapsed().seconds():6.3f}s, for ${expected_file} ...')
		}
		if os.exists(expected_file) {
			log.info('> done waiting for ${expected_file}, iteration: ${i:3d}, waited for: ${sw.elapsed().seconds():6.3f}s')
			return
		}
		time.sleep(1 * time.millisecond)
	}
	assert false, 'timed out waiting for ${expected_file}'
}

fn setup_cycles_environment() {
	mut max_live_cycles := 30000 // read by live_test_template.vv
	mut max_wait_cycles := 60000
	os.setenv('LIVE_CYCLES', '${max_live_cycles}', true)
	os.setenv('WAIT_CYCLES', '${max_wait_cycles}', true)
}

fn run_in_background(cmd string) thread {
	log.warn('running in background: ${cmd} ...')
	return spawn fn (cmd string) {
		res := os.exec(os.split_args(cmd) or { panic(err) })
		log.warn('Background cmd ended. res.exit_code: ${res.exit_code} | res.output.len: ${res.output.len}')
		if res.exit_code != 0 {
			eprintln('----------------------- background command failed: --------------------------')
			eprintln('----- exit_code: ${res.exit_code}, cmd: ${cmd}, output:')
			eprintln(res.output)
			eprintln('-----------------------------------------------------------------------------')
		}
		assert res.exit_code == 0
	}(cmd)
}

fn must_exec_cmd(cmd string) {
	res := os.exec(os.split_args(cmd) or { panic(err) })
	if res.exit_code == 0 {
		return
	}
	panic('command failed: ${cmd}\n${res.output}')
}

fn test_live_program_reloads_and_stops() {
	setup_cycles_environment()
	compile_cmd := '${os.quoted_path(vexe)} -cg -keepc -nocolor -live -o ${os.quoted_path(genexe_file)} ${os.quoted_path(main_source_file)}'
	log.info('Compiling with compile_cmd:')
	eprintln('> ${compile_cmd}')
	compile_res := os.system_args([vexe, '-cg', '-keepc', '-nocolor', '-live', '-o', genexe_file,
		main_source_file])
	log.info('> DONE')
	assert compile_res == 0
	program := run_in_background('${os.quoted_path(genexe_file)}')
	wait_for_file('ORIGINAL')
	change_source('CHANGED')
	remove_live_attr_from_source()
	change_source('ANOTHER')
	change_source('STOP')
	program.wait()
}

fn test_live_windows_sokol_sharedlive_build_uses_host_import_lib() {
	$if !windows || !msvc {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'live_windows_sokol_compile')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := os.join_path(@VEXEROOT, 'examples', 'sokol', 'drawing.v')
	exe_path := os.join_path(tmp_dir, 'drawing_live.exe')
	lib_path := exe_path[..exe_path.len - 4] + '.lib'
	dll_path := os.join_path(tmp_dir, 'drawing_live_shared.dll')
	must_exec_cmd('${os.quoted_path(vexe)} -nocolor -cc msvc -live -o ${os.quoted_path(exe_path)} ${os.quoted_path(source)}')
	assert os.exists(lib_path)
	must_exec_cmd('${os.quoted_path(vexe)} -nocolor -cc msvc -sharedlive -shared -ldflags ${os.quoted_path(lib_path)} -o ${os.quoted_path(dll_path)} ${os.quoted_path(source)}')
	assert os.exists(dll_path)
}

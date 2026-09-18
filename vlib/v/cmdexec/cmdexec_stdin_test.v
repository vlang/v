module cmdexec

import os
import time

#include <stdlib.h>

fn C._Exit(code int)

const stdin_eof_probe = 'v-cmdexec-stdin-eof-probe'
const stdin_eof_caller_input = 'only the caller may read this input'

// Bound unbounded entry points too, without relying on the collector under test.
fn stdin_eof_watchdog() {
	time.sleep(10 * time.second)
	C._Exit(91)
}

fn stdin_eof_entries() []string {
	return ['run', 'run_in', 'merged', 'bounded', 'zero', 'negative']
}

fn stdin_eof_run_entry(entry string, code int, work_folder string) os.Result {
	args := [stdin_eof_probe, 'read', code.str(), work_folder]
	return match entry {
		'run' { run(os.executable(), args) }
		'run_in' { run_in(os.executable(), args, work_folder) }
		'merged' { run_in_merged(os.executable(), args, work_folder) }
		'bounded' { run_with_timeout(os.executable(), args, 5_000) }
		'zero' { run_with_timeout(os.executable(), args, no_timeout) }
		'negative' { run_with_timeout(os.executable(), args, -1) }
		else { panic('unknown stdin probe entry: ${entry}') }
	}
}

fn stdin_eof_result_matches(result os.Result, code int, entry string) bool {
	output := result.output.replace('\r\n', '\n')
	lines := output.split_into_lines()
	if result.exit_code != code || lines.len != 2 || 'stdin-eof-stdout' !in lines
		|| 'stdin-eof-stderr' !in lines {
		return false
	}
	return entry != 'merged' || output == 'stdin-eof-stdout\nstdin-eof-stderr\n'
}

fn testsuite_begin() {
	if os.args.len < 2 || os.args[1] != stdin_eof_probe {
		return
	}
	if os.args.len != 5 {
		exit(2)
	}
	spawn stdin_eof_watchdog()
	mode := os.args[2]
	work_folder := os.args[4]
	if mode == 'read' {
		if os.real_path(os.getwd()) != os.real_path(work_folder) {
			exit(92)
		}
		// A closed/invalid descriptor is not EOF: require an actual zero-byte
		// read, repeatedly, instead of accepting a read error or empty string.
		for _ in 0 .. 3 {
			text, count := os.fd_read(0, 1)
			if count != 0 || text != '' {
				exit(93)
			}
		}
		println('stdin-eof-stdout')
		flush_stdout()
		eprintln('stdin-eof-stderr')
		flush_stderr()
		exit(os.args[3].int())
	}
	if mode == 'caller' {
		entry := os.args[3]
		result := stdin_eof_run_entry(entry, 37, work_folder)
		if !stdin_eof_result_matches(result, 37, entry) {
			eprintln('stdin probe failed: ${result.exit_code}: ${result.output}')
			exit(94)
		}
		// cmdexec must not inherit, consume, or close the caller's own input.
		input := os.fd_slurp(0).join('')
		if input != stdin_eof_caller_input {
			exit(95)
		}
		println('stdin-caller-preserved')
		exit(0)
	}
	exit(2)
}

fn test_capture_only_commands_receive_eof_and_preserve_output_and_exit_status() {
	root := os.join_path(os.vtmp_dir(), 'cmdexec stdin ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for entry in stdin_eof_entries() {
		work_folder := if entry in ['run_in', 'merged'] { root } else { os.getwd() }
		for code in [0, 37] {
			result := stdin_eof_run_entry(entry, code, work_folder)
			assert stdin_eof_result_matches(result, code, entry),
				'${entry}, expected ${code}; got ${result.exit_code}: ${result.output}'
		}
	}
}

fn test_capture_only_commands_leave_the_callers_stdin_untouched() {
	root := os.join_path(os.vtmp_dir(), 'cmdexec caller stdin ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	input_path := os.join_path(root, 'caller input.txt')
	os.write_file(input_path, stdin_eof_caller_input)!
	for entry in stdin_eof_entries() {
		work_folder := if entry in ['run_in', 'merged'] { root } else { os.getwd() }
		mut caller := os.new_process(os.executable())
		caller.set_args([stdin_eof_probe, 'caller', entry, work_folder])
		caller.set_stdin_path(input_path)
		caller.set_redirect_stdio_merged()
		caller.run()
		// The probe's watchdog bounds this tiny merged stream independently
		// of cmdexec, including when its unbounded child is blocked on stdin.
		output := caller.stdout_slurp()
		caller.wait()
		code := caller.code
		caller.close()
		assert code == 0, '${entry}: ${code}: ${output}'
		assert output.trim_space() == 'stdin-caller-preserved', output
	}
}

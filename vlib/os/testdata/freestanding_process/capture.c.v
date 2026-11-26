module main

import os

#include "@VMODROOT/vlib/os/testdata/freestanding_process/oracles.h"
#flag -DPROCESS_TEST_HOOKS

fn C.kill(i32, i32) i32
fn C.process_fd_count() i32
fn C.process_close_stdio() i32
fn C.process_restore_stdio()
fn C.process_limit_files(i32) i32
fn C.process_restore_files()

fn require(ok bool, label string) {
	if !ok {
		eprintln(label)
		exit(77)
	}
}

fn child() {
	match os.args[2] {
		'arguments' { print(os.args[3..].map('${it.len}:${it}').join('|')) }
		'argv0' { print(os.args[0]) }
		'path' { print(os.getenv_opt('PATH') or { 'absent' }) }
		'environment' { print(os.getenv('V_CAPTURE_VALUE')) }
		'large' {
			print('a'.repeat(262144))
			eprint('b'.repeat(262144))
		}
		'input' {
			text, count := os.fd_read(0, 128)
			require(count > 0, 'capture check 24')
			print(text)
		}
		'code' { exit(23) }
		'signal' { C.kill(i32(os.getpid()), 15) }
		else { exit(77) }
	}
}

fn concurrent_capture(executable string) bool {
	for _ in 0 .. 4 {
		result := os.exec([executable, 'child', 'code'])
		if result.exit_code != 23 { return false }
	}
	return true
}

fn main() {
	if os.args.len > 1 && os.args[1] == 'child' {
		child()
		return
	}
	executable := os.real_path(os.args[0])
	root := os.dir(executable)
	initial_fds := C.process_fd_count()
	// Environment snapshots exercise this too; keep an independent grow/free
	// regression so borrowed hash-table transfers remain valid under autofree.
	mut mapping := map[string]string{}
	for i in 0 .. 128 { mapping['key ${i}'] = 'value ${i}' }
	for i in 0 .. 128 { require(mapping['key ${i}'] == 'value ${i}', 'map expansion ownership') }
	values := ['', 'with spaces', 'quotes"\'', 'Ж', r'$(touch injection)']
	mut command := [executable, 'child', 'arguments']
	command << values
	result := os.exec(command)
	require(result.exit_code == 0, 'capture check 52')
	require(result.output == values.map('${it.len}:${it}').join('|'), 'capture check 53')
	require(!os.exists(os.join_path(root, 'injection')), 'capture check 54')
	require(os.setenv('V_CAPTURE_VALUE', 'changed value = Ж', true) == 0, 'capture check 55')
	environment := os.exec([executable, 'child', 'environment'])
	require(environment.exit_code == 0 && environment.output == 'changed value = Ж', 'capture check 57')
	helper := os.join_path(root, 'capture-command')
	if !os.exists(helper) { os.symlink(executable, helper) or { panic(err) } }
	previous_path := os.getenv('PATH')
	require(os.setenv('PATH', root + os.path_delimiter + previous_path, true) == 0, 'change search path')
	searched := os.exec(['capture-command', 'child', 'code'])
	require(searched.exit_code == 23, 'live PATH lookup')
	alias := os.exec(['capture-command', 'child', 'argv0'])
	require(alias.exit_code == 0 && alias.output == 'capture-command', 'literal argv zero for PATH aliases')
	first_dir := os.join_path(root, 'first')
	os.mkdir_all(first_dir) or { panic(err) }
	broken := os.join_path(first_dir, 'capture-command')
	os.write_file(broken, '#!/nonexistent-v-capture-interpreter\n') or { panic(err) }
	os.chmod(broken, 0o755) or { panic(err) }
	require(os.setenv('PATH', first_dir + os.path_delimiter + root, true) == 0, 'change search path')
	retry := os.exec(['capture-command', 'child', 'code'])
	require(retry.exit_code == 23, 'retry PATH after missing interpreter')
	require(os.setenv('PATH', '', true) == 0, 'empty search path')
	local := os.exec(['capture-command', 'child', 'argv0'])
	require(local.exit_code == 0 && local.output == 'capture-command', 'empty PATH searches current directory')
	require(os.unsetenv('PATH') == 0, 'unset search path')
	fallback := os.exec(['env', executable, 'child', 'path'])
	require(fallback.exit_code == 0 && fallback.output == 'absent', 'default search without changing child environment')
	require(os.setenv('PATH', previous_path, true) == 0, 'restore search path')
	large := os.exec([executable, 'child', 'large'])
	require(large.exit_code == 0 && large.output == 'a'.repeat(262144) + 'b'.repeat(262144), 'capture check 59')
	input := os.exec([executable, 'child', 'input'])
	require(input.exit_code == 0 && input.output == 'inherited stdin', 'capture check 61')
	code := os.exec([executable, 'child', 'code'])
	require(code.exit_code == 23, 'capture check 63')
	signal := os.exec([executable, 'child', 'signal'])
	require(signal.exit_code == 15, 'capture check 65')
	require(os.exec([]).exit_code == -1, 'capture check 66')
	require(os.exec([os.join_path(root, 'missing executable')]).exit_code == -1, 'capture check 67')
	bad := os.join_path(root, 'not executable')
	os.write_file(bad, 'not an executable') or { panic(err) }
	os.chmod(bad, 0o600) or { panic(err) }
	require(os.exec([bad]).exit_code == -1, 'capture check 70')
	os.chmod(bad, 0o700) or { panic(err) }
	os.write_file(bad, "printf 'text script'; exit 19\n") or { panic(err) }
	require(os.exec([bad]).exit_code == -1, 'text file without an interpreter')
	os.write_file(bad, "#!/bin/sh\nprintf 'text script'; exit 19\n") or { panic(err) }
	script := os.exec([bad])
	require(script.exit_code == 19 && script.output == 'text script', 'capture check 74')
	// -prod promotes the deprecated os.execute entry point to an error.
	// Its compatibility path is exercised by the other modes; use the public
	// argument-vector entry point for the same shell workload in production.
	shell := shell_capture("printf 'shell output'; printf ' + stderr' >&2; exit 7")
	require(shell.exit_code == 7 && shell.output == 'shell output + stderr', 'capture check 76')
	require(shell_capture('printf pipeline | tr a-z A-Z').output == 'PIPELINE', 'capture check 77')
	dynamic_command := "printf 'parent ${os.getpid()}'"
	dynamic_result := shell_capture(dynamic_command)
	require(dynamic_result.output == 'parent ${os.getpid()}', 'borrowed shell command')
	require(dynamic_command.starts_with('printf'), 'shell command lifetime')
	mut first := spawn concurrent_capture(executable)
	mut second := spawn concurrent_capture(executable)
	first_ok := first.wait()
	second_ok := second.wait()
	require(first_ok && second_ok, 'capture check 82')
	_, count := os.fd_read(987654, 16)
	require(count == -1, 'capture check 84')
	require(os.fd_slurp(987654).len == 0, 'capture check 85')
	require(C.process_fd_count() == initial_fds, 'capture descriptor cleanup')
	$if freestanding || syscalls {
		// Capture must also work when a pipe would otherwise reuse fd 0/1/2.
		require(C.process_close_stdio() == 0, 'save standard descriptors')
		closed_stdio := os.exec([executable, 'child', 'code'])
		C.process_restore_stdio()
		require(closed_stdio.exit_code == 23, 'capture with closed standard descriptors')
		for room in 0 .. 4 {
			require(C.process_limit_files(i32(room)) == 0, 'set descriptor limit')
			failed := os.exec([executable, 'child', 'code'])
			C.process_restore_files()
			require(failed.exit_code == -1, 'partial pipe allocation failure')
			require(C.process_fd_count() == initial_fds, 'partial pipe rollback')
		}
	}
	println('subprocess capture arguments, environment, pipes, inherited stdin, failures and concurrency passed')
}

fn shell_capture(command string) os.Result {
	$if capture_production ? {
		return os.exec(['/bin/sh', '-c', command])
	} $else {
		return os.execute(command)
	}
}

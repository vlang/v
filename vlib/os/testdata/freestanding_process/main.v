module main

import os
import time
import arena

#include "@VMODROOT/vlib/os/testdata/freestanding_process/oracles.h"

fn C.process_fd_count() i32
fn C.process_isolate_inherited_fds() i32
fn C.process_close_stdio() i32
fn C.process_restore_stdio()
fn C.process_limit_files(i32) i32
fn C.process_restore_files()
fn C.process_limit_children() i32
fn C.process_restore_children()
fn C.process_track_allocator()

fn require(ok bool, message string) {
	if !ok {
		eprintln(message)
		exit(77)
	}
}

fn child() {
	match os.args[2] {
		'echo' {
			print('${os.args[3]}|${os.getenv('PROCESS_VALUE')}|${os.getwd()}')
			eprint('child stderr')
		}
		'stdin' {
			data, n := os.fd_read(0, 128)
			print(data[..n])
		}
		'sleep' {
			println('ready')
			time.sleep(30 * time.second)
		}
		'code' { exit(23) }
		'fds' {
			$if process_test_hooks ? {
				require(C.process_fd_count() == 3, 'inherited descriptors')
			}
		}
		else { exit(78) }
	}
}

fn process(executable string, args []string) &os.Process {
	mut p := os.new_process(executable)
	p.set_environment({
		'PROCESS_VALUE': 'a value = with spaces'
	})
	p.set_args(args)
	p.set_redirect_stdio()
	return p
}

fn concurrent_worker(executable string) bool {
	for _ in 0 .. 8 {
		mut p := process(executable, ['child', 'echo', 'concurrent'])
		p.wait()
		ok := p.code == 0 && p.stdout_slurp().contains('concurrent|a value = with spaces|') && p.stderr_slurp() == 'child stderr'
		p.close()
		if !ok { return false }
	}
	return true
}

@[manualfree]
fn arena_spawns(executable string) {
	for _ in 0 .. 3 {
		mut scope := arena.new(chunk_size: 8192)
		scope.push()
		mut p := process(executable, ['child', 'code'])
		p.wait()
		require(p.code == 23, 'spawn inside allocation scope')
		p.close()
		scope.pop()
		scope.free()
	}
	mut p := process(executable, ['child', 'code'])
	p.wait()
	require(p.code == 23, 'spawn after freed allocation scopes')
	p.close()
}

fn main() {
	if os.args.len > 1 && os.args[1] == 'child' {
		child()
		return
	}
	$if process_test_hooks ? {
		require(C.process_isolate_inherited_fds() == 0, 'isolate inherited descriptors')
	}
	$if process_track_alloc ? {
		C.process_track_allocator()
	}
	// QEMU user-mode cannot exec an ARM64 image without a host binfmt handler.
	// Its ARM64 parent can instead exercise the same kernel Process ABI against
	// an independently built native copy of this child's fixture.
	executable := os.real_path(if os.args.len > 2 && os.args[1] == '--child-executable' {
		os.args[2]
	} else {
		os.args[0]
	})
	spelling := os.join_path(os.temp_dir(), 'freestanding_process_${os.getpid()}')
	os.mkdir_all(spelling) or { panic(err) }
	// Ask the kernel for the spelling so this fixture does not depend on the
	// separate real_path fix when /tmp itself is a symlink (as on Darwin).
	original_directory := os.getwd()
	os.chdir(spelling) or { panic(err) }
	root := os.getwd()
	os.chdir(original_directory) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	mut p := process(executable, ['child', 'echo', 'argument with spaces'])
	p.set_work_folder(root)
	p.run()
	p.wait()
	require(p.code == 0, 'echo status ${p.code}: ${p.err}')
	actual := p.stdout_slurp()
	require(actual == 'argument with spaces|a value = with spaces|${root}', 'args/env/cwd: ${actual}; expected directory ${root}')
	require(p.stderr_slurp() == 'child stderr', 'stderr')
	p.wait()
	p.close()
	p.close()
	mut merged := process(executable, ['child', 'echo', 'merged'])
	merged.set_redirect_stdio_merged()
	merged.run()
	merged.wait()
	out := merged.stdout_slurp()
	require(merged.code == 0 && out.contains('merged|a value = with spaces|') && out.ends_with('child stderr'), 'merged streams: ${out}')
	merged.close()
	mut input := process(executable, ['child', 'stdin'])
	input.run()
	input.stdin_write('pipe input')
	input.wait()
	require(input.code == 0 && input.stdout_slurp() == 'pipe input', 'stdin pipe')
	input.close()
	path := os.join_path(root, 'input')
	os.write_file(path, 'file input') or { panic(err) }
	mut file := process(executable, ['child', 'stdin'])
	file.set_stdin_path(path)
	file.run()
	file.wait()
	require(file.code == 0 && file.stdout_slurp() == 'file input', 'stdin file')
	file.close()
	mut code := process(executable, ['child', 'code'])
	code.wait()
	require(code.code == 23, 'exit code: ${code.code}')
	code.close()
	mut lookup := os.new_process(os.file_name(executable))
	lookup.set_environment({
		'PATH': os.dir(executable)
	})
	lookup.set_args(['child', 'code'])
	lookup.set_redirect_stdio()
	lookup.wait()
	require(lookup.code == 23, 'parent PATH lookup ${lookup.err}: ${lookup.stderr_slurp()}')
	lookup.close()
	mut unresolved := os.new_process('v_process_missing_executable_78231')
	unresolved.set_environment({})
	unresolved.set_redirect_stdio()
	unresolved.wait()
	require(unresolved.code == 1 && unresolved.stderr_slurp().len > 0, 'parent executable lookup failure')
	unresolved.close()
	// Public V status is 64 bits; successful wait must replace all of it, while
	// WNOHANG must preserve it. Adjacent guards also detect oversized writes.
	mut exact := process(executable, ['child', 'code'])
	exact.run()
	mut guarded := [int(0x123456789abcdef), int(0x7f12345600000000), int(0x765432101234567)]
	ret := os.wait4(exact.pid, unsafe { &guarded[1] }, 0, unsafe { nil })
	require(ret == exact.pid && guarded == [int(0x123456789abcdef), int(23 << 8),
		int(0x765432101234567)], 'wait status width: ${guarded}')
	exact.status = .exited
	exact.close()
	$if freestanding || syscalls {
		// The caller deliberately reaped this pid through the public wait API.
		// A later Process wait must report ECHILD without waiting for another pid.
		mut reaped := process(executable, ['child', 'code'])
		reaped.run()
		mut status := 0
		require(os.wait4(reaped.pid, &status, 0, unsafe { nil }) == reaped.pid, 'external wait')
		reaped.wait()
		require(reaped.code == -1 && reaped.status == .aborted && reaped.err.len > 0, 'external reap state')
		reaped.wait()
		reaped.close()
	}
	mut failures := ['missing', 'permission']
	$if freestanding || syscalls {
		failures << ['stdin', 'cwd']
	}
	for mode in failures {
		mut failure := process(if mode == 'missing' {
			root + '/missing'
		} else if mode == 'permission' {
			path
		} else {
			executable
		}, ['child', 'code'])
		if mode == 'stdin' { failure.set_stdin_path(root + '/missing') }
		if mode == 'cwd' { failure.set_work_folder(root + '/missing') }
		failure.wait()
		err := failure.stderr_slurp()
		expected_errno := if mode == 'permission' { 13 } else { 2 }
		require(failure.code == 1 && err.ends_with('; code: ${expected_errno}\n'), '${mode} failure ${failure.code}: ${err}')
		failure.close()
	}
	for mode in ['kill', 'term', 'group'] {
		mut sleeper := process(executable, ['child', 'sleep'])
		if mode == 'group' { sleeper.use_pgroup = true }
		sleeper.run()
		// Darwin may report SIGKILL when a fatal signal interrupts exec past its
		// point of no return. Test TERM against the installed image. Group KILL
		// stays immediate on the raw backend, exercising parent-side setpgid.
		mut wait_ready := mode == 'term'
		$if !freestanding && !syscalls {
			// Hosted Process creates its group in the child only.
			if mode == 'group' { wait_ready = true }
		}
		if wait_ready {
			mut ready := ''
			for ready.len < 6 {
				part, n := os.fd_read(sleeper.stdio_fd[1], 6 - ready.len)
				if n <= 0 { break }
				ready += part
			}
			require(ready == 'ready\n', 'child readiness: ${ready}')
		}
		require(sleeper.pid > 0 && sleeper.is_alive(), 'nohang ${sleeper.err}')
		mut pending := int(0x7f12345600000000)
		require(os.wait4(sleeper.pid, &pending, 1, unsafe { nil }) == 0 && pending == int(0x7f12345600000000), 'nohang changed status')
		if mode == 'kill' {
			sleeper.signal_stop()
			sleeper.signal_continue()
			sleeper.signal_kill()
		} else if mode == 'term' {
			sleeper.signal_term()
		} else {
			sleeper.signal_pgkill()
		}
		sleeper.wait()
		expected := if mode == 'term' { 143 } else { 137 }
		require(sleeper.code == expected && sleeper.status == .aborted, '${mode} signal ${sleeper.code}: ${sleeper.err}')
		sleeper.wait()
		sleeper.close()
	}
	mut workers := []thread bool{}
	for _ in 0 .. 4 { workers << spawn concurrent_worker(executable) }
	for worker in workers { require(worker.wait(), 'concurrent spawn') }
	arena_spawns(executable)
	$if process_test_hooks ? {
		before := C.process_fd_count()
		mut inherited := process(executable, ['child', 'fds'])
		inherited.wait()
		require(inherited.code == 0, 'unintended descriptors inherited: ${inherited.stderr_slurp()}')
		inherited.close()
		require(C.process_close_stdio() == 0, 'save stdio')
		mut low := process(executable, ['child', 'echo', 'low descriptors'])
		low.wait()
		low_out := low.stdout_slurp()
		low_err := low.stderr_slurp()
		low.close()
		C.process_restore_stdio()
		require(low.code == 0 && low_out.contains('low descriptors|') && low_err == 'child stderr', 'closed standard descriptors: ${low_out}/${low_err}')
		require(C.process_fd_count() == before, 'descriptor leak after low descriptors')
		$if freestanding || syscalls {
			for room in [0, 1, 2, 3, 4, 5] {
				mut failure := process(executable, ['child', 'code'])
				require(C.process_limit_files(i32(room)) == 0, 'limit descriptors')
				failure.run()
				C.process_restore_files()
				require(failure.pid == -1 && failure.code == -1 && failure.status == .aborted && failure.err.ends_with('code: 24'), 'pipe rollback ${room}: ${failure.err}')
				failure.wait()
				failure.close()
				require(C.process_fd_count() == before, 'descriptor leak after pipe failure ${room}')
			}
			$if linux {
				mut failure := process(executable, ['child', 'code'])
				limited := C.process_limit_children()
				require(limited >= 0, 'limit children')
				if limited == 0 {
					failure.run()
					C.process_restore_children()
					require(failure.pid == -1 && failure.status == .aborted && failure.err.ends_with('code: 11'), 'fork rollback ${failure.pid}: ${failure.err}')
					failure.wait()
					failure.close()
					require(C.process_fd_count() == before, 'descriptor leak after fork failure')
				}
			}
		}
	}
	println('freestanding Process passed')
}

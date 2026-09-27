module cmdexec

import os
import time

#include <stdlib.h>

fn C._Exit(code int)

$if !windows {
	#include <unistd.h>

	fn C.getpgrp() int
}

const unbounded_pipe_probe = 'v-cmdexec-unbounded-pipe-probe'
const unbounded_pipe_bytes = 2 * 1024 * 1024
const unbounded_group_env = 'VTEST_CMDEXEC_UNBOUNDED_GROUP'

// A separate thread also bounds a writer blocked inside print/eprint. This
// keeps the regression finite even when a collector stops reading stderr and
// the platform cannot kill every member of the outer process group.
fn unbounded_writer_watchdog() {
	time.sleep(10 * time.second)
	// Normal exit may wait for a stdio lock held by the blocked writer.
	C._Exit(91)
}

fn testsuite_begin() {
	if os.args.len < 2 || os.args[1] != unbounded_pipe_probe {
		return
	}
	if os.args.len != 6 {
		exit(2)
	}
	mode := os.args[2]
	entry := os.args[3]
	code := os.args[4].int()
	work_folder := os.args[5]
	if mode == 'collect' {
		$if !windows {
			os.setenv(unbounded_group_env, C.getpgrp().str(), true)
		}
		args := [unbounded_pipe_probe, 'leader', entry, code.str(), work_folder]
		result := match entry {
			'run' { run(os.executable(), args) }
			'run_in' { run_in(os.executable(), args, work_folder) }
			'zero' { run_with_timeout(os.executable(), args, no_timeout) }
			'negative' { run_with_timeout(os.executable(), args, -1) }
			'merged' { run_in_merged(os.executable(), args, work_folder) }
			else { exit(2) }
		}
		if result.exit_code != code || result.output.len != 2 * unbounded_pipe_bytes
			|| result.output.count('e') != unbounded_pipe_bytes
			|| result.output.count('o') != unbounded_pipe_bytes {
			eprintln('${entry}: expected exit ${code} and two complete streams; got exit ${result.exit_code}, ${result.output.len} bytes')
			exit(1)
		}
		if entry == 'merged'
			&& result.output != 'e'.repeat(unbounded_pipe_bytes) + 'o'.repeat(unbounded_pipe_bytes) {
			eprintln('merged output changed the writer order')
			exit(1)
		}
		$if !windows {
			mut status := 0
			if C.waitpid(-1, &status, C.WNOHANG) != -1 {
				eprintln('the unbounded command left a live or unreaped direct child')
				exit(1)
			}
		}
		println('unbounded-pipe-probe-ok')
		exit(0)
	}
	if mode in ['leader', 'writer'] {
		if os.real_path(os.getwd()) != os.real_path(work_folder) {
			eprintln('unbounded command did not preserve its working directory')
			exit(92)
		}
		$if !windows {
			if C.getpgrp() != os.getenv(unbounded_group_env).int() {
				eprintln('an unbounded command must inherit its caller process group')
				exit(93)
			}
		}
	}
	if mode == 'leader' {
		mut writer := os.new_process(os.executable())
		writer.set_args([unbounded_pipe_probe, 'writer', entry, code.str(), work_folder])
		// Keep both pipes inherited, but let this direct child exit immediately.
		writer.run()
		exit(code)
	}
	if mode == 'writer' {
		spawn unbounded_writer_watchdog()
		// Let the leader exit before filling stderr. The old collector switches
		// to stdout_slurp at that point, while this writer still owns stdout.
		time.sleep(100 * time.millisecond)
		eprint('e'.repeat(unbounded_pipe_bytes))
		flush_stderr()
		print('o'.repeat(unbounded_pipe_bytes))
		flush_stdout()
		exit(0)
	}
	if mode in ['quiet', 'closed'] {
		if mode == 'closed' {
			os.fd_close(1)
			os.fd_close(2)
		}
		time.sleep(200 * time.millisecond)
		exit(code)
	}
	exit(2)
}

fn test_unbounded_runs_drain_both_pipes_after_the_leader_exits() {
	root := os.join_path(os.vtmp_dir(), 'cmdexec unbounded ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	for entry in ['run', 'run_in', 'zero', 'negative', 'merged'] {
		work_folder := if entry in ['run_in', 'merged'] { root } else { os.getwd() }
		for code in [0, 7] {
			// Only the outer collector is bounded. The command under test must
			// use the public unbounded entry point, not the already-fixed timeout.
			result := run_with_timeout(os.executable(), [unbounded_pipe_probe, 'collect', entry,
				code.str(), work_folder], 20_000)
			assert result.exit_code == 0, '${entry}, leader exit ${code}: ${result.output}'
			assert result.output.trim_space() == 'unbounded-pipe-probe-ok', result.output
		}
	}
}

fn test_unbounded_runs_wait_for_quiet_children_and_children_that_closed_their_pipes() {
	for mode in ['quiet', 'closed'] {
		sw := time.new_stopwatch()
		result := run(os.executable(), [unbounded_pipe_probe, mode, '', '23', ''])
		assert result.exit_code == 23, result.output
		assert result.output == ''
		assert sw.elapsed().milliseconds() >= 100, 'returned before the child exited'
	}
}

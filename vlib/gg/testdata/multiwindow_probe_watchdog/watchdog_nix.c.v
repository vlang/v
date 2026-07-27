module multiwindow_probe_watchdog

import os
import time
import gg.testdata.multiwindow_probe_gate

#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

fn C.getpgid(pid int) int
fn C.getpgrp() int
fn C.kill(pid int, signal int) int
fn C.setpgid(pid int, pgid int) int

fn platform_watchdog_run(config Config) !Result {
	mut process := os.new_process(config.executable)
	process.set_args(config.args)
	if config.work_folder != '' {
		process.set_work_folder(config.work_folder)
	}
	mut environment := os.environ()
	for name, value in config.environment {
		environment[name] = value
	}
	if os.exists(config.start_file) {
		return error('multi-window probe watchdog: start gate already exists `${config.start_file}`')
	}
	environment[multiwindow_probe_gate.environment_name] = config.start_file
	process.set_environment(environment)
	process.use_pgroup = true
	process.set_redirect_stdio()
	started := time.now()
	mut output := WatchdogCapturedOutput{}
	process.run()
	if process.pid <= 1 {
		process.close()
		return error(output.failure_message('multi-window probe watchdog: failed to start `${config.executable}` in a safe process group'))
	}
	defer {
		process.close()
	}

	pgid := verify_posix_process_group(process.pid) or {
		process.signal_kill()
		process.wait()
		capture_posix_final_output(mut process, mut output)
		return error(output.failure_message(err.msg()))
	}
	os.write_file(config.start_file, '${process.pid}\n') or {
		start_error := err.msg()
		terminate_posix_group(pgid, mut process, mut output) or {
			cleanup_error := err.msg()
			process.signal_kill()
			process.wait()
			capture_posix_final_output(mut process, mut output)
			return error(output.failure_message('${start_error}; ${cleanup_error}'))
		}
		process.wait()
		capture_posix_final_output(mut process, mut output)
		return error(output.failure_message(start_error))
	}

	deadline := started.add(config.timeout)
	mut timed_out := false
	for {
		drain_posix_output(mut process, mut output)
		if !process.is_alive() {
			break
		}
		if time.now() >= deadline {
			timed_out = true
			break
		}
		time.sleep(watchdog_poll_interval)
	}

	if !timed_out {
		// is_alive() reaps a naturally exited POSIX leader; wait() documents and
		// preserves that requirement if the process implementation changes.
		process.wait()
	}
	cleanup_deadline := time.now().add(watchdog_cleanup_timeout)
	mut passive_deadline := time.now().add(watchdog_term_grace)
	if passive_deadline > cleanup_deadline {
		passive_deadline = cleanup_deadline
	}
	mut forced_cleanup := false
	mut confinement_empty := false
	if timed_out {
		terminate_posix_group_until(pgid, cleanup_deadline, mut process, mut output) or {
			process.signal_pgkill()
			process.wait()
			capture_posix_final_output(mut process, mut output)
			return error(output.failure_message(err.msg()))
		}
		forced_cleanup = true
		confinement_empty = true
	} else {
		confinement_empty = wait_for_posix_group_empty_until(pgid, passive_deadline, mut process, mut
			output) or {
			failure := err.msg()
			terminate_posix_group_until(pgid, cleanup_deadline, mut process, mut output) or {
				process.signal_pgkill()
				process.wait()
				capture_posix_final_output(mut process, mut output)
				return error(output.failure_message('${failure}; ${err.msg()}'))
			}
			process.wait()
			capture_posix_final_output(mut process, mut output)
			return error(output.failure_message(failure))
		}
		if !confinement_empty {
			terminate_posix_group_until(pgid, cleanup_deadline, mut process, mut output) or {
				process.signal_pgkill()
				process.wait()
				capture_posix_final_output(mut process, mut output)
				return error(output.failure_message(err.msg()))
			}
			forced_cleanup = true
			confinement_empty = true
		}
	}
	process.wait()
	capture_posix_final_output(mut process, mut output)
	exit_code := process.code
	reaped := process.status !in [.not_started, .running, .stopped]
	pid := process.pid
	return Result{
		pid:               pid
		exit_code:         exit_code
		stdout:            output.stdout
		stderr:            output.stderr
		timed_out:         timed_out
		reaped:            reaped
		confinement_empty: confinement_empty
		forced_cleanup:    forced_cleanup
		elapsed:           time.since(started)
	}
}

fn verify_posix_process_group(pid int) !int {
	if pid <= 1 {
		return error('multi-window probe watchdog: unsafe leader pid ${pid}')
	}
	set_result := C.setpgid(pid, pid)
	set_error := C.errno
	pgid := C.getpgid(pid)
	if pgid != pid {
		return error('multi-window probe watchdog: leader ${pid} entered process group ${pgid}, expected ${pid}')
	}
	if set_result != 0 && set_error !in [C.EACCES, C.EPERM] {
		return error('multi-window probe watchdog: parent setpgid(${pid}, ${pid}) failed with errno ${set_error}')
	}
	parent_group := C.getpgrp()
	if pgid <= 1 || pgid == parent_group {
		return error('multi-window probe watchdog: unsafe process group ${pgid} for leader ${pid}, parent group ${parent_group}')
	}
	return pgid
}

fn terminate_posix_group(pgid int, mut process os.Process, mut output WatchdogCapturedOutput) ! {
	cleanup_deadline := time.now().add(watchdog_cleanup_timeout)
	terminate_posix_group_until(pgid, cleanup_deadline, mut process, mut output)!
}

fn terminate_posix_group_until(pgid int, cleanup_deadline time.Time, mut process os.Process, mut output WatchdogCapturedOutput) ! {
	if pgid <= 1 || pgid == C.getpgrp() {
		return error('multi-window probe watchdog: refused to terminate unsafe process group ${pgid}')
	}
	signal_posix_group(pgid, C.SIGTERM)!
	mut grace_deadline := time.now().add(watchdog_term_grace)
	if grace_deadline > cleanup_deadline {
		grace_deadline = cleanup_deadline
	}
	if wait_for_posix_group_empty_until(pgid, grace_deadline, mut process, mut output)! {
		return
	}
	if posix_group_exists(pgid)! {
		signal_posix_group(pgid, C.SIGKILL)!
	}
	if !wait_for_posix_group_empty_until(pgid, cleanup_deadline, mut process, mut output)! {
		return error('multi-window probe watchdog: process group ${pgid} was not proven empty after TERM/KILL')
	}
}

fn wait_for_posix_group_empty_until(pgid int, deadline time.Time, mut process os.Process, mut output WatchdogCapturedOutput) !bool {
	for {
		drain_posix_output(mut process, mut output)
		_ = process.is_alive()
		if !posix_group_exists(pgid)! {
			return true
		}
		if time.now() >= deadline {
			return false
		}
		time.sleep(watchdog_poll_interval)
	}
	return false
}

fn signal_posix_group(pgid int, signal int) ! {
	if C.kill(-pgid, signal) != 0 && C.errno != C.ESRCH {
		return error('multi-window probe watchdog: signal ${signal} to process group ${pgid} failed with errno ${C.errno}')
	}
}

fn posix_group_exists(pgid int) !bool {
	if C.kill(-pgid, 0) == 0 {
		return true
	}
	match C.errno {
		C.ESRCH {
			return false
		}
		C.EPERM {
			return true
		}
		else {
			return error('multi-window probe watchdog: process group ${pgid} probe failed with errno ${C.errno}')
		}
	}
}

fn drain_posix_output(mut process os.Process, mut output WatchdogCapturedOutput) {
	for _ in 0 .. watchdog_drain_chunks_per_poll {
		if !process.is_pending(.stdout) {
			break
		}
		chunk := process.stdout_read()
		if chunk == '' {
			break
		}
		output.stdout += chunk
	}
	for _ in 0 .. watchdog_drain_chunks_per_poll {
		if !process.is_pending(.stderr) {
			break
		}
		chunk := process.stderr_read()
		if chunk == '' {
			break
		}
		output.stderr += chunk
	}
}

fn capture_posix_final_output(mut process os.Process, mut output WatchdogCapturedOutput) {
	drain_posix_output(mut process, mut output)
	output.stdout += process.stdout_slurp()
	output.stderr += process.stderr_slurp()
}

fn platform_process_exists(pid int) bool {
	if C.kill(pid, 0) == 0 {
		return true
	}
	return C.errno == C.EPERM
}

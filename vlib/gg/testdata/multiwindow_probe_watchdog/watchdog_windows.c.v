module multiwindow_probe_watchdog

import os
import time
import gg.testdata.multiwindow_probe_gate

#insert "@VMODROOT/vlib/gg/testdata/multiwindow_probe_watchdog/watchdog_windows_helpers.h"

fn C.v_multiwindow_watchdog_spawn(application &u16, command_line &u16, work_directory &u16, environment &u16) voidptr
fn C.v_multiwindow_watchdog_pid(state voidptr) u32
fn C.v_multiwindow_watchdog_leader_alive(state voidptr) int
fn C.v_multiwindow_watchdog_wait_leader(state voidptr, timeout_ms u32, exit_code &u32) int
fn C.v_multiwindow_watchdog_release_leader(state voidptr) int
fn C.v_multiwindow_watchdog_read_stdout(state voidptr, buffer &u8, capacity u32) int
fn C.v_multiwindow_watchdog_read_stderr(state voidptr, buffer &u8, capacity u32) int
fn C.v_multiwindow_watchdog_terminate_job(state voidptr) int
fn C.v_multiwindow_watchdog_active_processes(state voidptr) int
fn C.v_multiwindow_watchdog_close(state voidptr)
fn C.v_multiwindow_watchdog_process_exists(pid u32) int
fn C.v_multiwindow_watchdog_last_error() u32

fn platform_watchdog_run(config Config) !Result {
	if os.exists(config.start_file) {
		return error('multi-window probe watchdog: start gate already exists `${config.start_file}`')
	}
	executable := os.abs_path(config.executable)
	application := executable.to_wide()
	command_line := windows_watchdog_command_line(executable, config.args).to_wide()
	mut work_directory := &u16(unsafe { nil })
	if config.work_folder != '' {
		work_directory = os.abs_path(config.work_folder).to_wide()
	}
	environment := windows_watchdog_environment(config)
	state := C.v_multiwindow_watchdog_spawn(application, command_line, work_directory,
		&environment[0])
	if state == unsafe { nil } {
		return error('multi-window probe watchdog: suspended Job process creation failed with Windows error ${C.v_multiwindow_watchdog_last_error()}')
	}
	defer {
		C.v_multiwindow_watchdog_close(state)
	}
	mut output := WatchdogCapturedOutput{}
	mut leader_released := false
	pid := int(C.v_multiwindow_watchdog_pid(state))
	if pid <= 0 {
		failure := 'multi-window probe watchdog: suspended Job process has no leader pid'
		return error(windows_failure_after_cleanup(state, failure, leader_released, mut output))
	}
	started := time.now()
	os.write_file(config.start_file, '${pid}\n') or {
		start_error := err.msg()
		return error(windows_failure_after_cleanup(state, start_error, leader_released, mut output))
	}

	deadline := started.add(config.timeout)
	mut timed_out := false
	for {
		drain_windows_output(state, mut output) or {
			return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
				output))
		}
		alive := C.v_multiwindow_watchdog_leader_alive(state)
		if alive < 0 {
			failure := 'multi-window probe watchdog: leader state query failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
			return error(windows_failure_after_cleanup(state, failure, leader_released, mut output))
		}
		if alive == 0 {
			break
		}
		if time.now() >= deadline {
			timed_out = true
			break
		}
		time.sleep(watchdog_poll_interval)
	}

	cleanup_deadline := time.now().add(watchdog_cleanup_timeout)
	mut forced_cleanup := false
	if timed_out {
		terminate_windows_job(state) or {
			return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
				output))
		}
		forced_cleanup = true
	}
	mut raw_exit_code := u32(0)
	if C.v_multiwindow_watchdog_wait_leader(state, 2000, &raw_exit_code) == 0 {
		failure := C.v_multiwindow_watchdog_last_error()
		message := 'multi-window probe watchdog: leader wait failed with Windows error ${failure}'
		return error(windows_failure_after_cleanup(state, message, leader_released, mut output))
	}
	release_result := C.v_multiwindow_watchdog_release_leader(state)
	leader_released = true
	if release_result == 0 {
		failure := 'multi-window probe watchdog: leader process handle close failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
		return error(windows_failure_after_cleanup(state, failure, leader_released, mut output))
	}
	drain_windows_output(state, mut output) or {
		return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut output))
	}
	mut confinement_empty := false
	if timed_out {
		confinement_empty = wait_for_windows_job_empty_until(state, cleanup_deadline, mut output) or {
			return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
				output))
		}
	} else {
		mut passive_deadline := time.now().add(watchdog_term_grace)
		if passive_deadline > cleanup_deadline {
			passive_deadline = cleanup_deadline
		}
		confinement_empty = wait_for_windows_job_empty_until(state, passive_deadline, mut output) or {
			return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
				output))
		}
		if !confinement_empty {
			terminate_windows_job(state) or {
				return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
					output))
			}
			forced_cleanup = true
			confinement_empty = wait_for_windows_job_empty_until(state, cleanup_deadline, mut
				output) or {
				return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut
					output))
			}
		}
	}
	if !confinement_empty {
		failure := 'multi-window probe watchdog: Job Object was not proven empty after bounded cleanup'
		return error(windows_failure_after_cleanup(state, failure, leader_released, mut output))
	}
	capture_windows_final_output(state, mut output) or {
		return error(windows_failure_after_cleanup(state, err.msg(), leader_released, mut output))
	}
	return Result{
		pid:               pid
		exit_code:         int(raw_exit_code)
		stdout:            output.stdout
		stderr:            output.stderr
		timed_out:         timed_out
		reaped:            true
		confinement_empty: confinement_empty
		forced_cleanup:    forced_cleanup
		elapsed:           time.since(started)
	}
}

fn windows_failure_after_cleanup(state voidptr, failure string, leader_was_released bool, mut output WatchdogCapturedOutput) string {
	mut errors := [failure]
	terminate_windows_job(state) or { errors << err.msg() }
	if !leader_was_released {
		mut ignored_exit := u32(0)
		if C.v_multiwindow_watchdog_wait_leader(state, 2000, &ignored_exit) == 0 {
			errors << 'multi-window probe watchdog: bounded failure wait failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
		}
		if C.v_multiwindow_watchdog_release_leader(state) == 0 {
			errors << 'multi-window probe watchdog: failure-path leader handle close failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
		}
	}
	wait_for_windows_job_empty(state, mut output) or { errors << err.msg() }
	capture_windows_final_output(state, mut output) or { errors << err.msg() }
	return output.failure_message(errors.join('; '))
}

fn terminate_windows_job(state voidptr) ! {
	if C.v_multiwindow_watchdog_terminate_job(state) == 0 {
		return error('multi-window probe watchdog: TerminateJobObject failed with Windows error ${C.v_multiwindow_watchdog_last_error()}')
	}
}

fn wait_for_windows_job_empty(state voidptr, mut output WatchdogCapturedOutput) !bool {
	deadline := time.now().add(watchdog_cleanup_timeout)
	if wait_for_windows_job_empty_until(state, deadline, mut output)! {
		return true
	}
	return error('multi-window probe watchdog: Job Object was not proven empty after bounded cleanup')
}

fn wait_for_windows_job_empty_until(state voidptr, deadline time.Time, mut output WatchdogCapturedOutput) !bool {
	for {
		drain_windows_output(state, mut output)!
		if windows_job_active_processes(state)! == 0 {
			return true
		}
		if time.now() >= deadline {
			return false
		}
		time.sleep(watchdog_poll_interval)
	}
	return false
}

fn windows_job_active_processes(state voidptr) !int {
	active := C.v_multiwindow_watchdog_active_processes(state)
	if active < 0 {
		return error('multi-window probe watchdog: Job Object accounting failed with Windows error ${C.v_multiwindow_watchdog_last_error()}')
	}
	return active
}

fn drain_windows_output(state voidptr, mut output WatchdogCapturedOutput) !bool {
	mut errors := []string{}
	mut drained := false
	for _ in 0 .. watchdog_drain_chunks_per_poll {
		mut buffer := [4096]u8{}
		count := C.v_multiwindow_watchdog_read_stdout(state, &buffer[0], u32(buffer.len))
		if count < 0 {
			errors << 'multi-window probe watchdog: stdout drain failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
			break
		}
		if count == 0 {
			break
		}
		drained = true
		output.stdout += unsafe { tos(&buffer[0], count).clone() }
	}
	for _ in 0 .. watchdog_drain_chunks_per_poll {
		mut buffer := [4096]u8{}
		count := C.v_multiwindow_watchdog_read_stderr(state, &buffer[0], u32(buffer.len))
		if count < 0 {
			errors << 'multi-window probe watchdog: stderr drain failed with Windows error ${C.v_multiwindow_watchdog_last_error()}'
			break
		}
		if count == 0 {
			break
		}
		drained = true
		output.stderr += unsafe { tos(&buffer[0], count).clone() }
	}
	if errors.len > 0 {
		return error(errors.join('; '))
	}
	return drained
}

fn capture_windows_final_output(state voidptr, mut output WatchdogCapturedOutput) ! {
	for drain_windows_output(state, mut output)! {
	}
}

fn windows_watchdog_environment(config Config) []u16 {
	mut environment := os.environ()
	for name, value in config.environment {
		environment[name] = value
	}
	environment[multiwindow_probe_gate.environment_name] = config.start_file
	mut names := environment.keys()
	names.sort()
	mut block := []u16{cap: names.len * 32}
	for name in names {
		entry := '${name}=${environment[name]}'
		wide := entry.to_wide()
		mut index := 0
		for {
			character := unsafe { wide[index] }
			if character == 0 {
				break
			}
			block << character
			index++
		}
		block << u16(0)
	}
	block << u16(0)
	return block
}

fn windows_watchdog_command_line(executable string, args []string) string {
	mut values := []string{cap: args.len + 1}
	values << windows_watchdog_quote(executable)
	for arg in args {
		values << windows_watchdog_quote(arg)
	}
	return values.join(' ')
}

fn windows_watchdog_quote(value string) string {
	mut quote := value == ''
	for index in 0 .. value.len {
		character := value[index]
		if character == ` ` || character == `\t` || character == `\n` || character == u8(11)
			|| character == `"` {
			quote = true
			break
		}
	}
	if !quote {
		return value
	}
	mut result := '"'
	mut backslashes := 0
	for index in 0 .. value.len {
		character := value[index]
		if character == `\\` {
			backslashes++
			continue
		}
		if character == `"` {
			result += '\\'.repeat(backslashes * 2 + 1)
			result += '"'
			backslashes = 0
			continue
		}
		result += '\\'.repeat(backslashes)
		backslashes = 0
		result += value[index..index + 1]
	}
	result += '\\'.repeat(backslashes * 2)
	result += '"'
	return result
}

fn platform_process_exists(pid int) bool {
	return C.v_multiwindow_watchdog_process_exists(u32(pid)) != 0
}

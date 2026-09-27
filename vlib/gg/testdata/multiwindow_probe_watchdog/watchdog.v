module multiwindow_probe_watchdog

import time

const watchdog_poll_interval = 5 * time.millisecond
const watchdog_term_grace = 250 * time.millisecond
const watchdog_cleanup_timeout = 2 * time.second
const watchdog_drain_chunks_per_poll = 64

@[params]
pub struct Config {
pub:
	executable  string
	args        []string
	work_folder string
	environment map[string]string
	timeout     time.Duration
	start_file  string
}

pub struct Result {
pub:
	pid               int
	exit_code         int
	stdout            string
	stderr            string
	timed_out         bool
	reaped            bool
	confinement_empty bool
	forced_cleanup    bool
	elapsed           time.Duration
}

struct WatchdogCapturedOutput {
mut:
	stdout string
	stderr string
}

fn (output WatchdogCapturedOutput) failure_message(failure string) string {
	mut sections := [failure]
	if output.stdout != '' {
		sections << 'watchdog stdout:\n${output.stdout}'
	}
	if output.stderr != '' {
		sections << 'watchdog stderr:\n${output.stderr}'
	}
	return sections.join('\n')
}

pub fn (result Result) combined_output() string {
	if result.stderr == '' {
		return result.stdout
	}
	if result.stdout == '' {
		return result.stderr
	}
	return '${result.stdout}\n${result.stderr}'
}

pub fn run(config Config) !Result {
	if config.executable == '' {
		return error('multi-window probe watchdog: executable is required')
	}
	if config.timeout <= 0 {
		return error('multi-window probe watchdog: timeout must be positive')
	}
	if config.start_file == '' {
		return error('multi-window probe watchdog: a bounded parent start gate is required')
	}
	return platform_watchdog_run(config)
}

pub fn process_exists(pid int) bool {
	if pid <= 0 {
		return false
	}
	return platform_process_exists(pid)
}

pub fn wait_until_process_gone(pid int, timeout time.Duration) bool {
	deadline := time.now().add(timeout)
	for process_exists(pid) {
		if time.now() >= deadline {
			return false
		}
		time.sleep(watchdog_poll_interval)
	}
	return true
}

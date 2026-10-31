module main

import os
import time

const log_max_lines = 2000
const log_output_tail_lines = 40

// Log keeps the lines of one run for the status page, and echoes them to stdout (the journal).
struct Log {
mut:
	lines []string
	echo  bool = true
}

fn (mut l Log) note(line string) {
	stamped := '${time.now().format_ss()} ${line}'
	if l.echo {
		println(stamped)
		flush_stdout()
	}
	l.lines << stamped
	if l.lines.len > log_max_lines {
		l.lines.delete_many(0, l.lines.len - log_max_lines)
	}
}

// run_status runs `args`, logs the command and the tail of its output, and returns the result.
// Nothing secret may appear in `args`: the push token reaches git only through the environment.
fn (mut l Log) run_status(args []string) os.Result {
	l.note('\$ ${args.join(' ')}')
	result := os.exec(args)
	output := result.output.trim_right('\n')
	if output != '' {
		lines := output.split_into_lines()
		if lines.len > log_output_tail_lines {
			l.note('  ... ${lines.len - log_output_tail_lines} earlier lines')
		}
		for line in lines#[-log_output_tail_lines..] {
			l.note('  ${line}')
		}
	}
	if result.exit_code != 0 {
		l.note('  exit code ${result.exit_code}')
	}
	return result
}

fn (mut l Log) run(args []string, failure string) !string {
	result := l.run_status(args)
	if result.exit_code != 0 {
		return error('${failure} (exit code ${result.exit_code})')
	}
	return result.output
}

// run_limited is run with a time limit, where coreutils' `timeout` exists.
fn (mut l Log) run_limited(args []string, minutes int, failure string) !string {
	if minutes > 0 {
		if timeout := os.find_abs_path_of_executable('timeout') {
			// Plain seconds: busybox's timeout does not take unit suffixes.
			mut limited := [timeout, '${minutes * 60}']
			limited << args
			return l.run(limited, failure)
		}
	}
	return l.run(args, failure)
}

module main

import os
import time
import os.cmdline

enum Target {
	both
	stderr
	stdout
	alternate
}

fn s2target(s string) Target {
	return match s {
		'both' { Target.both }
		'stderr' { Target.stderr }
		'alternate' { Target.alternate }
		else { Target.stdout }
	}
}

struct Context {
mut:
	timeout_ms int
	period_ms  int
	exitcode   int
	target     Target
	omode      Target
	is_verbose bool
	show_wd    bool
	show_env   bool
}

fn (mut ctx Context) println(s string) {
	if ctx.target == .alternate {
		ctx.omode = if ctx.omode == .stderr { Target.stdout } else { Target.stderr }
	}
	if ctx.target in [.both, .stdout] || ctx.omode == .stdout {
		println('stdout, ${s}')
	}
	if ctx.target in [.both, .stderr] || ctx.omode == .stderr {
		eprintln('stderr, ${s}')
	}
}

fn do_timeout(c &Context) {
	mut ctx := unsafe { c }
	time.sleep(ctx.timeout_ms * time.millisecond)
	exit(ctx.exitcode)
}

fn main() {
	unbuffer_stdout()
	mut ctx := Context{}
	args := os.args[1..]
	if '-h' in args || '--help' in args {
		println("Usage:
	test_os_process [-v] [-h] [-target stderr/stdout/both/alternate] [-show_wd] [-show_env] [-exitcode 0] [-timeout_ms 200] [-period_ms 50]
		Prints lines periodically (-period_ms), to stdout/stderr (-target).
		After a while (-timeout_ms), exit with (-exitcode).
		This program is useful for platform independent testing
		of child process/standard input/output control.
		It is used in V's `os` module tests.
")
		return
	}
	ctx.is_verbose = '-v' in args
	ctx.show_wd = '-show_wd' in args
	ctx.show_env = '-show_env' in args
	ctx.target = s2target(cmdline.option(args, '-target', 'both'))
	ctx.exitcode = cmdline.option(args, '-exitcode', '0').int()
	ctx.timeout_ms = cmdline.option(args, '-timeout_ms', '200').int()
	ctx.period_ms = cmdline.option(args, '-period_ms', '50').int()
	if ctx.target == .alternate {
		ctx.omode = .stdout
	}
	if ctx.is_verbose {
		eprintln('> args: ${args} | context: ${ctx}')
	}
	if ctx.show_wd {
		ctx.println('WORK_DIR=${os.getwd()}')
	}
	if ctx.show_env {
		all := os.environ()
		for k, v in all {
			ctx.println('${k}=${v}')
		}
	}
	spawn do_timeout(&ctx)
	ctx.println('start')
	for i := 1; true; i++ {
		ctx.println('${i}')
		time.sleep(ctx.period_ms * time.millisecond)
	}
	ctx.println('done')
	time.sleep(100 * time.second)
}

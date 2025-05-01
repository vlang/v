import os
import time
import flag
import strconv

struct Context {
mut:
	timeout  f64
	cmd_args []string
}

fn main() {
	mut fp := flag.new_flag_parser(os.args[1..])
	fp.application('v timeout')
	fp.version('0.0.2')
	fp.description('Run a command with a time limit. Example: `v timeout 0.3 v run examples/hello_world.v`')
	fp.arguments_description('timeout_in_seconds CMD [ARGS]')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(2)!

	if fp.bool('help', `h`, false, 'Show this help screen.') {
		println(fp.usage())
		exit(0)
	}

	args := fp.finalize() or {
		eprintln('Argument error: ${err}')
		exit(125) // mimic the exit codes of `timeout` in coreutils
	}

	ctx := Context{
		timeout:  strconv.atof64(args[0]) or {
			eprintln('Invalid timeout: ${args[0]}')
			exit(125)
		}
		cmd_args: args[1..].clone()
	}

	mut cmd := ctx.cmd_args[0]
	if !os.exists(cmd) {
		cmd = os.find_abs_path_of_executable(cmd) or { cmd }
	}

	mut p := os.new_process(cmd)
	p.set_args(ctx.cmd_args[1..])
	p.run()
	if p.err != '' {
		eprintln('Cannot execute: ${ctx.cmd_args.join(' ')}')
		exit(if os.exists(ctx.cmd_args[0]) { 126 } else { 127 })
	}

	child_exit := chan int{}

	spawn fn (mut p os.Process, ch chan int) {
		p.wait()
		ch <- p.code
		ch.close()
	}(mut p, child_exit)

	mut exit_code := 0
	select {
		i64(ctx.timeout * time.second) {
			p.signal_term()
			time.sleep(2 * time.millisecond)
			if p.is_alive() {
				p.signal_kill()
			}
			p.wait()
			exit_code = 124 // timeout
		}
		code := <-child_exit {
			exit_code = code
		}
	}
	exit(exit_code)
}

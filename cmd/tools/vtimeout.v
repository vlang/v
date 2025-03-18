import os
import time
import flag

struct Context {
mut:
	show_help     bool
	cmd_line_opts []string
	full_cmd      string
	timeout       f64
}

fn main() {
	mut ctx := Context{}
	args := arguments()
	mut fp := flag.new_flag_parser(args#[1..])
	fp.application('v timeout')
	fp.version('0.0.1')
	fp.description('Run a command with a time limit. Example: `v timeout 0.3 v run examples/hello_world.v`')
	fp.arguments_description('timeout_in_seconds CMD [ARGS]')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(2)!
	ctx.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	if ctx.show_help {
		println(fp.usage())
		exit(0)
	}
	ctx.cmd_line_opts = fp.finalize() or {
		eprintln('> error: ${err}')
		exit(125) // mimic the exit codes of `timeout` in coreutils
	}
	ctx.timeout = ctx.cmd_line_opts[0].f64()
	ctx.cmd_line_opts = ctx.cmd_line_opts#[1..]
	ctx.full_cmd = ctx.cmd_line_opts.join(' ')
	spawn fn (ctx Context) {
		tperiod := time.Duration(i64(ctx.timeout * time.second))
		time.sleep(tperiod)
		// eprintln('> error: timeout of ${tperiod.seconds():5.3f}s reached, before command finished; command was: `${ctx.full_cmd}`')
		exit(124)
	}(ctx)
	ecode := os.system(ctx.full_cmd)
	exit(ecode)
}

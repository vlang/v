import os
import time
import flag

struct Context {
mut:
	show_help bool
	timeout   time.Duration
	delay     time.Duration
	retries   int
}

fn main() {
	mut context := Context{}
	args := os.args#[1..]
	// dump(args)
	mut fp := flag.new_flag_parser(args)
	fp.application('v retry')
	fp.version('0.0.1')
	fp.description('Run the command CMD in a loop, until it succeeds, or until a predetermined amount of seconds pass.')
	fp.arguments_description('CMD')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(1)!
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.timeout = fp.float('timeout', `t`, 600.0, 'Timeout in seconds (for all retries). Default: 600.0 seconds (10 minutes).') * time.second
	context.delay = fp.float('delay', `d`, 1.0, 'Delay between each retry in seconds. Default: 1.0 second.') * time.second
	context.retries = fp.int('retries', `r`, 10, 'Maximum number of retries. Default: 10.')
	if context.show_help {
		println(fp.usage())
		exit(0)
	}
	command_args := fp.finalize() or {
		eprintln('error: ${err}')
		exit(1)
	}
	cmd := command_args.join(' ')
	// dump(cmd)

	spawn fn (context Context) {
		time.sleep(context.timeout)
		eprintln('error: exceeded maximum timeout (${context.timeout.seconds()}s)!')
		exit(1)
	}(context)

	mut res := 0
	for i in 0 .. context.retries {
		res = os.system(cmd)
		if res == 0 {
			break
		}
		if i == context.retries - 1 {
			eprintln('error: exceeded maximum number of retries (${context.retries})!')
			exit(res)
		}
		time.sleep(context.delay)
	}
	exit(res)
}

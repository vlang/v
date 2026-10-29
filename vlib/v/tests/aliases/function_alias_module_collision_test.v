import jobfn_alias_no_args
import jobfn_alias_with_args as argjobs

type JobFn = fn (string) string

struct LocalJob {
	f JobFn @[required]
}

fn test_same_named_function_aliases_in_module_fields() ! {
	job := jobfn_alias_no_args.new(fn () ! {})
	job.run()!
	arg_job := argjobs.new(fn (value int) !int {
		return value + 1
	})
	assert arg_job.run(41)! == 42
	direct_job := argjobs.Job{
		f: fn (value int) !int {
			return value * 2
		}
	}
	assert direct_job.run(21)! == 42
	local_job := LocalJob{
		f: fn (value string) string {
			return value + '!'
		}
	}
	assert local_job.f('ok') == 'ok!'
}

fn test_same_named_function_aliases_preserve_errors() {
	job := jobfn_alias_no_args.new(fn () ! {
		return error('no arguments')
	})
	job.run() or {
		assert err.msg() == 'no arguments'
		return
	}
	assert false, 'callback error was lost'
}

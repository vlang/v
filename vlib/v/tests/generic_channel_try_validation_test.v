import os

struct ChannelTryCase {
	body    string
	typ     string = 'int'
	message string
}

fn test_generic_channel_try_methods_validate_specialized_arguments() {
	directory := os.join_path(os.vtmp_dir(), 'generic_channel_try_${os.getpid()}')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(directory) or {} }
	source := os.join_path(directory, 'main.v')
	for case in [
		ChannelTryCase{
			body:    'mut value := i64(0)\nch.try_pop(mut value)'
			message: 'cannot use `i64`'
		},
		ChannelTryCase{
			body:    'value := i64(0)\nch.try_push(value)'
			message: 'cannot use `i64`'
		},
		ChannelTryCase{
			body:    'ch.try_push(1.5)'
			typ:     'f64'
			message: 'cannot use `f64`'
		},
		ChannelTryCase{
			body:    'ch.try_pop()'
			message: 'argument count mismatch'
		},
		ChannelTryCase{
			body:    'mut value := T{}\nch.try_pop(value)'
			message: 'use a `mut` argument'
		},
		ChannelTryCase{
			body:    'ch.no_such_method()'
			message: 'unknown function'
		},
	] {
		os.write_file(source, 'fn invalid[T](ch chan T) {\n${case.body}\n}\nfn main() { invalid[${case.typ}](chan ${case.typ}{cap: 1}) }\n')!
		result := os.exec([@VEXE, '-new-compiler', '-o', os.join_path(directory, 'out.c'), source])
		assert result.exit_code != 0, case.body
		assert result.output.contains(case.message), result.output
	}
}

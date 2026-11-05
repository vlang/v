module main

fn test_full_compiler_dispatch_respects_argument_boundaries() {
	for args in [
		['-compile-backend', 'arm64', 'cmd/v'],
		['-all-backends', 'cmd/v'],
		['-b', 'arm64', '-b', 'c', 'main.v'],
		['-cflags', '-b=arm64', 'main.v'],
		['run', 'main.v', '-b', 'arm64'],
		['script.vsh', '-b', 'arm64'],
		['-e', 'println(1)', '-b', 'arm64'],
	] {
		assert !full_compiler_is_required(compiler_args_before_program(args)), args.str()
	}
	$if skip_arm64 ? {
		for args in [
			['-b', 'arm64', 'main.v'],
			['-backend=arm64', 'main.v'],
			['main.v', '-b', 'arm64'],
			['-b', 'c', '-backend', 'arm64', 'run', 'main.v'],
		] {
			assert full_compiler_is_required(compiler_args_before_program(args)), args.str()
		}
	}
	$if skip_x64 ? {
		for name in ['x64', 'amd64', 'x86_64', 'x86-64'] {
			assert full_compiler_is_required(['-b', name, 'main.v']), name
		}
	}
}

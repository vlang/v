// vtest build: !windows

import os
import strings
import v3.cmdexec

fn test_selfhost_parallel_workers_grow_for_struct_defaults() {
	root := os.join_path(os.temp_dir(), 'v3_selfhost_parallel_expansion_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_dir := os.dir(os.dir(@FILE))
	compiler := os.join_path(root, 'compiler')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-prealloc', '-path',
		'${os.dir(v3_dir)}|@vlib|@vmodules', '-o', compiler, os.join_path(v3_dir, 'v3.v')])
	assert build.exit_code == 0, build.output
	mut source := strings.new_builder(64_000)
	source.writeln('struct Entry {\nmut:\nvalue int = 7\n}')
	source.writeln('struct State {\nmut:\nvalue int\nvalues [300]Entry\n}')
	for i in 0 .. 300 {
		source.writeln('fn helper_${i}() int {')
		source.writeln('mut state := State{value: ${i}}')
		source.writeln('state.values[299].value = state.value + 1')
		source.writeln('return state.values[299].value + state.values[0].value')
		source.writeln('}')
	}
	source.writeln('fn main() { mut total := 0')
	for i in 0 .. 300 {
		source.writeln('total += helper_${i}()')
	}
	source.writeln('println(total) }')
	input := os.join_path(root, 'main.v')
	os.write_file(input, source.str()) or { panic(err) }
	old_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if jobs := old_jobs {
			os.setenv('VJOBS', jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	for parallel in [true, false] {
		output := os.join_path(root, if parallel { 'parallel' } else { 'serial' })
		mut args := ['-nocache', '-building-v', '-o', output]
		if !parallel {
			args << '-no-parallel'
		}
		args << input
		compile := cmdexec.run(compiler, args)
		assert compile.exit_code == 0, compile.output
		if parallel {
			assert compile.output.contains('transform (parallel)'), compile.output
		}
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '47250', run.output
	}
}

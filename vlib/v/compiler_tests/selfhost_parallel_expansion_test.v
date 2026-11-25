// vtest build: !windows

import os
import strings
import v.cmdexec
import v.tempname

fn test_selfhost_parallel_workers_grow_for_struct_defaults() {
	root := os.join_path(os.vtmp_dir(), 'v3_selfhost_parallel_expansion_${tempname.unique_token()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
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
	for parallel in [true, false] {
		output := os.join_path(root, if parallel { 'parallel' } else { 'serial' })
		// The stage table is printed only with `-v` or `-show-timings` (2a45ec1b41).
		mut args := ['-v', '-nocache', '-building-v', '-show-timings', '-o', output]
		if !parallel {
			args << '-no-parallel'
		}
		args << input
		compile := cmdexec.run_in_merged_with_env(@VEXE, args, '', {
			'VFLAGS': ''
			'VJOBS':  '4'
		})
		assert compile.exit_code == 0, compile.output
		if parallel {
			assert compile.output.contains('transform (parallel)'), compile.output
		}
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '47250', run.output
	}
}

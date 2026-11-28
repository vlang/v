module amd64

import os

fn test_debug_literal_program_keeps_native_backtrace_runtime() {
	$if linux && amd64 {
		output := os.join_path(os.vtmp_dir(), 'amd64_debug_literal_${os.getpid()}')
		defer { os.rm(output) or {} }
		source := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
		for flags in [[]string{}, ['-prod']] {
			mut command := [@VEXE, '-g', '-b', 'amd64', '-o', output]
			command << flags
			command << source
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([output])
			assert result.exit_code == 0, result.output
			assert result.output == 'Hello, World!\n'
			symbols := os.exec(['nm', output])
			assert symbols.exit_code == 0, symbols.output
			assert symbols.output.contains('print_backtrace')
		}
	}
}

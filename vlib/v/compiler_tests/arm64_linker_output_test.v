module main

import os
import v.cmdexec

fn test_arm64_linker_is_silent_by_default() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_linker_output_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'hello.v')
		output := os.join_path(root, 'hello')
		os.write_file(source, "fn main() { println('hi') }\n")!
		result := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert result.exit_code == 0, result.output
		assert result.output == '', result.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'hi', run.output
	}
}

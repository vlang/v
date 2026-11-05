module main

import os
import v.cmdexec

fn test_arm64_reports_unresolved_c_constants() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_c_constants_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		for name in ['THIS_MACRO_DOES_NOT_EXIST', 'HEADER_DEFINED_MACRO'] {
			for code in [
				'fn main() { println(int(C.${name})) }',
				'const number = int(C.${name})\nfn main() { println(number) }',
			] {
				os.write_file(source, code)!
				result := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
				assert result.exit_code == 1, result.output
				assert result.output.count('cannot resolve C constant `C.${name}`') == 1, result.output
				assert !os.exists(output)
			}
		}
		os.write_file(source, 'fn unused() { println(int(C.UNUSED_MACRO)) }
fn C.getpid() int
fn main() {
    println([int(C.F_SETFD), int(C.FD_CLOEXEC), int(C.SIGKILL), int(C.SIGTERM)])
    assert C.getpid() > 0
}
')!
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '[2, 1, 9, 15]', run.output
	}
}

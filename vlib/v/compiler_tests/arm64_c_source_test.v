module main

import os
import v.cmdexec

fn test_arm64_rejects_included_c_source_before_writing_an_executable() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_c_source_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_source' }\n")!
		os.write_file(os.join_path(root, 'native.c'), 'int add2(int a, int b) { return a + b; }\n')!
		os.write_file(source, '#include "@VMODROOT/native.c"
fn C.add2(a int, b int) int
fn main() { println(C.add2(2, 3)) }
')!
		result := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert result.exit_code == 1, result.output
		assert result.output.count('does not support including native source') == 1, result.output
		assert result.output.contains('@VMODROOT/native.c'), result.output
		assert result.output.contains('use the C backend'), result.output
		assert !os.exists(output)
	}
}

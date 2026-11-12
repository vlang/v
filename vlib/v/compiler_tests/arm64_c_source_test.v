module main

import os
import v.cmdexec

fn test_arm64_links_included_c_source_with_the_system_linker() {
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
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '5', run.output
		os.rm(output)!

		// The built-in linker still refuses native sources before writing anything.
		internal := cmdexec.run(@VEXE, ['-b', 'arm64', '-d', 'arm64_internal_link', '-o', output,
			source])
		assert internal.exit_code == 1, internal.output
		assert internal.output.count('cannot link native source') == 1, internal.output
		assert internal.output.contains('@VMODROOT/native.c'), internal.output
		assert internal.output.contains('arm64_internal_link'), internal.output
		assert !os.exists(output)
	}
}

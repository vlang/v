import os

struct ExitCodeCase {
	argument string
	expected int
}

fn test_windows_child_exit_codes() {
	dir := os.join_path(os.vtmp_dir(), 'windows_exit_codes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'child.v')
	child := os.join_path(dir, 'child.exe')
	os.write_file(source, 'import os\nfn main() { os.exit(os.args[1].int()) }\n')!
	compiled := os.exec([@VEXE, '-o', child, source])
	assert compiled.exit_code == 0, compiled.output
	for case in [ExitCodeCase{'0', 0}, ExitCodeCase{'1', 1}, ExitCodeCase{'255', 255},
		ExitCodeCase{'256', 256}, ExitCodeCase{'65535', 65535}, ExitCodeCase{'2147483647', 2147483647},
		ExitCodeCase{'2147483648', -2147483648}, ExitCodeCase{'4294967295', -1}] {
		direct := os.exec([child, case.argument])
		assert direct.exit_code == case.expected, '${case.argument}: ${direct}'
		shell := os.execute('${os.quoted_path(child)} ${case.argument}')
		assert shell.exit_code == case.expected, '${case.argument}: ${shell}'
		mut process := os.new_process(child)
		process.set_args([case.argument])
		process.wait()
		assert process.status == .exited
		assert process.code == case.expected, '${case.argument}: ${process.code}'
		process.close()
	}
}

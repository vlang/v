module c

import os

fn test_no_main_object_keeps_void_returns() {
	root := os.join_path(os.vtmp_dir(), 'no_main_returns_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main
import os
fn main() {
 if os.args.len > 0 { return }
 println("unreachable")
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		object := os.join_path(root, 'main.o')
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -gc none -d no_main -o ${os.quoted_path(object)} ${os.quoted_path(source)}')
		assert result.exit_code == 0, result.output
		assert os.exists(object)
	}
	executable := os.join_path(root, 'program')
	result := os.execute('${os.quoted_path(@VEXE)} -gc none -o ${os.quoted_path(executable)} ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
	run := os.execute(os.quoted_path(executable))
	assert run.exit_code == 0, run.output
	assert run.output == ''
}

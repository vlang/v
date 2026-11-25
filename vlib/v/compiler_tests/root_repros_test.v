import os
import v.cmdexec
import v.tempname

const root_repros_v3_dir = os.dir(os.dir(@FILE))

fn test_root_has_no_temporary_v_programs_and_repros_compile() {
	entries := os.ls(root_repros_v3_dir) or { panic(err) }
	assert !entries.any(it.starts_with('_tmp') && it.ends_with('.v'))

	root := os.join_path(os.vtmp_dir(), 'v3_root_repros_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	for name in ['inner_loop_pointer', 'embedded_child'] {
		source := os.join_path(os.dir(@FILE), 'repros', name, 'main.v')
		output := os.join_path(root, name)
		compile := cmdexec.run(@VEXE, [source, '-o', output])
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run(output, [])
		assert run.exit_code == 0, run.output
	}
}

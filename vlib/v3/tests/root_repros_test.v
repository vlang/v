import os
import v3.cmdexec

const root_repros_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const root_repros_v3_dir = os.dir(os.dir(@FILE))
const root_repros_v3_src = os.join_path(root_repros_v3_dir, 'v3.v')

fn test_root_has_no_temporary_v_programs_and_repros_compile() {
	entries := os.ls(root_repros_v3_dir) or { panic(err) }
	assert !entries.any(it.starts_with('_tmp') && it.ends_with('.v'))

	root := os.join_path(os.vtmp_dir(), 'v3_root_repros_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${root_repros_vlib_dir}|@vlib|@vmodules',
		'-o', v3_bin, root_repros_v3_src])
	assert build.exit_code == 0, build.output

	for name in ['inner_loop_pointer', 'embedded_child'] {
		source := os.join_path(root_repros_v3_dir, 'tests', 'repros', name, 'main.v')
		output := os.join_path(root, name)
		compile := cmdexec.run(v3_bin, [source, '-o', output])
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run(output, [])
		assert run.exit_code == 0, run.output
	}
}

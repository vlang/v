import os
import v.cmdexec

fn test_build_tools_keeps_manifests_beside_each_packaged_executable() ! {
	root := os.join_path(os.vtmp_dir(), 'v build tools manifests ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	suffix := $if windows { '.exe' } $else { '' }
	tool := os.join_path(root, 'vbuild-tools' + suffix)
	built := cmdexec.run(@VEXE, ['-o', tool, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vbuild-tools.v')])
	assert built.exit_code == 0, built.output
	// A small compiler substitute makes packaging produce sidecars on every host,
	// independently of which Windows linker is installed.
	fake_source := os.join_path(root, 'compiler.v')
	os.write_file(fake_source, "module main
import os
fn main() {
	mut output := ''
	for i, arg in os.args {
		if arg == '-o' && i + 1 < os.args.len { output = os.args[i + 1] }
	}
	if output == '' { return }
	os.cp(os.executable(), output) or { panic(err) }
	os.chmod(output, 0o755) or { panic(err) }
	os.write_file(output + '.manifest', os.file_name(output)) or { panic(err) }
}
")!
	fake := os.join_path(root, 'v' + suffix)
	compiler := cmdexec.run(@VEXE, ['-o', fake, fake_source])
	assert compiler.exit_code == 0, compiler.output
	tools := os.join_path(root, 'cmd', 'tools')
	os.mkdir_all(os.join_path(root, 'vlib'))!
	os.mkdir_all(os.join_path(tools, 'vwhere'))!
	os.mkdir_all(os.join_path(tools, 'builders'))!
	os.mkdir_all(os.join_path(tools, 'testing'))!
	os.cp(os.join_path(@VEXEROOT, 'cmd', 'tools', 'testing', 'output_normal.v'),
		os.join_path(tools, 'testing', 'output_normal.v'))!
	for source in ['vplain.v', 'vsample_builder.v', os.join_path('vwhere', 'main.v')] {
		os.write_file(os.join_path(tools, source), 'fn main() {}\n')!
	}
	// A former sidecar must be replaced along with its tool.
	os.write_file(os.join_path(tools, 'vwhere', 'vwhere' + suffix + '.manifest'), 'stale')!
	mut environment := os.environ()
	environment['VEXE'] = fake
	environment['VFLAGS'] = ''
	environment['VTEST_SANDBOXED_PACKAGING'] = '1'
	environment['VTEST_ONLY'] = 'vplain,vsample_builder,vwhere'
	environment['VJOBS'] = '1'
	mut process := os.new_process(tool)
	process.set_args(['build-tools'])
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.run()
	output := process.stdout_slurp()
	process.wait()
	code := process.code
	process.close()
	assert code == 0, output
	for relative in ['vplain', os.join_path('vwhere', 'vwhere'),
		os.join_path('builders', 'vsample_builder')] {
		binary := os.join_path(tools, relative + suffix)
		assert os.is_executable(binary), output
		assert os.read_file(binary + '.manifest')! == os.file_name(binary)
	}
	assert !os.exists(os.join_path(tools, 'vwhere' + suffix + '.manifest'))
	assert !os.exists(os.join_path(tools, 'vsample_builder' + suffix + '.manifest'))
}

import os
import v.cmdexec
import v.pref

fn test_build_vbinaries_selects_a_production_compiler_and_keeps_literal_paths() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v binary builds ${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib'))!
	defer { os.rmdir_all(root) or {} }
	tool := os.join_path(root, 'build-vbinaries')
	built := cmdexec.run(@VEXE, ['-o', tool,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vbuild-vbinaries.v')])
	assert built.exit_code == 0, built.output
	source := os.join_path(root, 'compiler.v')
	os.write_file(source, "import os
fn main() {
	args := os.args[1..]
	if '-prod' in args && os.getenv('VFLAGS') == '-cc tcc' && '-cc' !in args { exit(88) }
	name := args[args.index('-o') + 1]
	os.write_file(os.getenv('VBUILD_CAPTURE') + name, args.join('\\n')) or { panic(err) }
}")!
	compiler := os.join_path(root, 'v compiler')
	compiled := cmdexec.run(@VEXE, ['-o', compiler, source])
	assert compiled.exit_code == 0, compiled.output
	for index, flags in ['-cc tcc', '-cc gcc', '-cc tcc -cc gcc'] {
		capture := os.join_path(root, 'capture_${index}_')
		mut environment := os.environ()
		environment['VEXE'] = compiler
		environment['VFLAGS'] = flags
		environment['VBUILD_CAPTURE'] = capture
		mut process := os.new_process(tool)
		process.set_environment(environment)
		process.set_redirect_stdio_merged()
		process.run()
		output := process.stdout_slurp()
		process.wait()
		code := process.code
		process.close()
		assert code == 0, output
		for name in ['v_g', 'v_prod_g', 'v_cg', 'v_prod_cg', 'v_prod'] {
			args := os.read_file(capture + name)!.split('\n')
			assert args[0..2] == ['-o', name]
			assert args.last() == 'cmd/v'
			assert ('-prod' in args) == name.starts_with('v_prod')
			assert ('-g' in args) == (name in ['v_g', 'v_prod_g'])
			assert ('-cg' in args) == (name in ['v_cg', 'v_prod_cg'])
			if index == 0 && name.starts_with('v_prod') {
				cc := args.index('-cc')
				assert cc >= 0, args.str()
				assert !pref.ccompiler_is_tinycc(args[cc + 1]), args.str()
			} else {
				assert '-cc' !in args, args.str()
			}
		}
	}
}

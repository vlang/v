import os

// v-workflow's FLAGS.md says that `-cg` keeps the C beside the output.
fn test_cg_skill_claim_keeps_the_c_sources_beside_the_output() {
	root := os.join_path(os.vtmp_dir(), 'skill_cg_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("kept") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	result := os.exec([@VEXE, '-new-compiler', '-cg', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert result.exit_code == 0, result.output
	kept := os.join_path(root, '.${os.base(executable)}.v3cc.cg')
	assert os.is_dir(kept), 'no ${kept} after a -cg build:\n${result.output}'
	c_source := os.read_file(os.join_path(kept, 'src.c'))!
	assert c_source.contains('kept'), 'src.c is not the C of this program'
	assert result.output.contains(kept), result.output
}

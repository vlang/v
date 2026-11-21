import os

// v-workflow's FLAGS.md says that `-showcc` prints the C command and the post-link steps.
fn test_showcc_skill_claim_prints_the_c_command_and_then_the_move_into_place() {
	root := os.join_path(os.vtmp_dir(), 'skill_showcc_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("linked") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	result := os.exec([@VEXE, '-new-compiler', '-showcc', '-gc', 'none', '-nocache', '-o', executable,
		source])
	assert result.exit_code == 0, result.output
	shown := result.output.split_into_lines().filter(it.starts_with('  > '))
	assert shown.len >= 2, result.output
	assert !shown[0].contains('mv '), 'the first line shown is not the C command: ${shown[0]}'
	moves := shown.filter(it.contains('mv ') && it.contains(' ${executable}'))
	assert moves.len == 1, result.output
	assert shown.index(moves[0]) > 0
}

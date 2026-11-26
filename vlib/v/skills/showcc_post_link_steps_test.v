import os
import v.cmdexec

// v-workflow's FLAGS.md says that `-showcc` prints the C command and the post-link steps.
//
// The output is built in a directory whose name has a space: the step line shows its paths the
// way cmdexec.display writes an argument, quoted when a shell would need it, and that is what
// the line is matched by.
fn test_showcc_skill_claim_prints_the_c_command_and_then_the_move_into_place() {
	root := os.join_path(os.vtmp_dir(), 'skill showcc ${os.getpid()}')
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
	shown_executable := cmdexec.display('mv', [executable]).all_after('mv ')
	assert shown_executable != executable, 'the path has nothing to quote: ${executable}'
	result := os.exec([@VEXE, '-showcc', '-gc', 'none', '-nocache', '-o', executable, source])
	assert result.exit_code == 0, result.output
	shown := result.output.split_into_lines().filter(it.starts_with('  > '))
	assert shown.len >= 2, result.output
	assert !shown[0].starts_with('  > mv '), 'the first line shown is not the C command: ${shown[0]}'
	moves := shown.filter(it.starts_with('  > mv ') && it.ends_with(' ' + shown_executable))
	assert moves.len == 1, result.output
	assert shown.index(moves[0]) > 0
	assert os.exec([executable]).output.trim_space() == 'linked'
}

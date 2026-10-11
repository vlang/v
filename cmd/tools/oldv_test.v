import os
import rand
import v.cmdexec

fn test_oldv_runs_shell_commands_in_the_old_checkout() {
	root := os.join_path(os.vtmp_dir(), 'oldv shell ${rand.ulid()}')
	checkout := os.join_path(root, 'v_at_HEAD')
	os.mkdir_all(checkout)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	executable_name := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	// An existing compiler skips cloning and bootstrapping in the old checkout.
	os.cp(@VEXE, os.join_path(checkout, executable_name))!
	oldv := os.join_path(root, if os.user_os() == 'windows' { 'oldv.exe' } else { 'oldv' })
	built := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-o', oldv,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'oldv.v')], 120_000)
	assert built.exit_code == 0, built.output
	dir_command := if os.user_os() == 'windows' { 'cd' } else { 'pwd' }
	command := 'mkdir "copy destination" && echo copied > "copy destination${os.path_separator}result.txt" && ${dir_command} > "root.txt" && echo shell-command-ok'
	args := ['--cache=false', '--workdir', root, '--command', command, 'HEAD']
	result := cmdexec.run_with_timeout(oldv, args, 30_000)
	assert result.exit_code == 0, result.output
	assert result.output.contains('shell-command-ok'), result.output
	assert os.read_file(os.join_path(checkout, 'copy destination', 'result.txt'))!.trim_space() == 'copied'
	assert os.real_path(os.read_file(os.join_path(checkout, 'root.txt'))!.trim_space()) == os.real_path(checkout)
	failure := cmdexec.run_with_timeout(oldv, ['--cache=false', '--workdir', root, '--command',
		'exit 37', 'HEAD'], 30_000)
	assert failure.exit_code == 37, failure.output
}

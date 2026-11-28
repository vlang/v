// vtest build: !windows
module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The child runs the CLI with the working directory and environment of the fixture.
	if os.getenv('VSYMLINK_TEST_CHILD') == '1' {
		main()
		exit(0)
	}
}

fn test_symlink_preserves_shared_temporary_files() ! {
	$if windows {
		return
	} $else {
		root := os.join_path(os.temp_dir(), 'vsymlink temporary files ${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		tool := os.executable()
		shared_tmp := os.join_path(root, 'shared temporary directory')
		os.mkdir_all(shared_tmp)!
		sentinel := os.join_path(shared_tmp, 'another process.txt')
		os.write_file(sentinel, 'still in use')!
		link_dir := os.join_path(root, 'bin')
		github_path := os.join_path(root, 'github_path')
		os.write_file(github_path, '')!
		environment := {
			'VTMP':                shared_tmp
			'VEXE':                @VEXE
			'GITHUB_JOB':          'symlink-test'
			'GITHUB_PATH':         github_path
			'VSYMLINK_TEST_CHILD': '1'
		}
		for args in [[link_dir], ['-githubci'], [link_dir, 'extra']] {
			result := cmdexec.run_in_merged_with_env(tool, args, root, environment)
			assert result.exit_code == if args.len == 2 { 1 } else { 0 }, result.output
			assert os.read_file(sentinel)! == 'still in use'
		}
		assert os.real_path(os.join_path(link_dir, 'v')) == os.real_path(@VEXE)
		assert os.read_file(github_path)!.contains(root)
	}
}

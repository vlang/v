import v.util.version
import os

fn test_githash() {
	if os.getenv('GITHUB_JOB') == '' {
		eprintln('> skipping test, since it needs GITHUB_JOB to be defined (it is flaky on development machines, with changing repos and v compiled with `./v self` from uncommitted changes).')
		return
	}
	if !os.exists(os.join_path(@VMODROOT, '.git')) {
		eprintln('> skipping test due to missing V .git directory')
		return
	}
	sha := version.githash(@VMODROOT)!
	assert sha == @VCURRENTHASH

	git_proj_path := os.join_path(os.vtmp_dir(), 'test_githash')
	defer {
		os.rmdir_all(git_proj_path) or {}
	}
	os.exec_opt(['git', 'init', git_proj_path])!
	os.chdir(git_proj_path)!
	if sha_ := version.githash(git_proj_path) {
		assert false, 'Should not have found an unknown revision'
	} else {
		assert err.msg().contains('failed to find revision file'), err.msg()
	}
	os.exec_opt(['git', 'config', 'user.name']) or {
		os.exec_opt(['git', 'config', 'user.email', 'ci@vlang.io'])!
		os.exec_opt(['git', 'config', 'user.name', 'V CI'])!
	}
	os.write_file('v.mod', '')!
	os.exec_opt(['git', 'add', '.'])!
	os.exec_opt(['git', 'commit', '-m', 'test1'])!
	test_rev := os.exec_opt(['git', 'rev-parse', '--short=7', 'HEAD'])!.output.trim_space()
	assert test_rev == version.githash(git_proj_path)!
	os.write_file('README.md', '')!
	os.exec_opt(['git', 'add', '.'])!
	os.exec_opt(['git', 'commit', '-m', 'test2'])!
	test_rev2 := os.exec_opt(['git', 'rev-parse', '--short=7', 'HEAD'])!.output.trim_space()
	assert test_rev2 != test_rev
	assert test_rev2 == version.githash(git_proj_path)!
}

fn create_githash_fixture(root string, linked bool) !(string, string) {
	common_dir := os.join_path(root, 'main', '.git')
	checkout := if linked { os.join_path(root, 'linked') } else { os.join_path(root, 'main') }
	git_dir := if linked { os.join_path(common_dir, 'worktrees', 'linked') } else { common_dir }
	os.mkdir_all(checkout)!
	os.mkdir_all(git_dir)!
	os.mkdir_all(os.join_path(common_dir, 'refs', 'heads'))!
	if linked {
		os.write_file(os.join_path(checkout, '.git'), 'gitdir: ../main/.git/worktrees/linked\n')!
		os.write_file(os.join_path(git_dir, 'commondir'), '../..\n')!
	}
	os.write_file(os.join_path(git_dir, 'HEAD'), 'ref: refs/heads/main\n')!
	return checkout, common_dir
}

fn test_githash_reads_packed_refs_and_prefers_loose_refs() {
	for linked in [false, true] {
		root := os.join_path(os.vtmp_dir(), 'version_githash_packed_${os.getpid()}_${linked}')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		checkout, common_dir := create_githash_fixture(root, linked)!
		for hash in ['1234567890abcdef1234567890abcdef12345678',
			'1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef'] {
			packed_refs := ['# pack-refs with: peeled fully-peeled sorted',
				'abcdef1234567890abcdef1234567890abcdef12 refs/heads/main-other',
				'invalid refs/heads/main', '${hash} refs/heads/main',
				'^abcdef1234567890abcdef1234567890abcdef12'].join('\n')
			os.write_file(os.join_path(common_dir, 'packed-refs'), packed_refs + '\n')!
			assert version.githash(checkout)! == '1234567'
		}
		os.write_file(os.join_path(common_dir, 'refs', 'heads', 'main'),
			'abcdef1234567890abcdef1234567890abcdef12\n')!
		assert version.githash(checkout)! == 'abcdef1'
		os.write_file(os.join_path(common_dir, 'refs', 'heads', 'main'), 'bad\n')!
		if hash := version.githash(checkout) {
			assert false, 'a malformed loose ref must not use a stale packed ref, got ${hash}'
		} else {
			assert err.msg().contains('failed to limit hash')
		}
	}
}

fn test_githash_rejects_missing_or_malformed_packed_refs() {
	for linked in [false, true] {
		root := os.join_path(os.vtmp_dir(), 'version_githash_bad_packed_${os.getpid()}_${linked}')
		os.rmdir_all(root) or {}
		defer {
			os.rmdir_all(root) or {}
		}
		checkout, common_dir := create_githash_fixture(root, linked)!
		if hash := version.githash(checkout) {
			assert false, 'missing revision must fail, got ${hash}'
		} else {
			assert err.msg().contains('failed to find revision file')
		}
		for content in ['# pack-refs with: peeled\n',
			'1234567890abcdef1234567890abcdef12345678 refs/heads/main-other\n', 'short refs/heads/main\n',
			'g234567890abcdef1234567890abcdef12345678 refs/heads/main\n',
			'1234567890abcdef1234567890abcdef12345678 refs/heads/main extra\n', '# refs/heads/main\n',
			'^ refs/heads/main\n'] {
			os.write_file(os.join_path(common_dir, 'packed-refs'), content)!
			if hash := version.githash(checkout) {
				assert false, 'malformed or unrelated revision must fail, got ${hash}'
			} else {
				assert err.msg().contains('failed to find revision file')
			}
		}
	}
}

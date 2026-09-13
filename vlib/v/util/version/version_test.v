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
	os.execute_opt('git init ${git_proj_path}')!
	os.chdir(git_proj_path)!
	if sha_ := version.githash(git_proj_path) {
		assert false, 'Should not have found an unknown revision'
	} else {
		assert err.msg().contains('failed to find revision file'), err.msg()
	}
	os.execute_opt('git config user.name') or {
		os.execute_opt('git config user.email "ci@vlang.io"')!
		os.execute_opt('git config user.name "V CI"')!
	}
	os.write_file('v.mod', '')!
	os.execute_opt('git add .')!
	os.execute_opt('git commit -m "test1"')!
	test_rev := os.execute_opt('git rev-parse --short=7 HEAD')!.output.trim_space()
	assert test_rev == version.githash(git_proj_path)!
	os.write_file('README.md', '')!
	os.execute_opt('git add .')!
	os.execute_opt('git commit -m "test2"')!
	test_rev2 := os.execute_opt('git rev-parse --short=7 HEAD')!.output.trim_space()
	assert test_rev2 != test_rev
	assert test_rev2 == version.githash(git_proj_path)!
}

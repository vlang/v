import v.util.version
import os

fn test_githash() {
	sha := version.githash(@VMODROOT)!
	assert sha == @VCURRENTHASH

	git_proj_path := os.join_path(os.vtmp_dir(), 'test_githash')
	defer {
		os.rmdir_all(git_proj_path) or {}
	}
	os.execute_opt('git init ${git_proj_path}')!
	os.chdir(git_proj_path)!
	os.write_file('v.mod', '')!
	os.execute_opt('git add .')!
	os.execute_opt('git commit -m "test"')!
	test_rev := os.execute_opt('git rev-parse HEAD')!.output[..7]
	assert version.githash(git_proj_path)! == test_rev
}

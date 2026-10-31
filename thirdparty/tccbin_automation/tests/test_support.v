module tests

import os

fn automation_root() string {
	return os.real_path(os.join_path(os.getwd(), 'thirdparty', 'tccbin_automation'))
}

// write_cmd_git_wrapper writes a `git.cmd` that runs the installed git.exe and returns its directory.
// `tag` keeps the directory name apart for each caller.
fn write_cmd_git_wrapper(tag string) !string {
	real_git := os.find_abs_path_of_executable('git')!
	wrapper_dir := os.join_path(os.temp_dir(), 'tccbin-git-cmd-${tag}-${os.getpid()}')
	os.rmdir_all(wrapper_dir) or {}
	os.mkdir_all(wrapper_dir)!
	os.write_file(os.join_path(wrapper_dir, 'git.cmd'), '@"${real_git}" %*\r\n')!
	return wrapper_dir
}

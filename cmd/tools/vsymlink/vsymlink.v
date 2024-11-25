import os

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

fn main() {
	at_exit(|| os.rmdir_all(os.vtmp_dir()) or {}) or {}
	if os.args.len > 2 {
		if '-githubci' in os.args {
			// TODO: [AFTER 2024-09-31] remove `-githubci` flag and function and only print usage and exit(1) .
			if os.getenv('GITHUB_JOB') != '' {
				println('::warning::Use `v symlink` instead of `v symlink -githubci`')
			}
			setup_symlink_github()
			return
		} else {
			println('usage: v symlink')
			exit(1)
		}
	}
	setup_symlink()
}

fn setup_symlink_github() {
	// We append V's install location (which should
	// be the current directory) to the PATH environment variable.

	// Resources:
	// 1. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#environment-files
	// 2. https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-environment-variable
	mut content := os.read_file(os.getenv('GITHUB_PATH')) or {
		eprintln('The `GITHUB_PATH` env variable is not defined.')
		eprintln('    This command: `v symlink -githubci` is intended to be used within GithubActions .yml files.')
		eprintln('    It also needs to be run *as is*, *** without `sudo` ***, otherwise it will not work.')
		eprintln('    For local usage, outside CIs, on !windows, prefer `sudo ./v symlink` .')
		eprintln('    On windows, use `.\\v.exe symlink` instead.')
		exit(1)
	}
	content += '\n${os.getwd()}\n'
	os.write_file(os.getenv('GITHUB_PATH'), content) or { panic('Failed to write to GITHUB_PATH.') }
}

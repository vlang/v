import os

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })
const symlink_usage = 'usage: v symlink [directory]'

struct SymlinkOptions {
	link_dir  string
	github_ci bool
}

fn main() {
	at_exit(|| os.rmdir_all(os.vtmp_dir()) or {}) or {}
	options := parse_symlink_options(os.args[1..]) or {
		println(symlink_usage)
		exit(1)
	}
	if options.github_ci {
		setup_symlink_github()
		return
	}
	setup_symlink(options.link_dir)
}

fn parse_symlink_options(raw_args []string) !SymlinkOptions {
	args := trim_symlink_command(raw_args)
	if args.len == 0 {
		return SymlinkOptions{}
	}
	if args == ['-githubci'] {
		// TODO: [AFTER 2024-09-31] remove `-githubci` flag and function and only print usage and exit(1) .
		if os.getenv('GITHUB_JOB') != '' {
			println('::warning::Use `v symlink` instead of `v symlink -githubci`')
		}
		return SymlinkOptions{
			github_ci: true
		}
	}
	if args.len == 1 {
		return SymlinkOptions{
			link_dir: args[0]
		}
	}
	return error(symlink_usage)
}

fn trim_symlink_command(raw_args []string) []string {
	if raw_args.len > 0 && raw_args[0] == 'symlink' {
		return raw_args[1..]
	}
	return raw_args
}

fn normalized_link_dir(custom_link_dir string) string {
	if custom_link_dir != '' {
		return os.expand_tilde_to_home(custom_link_dir)
	}
	return default_link_dir()
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

import os

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

fn main() {
	at_exit(|| os.rmdir_all(os.vtmp_dir()) or {}) or {}
	if os.args.len > 2 {
		if '-githubci' in os.args {
			// TODO: remove warning for deprecation of `-githubci` flag and
			// only use the block below after 2024-09-31.
			println('::warning::Use `v symlink` instead of `v symlink -githubci`')
		} else {
			println('usage: v symlink')
			exit(1)
		}
	}
	setup_symlink()
}

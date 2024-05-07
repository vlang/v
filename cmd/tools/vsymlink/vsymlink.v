import os

const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })

fn main() {
	at_exit(|| os.rmdir_all(os.vtmp_dir()) or {}) or {}
	if os.args.len > 2 {
		print('usage: v symlink')
		exit(1)
	}
	setup_symlink()
}

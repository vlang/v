import os

fn setup_symlink(custom_link_dir string) {
	link_dir := normalized_link_dir(custom_link_dir)
	if create_symlink_in(link_dir) {
		return
	}
	// A stock Apple-silicon macOS has no /usr/local/bin at all, and creating it
	// needs root, so the fallback has to cover a failed `mkdir` too, not just a
	// failed `symlink`.
	home := os.home_dir()
	local_bin := if home == '' { '' } else { os.join_path(home, '.local', 'bin') }
	if local_bin != '' && local_bin != link_dir && create_symlink_in(local_bin) {
		eprintln('Note: Symlink created in "${local_bin}" instead of "${link_dir}".')
		if path := os.getenv_opt('PATH') {
			if !path.contains(local_bin) {
				eprintln('Make sure "${local_bin}" is in your PATH.')
			}
		}
		return
	}
	eprintln('Failed to create the `v` symlink in "${link_dir}".')
	eprintln('Try again with sudo.')
	exit(1)
}

// create_symlink_in points `<link_dir>/v` at the running V executable, creating
// link_dir when needed. It reports whether the symlink is in place.
fn create_symlink_in(link_dir string) bool {
	if !os.exists(link_dir) {
		os.mkdir_all(link_dir) or { return false }
	}
	link_path := symlink_path(link_dir)
	os.rm(link_path) or {}
	os.symlink(vexe, link_path) or { return false }
	return true
}

fn default_link_dir() string {
	if os.is_dir('/data/data/com.termux/files') {
		return '/data/data/com.termux/files/usr/bin'
	}
	return '/usr/local/bin'
}

fn symlink_path(link_dir string) string {
	return os.join_path(link_dir, 'v')
}

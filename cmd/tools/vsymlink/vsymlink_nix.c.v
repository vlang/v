import os

fn setup_symlink(custom_link_dir string) {
	link_dir := normalized_link_dir(custom_link_dir)
	if !os.exists(link_dir) {
		os.mkdir_all(link_dir) or { panic(err) }
	}
	mut link_path := symlink_path(link_dir)
	os.rm(link_path) or {}
	os.symlink(vexe, link_path) or {
		// Try ~/.local/bin as a fallback when /usr/local/bin is not writable.
		home := os.home_dir()
		if home == '' {
			eprintln('Failed to create symlink "${link_path}": ${err}')
			eprintln('Try again with sudo.')
			exit(1)
		}
		local_bin := os.join_path(home, '.local', 'bin')
		if !os.exists(local_bin) {
			os.mkdir_all(local_bin) or {
				eprintln('Failed to create symlink "${link_path}": ${err}')
				eprintln('Try again with sudo.')
				exit(1)
			}
		}
		link_path = os.join_path(local_bin, 'v')
		os.rm(link_path) or {}
		os.symlink(vexe, link_path) or {
			eprintln('Failed to create symlink "${link_path}": ${err}')
			eprintln('Try again with sudo.')
			exit(1)
		}
		eprintln('Note: Symlink created in "${local_bin}" instead of "/usr/local/bin".')
		if path := os.getenv_opt('PATH') {
			if !path.contains(local_bin) {
				eprintln('Make sure "${local_bin}" is in your PATH.')
			}
		}
	}
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

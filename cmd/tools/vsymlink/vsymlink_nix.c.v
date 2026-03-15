import os

fn setup_symlink() {
	mut link_path := '/data/data/com.termux/files/usr/bin/v'
	if !os.is_dir('/data/data/com.termux/files') {
		link_dir := '/usr/local/bin'
		if !os.exists(link_dir) {
			os.mkdir_all(link_dir) or { panic(err) }
		}
		link_path = link_dir + '/v'
	}
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

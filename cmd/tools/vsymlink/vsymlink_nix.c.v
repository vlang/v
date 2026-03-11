import os

fn setup_symlink(custom_link_dir string) {
	link_dir := normalized_link_dir(custom_link_dir)
	if !os.exists(link_dir) {
		os.mkdir_all(link_dir) or { panic(err) }
	}
	link_path := symlink_path(link_dir)
	os.rm(link_path) or {}
	os.symlink(vexe, link_path) or {
		eprintln('Failed to create symlink "${link_path}". Try again with sudo.')
		exit(1)
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

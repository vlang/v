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
		eprintln('Failed to create symlink "${link_path}". Try again with sudo.')
		exit(1)
	}
}

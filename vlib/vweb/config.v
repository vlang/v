module vweb

import os

pub struct Config {
mut:
	// private data for static paths
	// this may move
	static_files      map[string]string
	static_mime_types map[string]string
pub mut:
	port int
}

// TODO: these Config methods may be removed

// Handles a directory static
// If `root` is set the mount path for the dir will be in '/'
pub fn (mut c Config) handle_static(directory_path string, root bool) bool {
	if !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_space().trim_right('/')
	mut mount_path := ''
	if dir_path != '.' && os.is_dir(dir_path) && !root {
		// Mount point hygene, "./assets" => "/assets".
		mount_path = '/' + dir_path.trim_left('.').trim('/')
	}
	c.scan_static_directory(dir_path, mount_path)
	return true
}

// mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://server/mount_path
// For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://server/assets/main.css .
pub fn (mut c Config) mount_static_folder_at(directory_path string, mount_path string) bool {
	if mount_path.len < 1 || mount_path[0] != `/` || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_right('/')
	c.scan_static_directory(dir_path, mount_path[1..])
	return true
}

// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the file type
pub fn (mut c Config) serve_static(url string, file_path string, mime_type string) {
	c.static_files[url] = file_path
	c.static_mime_types[url] = mime_type
}

fn (mut c Config) scan_static_directory(directory_path string, mount_path string) {
	files := os.ls(directory_path) or { panic(err) }
	if files.len > 0 {
		for file in files {
			full_path := os.join_path(directory_path, file)
			if os.is_dir(full_path) {
				c.scan_static_directory(full_path, mount_path + '/' + file)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in mime_types {
					c.serve_static(mount_path + '/' + file, full_path, mime_types[ext])
				}
			}
		}
	}
}

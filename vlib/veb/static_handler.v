module veb

import os

pub interface StaticApp {
mut:
	static_files      map[string]string
	static_mime_types map[string]string
	static_hosts      map[string]string
}

// StaticHandler provides methods to handle static files in your veb App
pub struct StaticHandler {
pub mut:
	static_files      map[string]string
	static_mime_types map[string]string
	static_hosts      map[string]string
}

// scan_static_directory recursively scans `directory_path` and returns an error if
// no valid MIME type can be found
fn (mut sh StaticHandler) scan_static_directory(directory_path string, mount_path string, host string) ! {
	files := os.ls(directory_path) or { panic(err) }
	if files.len > 0 {
		for file in files {
			full_path := os.join_path(directory_path, file)
			if os.is_dir(full_path) {
				sh.scan_static_directory(full_path, mount_path.trim_right('/') + '/' + file,
					host)!
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				sh.host_serve_static(host, mount_path.trim_right('/') + '/' + file, full_path)!
			}
		}
	}
}

// handle_static is used to mark a folder (relative to the current working folder)
// as one that contains only static resources (css files, images etc).
// If `root` is set the mount path for the dir will be in '/'
// Usage:
// ```v
// os.chdir( os.executable() )?
// app.handle_static('assets', true)
// ```
pub fn (mut sh StaticHandler) handle_static(directory_path string, root bool) !bool {
	return sh.host_handle_static('', directory_path, root)!
}

// host_handle_static is used to mark a folder (relative to the current working folder)
// as one that contains only static resources (css files, images etc).
// If `root` is set the mount path for the dir will be in '/'
// Usage:
// ```v
// os.chdir( os.executable() )?
// app.host_handle_static('localhost', 'assets', true)
// ```
pub fn (mut sh StaticHandler) host_handle_static(host string, directory_path string, root bool) !bool {
	if !os.exists(directory_path) {
		return error('directory `${directory_path}` does not exist. The directory should be relative to the current working directory: ${os.getwd()}')
	}
	dir_path := directory_path.trim_space().trim_right('/')
	mut mount_path := ''
	if dir_path != '.' && os.is_dir(dir_path) && !root {
		// Mount point hygiene, "./assets" => "/assets".
		mount_path = '/' + dir_path.trim_left('.').trim('/')
	}
	sh.scan_static_directory(dir_path, mount_path, host)!
	return true
}

// mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://server/mount_path
// For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://server/assets/main.css .
pub fn (mut sh StaticHandler) mount_static_folder_at(directory_path string, mount_path string) !bool {
	return sh.host_mount_static_folder_at('', directory_path, mount_path)!
}

// host_mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://host/mount_path
// For example: suppose you have called .host_mount_static_folder_at('localhost', '/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://localhost/assets/main.css .
pub fn (mut sh StaticHandler) host_mount_static_folder_at(host string, directory_path string, mount_path string) !bool {
	if mount_path == '' || mount_path[0] != `/` {
		return error('invalid mount path! The path should start with `/`')
	} else if !os.exists(directory_path) {
		return error('directory `${directory_path}` does not exist. The directory should be relative to the current working directory: ${os.getwd()}')
	}

	dir_path := directory_path.trim_right('/')

	trim_mount_path := mount_path.trim_left('/').trim_right('/')
	sh.scan_static_directory(dir_path, '/${trim_mount_path}', host)!
	return true
}

// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the file type
pub fn (mut sh StaticHandler) serve_static(url string, file_path string) ! {
	sh.host_serve_static('', url, file_path)!
}

// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file
// `host` is the host to serve the file from
pub fn (mut sh StaticHandler) host_serve_static(host string, url string, file_path string) ! {
	ext := os.file_ext(file_path).to_lower()

	// Rudimentary guard against adding files not in mime_types.
	if ext !in sh.static_mime_types && ext !in mime_types {
		return error('unknown MIME type for file extension "${ext}". You can register your MIME type in `app.static_mime_types`')
	}
	sh.static_files[url] = file_path
	sh.static_hosts[url] = host
}

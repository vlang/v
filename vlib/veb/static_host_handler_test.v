module veb

import os

// cov_static_tree builds a throwaway directory holding files with known
// extensions plus one with none, so the MIME-type guard in host_serve_static
// can be exercised without a fixture inside the repo.
fn cov_static_tree() string {
	root := os.join_path(os.temp_dir(), 'vcov_veb_static_host')
	os.rmdir_all(root) or {}
	for sub in ['', 'sub', 'assets'] {
		os.mkdir_all(os.join_path(root, sub)) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.css'), 'body{}') or { panic(err) }
	os.write_file(os.join_path(root, 'sub', 'deep.css'), 'deep{}') or { panic(err) }
	os.write_file(os.join_path(root, 'assets', 'app.css'), 'app{}') or { panic(err) }
	os.write_file(os.join_path(root, 'plain'), 'plain') or { panic(err) }
	return root
}

fn test_host_serve_static_records_the_host_for_the_url() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_serve_static('cov.example', '/x/main.css', os.join_path(root, 'main.css')) or {
		assert false, err.msg()
		return
	}
	assert sh.static_files['/x/main.css'] == os.join_path(root, 'main.css')
	assert sh.static_hosts['/x/main.css'] == 'cov.example'
	assert sh.static_prefixes == ['/x/']
}

fn test_host_serve_static_rejects_an_extension_less_file() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_serve_static('cov.example', '/x/plain', os.join_path(root, 'plain')) or {
		assert err.msg() == 'unknown MIME type for file extension "". You can register your MIME type in `app.static_mime_types`'
		return
	}
	assert false, 'an extension-less file must be rejected'
	assert sh.static_files.len == 0
	assert sh.static_hosts.len == 0
}

fn test_host_serve_static_accepts_a_registered_custom_mime_type() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	custom := os.join_path(root, 'sub', 'thing.custom')
	os.write_file(custom, 'x') or { panic(err) }
	mut sh := StaticHandler{}
	sh.static_mime_types['.custom'] = mime_types['.txt']
	sh.host_serve_static('', '/t/thing.custom', custom) or { assert false, err.msg() }
	assert sh.static_files['/t/thing.custom'] == custom
	assert sh.static_hosts['/t/thing.custom'] == ''
}

fn test_serve_static_registers_an_empty_host() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.serve_static('/plain.css', os.join_path(root, 'main.css')) or { assert false, err.msg() }
	assert sh.static_files['/plain.css'] == os.join_path(root, 'main.css')
	assert sh.static_hosts['/plain.css'] == ''
	assert sh.static_prefixes == ['/plain.css']
}

fn test_static_prefixes_are_deduplicated() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_serve_static('', '/x/main.css', os.join_path(root, 'main.css')) or { assert false, err.msg() }
	sh.host_serve_static('', '/x/deep.css', os.join_path(root, 'sub', 'deep.css')) or {
		assert false, err.msg()
	}
	assert sh.static_prefixes == ['/x/']
	assert sh.static_files.len == 2
}

fn test_host_mount_static_folder_at_mounts_every_file_under_the_given_path() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_mount_static_folder_at('cov.example', root, '/assets') or { assert false, err.msg() }
	assert sh.static_files.len == 3
	assert sh.static_files['/assets/main.css'] == os.join_path(root, 'main.css')
	assert sh.static_files['/assets/sub/deep.css'] == os.join_path(root, 'sub', 'deep.css')
	assert sh.static_files['/assets/assets/app.css'] == os.join_path(root, 'assets', 'app.css')
	assert sh.static_hosts['/assets/main.css'] == 'cov.example'
	assert sh.static_prefixes == ['/assets/']
}

fn test_host_mount_static_folder_at_trims_a_trailing_slash_from_the_mount_path() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_mount_static_folder_at('', root, '/assets/') or { assert false, err.msg() }
	assert sh.static_files['/assets/main.css'] == os.join_path(root, 'main.css')
	assert sh.static_prefixes == ['/assets/']
}

fn test_host_mount_static_folder_at_rejects_a_mount_path_without_a_leading_slash() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_mount_static_folder_at('', root, 'assets') or {
		assert err.msg() == 'invalid mount path! The path should start with `/`'
		return
	}
	assert false, 'a mount path without a leading slash must be rejected'
}

fn test_host_mount_static_folder_at_reports_a_missing_directory() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	missing := os.join_path(root, 'nope')
	mut sh := StaticHandler{}
	sh.host_mount_static_folder_at('', missing, '/assets') or {
		assert err.msg().starts_with('directory `${missing}` does not exist')
		return
	}
	assert false, 'a missing directory must be reported'
}

// NOTE: the extension-less `plain` file is dropped by the `file.contains('.')`
// guard in scan_static_directory before any MIME type is looked up, so a root
// mount registers three of the four files it walks.
fn test_host_handle_static_mounts_at_the_root_when_root_is_requested() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	mut sh := StaticHandler{}
	sh.host_handle_static('cov.example', root, true) or { assert false, err.msg() }
	assert sh.static_files.len == 3
	assert sh.static_files['/main.css'] == os.join_path(root, 'main.css')
	assert sh.static_files['/sub/deep.css'] == os.join_path(root, 'sub', 'deep.css')
	assert sh.static_files['/assets/app.css'] == os.join_path(root, 'assets', 'app.css')
	assert sh.static_hosts['/main.css'] == 'cov.example'
	assert sh.static_hosts['/sub/deep.css'] == 'cov.example'
	assert sh.static_prefixes.len == 3
	assert '/main.css' in sh.static_prefixes
	assert '/sub/' in sh.static_prefixes
	assert '/assets/' in sh.static_prefixes
}

// The stored path is kept exactly as it was walked, so a directory given as a
// relative name registers relative file paths under it.
fn test_host_handle_static_derives_the_mount_path_from_the_directory_name() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	want_wd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(want_wd) or {}
	}
	mut sh := StaticHandler{}
	sh.host_handle_static('cov.example', 'assets', false) or { assert false, err.msg() }
	assert sh.static_files.len == 1
	assert sh.static_files['/assets/app.css'] == os.join_path('assets', 'app.css')
	assert sh.static_hosts['/assets/app.css'] == 'cov.example'
	assert sh.static_prefixes == ['/assets/']
}

fn test_host_handle_static_reports_a_missing_directory() {
	root := cov_static_tree()
	defer {
		os.rmdir_all(root) or {}
	}
	missing := os.join_path(root, 'nope')
	mut sh := StaticHandler{}
	sh.host_handle_static('', missing, true) or {
		assert err.msg().starts_with('directory `${missing}` does not exist')
		return
	}
	assert false, 'a missing directory must be reported'
}

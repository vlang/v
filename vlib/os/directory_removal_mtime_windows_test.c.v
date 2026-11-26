module os

import time

// Compare the CRT timestamp with the handle-based timestamp without changing either.
fn directory_removal_mtime_probe(use_crt bool, iteration int) !bool {
	dir := join_path(vtmp_dir(), 'directory_mtime_${getpid()}_${use_crt}_${iteration}')
	sub := join_path(dir, 'src', 'sub')
	file := join_path(sub, 'a.v')
	output := join_path(dir, 'out')
	mkdir_all(sub)!
	defer {
		rmdir_all(dir) or {}
	}
	write_file(file, '')!
	write_file(output, '')!
	now := time.now().unix()
	for path in [file, sub, join_path(dir, 'src')] {
		utime(path, now - 100, now - 100)!
	}
	utime(output, now - 50, now - 50)!
	before := stat(sub)!
	if use_crt {
		wide := file.to_wide()
		defer {
			unsafe { free(wide) }
		}
		if C._wremove(wide) != 0 {
			return error('direct CRT removal failed')
		}
	} else {
		rm(file)!
	}
	// Capture stat before opening a metadata handle, which might refresh it.
	after := stat(sub)!
	info := windows_stat_file_information(sub) or { return error('handle query failed') }
	handle_mtime := windows_filetime_to_unix_seconds(info.write_time)
	refreshed := stat(sub)!
	out := stat(output)!
	remaining := ls(sub)!
	println('DIRECTORY_MTIME crt=${use_crt} iteration=${iteration} before=${before.mtime} after=${after.mtime} handle=${handle_mtime} refreshed=${refreshed.mtime} output=${out.mtime} remaining=${remaining}')
	return before.mtime == now - 100 && out.mtime == now - 50 && remaining.len == 0
		&& after.mtime > out.mtime && handle_mtime > out.mtime
}

fn test_windows_directory_removal_mtime() {
	mut failures := 0
	for use_crt in [false, true] {
		for iteration in 0 .. 12 {
			if !directory_removal_mtime_probe(use_crt, iteration)! {
				failures++
			}
		}
	}
	assert failures == 0
}

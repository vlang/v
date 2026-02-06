module asset

import os

// get_path returns a platform specific path, based on the given asset base folder and relative path
// of a resource in it. On desktop systems, it returns a path relative to the location of the executable.
// On Android, it will return just the relative_path segment, allowing you to later use os.read_apk_asset
// to read from it.
@[manualfree]
pub fn get_path(base_folder string, relative_path string) string {
	$if android {
		return relative_path.clone()
	} $else {
		fpath := os.join_path_single(base_folder, relative_path)
		defer { unsafe { fpath.free() } }
		return os.resource_abs_path(fpath)
	}
}

// read_bytes will read all of the given asset, specified by its base_folder and relative path in it.
// On Android, it will use os.read_apk_asset, relying that the asset base folder has been prepared, and
// prepackaged inside your APK. On desktop systems, it will use the base_folder and relative_path, to
// locate the file, in a way, that is relative to the executable.
@[manualfree]
pub fn read_bytes(base_folder string, relative_path string) ![]u8 {
	fpath := get_path(base_folder, relative_path)
	defer { unsafe { fpath.free() } }
	mut f_read := os.read_bytes
	$if android {
		f_read = os.read_apk_asset
	}
	res := f_read(fpath)!
	return res
}

// read_text will return the full content of the given asset as a string.
// See also read_bytes.
@[manualfree]
pub fn read_text(base_folder string, relative_path string) !string {
	bytes := read_bytes(base_folder, relative_path)!
	defer { unsafe { bytes.free() } }
	res := bytes.bytestr()
	return res
}

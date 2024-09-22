module asset

// The `asset` module provides a cross platform way to read assets, without cluttering
// the user code with comptime conditionals, like `$if ios {` or `$if android {` etc.
// Currently it supports Android and desktop applications, that do not use archived/zipped
// files, but could be extended to support them too in the future.
// It relies on the assumption that each platform has a way to either read files,
// relative to the location of the executable, or a platform specific way, to package all
// asset files into the application archive/package/executable, that is then distributed
// to the end users.
import os

// get_path returns a platform specific path, based on the given asset base folder and relative path
// of a resource in it. On desktop systems, it returns a path relative to the location of the executable.
// On Android, it will return just the relative_path segment, allowing you to later use os.read_apk_asset
// to read from it.
pub fn get_path(base_folder string, relative_path string) string {
	$if android {
		return relative_path
	} $else {
		return os.resource_abs_path(os.join_path(base_folder, relative_path))
	}
}

// read_bytes will read all of the given asset, specified by its base_folder and relative path in it.
// On Android, it will use os.read_apk_asset, relying that the asset base folder has been prepared, and
// prepackaged inside your APK. On desktop systems, it will use the base_folder and relative_path, to
// locate the file, in a way, that is relative to the executable.
pub fn read_bytes(base_folder string, relative_path string) ![]u8 {
	$if android {
		return os.read_apk_asset(get_path(base_folder, relative_path))
	} $else {
		return os.read_bytes(get_path(base_folder, relative_path))
	}
}

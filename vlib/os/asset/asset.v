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

pub fn get_path(base_folder string, relative_path string) string {
	$if android {
		return relative_path
	} $else {
		return os.resource_abs_path(os.join_path(base_folder, relative_path))
	}
}

pub fn read_bytes(base_folder string, relative_path string) ![]u8 {
	$if android {
		return os.read_apk_asset(get_path(base_folder, relative_path))
	} $else {
		return os.read_bytes(get_path(base_folder, relative_path))
	}
}

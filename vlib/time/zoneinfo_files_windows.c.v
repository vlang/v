module time

import os

const zoneinfo_vroot_zip = os.join_path(@VEXEROOT, 'vlib', 'time', 'tzdata', 'zoneinfo.zip')

// zoneinfo_getenv returns the value of the environment variable `name`, when it is set.
fn zoneinfo_getenv(name string) ?string {
	return os.getenv_opt(name)
}

// zoneinfo_join returns the path of `name` inside the directory `dir`.
fn zoneinfo_join(dir string, name string) string {
	return os.join_path(dir, name)
}

// zoneinfo_is_dir reports whether `path` names a directory.
fn zoneinfo_is_dir(path string) bool {
	return os.is_dir(path)
}

// zoneinfo_is_file reports whether `path` names a file.
fn zoneinfo_is_file(path string) bool {
	return os.is_file(path)
}

// zoneinfo_read_file returns the contents of the file `path`.
fn zoneinfo_read_file(path string) ![]u8 {
	return os.read_bytes(path)
}

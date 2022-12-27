module pathlib

import os

[noinit]
struct Path {
	parts []string
pub:
	name string
	root string
	stem string
	suffix string
	sep string
}

fn path_from_parts(parts []string) Path {
	// parts[0] == '' means the path string started with a '/'
	root := if parts[0] == '' { sep } else { '' }
	filename := os.file_name(path)
	suffix := os.file_ext(name)
	stem := name.trim_string_right(suffix)

	return Path{
		parts: parts
		name: name
		root: root
		stem: stem
		suffix: suffix
		sep: os.path_separator
	}
}

pub fn path(path string) Path {
	// TODO: reduce repeating separators (/) to one
	mut clean_path := path
	if path.ends_with(os.path_separator) {
		clean_path = path.trim_string_right(os.path_separator)
	}
	splitted := clean_path.split(os.path_separator)
	return path_from_parts(splitted)
}

pub fn (p Path) str() string {
	return p.parts.join(p.sep)
}

pub fn (p1 Path) / (p2 Path) Path {
	if p2.root == os.path_separator {
		return p2
	} else {
		return path('${p1.str()}${os.path_separator}${p2.str()}')
	}
}

pub fn (p Path) absolute() Path {
	absolute_path := os.abs_path(p.str())
	return path(absolute_path)
}

pub fn (p Path) as_uri() string {
	absolute_path_str := p.absolute().str()
	return 'file://${absolute_path_str}'
}

pub fn (p Path) chmod(mode int) ! {
	return os.chmod(p.str(), mode)!
}

pub fn (p Path) exists() bool {
	return os.exists(p.str())
}

pub fn (p Path) expanduser() Path {
	return path(os.expand_tilde_to_home(p.str()))
}

pub fn (p Path) glob(pattern string) []Path {
	// TODO
}

pub fn (p Path) group() string {
	// TODO
}

pub fn (p Path) link(target string) ! {
	os.link(p.str(), target)!
}

pub fn (p Path) is_block_device() bool {
	// TODO
}

pub fn (p Path) is_char_device() bool {
	// TODO
}

pub fn (p Path) is_dir() bool {
	return os.is_dir(p.str())
}

pub fn (p Path) is_fifo() bool {
	// TODO
}

pub fn (p Path) is_file() bool {
	return os.is_file(p.str())
}

pub fn (p Path) is_mount() bool {
	// TODO
}

pub fn (p Path) is_socket() bool {
	// TODO
}

pub fn (p Path) is_link() bool {
	return os.is_link(p.str())
}

pub fn (p Path) iterdir() []Path {
	files := os.ls(p.str())
	// convert string filenames to paths
	return files.map(path(it))
}

//     Like chmod(), except if the path points to a symlink, the symlink's
//     permissions are changed, rather than its target's.
pub fn (p Path) lchmod(mode int) {
// 	// TODO
}

//     Like stat(), except if the path points to a symlink, the symlink's
//     status information is returned, rather than its target's.
pub fn (p Path) lstat() {
	// TODO
}

pub fn (p Path) mkdir(params os.MkdirParams) !Path {
	// TODO: include mkdir_all?
	os.mkdir(p.str(), params)!
	return p
}

pub fn (p Path) open(mode string, options ...int) !File {
	return open_file(p.str(), mode, options)!
}

pub fn (p Path) owner() string {
	// TODO
}

pub fn (p Path) parent() Path {
	// parent of root ('/') and current ('.') are itself
	if p.name == '' || p.name == '.' {
		return p
	}

	if p.parts.len == 1 {
		return path('.')
	}

	parts := path.parts#[..-1]

	return path_from_parts(parts)
}

pub fn (p Path) parents() []Path {
	if p.parts.len == 1 {
		return []
	}

	mut parents := []Path
	for part in p.parts.reverse() {
		parents << Path(part)
	}

	return parents
}

pub fn (p Path) read_bytes()
	// TODO
//     Open the file in bytes mode, read it, and close the file.

// pub fn (p Path) read_text(encoding=None, errors=None)
// 	// TODO
// //     Open the file in text mode, read it, and close the file.

pub fn (p Path) readlink() Path {
	// TODO
}
//     Return the path to which the symbolic link points.

//     Rename this path to the target path.
//     The target path may be absolute or relative. Relative paths are
//     interpreted relative to the current working directory, *not* the
//     directory of the Path object.

//     Returns the new Path instance pointing to the target path.
pub fn (p Path) rename(target) Path {
	// TODO
}

//     Rename this path to the target path, overwriting if that path exists.

//     The target path may be absolute or relative. Relative paths are
//     interpreted relative to the current working directory, *not* the
//     directory of the Path object.

//     Returns the new Path instance pointing to the target path.
pub fn (p Path) replace(target) Path {
	// TODO
}

//     Make the path absolute, resolving all symlinks on the way and also
//     normalizing it (for example turning slashes into backslashes under
//     Windows).
pub fn (p Path) resolve(strict=False) Path {
	// TODO
}

//     Recursively yield all existing files (of any kind, including
//     directories) matching the given relative pattern, anywhere in
//     this subtree.
pub fn (p Path) rglob(pattern) []Path {
	// TODO
}

pub fn (p Path) rmdir() {
	os.rmdir(p.str())
}

//     Return whether other_path is the same or not as this file
//     (as returned by os.path.samefile()).
pub fn (p Path) samefile(other_path) bool {
	// TODO
}

//     Return the result of the stat() system call on this path, like
//     os.stat() does.
pub fn (p Path) stat(*, follow_symlinks=True)
	// TODO


//     Create this file with the given access mode, if it doesn't exist.
pub fn (p Path) touch() Path {
	os.create(p.str())!
	return p
}

pub fn (p Path) unlink() ! {
	os.rm(p.str())!
}

pub fn (p Path) with_name(name string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error("path does not have a file name")
	}

	if name.contains(p.sep) {
		error("name can't contain path separator")
	}

	mut parts := p.parts
	parts[parts.len - 1] = name

	return path_from_parts(parts)
}

pub fn (p Path) with_stem(stem string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error("path does not have a file name")
	}

	if stem.contains(p.sep) {
		error("stem can't contain path separator")
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.gz.zip, only .zip is the suffix and thus removed
	mut dot_position := p.name.last_index('.') or { p.name.len }
	filename_with_stem := stem + current_name[dot_position..]

	mut parts := p.parts
	parts[parts.len - 1] = filename_with_stem

	return path_from_parts(parts)
}

pub fn (p Path) with_suffix(suffix string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error("path does not have a file name")
	}

	if !suffix.starts_with('.') {
		error("suffix has to start with a `.'")
	}

	if suffix.contains(p.sep) {
		error("suffix can't contain path separator")
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.gz.zip, only .zip is the suffix and thus removed
	mut dot_position := p.name.last_index('.') or { p.name.len }
	filename_with_suffix := current_name[..dot_position] + suffix

	mut parts := p.parts
	parts[parts.len - 1] = filename_with_suffix

	return path_from_parts(parts)
}

// pub fn (p Path) write_bytes(data)
	// TODO
//     Open the file in bytes mode, write to it, and close the file.

// pub fn (p Path) write_text(data, encoding=None, errors=None, newline=None)
	// TODO
//     Open the file in text mode, write to it, and close the file.

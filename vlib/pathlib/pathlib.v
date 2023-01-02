module pathlib

import os
import regex

[noinit]
struct Path {
	parts []string
pub:
	name   string
	root   string
	stem   string
	suffix string
	sep    string
}

fn path_from_parts(parts []string) Path {
	sep := os.path_separator
	// parts[0] == '' means the path string started with a '/'
	root := if parts[0] == '' { sep } else { '' }
	filename := parts.last()
	suffix := os.file_ext(filename)
	stem := filename.trim_string_right(suffix)

	return Path{
		parts: parts
		name: filename
		root: root
		stem: stem
		suffix: suffix
		sep: sep
	}
}

pub fn path(path string) Path {
	mut clean_path := path

	// TODO: reduce repeating separators (/) to one

	if path.ends_with(os.path_separator) {
		clean_path = path.trim_string_right(os.path_separator)
	}
	splitted := clean_path.split(os.path_separator)
	return path_from_parts(splitted)
}

// Constructs a path instance representing the current working directory.
pub fn cwd() Path {
	return path(os.getwd())
}

// Constructs a path instance representing the home folder of the current user.
pub fn home() Path {
	return path(os.home_dir())
}

// Converts the path to a string representation.
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
	os.chmod(p.str(), mode)!
}

pub fn (p Path) exists() bool {
	return os.exists(p.str())
}

pub fn (p Path) expanduser() Path {
	// TODO: Python supports ~user, v does not
	return path(os.expand_tilde_to_home(p.str()))
}

pub fn (p Path) glob(patterns ...string) ![]Path {
	glob_result := os.glob(patterns)!
	// TODO
	paths := []Path{}
	for result in glob_result {
		paths << path(result)
	}
	return paths
}

pub fn (p Path) group() string {
	// TODO
}

pub fn (p Path) link(target string) ! {
	os.link(p.str(), target)!
}

pub fn (p Path) is_absolute() bool {
	return p.root != ''
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

pub fn (p Path) iterdir() ![]Path {
	// TODO
	files := os.ls(p.str())!
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

pub fn (p Path) open(mode string, options ...int) !os.File {
	return os.open_file(p.str(), mode, ...options)!
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

	parts := p.parts#[..-1]

	return path_from_parts(parts)
}

pub fn (p Path) parents() []Path {
	if p.parts.len == 1 {
		return []
	}

	parent_parts := p.parts#[..-1]
	mut parents := []Path{}

	for i, _ in parent_parts {
		parents << path_from_parts(parent_parts[..i + 1])
	}

	return parents.reverse()
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

// Returns the new Path instance pointing to the target path.
pub fn (p Path) rename(target string) !Path {
	os.mv(p.str(), target)!
	return path(target)
}


//     Returns the new Path instance pointing to the target path.
pub fn (p Path) replace(target string) Path {
	// TODO
}

// Make the path absolute, resolving all symlinks, `..`, etc. on the way and also
// normalizing it.
pub fn (p Path) resolve() Path {
	return path(os.real_path(p.str()))
}

//     Recursively yield all existing files (of any kind, including
//     directories) matching the given relative pattern, anywhere in
//     this subtree.
pub fn (p Path) rglob(pattern string) []Path {
	// TODO
}

pub fn (p Path) rmdir() ! {
	os.rmdir(p.str())!
}

//     Return whether other_path is the same or not as this file
//     (as returned by os.path.samefile()).
pub fn (p Path) samefile(other_path Path) bool {
	// TODO
}

//     Return the result of the stat() system call on this path, like
//     os.stat() does.
// pub fn (p Path) stat(*, follow_symlinks=True)
// TODO

//     Create this file with the given access mode, if it doesn't exist.
pub fn (p Path) touch() !Path {
	os.create(p.str())!
	return p
}

pub fn (p Path) unlink() ! {
	os.rm(p.str())!
}

// Replaces the last element of the path with given `name`. The paths `/`, `.`
// and `..` have no name and thus can't be used with this function.
//
// Example: `path('pathlib/pathlib.v').with_name('helper.py') == path('pathlib/helper.py')`
pub fn (p Path) with_name(name string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error('path does not have a file name')
	}

	if name.contains(p.sep) {
		error("name can't contain path separator")
	}

	mut parts := p.parts
	parts[parts.len - 1] = name

	return path_from_parts(parts)
}

// Replaces the filename of the last element of path (the name without file
// extension) with given `stem`. The paths `/`, `.` and `..` have no
// name and thus can't be used with this function.
//
// Example: `path('pathlib/pathlib.v').with_stem('helper') == path('pathlib/helper.v')`
pub fn (p Path) with_stem(stem string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error('path does not have a file name')
	}

	if stem.contains(p.sep) {
		error("stem can't contain path separator")
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.gz.zip, only .zip is the suffix and thus removed
	curent_ext := current_name.all_after_last('.')
	filename_with_stem := stem + '.' + current_ext

	mut parts := p.parts
	parts[parts.len - 1] = filename_with_stem

	return path_from_parts(parts)
}

// Replaces the (last) suffix (file extension) of the last element of the path.
// Must start with a `.` and can contain multiple (e.g. `.tar.gz`). The paths
// `/`, `.` and `..` have no name and thus can't be used with this function.
//
// Example: `path('pathlib/pathlib.v').with_suffix('.py') == path('pathlib/pathlib.py')`
pub fn (p Path) with_suffix(suffix string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		error('path does not have a file name')
	}

	if !suffix.starts_with('.') {
		error("suffix has to start with a `.'")
	}

	if suffix.contains(p.sep) {
		error("suffix can't contain path separator")
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.tar.gz, only .gz is the suffix and thus removed
	current_stem := current_name.all_before_last('.')
	filename_with_suffix := current_stem + suffix

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

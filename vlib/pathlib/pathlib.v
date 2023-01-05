module pathlib

import os

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


// CONSTRUCTORS

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

pub fn path(path_string string) Path {
	if path_string.len == 0 {
		return path('.')
	}

	clean_path := path_string.trim_string_right(os.path_separator)

	// TODO: reduce repeating separators (/) to one

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

pub fn (p Path) as_posix() string {
	// posix uses `/' as separator
	return p.parts.join('/')
}

pub fn (p Path) as_uri() string {
	// TODO: call as_posix first
	absolute_path_str := p.absolute().str()
	if !absolute_path_str.starts_with('/') {
		windows_add_slash := '/'
	} else {
		windows_add_slash := ''
	}

	return 'file://${windows_add_slash}${absolute_path_str}'
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
	glob_result := os.glob(...patterns)!
	// TODO
	mut paths := []Path{}
	for result in glob_result {
		paths << path(result)
	}
	return paths
}

pub fn (p Path) is_absolute() bool {
	return os.is_abs_path(p.str())
}

pub fn (p Path) is_block_device() bool {
	return os.inode(p.str()).typ == 'block_device'
}

pub fn (p Path) is_char_device() bool {
	return os.inode(p.str()).typ == 'character_device'
}

pub fn (p Path) is_dir() bool {
	return os.is_dir(p.str())
}

pub fn (p Path) is_fifo() bool {
	return os.inode(p.str()).typ == 'fifo'
}

pub fn (p Path) is_file() bool {
	return os.is_file(p.str())
}

// pub fn (p Path) is_mount() bool {
// 	// TODO
// }

// pub fn (p Path) is_relative_to(other Path) bool {
// 	// TODO
// }

pub fn (p Path) is_socket() bool {
	return os.inode(p.str()).typ == 'socket'
}

pub fn (p Path) is_link() bool {
	return os.is_link(p.str())
}

pub fn (p Path) is_regular() bool {
	return os.inode(p.str()).typ == 'regular'
}

pub fn (p Path) iterdir() ![]Path {
	files := os.ls(p.str())!
	// convert string filenames to paths
	return files.map(path(it))
}

pub fn (p Path) join(other Path) Path {
	return path(os.join_path_single(p.str(), other.str()))
}

pub fn (p Path) link(target Path) ! {
	os.link(p.str(), target.str())!
}

pub fn (p Path) mkdir(params os.MkdirParams) !Path {
	// TODO: include mkdir_all?
	os.mkdir(p.str(), params)!
	return p
}

pub fn (p Path) open(mode string, options ...int) !os.File {
	return os.open_file(p.str(), mode, ...options)!
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

pub fn (p Path) quoted() string {
	return os.quoted_path(p.str())
}

// Make the path absolute, resolving all symlinks, `..`, etc. on the way and also
// normalizing it.
pub fn (p Path) resolve() Path {
	return path(os.real_path(p.str()))
}

pub fn (p Path) rmdir() ! {
	os.rmdir(p.str())!
}

// Converts the path to a string representation.
pub fn (p Path) str() string {
	return p.parts.join(p.sep)
}

pub fn (p Path) symlink(target Path) ! {
	os.symlink(p.str(), target.str())
}

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

	mut parts := p.parts.clone()
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
	current_ext := current_name.all_after_last('.')
	filename_with_stem := stem + '.' + current_ext

	mut parts := p.parts.clone()
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

	mut parts := p.parts.clone()
	parts[parts.len - 1] = filename_with_suffix

	return path_from_parts(parts)
}

// pathlib is an object-orientated filesystem paths module. Most I/O operations
// are wrapped around functions from the `os` module. For example,
// `os.link('path/to/file.v', 'target.v')` becomes
// `path('path/to/file.v').link(path('target.v'))`. It is heavily inspired by
// Python's pathlib module (https://docs.python.org/3/library/pathlib.html).
module pathlib

import net.urllib
import os

// Path represents a filesystem path. It does not have to be an existing path
// on the current system. Path provides many methods for performing different
// operations on the path or perform I/O actions on the file / directory
// path points to.
[noinit]
pub struct Path {
	parts []string [required]
pub: // parts are the names of the folders, between the separators.
	name   string [required] // name is the last part of the path.
	root   string [required] // root is the first bit of the path, if absolute.
	stem   string [required] // stem (name without suffix) of the filename.
	suffix string [required] // suffix ('file extension') of the filename, including the `.`.
	sep    string [required] // sep is the system separator used in e.g. `Path.str()`.
}

// CONSTRUCTORS

// path_from_parts constructs a new path instance, but from a list of folders
// pointing to the file.
//
// Example:
// ```v
// assert path_from_parts(['a', 'b', 'c']) == path('a/b/c')
// ```
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

// path constructs a new path instance given a string representation of a path.
// Example:
// ```v
// assert path('.').absolute().str() == os.getwd()
// ```
pub fn path(path_string string) Path {
	if path_string.len == 0 {
		return path('.')
	}

	if path_string == os.path_separator {
		return path_from_parts([''])
	}

	mut clean_path := path_string

	// only trim right sep if it's not just `/'
	if path_string.len != 1 {
		clean_path = clean_path.trim_string_right(os.path_separator)
	}

	// TODO: reduce repeating separators (/) to one

	splitted := clean_path.split(os.path_separator)
	return path_from_parts(splitted)
}

// cwd constructs a path instance representing the current working directory.
// See also: `os.getwd`.
pub fn cwd() Path {
	return path(os.getwd())
}

// home constructs a path instance representing the home folder of the current user.
// See also: `os.home_dir`.
pub fn home() Path {
	return path(os.home_dir())
}

// / joins paths.
//
// Example:
// ```v
// assert path('a/b') / path('c') == path('a/b/c')
// ```
pub fn (p1 Path) / (p2 Path) Path {
	if p2.root == os.path_separator {
		return p2
	} else {
		return path('${p1}${os.path_separator}${p2}')
	}
}

// absolute returns the absolute path representation.
// See also: `os.abs_path`.
//
// Example:
// ```v
// assert path('.').absolute() == path('/user/home/v/vlib/pathlib')
// ```
pub fn (p Path) absolute() Path {
	absolute_path := os.abs_path(p.str())
	return path(absolute_path)
}

// as_posix converts a Windows path to use posix separators (`/`).
//
// Example:
// ```v
// assert path('c:\\v\\vlib\\pathlib.v).as_posix() == 'c:/v/vlib/pathlib.v'
// ```
pub fn (p Path) as_posix() string {
	// posix uses `/' as separator
	return p.parts.join('/')
}

// as_uri converts the path to a `file://` uri, possibly escaping the path where needed.
//
// Example:
// ```v
// assert path('/a/b/c d').as_uri() == 'file:///a/b/c%20d'
// ```
pub fn (p Path) as_uri() string {
	absolute_path := p.absolute()

	escaped_parts := absolute_path.parts.map(urllib.path_escape(it))
	escaped_path := path_from_parts(escaped_parts)

	posix_path := escaped_path.as_posix()

	mut windows_add_sep := ''
	if !posix_path.starts_with('/') {
		windows_add_sep = '/'
	}

	return 'file://${windows_add_sep}${posix_path}'
}

// chmod modifies the permissions if the path is a file.
// See also: `os.chmod`.
pub fn (p Path) chmod(mode int) ! {
	os.chmod(p.str(), mode)!
}

// exists returns if the path is an existing file.
// See also: `os.exists`.
pub fn (p Path) exists() bool {
	return os.exists(p.str())
}

// expanduser expands `~` to the absolute path of the users homefolder.
// See also: `os.expand_tilde_to_home`.
//
// Example:
// ```v
// assert path('~/test') == '/user/home/test'
// ```
pub fn (p Path) expanduser() Path {
	// TODO: Python supports ~user, v does not
	return path(os.expand_tilde_to_home(p.str()))
}

// // glob returns a list of paths that
// // See also: `os.glob`.
// pub fn (p Path) glob(patterns ...string) ![]Path {
// 	glob_result := os.glob(...patterns)!
// 	// TODO
// 	mut paths := []Path{}
// 	for result in glob_result {
// 		paths << path(result)
// 	}
// 	return paths
// }

// pub fn (p Path) inode() os.FileMode {
// 	return os.inode(p.str())
// }

// is_absolute returns if the path is an absolute path.
// See also: `os.is_abs_path`.
pub fn (p Path) is_absolute() bool {
	return os.is_abs_path(p.str())
}

// is_block_device returns if the file the path is pointing to is a block device.
pub fn (p Path) is_block_device() bool {
	return match os.inode(p.str()).typ {
		.block_device { true }
		else { false }
	}
}

// is_block_device returns if the file the path is pointing to is a character device.
pub fn (p Path) is_char_device() bool {
	return match os.inode(p.str()).typ {
		.character_device { true }
		else { false }
	}
}

// is_dir returns if the path is a directory.
// See also: `os.is_dir`.
pub fn (p Path) is_dir() bool {
	return os.is_dir(p.str())
}

// is_fifo returns if the file the path is pointing to is a fifo file.
pub fn (p Path) is_fifo() bool {
	return match os.inode(p.str()).typ {
		.fifo { true }
		else { false }
	}
}

// is_file returns if the path is a file.
// See also: `os.is_file`.
pub fn (p Path) is_file() bool {
	return os.is_file(p.str())
}

// pub fn (p Path) is_mount() bool {
// 	// TODO
// }

// is_relative_to returns if the other path is relative to the current path.
// A path being relative to the other path means it's a child (or subpath) of
// the other path.
pub fn (p Path) is_relative_to(other Path) bool {
	window := other.parts

	if window.len > p.parts.len {
		return false
	}

	return p.parts[..window.len] == window
}

// is_socket returns if the file the path is pointing to is a socket.
pub fn (p Path) is_socket() bool {
	return match os.inode(p.str()).typ {
		.socket { true }
		else { false }
	}
}

// is_socket returns if the file the path is pointing to is a link.
// See also: `os.is_link`.
pub fn (p Path) is_link() bool {
	return os.is_link(p.str())
}

// is_regular returns if the file the path is pointing to is a regular file.
pub fn (p Path) is_regular() bool {
	return match os.inode(p.str()).typ {
		.regular { true }
		else { false }
	}
}

// iterdir returns a list of all files in the current directory, including
// hidden files but excluding `.` and `..`.
// See also: `os.ls`.
pub fn (p Path) iterdir() ![]Path {
	files := os.ls(p.str()) or {
		return PathError{p, 600, '${p} is not an existing directory', 'iterdir'}
	}
	// convert string filenames to paths
	return files.map(path(it))
}

// join two paths together.
// See also: `os.join_path_single`.
pub fn (p Path) join(other Path) Path {
	return path(os.join_path_single(p.str(), other.str()))
}

// link current path to the target path.
// See also: `os.link`.
pub fn (p Path) link(target Path) ! {
	os.link(p.str(), target.str())!
}

// mkdir creates the directory `path` points to.
// See also: `os.mkdir`.
pub fn (p Path) mkdir(params os.MkdirParams) !Path {
	// TODO: include mkdir_all?
	os.mkdir(p.str(), params)!
	return p
}

// open the `path` if it's a file.
// See also: `os.open_file`.
pub fn (p Path) open(mode string, options ...int) !os.File {
	return os.open_file(p.str(), mode, ...options) or {
		PathError{p, 200, 'cannot open "${p}"', 'open'}
	}
}

// parent returns the parent of the `path`.
//
// Example:
// ```v
// assert path('/a/b/c').parent() == path('/a/b')
// ```
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

// parents returns a list of the parents.
//
// Example:
// ```v
// assert path('/a/b/c').parents() == [path('/a/b'), path('/a'), path('/')]
// ```
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

// quoted returns a quoted version of path.
// See also: `os.quoted_path`.
pub fn (p Path) quoted() string {
	return os.quoted_path(p.str())
}

// relative_to returns the path relative to a given parent directory. Errors if
// `parent` is not actually a parent directory
//
// Example:
// ```v
// assert path('/a/b/c').relative_to(path('/')) == path('a/b/c')
// assert path('/a/b/c').relative_to(path('/a')) == path('b/c')
// assert path('/a/b/c').relative_to(path('/a/b')) == path('c')
// ```
pub fn (p Path) relative_to(parent Path) !Path {
	if !p.is_relative_to(parent) || p.parts == parent.parts {
		return PathError{p, 401, '"${p}" not a subpath of "${parent}"', 'relative_to'}
	}

	return path_from_parts(p.parts[parent.parts.len..])
}

// Make the path absolute, resolving all symlinks, `..`, etc. on the way and also
// normalizing it.
// See also: `os.resolve`.
pub fn (p Path) resolve() Path {
	return path(os.real_path(p.str()))
}

// rmdir removes the directory if path is a directory.
// See also: `os.rmdir`.
pub fn (p Path) rmdir() ! {
	os.rmdir(p.str())!
}

// str converts the path to a string representation.
pub fn (p Path) str() string {
	if p.parts.len == 1 && p.parts[0] == '' {
		return '/'
	}
	return p.parts.join(p.sep)
}

// symlink `path` to the target.
// See also: `os.symlink`.
pub fn (p Path) symlink(target Path) ! {
	os.symlink(p.str(), target.str())!
}

// touch creates a file at `path`.
// See also: `os.create`.
pub fn (p Path) touch() !Path {
	os.create(p.str())!
	return p
}

// unlike (remove) the file `path` points to.
// See also: `os.rm`.
pub fn (p Path) unlink() ! {
	os.rm(p.str())!
}

// with_name replaces the last element of the path with given `name`. The paths
// `/`, `.` and `..` have no name and thus can't be used with this function.
//
// Example:
// ```v
// assert path('pathlib/pathlib.v').with_name('helper.py') == path('pathlib/helper.py')
// ```
pub fn (p Path) with_name(name string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		return PathError{p, 20, 'path does not have a file name', 'with_name'}
	}

	if name.contains(p.sep) {
		return PathError{p, 23, "name can't contain path separator", 'with_name'}
	}

	mut parts := p.parts.clone()
	parts[parts.len - 1] = name

	return path_from_parts(parts)
}

// with_stem replaces the filename of the last element of path (the name without file
// extension) with given `stem`. The paths `/`, `.` and `..` have no
// name and thus can't be used with this function.
//
// Example:
// ```v
// assert path('pathlib/pathlib.v').with_stem('helper') == path('pathlib/helper.v')
// ```
pub fn (p Path) with_stem(stem string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		return PathError{p, 20, 'path does not have a file name', 'with_stem'}
	}

	if stem.contains(p.sep) {
		return PathError{p, 23, "stem can't contain path separator", 'with_stem'}
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.gz.zip, only .zip is the suffix and thus removed
	current_ext := current_name.all_after_last('.')
	filename_with_stem := stem + '.' + current_ext

	mut parts := p.parts.clone()
	parts[parts.len - 1] = filename_with_stem

	return path_from_parts(parts)
}

// with_suffix replaces the (last) suffix (file extension) of the last element of the path.
// Must start with a `.` and can contain multiple (e.g. `.tar.gz`). The paths
// `/`, `.` and `..` have no name and thus can't be used with this function.
//
// Example:
// ```v
// assert path('pathlib/pathlib.v').with_suffix('.py') == path('pathlib/pathlib.py')
// ```
pub fn (p Path) with_suffix(suffix string) !Path {
	current_name := p.name

	no_filename := ['', '.', '..']
	if current_name in no_filename {
		return PathError{p, 20, 'path does not have a file name', 'with_suffix'}
	}

	if !suffix.starts_with('.') {
		return PathError{p, 21, "suffix has to start with a `.'", 'with_suffix'}
	}

	if suffix.contains(p.sep) {
		return PathError{p, 22, "suffix can't contain path separator", 'with_suffix'}
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.tar.gz, only .gz is the suffix and thus removed
	current_stem := current_name.all_before_last('.')
	filename_with_suffix := current_stem + suffix

	mut parts := p.parts.clone()
	parts[parts.len - 1] = filename_with_suffix

	return path_from_parts(parts)
}

// write_text writes text to file at `path`.
// See also: `os.write_file`.
pub fn (p Path) write_text(text string) ! {
	os.write_file(p.str(), text) or {
		return PathError{p, 500, 'cannot write to "${p}"', 'write_text'}
	}
}

pub struct PathError {
	path Path
	code int
	msg  string
	func string
}

fn (err PathError) msg() string {
	mut func := ''
	if err.func != '' {
		func = '.${err.func}'
	}
	return "pathlib.path('${err.path}')${func}: ${err.msg}"
}

fn (err PathError) code() int {
	return err.code
}

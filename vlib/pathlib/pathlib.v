// pathlib is an object-orientated filesystem paths module. Most I/O operations
// are wrapped around functions from the `os` module. For example,
// `os.link('path/to/file.v', 'target.v')` becomes
// `path('path/to/file.v').link(path('target.v'))`. It is heavily inspired by
// Python's pathlib module (https://docs.python.org/3/library/pathlib.html).
module pathlib

import net.urllib
import os
import regex

// Path represents a filesystem path. It does not have to be an existing path
// on the current system. Path provides many methods for performing different
// operations on the path or perform I/O actions on the file / directory
// path points to.
[noinit]
pub struct Path {
	parts []string [required]
pub: // parts are the names of the folders, between the separators.
	drive  string // drive letter, including the `:`.
	name   string // name is the last part of the path.
	root   string // root is the first bit of the path, if absolute.
	sep    string = os.path_separator // sep is the system separator used in e.g. `Path.str()`.
	stem   string // stem (name without suffix) of the filename.
	suffix string // suffix ('file extension') of the filename, including the `.`.
}

// CONSTRUCTORS

// path_from_parts constructs a new path instance, but from a list of folders
// pointing to the file. It works naive, i.e. all parts are assumed to be
// normal path segments (so not containing one or more path separators).
//
// Example:
// ```v
// assert path_from_parts(['a', 'b', 'c']) == path('a/b/c')
// ```
fn path_from_parts(parts []string) Path {
	panic(10)
}

// path constructs a new path instance given a string representation of a path.
// Example:
// ```v
// assert path('.').absolute().str() == os.getwd()
// ```
pub fn path(path_string string) Path {
	if path_string.len == 0 || path_string == "." || path_string == "./" {
		return Path{
			parts: ['.']
		}
	}

	mut rest_path := path_string
	mut parts := []string{}
	mut drive := ''
	mut name := ''
	mut root := ''
	sep := os.path_separator
	mut stem := ''
	mut suffix := ''

	// work with right path separators from now on
	if os.path_separator == '/' {
		rest_path = rest_path.replace('\\', '/')
	} else {
		// fwd slashes don't always work on windows, so better to be safe
		rest_path = rest_path.replace('/', '\\')
	}

	// strip drive from path
	drive_re, _, _ := regex.regex_base(r"^[\a\A]:")
	if drive_re.matches_string(path_string) {
		// drive is two characters long
		drive = rest_path[..2]
		rest_path = rest_path[2..]
	}

	// two slashes as root is allowed, see
	// https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap04.html#tag_04_11
	mut two_slash_root_re, _, _ := regex.regex_base(r"^([\/])+")
	_, slash_count := two_slash_root_re.match_string(rest_path)
	if slash_count == 2 {
		root = rest_path[..2]
		rest_path = rest_path[2..]
	}

	// strip `/./` parts
	rest_path = rest_path.replace("/.", "")

	// reduce repeating separators to one
	mut sep_re, _, _ := regex.regex_base(r"([\\/])+")
	rest_path = sep_re.replace(rest_path, r"\0")

	// if path is absolute and root is not set yet
	path_starts_with_slash := rest_path.len > 0 && rest_path[0] in [`/`, `\\`]
	if path_starts_with_slash && root == '' {
		// [0..1]: want string instead of rune
		root = rest_path[0..1]
		rest_path = rest_path[1..]
	}

	// extract filename if any
	parts = rest_path.split_any(r'\/')
	if parts.len > 0 {
		name = parts.last()
		suffix = os.file_ext(name)
		stem = name.trim_string_right(suffix)
	}

	return Path{parts, drive, name, root, sep, stem, suffix}
}

// pub fn path_from_segments(...segments) Path {
// }

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
pub fn (p1 Path) / (p2 Path) !Path {
	// TODO: make more intelligent
	return p1.join(p2)!
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
// assert path('c:\\v\\vlib\\pathlib.v').as_posix() == 'c:/v/vlib/pathlib.v'
// ```
pub fn (p Path) as_posix() string {
	// posix uses '/' as separator
	return '${p.drive}${p.root}${p.parts.join("/")}'
}

// as_uri converts the path to a `file://` URI, possibly escaping the path where needed.
//
// Example:
// ```v
// assert path('/a/b/c d').as_uri() == 'file:///a/b/c%20d'
// ```
pub fn (p Path) as_uri() string {
	absolute_path := p.absolute()

	// first URL-escape the parts, because otherwise the separators are escaped
	escaped_parts := absolute_path.parts.map(urllib.path_escape(it))
	escaped_path := p.with_parts(escaped_parts)

	mut posix_path := escaped_path.as_posix()

	// for windows paths starting with the drive name
	if !posix_path.starts_with('/') {
		posix_path = '/' + posix_path
	}

	return 'file://${posix_path}'
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

// // group_id returns the group ID (GID) of the file.
// // Note: if you want the name of the group, use the GID and /etc/groups.
// pub fn (p Path) group_id() int {
// }

// inode returns inode type and permission information.
// See also: `os.inode`, `os.FileMode`.
pub fn (p Path) inode() os.FileMode {
	return os.inode(p.str())
}

// is_absolute returns if the path is an absolute path.
pub fn (p Path) is_absolute() bool {
	return p.root != ''
}

// is_block_device returns if path is a block device file.
pub fn (p Path) is_block_device() bool {
	return match os.inode(p.str()).typ {
		.block_device { true }
		else { false }
	}
}

// is_char_device returns if the path is a character device file.
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

// is_fifo returns if the path is a FIFO file.
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

	// drive and root also have to be the same
	if p.root != other.root || p.drive != other.drive {
		return false
	}

	return p.parts[..window.len] == window
}

// is_socket returns if the path is a socket file.
pub fn (p Path) is_socket() bool {
	return match os.inode(p.str()).typ {
		.socket { true }
		else { false }
	}
}

// is_link returns if the path is a link file.
// See also: `os.is_link`.
pub fn (p Path) is_link() bool {
	return os.is_link(p.str())
}

// is_regular returns if the path is a regular file.
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
	path_str := p.str()
	files := os.ls(path_str) or {
		return IError(PathError{
			path: p
			msg: '${p} is not an existing directory'
			func: 'iterdir'
		})
	}
	// convert string filenames to paths
	return files.map(path('${path_str}/${it}'))
}

// join two paths together. raises an error if the other path is absolute.
pub fn (p Path) join(other Path) !Path {
	if other.is_absolute() {
		return IError(PathError{
			path: p
			msg: 'other path ${other} is absolute'
			func: 'join'
		})
	}

	mut joined_path := p.parts.clone()
	joined_path << other.parts.clone()
	return path_from_parts(joined_path)
}

// link current path to the target path.
// See also: `os.link`.
pub fn (p Path) link(target Path) ! {
	os.link(p.str(), target.str())!
}

// mkdir creates the directory path points to.
// See also: `os.mkdir`.
pub fn (p Path) mkdir(params os.MkdirParams) ! {
	// TODO: include mkdir_all?
	os.mkdir(p.str(), params)!
}

// open the path if it's a file.
// See also: `os.open_file`.
pub fn (p Path) open(mode string, options ...int) !os.File {
	return os.open_file(p.str(), mode, ...options) or {
		IError(PathError{
			path: p
			msg: 'cannot open "${p}"'
			func: 'open'
		})
	}
}

// parent returns the parent of the path.
//
// Example:
// ```v
// assert path('/a/b/c').parent() == path('/a/b')
// ```
pub fn (p Path) parent() Path {
	// parent of root ('/') and current ('.') are itself
	if (p.parts.len == 0 && p.root != '') || p.name == '.' {
		return p
	}

	if p.parts.len == 1 {
		return path('.')
	}

	parts := p.parts#[..-1]

	return p.with_parts(parts)
}

// parents returns a list of the parents.
//
// Example:
// ```v
// assert path('/a/b/c').parents() == [path('/a/b'), path('/a'), path('/')]
// ```
pub fn (p Path) parents() []Path {
	if p.parts.len == 1 {
		return [path('.')]
	}

	parent_parts := p.parts#[..-1]
	mut parents := []Path{}

	for i, _ in parent_parts {
		parents << p.with_parts(parent_parts[..i + 1])
	}

	// TODO: drive+root is one part
	if p.is_absolute() {
		parents << p.with_parts([])
	}

	return parents.reverse()
}

// quoted returns a quoted version of path.
// See also: `os.quoted_path`.
pub fn (p Path) quoted() string {
	return os.quoted_path(p.str())
}

// read_text reads text from path and returns it as a string.
pub fn (p Path) read_text() !string {
	return os.read_file(p.str()) or {
		IError(PathError{
			path: p
			msg: 'cannot read from ${p}'
			func: 'read_text'
		})
	}
}

// relative_to returns the path relative to a given parent directory. Errors if
// `parent` is not actually a parent directory.
//
// Example:
// ```v
// assert path('/a/b/c').relative_to(path('/')) == path('a/b/c')
// assert path('/a/b/c').relative_to(path('/a')) == path('b/c')
// assert path('/a/b/c').relative_to(path('/a/b')) == path('c')
// ```
pub fn (p Path) relative_to(parent Path) !Path {
	if !p.is_relative_to(parent) || p.parts == parent.parts {
		IError(PathError{
			path: p
			msg: '"${p}" not a subpath of "${parent}"'
			func: 'relative_to'
		})
	}

	return path_from_parts(p.parts[parent.parts.len..])
}

// resolve all symlinks, make the path absolute, and resolve `..`, etc. on the
// way and also normalizing it.
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
	// if p.parts.len == 1 && p.parts[0] == '' {
	// 	return '/'
	// }
	return p.drive + p.root + p.parts.join(p.sep)
}

// symlink path to the target.
// See also: `os.symlink`.
pub fn (p Path) symlink(target Path) ! {
	os.symlink(p.str(), target.str())!
}

// touch creates a file at path.
// See also: `os.create`.
pub fn (p Path) touch() !Path {
	os.create(p.str())!
	return p
}

// unlink (remove) the file path points to.
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
		return IError(PathError{
			path: p
			msg: 'path does not have a file name'
			func: 'with_name'
		})
	}

	if name.contains(p.sep) {
		return IError(PathError{
			path: p
			msg: "name can't contain path separator"
			func: 'with_name'
		})
	}

	mut parts := p.parts.clone()
	parts[parts.len - 1] = name

	return path_from_parts(parts)
}

// with_parts returns the path where parts of it may be modified. Useful inside
// the `pathlib` module.
//
// Example:
// ```v
// p := path('a/b/c d.txt')
// escaped := p.parts.map(urllib.path_escape(it))
// assert p.with_parts(escaped).parts == ['a', 'b', 'c%20d.txt']
// ```
fn (p Path) with_parts(parts []string) Path {
	return Path{
		parts: parts
		drive: p.drive
		name: p.name
		root: p.root
		sep: p.sep
		stem: p.stem
		suffix: p.suffix
	}
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
		return IError(PathError{
			path: p
			msg: 'path does not have a file name'
			func: 'with_stem'
		})
	}

	if stem.contains(p.sep) {
		return IError(PathError{
			path: p
			msg: "stem can't contain path separator"
			func: 'with_stem'
		})
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
		return IError(PathError{
			path: p
			msg: 'path does not have a file name'
			func: 'with_suffix'
		})
	}

	if !suffix.starts_with('.') {
		return IError(PathError{
			path: p
			msg: 'suffix has to start with a `.`'
			func: 'with_suffix'
		})
	}

	if suffix.contains(p.sep) {
		return IError(PathError{
			path: p
			msg: 'suffix cannot contain path separator'
			func: 'with_suffix'
		})
	}

	// remove current suffix if it has one, otherwise use full name
	// in zipfile.tar.gz, only .gz is the suffix and thus removed
	current_stem := current_name.all_before_last('.')
	filename_with_suffix := current_stem + suffix

	mut parts := p.parts.clone()
	parts[parts.len - 1] = filename_with_suffix

	return path_from_parts(parts)
}

// write_text writes text to file at path.
// See also: `os.write_file`.
pub fn (p Path) write_text(text string) ! {
	os.write_file(p.str(), text) or {
		return IError(PathError{
			path: p
			msg: 'cannot write to "${p}"'
			func: 'write_text'
		})
	}
}

pub struct PathError {
	Error
	path Path
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

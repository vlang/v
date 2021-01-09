// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

pub struct Result {
pub:
	exit_code int
	output    string
	// stderr string // TODO
}

pub const (
	args          = []string{}
	max_path_len  = 4096
	wd_at_startup = getwd()
)

const (
	f_ok = 0
	x_ok = 1
	w_ok = 2
	r_ok = 4
)

[deprecated]
pub fn cp_r(osource_path string, odest_path string, overwrite bool) ? {
	eprintln('warning: `os.cp_r` has been deprecated, use `os.cp_all` instead')
	return cp_all(osource_path, odest_path, overwrite)
}

// cp_all will recursively copy `src` to `dst`,
// optionally overwriting files or dirs in `dst`.
pub fn cp_all(src string, dst string, overwrite bool) ? {
	source_path := real_path(src)
	dest_path := real_path(dst)
	if !exists(source_path) {
		return error("Source path doesn\'t exist")
	}
	// single file copy
	if !is_dir(source_path) {
		adjusted_path := if is_dir(dest_path) {
			join_path(dest_path, file_name(source_path))
		} else {
			dest_path
		}
		if exists(adjusted_path) {
			if overwrite {
				rm(adjusted_path)
			} else {
				return error('Destination file path already exist')
			}
		}
		cp(source_path, adjusted_path) ?
		return
	}
	if !is_dir(dest_path) {
		return error('Destination path is not a valid directory')
	}
	files := ls(source_path) ?
	for file in files {
		sp := join_path(source_path, file)
		dp := join_path(dest_path, file)
		if is_dir(sp) {
			mkdir(dp) ?
		}
		cp_all(sp, dp, overwrite) or {
			rmdir(dp)
			return error(err)
		}
	}
}

// mv_by_cp first copies the source file, and if it is copied successfully, deletes the source file.
// may be used when you are not sure that the source and target are on the same mount/partition.
pub fn mv_by_cp(source string, target string) ? {
	cp(source, target) ?
	rm(source) ?
}

// read_lines reads the file in `path` into an array of lines.
pub fn read_lines(path string) ?[]string {
	buf := read_file(path) ?
	return buf.split_into_lines()
}

// read_ulines reads the file in `path` into an array of ustring lines.
fn read_ulines(path string) ?[]ustring {
	lines := read_lines(path) ?
	// mut ulines := new_array(0, lines.len, sizeof(ustring))
	mut ulines := []ustring{}
	for myline in lines {
		// ulines[i] = ustr
		ulines << myline.ustring()
	}
	return ulines
}

// sigint_to_signal_name will translate `si` signal integer code to it's string code representation.
pub fn sigint_to_signal_name(si int) string {
	// POSIX signals:
	match si {
		1 { return 'SIGHUP' }
		2 { return 'SIGINT' }
		3 { return 'SIGQUIT' }
		4 { return 'SIGILL' }
		6 { return 'SIGABRT' }
		8 { return 'SIGFPE' }
		9 { return 'SIGKILL' }
		11 { return 'SIGSEGV' }
		13 { return 'SIGPIPE' }
		14 { return 'SIGALRM' }
		15 { return 'SIGTERM' }
		else {}
	}
	$if linux {
		// From `man 7 signal` on linux:
		match si {
			// TODO dependent on platform
			// works only on x86/ARM/most others
			10 /* , 30, 16 */ { return 'SIGUSR1' }
			12 /* , 31, 17 */ { return 'SIGUSR2' }
			17 /* , 20, 18 */ { return 'SIGCHLD' }
			18 /* , 19, 25 */ { return 'SIGCONT' }
			19 /* , 17, 23 */ { return 'SIGSTOP' }
			20 /* , 18, 24 */ { return 'SIGTSTP' }
			21 /* , 26 */ { return 'SIGTTIN' }
			22 /* , 27 */ { return 'SIGTTOU' }
			// /////////////////////////////
			5 { return 'SIGTRAP' }
			7 { return 'SIGBUS' }
			else {}
		}
	}
	return 'unknown'
}

[deprecated]
pub fn file_exists(_path string) bool {
	eprintln('warning: `os.file_exists` has been deprecated, use `os.exists` instead')
	return exists(_path)
}

[deprecated]
pub fn rmdir_recursive(path string) {
	eprintln('warning: `os.rmdir_recursive` has been deprecated, use `os.rmdir_all` instead')
	rmdir_all(path)
}

// rmdir_all recursively removes the specified directory.
pub fn rmdir_all(path string) ? {
	mut ret_err := ''
	items := ls(path) ?
	for item in items {
		if is_dir(join_path(path, item)) {
			rmdir_all(join_path(path, item))
		}
		rm(join_path(path, item)) or { ret_err = err }
	}
	rmdir(path) or { ret_err = err }
	if ret_err.len > 0 {
		return error(ret_err)
	}
}

// is_dir_empty will return a `bool` whether or not `path` is empty.
pub fn is_dir_empty(path string) bool {
	items := ls(path) or { return true }
	return items.len == 0
}

// file_ext will return the part after the last occurence of `.` in `path`.
// The `.` is included.
pub fn file_ext(path string) string {
	pos := path.last_index('.') or { return '' }
	return path[pos..]
}

// dir returns all but the last element of path, typically the path's directory.
// After dropping the final element, trailing slashes are removed.
// If the path is empty, dir returns ".". If the path consists entirely of separators,
// dir returns a single separator.
// The returned path does not end in a separator unless it is the root directory.
pub fn dir(path string) string {
	if path == '' {
		return '.'
	}
	pos := path.last_index(path_separator) or { return '.' }
	return path[..pos]
}

// base returns the last element of path.
// Trailing path separators are removed before extracting the last element.
// If the path is empty, base returns ".". If the path consists entirely of separators, base returns a
// single separator.
pub fn base(path string) string {
	if path == '' {
		return '.'
	}
	if path == path_separator {
		return path_separator
	}
	if path.ends_with(path_separator) {
		path2 := path[..path.len - 1]
		pos := path2.last_index(path_separator) or { return path2.clone() }
		return path2[pos + 1..]
	}
	pos := path.last_index(path_separator) or { return path.clone() }
	return path[pos + 1..]
}

// file_name will return all characters found after the last occurence of `path_separator`.
// file extension is included.
pub fn file_name(path string) string {
	return path.all_after_last(path_separator)
}

// input returns a one-line string from stdin, after printing a prompt.
pub fn input(prompt string) string {
	print(prompt)
	flush()
	return get_line()
}

// get_line returns a one-line string from stdin
pub fn get_line() string {
	str := get_raw_line()
	$if windows {
		return str.trim_right('\r\n')
	} $else {
		return str.trim_right('\n')
	}
}

// get_lines returns an array of strings read from from stdin.
// reading is stopped when an empty line is read.
pub fn get_lines() []string {
	mut line := ''
	mut inputstr := []string{}
	for {
		line = get_line()
		if line.len <= 0 {
			break
		}
		line = line.trim_space()
		inputstr << line
	}
	return inputstr
}

// get_lines_joined returns a string of the values read from from stdin.
// reading is stopped when an empty line is read.
pub fn get_lines_joined() string {
	mut line := ''
	mut inputstr := ''
	for {
		line = get_line()
		if line.len <= 0 {
			break
		}
		line = line.trim_space()
		inputstr += line
	}
	return inputstr
}

// user_os returns current user operating system name.
pub fn user_os() string {
	$if linux {
		return 'linux'
	}
	$if macos {
		return 'macos'
	}
	$if windows {
		return 'windows'
	}
	$if freebsd {
		return 'freebsd'
	}
	$if openbsd {
		return 'openbsd'
	}
	$if netbsd {
		return 'netbsd'
	}
	$if dragonfly {
		return 'dragonfly'
	}
	$if android {
		return 'android'
	}
	$if solaris {
		return 'solaris'
	}
	$if haiku {
		return 'haiku'
	}
	return 'unknown'
}

// home_dir returns path to the user's home directory.
pub fn home_dir() string {
	$if windows {
		return getenv('USERPROFILE')
	} $else {
		// println('home_dir() call')
		// res:= os.getenv('HOME')
		// println('res="$res"')
		return getenv('HOME')
	}
}

// write_file writes `text` data to a file in `path`.
pub fn write_file(path string, text string) ? {
	mut f := create(path) ?
	f.write(text.bytes())
	f.close()
}

// write_file_array writes the data in `buffer` to a file in `path`.
pub fn write_file_array(path string, buffer array) ? {
	mut f := create(path) ?
	f.write_bytes_at(buffer.data, (buffer.len * buffer.element_size), 0)
	f.close()
}

// executable_fallback is used when there is not a more platform specific and accurate implementation.
// It relies on path manipulation of os.args[0] and os.wd_at_startup, so it may not work properly in
// all cases, but it should be better, than just using os.args[0] directly.
fn executable_fallback() string {
	if args.len == 0 {
		// we are early in the bootstrap, os.args has not been initialized yet :-|
		return ''
	}
	mut exepath := args[0]
	$if windows {
		if !exepath.contains('.exe') {
			exepath += '.exe'
		}
	}
	if !is_abs_path(exepath) {
		if exepath.contains(path_separator) {
			exepath = join_path(wd_at_startup, exepath)
		} else {
			// no choice but to try to walk the PATH folders :-| ...
			foundpath := find_abs_path_of_executable(exepath) or { '' }
			if foundpath.len > 0 {
				exepath = foundpath
			}
		}
	}
	exepath = real_path(exepath)
	return exepath
}

// find_exe_path walks the environment PATH, just like most shell do, it returns
// the absolute path of the executable if found
pub fn find_abs_path_of_executable(exepath string) ?string {
	if is_abs_path(exepath) {
		return real_path(exepath)
	}
	mut res := ''
	paths := getenv('PATH').split(path_delimiter)
	for p in paths {
		found_abs_path := join_path(p, exepath)
		if exists(found_abs_path) && is_executable(found_abs_path) {
			res = found_abs_path
			break
		}
	}
	if res.len > 0 {
		return real_path(res)
	}
	return error('failed to find executable')
}

// exists_in_system_path returns `true` if `prog` exists in the system's PATH
pub fn exists_in_system_path(prog string) bool {
	find_abs_path_of_executable(prog) or { return false }
	return true
}

[deprecated]
pub fn dir_exists(path string) bool {
	eprintln('warning: `os.dir_exists` has been deprecated, use `os.is_dir` instead')
	return is_dir(path)
}

// is_file returns a `bool` indicating whether the given `path` is a file.
pub fn is_file(path string) bool {
	return exists(path) && !is_dir(path)
}

// is_abs_path returns `true` if `path` is absolute.
pub fn is_abs_path(path string) bool {
	$if windows {
		return path[0] == `/` ||  // incase we're in MingGW bash
		(path[0].is_letter() && path[1] == `:`)
	}
	return path[0] == `/`
}

// join_path returns a path as string from input string parameter(s).
pub fn join_path(base string, dirs ...string) string {
	mut result := []string{}
	result << base.trim_right('\\/')
	for d in dirs {
		result << d
	}
	return result.join(path_separator)
}

// walk_ext returns a recursive list of all files in `path` ending with `ext`.
pub fn walk_ext(path string, ext string) []string {
	if !is_dir(path) {
		return []
	}
	mut files := ls(path) or { return [] }
	mut res := []string{}
	separator := if path.ends_with(path_separator) { '' } else { path_separator }
	for file in files {
		if file.starts_with('.') {
			continue
		}
		p := path + separator + file
		if is_dir(p) && !is_link(p) {
			res << walk_ext(p, ext)
		} else if file.ends_with(ext) {
			res << p
		}
	}
	return res
}

// walk recursively traverses the given directory `path`.
// When a file is encountred it will call the callback function with current file as argument.
pub fn walk(path string, f fn (string)) {
	if !is_dir(path) {
		return
	}
	mut files := ls(path) or { return }
	for file in files {
		p := path + path_separator + file
		if is_dir(p) && !is_link(p) {
			walk(p, f)
		} else if exists(p) {
			f(p)
		}
	}
	return
}

// log will print "os.log: "+`s` ...
pub fn log(s string) {
	println('os.log: ' + s)
}

[deprecated]
pub fn flush_stdout() {
	eprintln('warning: `os.flush_stdout` has been deprecated, use `os.flush` instead')
	flush()
}

// mkdir_all will create a valid full path of all directories given in `path`.
pub fn mkdir_all(path string) ? {
	mut p := if path.starts_with(path_separator) { path_separator } else { '' }
	path_parts := path.trim_left(path_separator).split(path_separator)
	for subdir in path_parts {
		p += subdir + path_separator
		if exists(p) && is_dir(p) {
			continue
		}
		mkdir(p) or { return error('folder: $p, error: $err') }
	}
}

// cache_dir returns the path to a *writable* user specific folder, suitable for writing non-essential data.
pub fn cache_dir() string {
	// See: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
	// There is a single base directory relative to which user-specific non-essential
	// (cached) data should be written. This directory is defined by the environment
	// variable $XDG_CACHE_HOME.
	// $XDG_CACHE_HOME defines the base directory relative to which user specific
	// non-essential data files should be stored. If $XDG_CACHE_HOME is either not set
	// or empty, a default equal to $HOME/.cache should be used.
	$if !windows {
		xdg_cache_home := getenv('XDG_CACHE_HOME')
		if xdg_cache_home != '' {
			return xdg_cache_home
		}
	}
	cdir := join_path(home_dir(), '.cache')
	if !is_dir(cdir) && !is_link(cdir) {
		mkdir(cdir) or { panic(err) }
	}
	return cdir
}

// temp_dir returns the path to a folder, that is suitable for storing temporary files.
pub fn temp_dir() string {
	mut path := getenv('TMPDIR')
	$if windows {
		if path == '' {
			// TODO see Qt's implementation?
			// https://doc.qt.io/qt-5/qdir.html#tempPath
			// https://github.com/qt/qtbase/blob/e164d61ca8263fc4b46fdd916e1ea77c7dd2b735/src/corelib/io/qfilesystemengine_win.cpp#L1275
			path = getenv('TEMP')
			if path == '' {
				path = getenv('TMP')
			}
			if path == '' {
				path = 'C:/tmp'
			}
		}
	}
	$if android {
		// TODO test+use '/data/local/tmp' on Android before using cache_dir()
		if path == '' {
			path = cache_dir()
		}
	}
	if path == '' {
		path = '/tmp'
	}
	return path
}

fn default_vmodules_path() string {
	return join_path(home_dir(), '.vmodules')
}

// vmodules_dir returns the path to a folder, where v stores its global modules.
pub fn vmodules_dir() string {
	paths := vmodules_paths()
	if paths.len > 0 {
		return paths[0]
	}
	return default_vmodules_path()
}

// vmodules_paths returns a list of paths, where v looks up for modules.
// You can customize it through setting the environment variable VMODULES
pub fn vmodules_paths() []string {
	mut path := getenv('VMODULES')
	if path == '' {
		path = default_vmodules_path()
	}
	list := path.split(path_delimiter).map(it.trim_right(path_separator))
	return list
}

// resource_abs_path returns an absolute path, for the given `path`.
// (the path is expected to be relative to the executable program)
// See https://discordapp.com/channels/592103645835821068/592294828432424960/630806741373943808
// It gives a convenient way to access program resources like images, fonts, sounds and so on,
// *no matter* how the program was started, and what is the current working directory.
pub fn resource_abs_path(path string) string {
	mut base_path := real_path(dir(executable()))
	vresource := getenv('V_RESOURCE_PATH')
	if vresource.len != 0 {
		base_path = vresource
	}
	return real_path(join_path(base_path, path))
}

pub struct Uname {
pub mut:
	sysname  string
	nodename string
	release  string
	version  string
	machine  string
}

// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

import strings

pub const max_path_len = 4096

pub const wd_at_startup = getwd()

const f_ok = 0

const x_ok = 1

const w_ok = 2

const r_ok = 4

pub struct Result {
pub:
	exit_code int
	output    string
	// stderr string // TODO
}

pub struct Command {
mut:
	f voidptr
pub mut:
	eof       bool
	exit_code int
pub:
	path            string
	redirect_stdout bool
}

[unsafe]
pub fn (mut result Result) free() {
	unsafe { result.output.free() }
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
		rexepath := exepath.replace_each(['/', path_separator, '\\', path_separator])
		if rexepath.contains(path_separator) {
			exepath = join_path_single(os.wd_at_startup, exepath)
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

// cp_all will recursively copy `src` to `dst`,
// optionally overwriting files or dirs in `dst`.
pub fn cp_all(src string, dst string, overwrite bool) ! {
	source_path := real_path(src)
	dest_path := real_path(dst)
	if !exists(source_path) {
		return error("Source path doesn't exist")
	}
	// single file copy
	if !is_dir(source_path) {
		fname := file_name(source_path)
		adjusted_path := if is_dir(dest_path) {
			join_path_single(dest_path, fname)
		} else {
			dest_path
		}
		if exists(adjusted_path) {
			if overwrite {
				rm(adjusted_path)!
			} else {
				return error('Destination file path already exist')
			}
		}
		cp(source_path, adjusted_path)!
		return
	}
	if !exists(dest_path) {
		mkdir(dest_path)!
	}
	if !is_dir(dest_path) {
		return error('Destination path is not a valid directory')
	}
	files := ls(source_path)!
	for file in files {
		sp := join_path_single(source_path, file)
		dp := join_path_single(dest_path, file)
		if is_dir(sp) {
			if !exists(dp) {
				mkdir(dp)!
			}
		}
		cp_all(sp, dp, overwrite) or {
			rmdir(dp) or { return err }
			return err
		}
	}
}

// mv_by_cp first copies the source file, and if it is copied successfully, deletes the source file.
// may be used when you are not sure that the source and target are on the same mount/partition.
pub fn mv_by_cp(source string, target string) ! {
	cp(source, target)!
	rm(source)!
}

// read_lines reads the file in `path` into an array of lines.
[manualfree]
pub fn read_lines(path string) ![]string {
	buf := read_file(path)!
	res := buf.split_into_lines()
	unsafe { buf.free() }
	return res
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

// rmdir_all recursively removes the specified directory.
pub fn rmdir_all(path string) ! {
	mut ret_err := ''
	items := ls(path)!
	for item in items {
		fullpath := join_path_single(path, item)
		if is_dir(fullpath) && !is_link(fullpath) {
			rmdir_all(fullpath) or { ret_err = err.msg() }
		} else {
			rm(fullpath) or { ret_err = err.msg() }
		}
	}
	rmdir(path) or { ret_err = err.msg() }
	if ret_err.len > 0 {
		return error(ret_err)
	}
}

// is_dir_empty will return a `bool` whether or not `path` is empty.
// Note that it will return `true` if `path` does not exist.
[manualfree]
pub fn is_dir_empty(path string) bool {
	items := ls(path) or { return true }
	res := items.len == 0
	unsafe { items.free() }
	return res
}

// file_ext will return the part after the last occurence of `.` in `path`.
// The `.` is included.
// Examples:
// ```v
// assert os.file_ext('file.v') == '.v'
// assert os.file_ext('.ignore_me') == ''
// assert os.file_ext('.') == ''
// ```
pub fn file_ext(path string) string {
	if path.len < 3 {
		return empty_str
	}
	pos := path.last_index(dot_str) or { return empty_str }
	if pos + 1 >= path.len || pos == 0 {
		return empty_str
	}
	return path[pos..]
}

// dir returns all but the last element of path, typically the path's directory.
// After dropping the final element, trailing slashes are removed.
// If the path is empty, dir returns ".". If the path consists entirely of separators,
// dir returns a single separator.
// The returned path does not end in a separator unless it is the root directory.
pub fn dir(opath string) string {
	if opath == '' {
		return '.'
	}
	path := opath.replace_each(['/', path_separator, '\\', path_separator])
	pos := path.last_index(path_separator) or { return '.' }
	if pos == 0 && path_separator == '/' {
		return '/'
	}
	return path[..pos]
}

// base returns the last element of path.
// Trailing path separators are removed before extracting the last element.
// If the path is empty, base returns ".". If the path consists entirely of separators, base returns a
// single separator.
pub fn base(opath string) string {
	if opath == '' {
		return '.'
	}
	path := opath.replace_each(['/', path_separator, '\\', path_separator])
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
pub fn file_name(opath string) string {
	path := opath.replace_each(['/', path_separator, '\\', path_separator])
	return path.all_after_last(path_separator)
}

// input_opt returns a one-line string from stdin, after printing a prompt.
// In the event of error (end of input), it returns `none`.
pub fn input_opt(prompt string) ?string {
	print(prompt)
	flush()
	res := get_raw_line()
	if res.len > 0 {
		return res.trim_right('\r\n')
	}
	return none
}

// input returns a one-line string from stdin, after printing a prompt.
// In the event of error (end of input), it returns '<EOF>'.
pub fn input(prompt string) string {
	res := input_opt(prompt) or { return '<EOF>' }
	return res
}

// get_line returns a one-line string from stdin
pub fn get_line() string {
	str := get_raw_line()
	$if windows {
		return str.trim_right('\r\n')
	}
	return str.trim_right('\n')
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

// get_raw_lines_joined reads *all* input lines from stdin.
// It returns them as one large string. Note: unlike os.get_lines_joined,
// empty lines (that contain only `\r\n` or `\n`), will be present in
// the output.
// Reading is stopped, only on EOF of stdin.
pub fn get_raw_lines_joined() string {
	mut line := ''
	mut lines := []string{}
	for {
		line = get_raw_line()
		if line.len <= 0 {
			break
		}
		lines << line
	}
	res := lines.join('')
	return res
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
	$if termux {
		return 'termux'
	}
	$if solaris {
		return 'solaris'
	}
	$if haiku {
		return 'haiku'
	}
	$if serenity {
		return 'serenity'
	}
	$if vinix {
		return 'vinix'
	}
	if getenv('TERMUX_VERSION') != '' {
		return 'termux'
	}
	return 'unknown'
}

// user_names returns an array of the name of every user on the system.
pub fn user_names() ![]string {
	$if windows {
		result := execute('wmic useraccount get name')
		if result.exit_code != 0 {
			return error('Failed to get user names. Exited with code $result.exit_code: $result.output')
		}
		mut users := result.output.split_into_lines()
		// windows command prints an empty line at the end of output
		users.delete(users.len - 1)
		return users
	} $else {
		lines := read_lines('/etc/passwd')!
		mut users := []string{cap: lines.len}
		for line in lines {
			end_name := line.index(':') or { line.len }
			users << line[0..end_name]
		}
		return users
	}
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

// expand_tilde_to_home expands the character `~` in `path` to the user's home directory.
// See also `home_dir()`.
pub fn expand_tilde_to_home(path string) string {
	if path == '~' {
		return home_dir().trim_right(path_separator)
	}
	if path.starts_with('~' + path_separator) {
		return path.replace_once('~' + path_separator, home_dir().trim_right(path_separator) +
			path_separator)
	}
	return path
}

// write_file writes `text` data to the file in `path`.
// If `path` exists, the contents of `path` will be overwritten with the contents of `text`.
pub fn write_file(path string, text string) ! {
	mut f := create(path)!
	unsafe { f.write_full_buffer(text.str, usize(text.len))! }
	f.close()
}

pub struct ExecutableNotFoundError {
	Error
}

pub fn (err ExecutableNotFoundError) msg() string {
	return 'os: failed to find executable'
}

fn error_failed_to_find_executable() IError {
	return &ExecutableNotFoundError{}
}

// find_abs_path_of_executable walks the environment PATH, just like most shell do, it returns
// the absolute path of the executable if found
pub fn find_abs_path_of_executable(exepath string) !string {
	if exepath == '' {
		return error('expected non empty `exepath`')
	}

	for suffix in executable_suffixes {
		fexepath := exepath + suffix
		if is_abs_path(fexepath) {
			return real_path(fexepath)
		}
		mut res := ''
		path := getenv('PATH')
		paths := path.split(path_delimiter)
		for p in paths {
			found_abs_path := join_path_single(p, fexepath)
			$if trace_find_abs_path_of_executable ? {
				dump(found_abs_path)
			}
			if exists(found_abs_path) && is_executable(found_abs_path) {
				res = found_abs_path
				break
			}
		}
		if res.len > 0 {
			return real_path(res)
		}
	}
	return error_failed_to_find_executable()
}

// exists_in_system_path returns `true` if `prog` exists in the system's PATH
pub fn exists_in_system_path(prog string) bool {
	find_abs_path_of_executable(prog) or { return false }
	return true
}

// is_file returns a `bool` indicating whether the given `path` is a file.
pub fn is_file(path string) bool {
	return exists(path) && !is_dir(path)
}

// join_path returns a path as string from input string parameter(s).
[manualfree]
pub fn join_path(base string, dirs ...string) string {
	// TODO: fix freeing of `dirs` when the passed arguments are variadic,
	// but do not free the arr, when `os.join_path(base, ...arr)` is called.
	mut sb := strings.new_builder(base.len + dirs.len * 50)
	defer {
		unsafe { sb.free() }
	}
	sbase := base.trim_right('\\/')
	defer {
		unsafe { sbase.free() }
	}
	sb.write_string(sbase)
	for d in dirs {
		sb.write_string(path_separator)
		sb.write_string(d)
	}
	return sb.str()
}

// join_path_single appends the `elem` after `base`, using a platform specific
// path_separator.
[manualfree]
pub fn join_path_single(base string, elem string) string {
	// TODO: deprecate this and make it `return os.join_path(base, elem)`,
	// when freeing variadic args vs ...arr is solved in the compiler
	mut sb := strings.new_builder(base.len + elem.len + 1)
	defer {
		unsafe { sb.free() }
	}
	sbase := base.trim_right('\\/')
	defer {
		unsafe { sbase.free() }
	}
	sb.write_string(sbase)
	sb.write_string(path_separator)
	sb.write_string(elem)
	return sb.str()
}

// walk_ext returns a recursive list of all files in `path` ending with `ext`.
pub fn walk_ext(path string, ext string) []string {
	mut res := []string{}
	impl_walk_ext(path, ext, mut res)
	return res
}

fn impl_walk_ext(path string, ext string, mut out []string) {
	if !is_dir(path) {
		return
	}
	mut files := ls(path) or { return }
	separator := if path.ends_with(path_separator) { '' } else { path_separator }
	for file in files {
		if file.starts_with('.') {
			continue
		}
		p := path + separator + file
		if is_dir(p) && !is_link(p) {
			impl_walk_ext(p, ext, mut out)
		} else if file.ends_with(ext) {
			out << p
		}
	}
}

// walk traverses the given directory `path`.
// When a file is encountred, it will call the callback `f` with current file as argument.
// Note: walk can be called even for deeply nested folders,
// since it does not recurse, but processes them iteratively.
pub fn walk(path string, f fn (string)) {
	if path.len == 0 {
		return
	}
	if !is_dir(path) {
		return
	}
	mut remaining := []string{cap: 1000}
	clean_path := path.trim_right(path_separator)
	$if windows {
		remaining << clean_path.replace('/', '\\')
	} $else {
		remaining << clean_path
	}
	for remaining.len > 0 {
		cpath := remaining.pop()
		pkind := kind_of_existing_path(cpath)
		if pkind.is_link || !pkind.is_dir {
			f(cpath)
			continue
		}
		mut files := ls(cpath) or { continue }
		for idx := files.len - 1; idx >= 0; idx-- {
			remaining << cpath + path_separator + files[idx]
		}
	}
}

// FnWalkContextCB is used to define the callback functions, passed to os.walk_context
pub type FnWalkContextCB = fn (voidptr, string)

// walk_with_context traverses the given directory `path`.
// For each encountred file *and* directory, it will call your `fcb` callback,
// passing it the arbitrary `context` in its first parameter,
// and the path to the file in its second parameter.
// Note: walk_with_context can be called even for deeply nested folders,
// since it does not recurse, but processes them iteratively.
pub fn walk_with_context(path string, context voidptr, fcb FnWalkContextCB) {
	if path.len == 0 {
		return
	}
	if !is_dir(path) {
		return
	}
	mut remaining := []string{cap: 1000}
	clean_path := path.trim_right(path_separator)
	$if windows {
		remaining << clean_path.replace('/', '\\')
	} $else {
		remaining << clean_path
	}
	mut loops := 0
	for remaining.len > 0 {
		loops++
		cpath := remaining.pop()
		// call `fcb` for everything, but the initial folder:
		if loops > 1 {
			fcb(context, cpath)
		}
		pkind := kind_of_existing_path(cpath)
		if pkind.is_link || !pkind.is_dir {
			continue
		}
		mut files := ls(cpath) or { continue }
		for idx := files.len - 1; idx >= 0; idx-- {
			remaining << cpath + path_separator + files[idx]
		}
	}
}

// log will print "os.log: "+`s` ...
pub fn log(s string) {
	println('os.log: ' + s)
}

[params]
pub struct MkdirParams {
	mode u32 = 0o777 // note that the actual mode is affected by the process's umask
}

// mkdir_all will create a valid full path of all directories given in `path`.
pub fn mkdir_all(opath string, params MkdirParams) ! {
	path := opath.replace('/', path_separator)
	mut p := if path.starts_with(path_separator) { path_separator } else { '' }
	path_parts := path.trim_left(path_separator).split(path_separator)
	for subdir in path_parts {
		p += subdir + path_separator
		if exists(p) && is_dir(p) {
			continue
		}
		mkdir(p, params) or { return error('folder: $p, error: $err') }
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
	xdg_cache_home := getenv('XDG_CACHE_HOME')
	if xdg_cache_home != '' {
		return xdg_cache_home
	}
	cdir := join_path_single(home_dir(), '.cache')
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
	$if macos {
		// avoid /var/folders/6j/cmsk8gd90pd.... on macs
		return '/tmp'
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

// vtmp_dir returns the path to a folder, that is writable to V programs, *and* specific
// to the OS user. It can be overriden by setting the env variable `VTMP`.
pub fn vtmp_dir() string {
	mut vtmp := getenv('VTMP')
	if vtmp.len > 0 {
		return vtmp
	}
	uid := getuid()
	vtmp = join_path_single(temp_dir(), 'v_$uid')
	if !exists(vtmp) || !is_dir(vtmp) {
		// create a new directory, that is private to the user:
		mkdir_all(vtmp, mode: 0o700) or { panic(err) }
	}
	setenv('VTMP', vtmp, true)
	return vtmp
}

fn default_vmodules_path() string {
	hdir := home_dir()
	res := join_path_single(hdir, '.vmodules')
	return res
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
// [manualfree]
pub fn vmodules_paths() []string {
	mut path := getenv('VMODULES')
	if path == '' {
		// unsafe { path.free() }
		path = default_vmodules_path()
	}
	defer {
		// unsafe { path.free() }
	}
	splitted := path.split(path_delimiter)
	defer {
		// unsafe { splitted.free() }
	}
	mut list := []string{cap: splitted.len}
	for i in 0 .. splitted.len {
		si := splitted[i]
		trimmed := si.trim_right(path_separator)
		list << trimmed
		// unsafe { trimmed.free() }
		// unsafe { si.free() }
	}
	return list
}

// resource_abs_path returns an absolute path, for the given `path`.
// (the path is expected to be relative to the executable program)
// See https://discordapp.com/channels/592103645835821068/592294828432424960/630806741373943808
// It gives a convenient way to access program resources like images, fonts, sounds and so on,
// *no matter* how the program was started, and what is the current working directory.
[manualfree]
pub fn resource_abs_path(path string) string {
	exe := executable()
	dexe := dir(exe)
	mut base_path := real_path(dexe)
	vresource := getenv('V_RESOURCE_PATH')
	if vresource.len != 0 {
		unsafe { base_path.free() }
		base_path = vresource
	}
	fp := join_path_single(base_path, path)
	res := real_path(fp)
	unsafe {
		fp.free()
		vresource.free()
		base_path.free()
		dexe.free()
		exe.free()
	}
	return res
}

pub struct Uname {
pub mut:
	sysname  string
	nodename string
	release  string
	version  string
	machine  string
}

pub fn execute_or_panic(cmd string) Result {
	res := execute(cmd)
	if res.exit_code != 0 {
		eprintln('failed    cmd: $cmd')
		eprintln('failed   code: $res.exit_code')
		panic(res.output)
	}
	return res
}

pub fn execute_or_exit(cmd string) Result {
	res := execute(cmd)
	if res.exit_code != 0 {
		eprintln('failed    cmd: $cmd')
		eprintln('failed   code: $res.exit_code')
		eprintln(res.output)
		exit(1)
	}
	return res
}

// quoted path - return a quoted version of the path, depending on the platform.
pub fn quoted_path(path string) string {
	$if windows {
		return if path.ends_with(path_separator) { '"${path + path_separator}"' } else { '"$path"' }
	} $else {
		return "'$path'"
	}
}

// config_dir returns the path to the user configuration directory (depending on the platform).
// On windows, that is `%AppData%`.
// On macos, that is `~/Library/Application Support`.
// On the rest, that is `$XDG_CONFIG_HOME`, or if that is not available, `~/.config`.
// If the path cannot be determined, it returns an error.
// (for example, when $HOME on linux, or %AppData% on windows is not defined)
pub fn config_dir() !string {
	$if windows {
		app_data := getenv('AppData')
		if app_data != '' {
			return app_data
		}
	} $else $if macos || darwin || ios {
		home := home_dir()
		if home != '' {
			return home + '/Library/Application Support'
		}
	} $else {
		xdg_home := getenv('XDG_CONFIG_HOME')
		if xdg_home != '' {
			return xdg_home
		}
		home := home_dir()
		if home != '' {
			return home + '/.config'
		}
	}
	return error('Cannot find config directory')
}

[deprecated: 'use os.ensure_folder_is_writable instead']
pub fn is_writable_folder(folder string) !bool {
	ensure_folder_is_writable(folder)!
	return true
}

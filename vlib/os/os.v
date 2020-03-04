// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

import filepath

#include <sys/stat.h> // #include <signal.h>
#include <errno.h>

/*
struct dirent {
     d_ino int
     d_off int
	d_reclen u16
	d_type byte
	d_name [256]byte
}
*/

struct C.dirent {
	d_name byteptr
}

fn C.readdir(voidptr) C.dirent


pub const (
	args = []string
	MAX_PATH = 4096
)

pub struct File {
	cfile  voidptr // Using void* instead of FILE*
pub:
	fd     int
mut:
	opened bool
}

struct FileInfo {
	name string
	size int
}

struct C.stat {
	st_size  int
	st_mode  u32
	st_mtime int
}

struct C.DIR {}

// struct C.dirent {
// d_name byteptr
// }
struct C.sigaction {
mut:
	sa_mask      int
	sa_sigaction int
	sa_flags     int
}

fn C.getline(voidptr, voidptr, voidptr) int


fn C.ftell(fp voidptr) int


fn C.getenv(byteptr) &char


fn C.sigaction(int, voidptr, int)


fn C.open(charptr, int, int) int


fn C.fdopen(int, string) voidptr


pub fn (f File) is_opened() bool {
	return f.opened
}

// read_bytes reads an amount of bytes from the beginning of the file
pub fn (f &File) read_bytes(size int) []byte {
	return f.read_bytes_at(size, 0)
}

// read_bytes_at reads an amount of bytes at the given position in the file
pub fn (f &File) read_bytes_at(size, pos int) []byte {
	mut arr := [`0`].repeat(size)
	C.fseek(f.cfile, pos, C.SEEK_SET)
	nreadbytes := C.fread(arr.data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_SET)
	return arr[0..nreadbytes]
}


pub fn read_bytes(path string) ?[]byte {
	mut fp := vfopen(path, 'rb')
	if isnil(fp) {
		return error('failed to open file "$path"')
	}
	C.fseek(fp, 0, C.SEEK_END)
	fsize := C.ftell(fp)
	C.rewind(fp)
	mut res := [`0`].repeat(fsize)
	nr_read_elements := C.fread(res.data, fsize, 1, fp)
	C.fclose(fp)
	return res[0..nr_read_elements * fsize]
}

// read_file reads the file in `path` and returns the contents.
pub fn read_file(path string) ?string {
	mode := 'rb'
	mut fp := vfopen(path, mode)
	if isnil(fp) {
		return error('failed to open file "$path"')
	}
	defer { C.fclose(fp) }
	C.fseek(fp, 0, C.SEEK_END)
	fsize := C.ftell(fp)
	// C.fseek(fp, 0, SEEK_SET)  // same as `C.rewind(fp)` below
	C.rewind(fp)
	mut str := &byte(0)
	unsafe { str = malloc(fsize + 1) }
	C.fread(str, fsize, 1, fp)
	str[fsize] = 0
	return string(str,fsize)
}

// file_size returns the size of the file located in `path`.
pub fn file_size(path string) int {
	mut s := C.stat{}
	$if windows {
		C._wstat(path.to_wide(), voidptr(&s))
	} $else {
		C.stat(charptr(path.str), &s)
	}
	return s.st_size
}

pub fn mv(old, new string) {
	$if windows {
		C._wrename(old.to_wide(), new.to_wide())
	} $else {
		C.rename(charptr(old.str), charptr(new.str))
	}
}

fn C.CopyFile(&u32, &u32, int) int
// TODO implement actual cp for linux
pub fn cp(old, new string) ?bool {
	$if windows {
		_old := old.replace('/', '\\')
		_new := new.replace('/', '\\')
		C.CopyFile(_old.to_wide(), _new.to_wide(), false)
		result := C.GetLastError()
		if result == 0 {
			return true
		}
		else {
			return error_with_code('failed to copy $old to $new', int(result))
		}
	} $else {
		os.system('cp $old $new')
		return true // TODO make it return true or error when cp for linux is implemented
	}
}

[deprecated]
pub fn cp_r(osource_path, odest_path string, overwrite bool) ?bool {
	panic('Use `os.cp_all` instead of `os.cp_r`')
}

pub fn cp_all(osource_path, odest_path string, overwrite bool) ?bool {
	source_path := os.realpath(osource_path)
	dest_path := os.realpath(odest_path)
	if !os.exists(source_path) {
		return error("Source path doesn\'t exist")
	}
	// single file copy
	if !os.is_dir(source_path) {
		adjasted_path := if os.is_dir(dest_path) { filepath.join(dest_path,filepath.filename(source_path)) } else { dest_path }
		if os.exists(adjasted_path) {
			if overwrite {
				os.rm(adjasted_path)
			}
			else {
				return error('Destination file path already exist')
			}
		}
		os.cp(source_path, adjasted_path) or {
			return error(err)
		}
		return true
	}
	if !os.is_dir(dest_path) {
		return error('Destination path is not a valid directory')
	}
	files := os.ls(source_path) or {
		return error(err)
	}
	for file in files {
		sp := filepath.join(source_path,file)
		dp := filepath.join(dest_path,file)
		if os.is_dir(sp) {
			os.mkdir(dp) or {
				panic(err)
			}
		}
		cp_all(sp, dp, overwrite) or {
			os.rmdir(dp)
			panic(err)
		}
	}
	return true
}

// mv_by_cp first copies the source file, and if it is copied successfully, deletes the source file.
// mv_by_cp may be used when you are not sure that the source and target are on the same mount/partition.
pub fn mv_by_cp(source string, target string) ?bool {
	os.cp(source, target) or {
		return error(err)
	}
	os.rm(source)
	return true
}

fn vfopen(path, mode string) &C.FILE {
	$if windows {
		return C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		return C.fopen(charptr(path.str), charptr(mode.str))
	}
}

// read_lines reads the file in `path` into an array of lines.
pub fn read_lines(path string) ?[]string {
	buf := read_file(path) or {
		return err
	}
	return buf.split_into_lines()
}

fn read_ulines(path string) ?[]ustring {
	lines := read_lines(path) or {
		return err
	}
	// mut ulines := new_array(0, lines.len, sizeof(ustring))
	mut ulines := []ustring
	for myline in lines {
		// ulines[i] = ustr
		ulines << myline.ustring()
	}
	return ulines
}

pub fn open_append(path string) ?File {
	mut file := File{}
	$if windows {
		wpath := path.replace('/', '\\').to_wide()
		mode := 'ab'
		file = File{
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str
		file = File{
			cfile: C.fopen(charptr(cpath), 'ab')
		}
	}
	if isnil(file.cfile) {
		return error('failed to create(append) file "$path"')
	}
	file.opened = true
	return file
}

// open_file can be used to open or create a file with custom flags and permissions and returns a `File` object
pub fn open_file(path string, mode string, options ...int) ?File {
	mut flags := 0
	for m in mode {
		match m {
			`r` { flags |= O_RDONLY }
			`w` { flags |= O_CREATE | O_TRUNC }
			`a` { flags |= O_CREATE | O_APPEND }
			`s` { flags |= O_SYNC }
			`n` { flags |= O_NONBLOCK }
			`c` { flags |= O_NOCTTY }
			`+` { flags |= O_RDWR }
			else {}
		}
	}

	mut permission := 0o666
	if options.len > 0 {
		permission = options[0]
	}

	$if windows {
		if permission < 0o600 {
			permission = 0x0100
		}
		else {
			permission = 0x0100 | 0x0080
		}
	}

	mut p := path
	$if windows {
		p = path.replace('/', '\\')
	}

	fd := C.open(charptr(p.str), flags, permission)
	if fd == -1 {
		return error(posix_get_error_msg(C.errno))
	}

	cfile := C.fdopen(fd, charptr(mode.str))
	if isnil(cfile) {
		return error('Failed to open or create file "$path"')
	}

	return File{
		cfile: cfile
		fd: fd
		opened: true
	}
}

pub fn (f mut File) write_bytes_at(data voidptr, size, pos int) {
	//$if linux {
	//}
	//$else {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
	//}
}

pub fn (f mut File) flush() {
	if !f.opened {
		return
	}
	C.fflush(f.cfile)
}

// system starts the specified command, waits for it to complete, and returns its code.
fn vpopen(path string) voidptr {
	// *C.FILE {
	$if windows {
		mode := 'rb'
		wpath := path.to_wide()
		return C._wpopen(wpath, mode.to_wide())
	} $else {
		cpath := path.str
		return C.popen(cpath, 'r')
	}
}

fn posix_wait4_to_exit_status(waitret int) (int,bool) {
	$if windows {
		return waitret,false
	} $else {
		mut ret := 0
		mut is_signaled := true
		// (see man system, man 2 waitpid: C macro WEXITSTATUS section)
		if C.WIFEXITED(waitret) {
			ret = C.WEXITSTATUS(waitret)
			is_signaled = false
		}
		else if C.WIFSIGNALED(waitret) {
			ret = C.WTERMSIG(waitret)
			is_signaled = true
		}
		return ret,is_signaled
	}
}

// posix_get_error_msg return error code representation in string.
pub fn posix_get_error_msg(code int) string {
	ptr_text := C.strerror(code) // voidptr?
	if ptr_text == 0 {
		return ''
	}
	return tos3(ptr_text)
}

fn vpclose(f voidptr) int {
	$if windows {
		return C._pclose(f)
	} $else {
		ret,_ := posix_wait4_to_exit_status(C.pclose(f))
		return ret
	}
}

pub struct Result {
pub:
	exit_code int
	output    string
	// stderr string // TODO
}
// `system` works like `exec()`, but only returns a return code.
pub fn system(cmd string) int {
	// if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
	// TODO remove panic
	// panic(';, &&, || and \\n are not allowed in shell commands')
	// }
	mut ret := 0
	$if windows {
		// overcome bug in system & _wsystem (cmd) when first char is quote `"`
		wcmd := if cmd.len > 1 && cmd[0] == `"` && cmd[1] != `"` { '"$cmd"' } else { cmd }
		ret = C._wsystem(wcmd.to_wide())
	} $else {
		ret = C.system(cmd.str)
	}
	if ret == -1 {
		print_c_errno()
	}
	$if !windows {
		pret,is_signaled := posix_wait4_to_exit_status(ret)
		if is_signaled {
			println('Terminated by signal ${ret:2d} (' + sigint_to_signal_name(pret) + ')')
		}
		ret = pret
	}
	return ret
}

pub fn sigint_to_signal_name(si int) string {
	// POSIX signals:
	match si {
		1 {
			return 'SIGHUP'
		}
		2 {
			return 'SIGINT'
		}
		3 {
			return 'SIGQUIT'
		}
		4 {
			return 'SIGILL'
		}
		6 {
			return 'SIGABRT'
		}
		8 {
			return 'SIGFPE'
		}
		9 {
			return 'SIGKILL'
		}
		11 {
			return 'SIGSEGV'
		}
		13 {
			return 'SIGPIPE'
		}
		14 {
			return 'SIGALRM'
		}
		15 {
			return 'SIGTERM'
		}
		else {}
	}
	$if linux {
		// From `man 7 signal` on linux:
		match si {
			30, 10, 16 {
				return 'SIGUSR1'
			}
			31, 12, 17 {
				return 'SIGUSR2'
			}
			20, 17, 18 {
				return 'SIGCHLD'
			}
			19, 18, 25 {
				return 'SIGCONT'
			}
			17, 19, 23 {
				return 'SIGSTOP'
			}
			18, 20, 24 {
				return 'SIGTSTP'
			}
			21, 21, 26 {
				return 'SIGTTIN'
			}
			22, 22, 27 {
				return 'SIGTTOU'
			}
			// /////////////////////////////
			5 {
				return 'SIGTRAP'
			}
			7 {
				return 'SIGBUS'
			}
			else {}
		}
	}
	return 'unknown'
}

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {
	$if windows {
		s := C._wgetenv(key.to_wide())
		if s == 0 {
			return ''
		}
		return string_from_wide(s)
	} $else {
		s := C.getenv(key.str)
		if s == 0 {
			return ''
		}
		// NB: C.getenv *requires* that the result be copied.
		return cstring_to_vstring(byteptr(s))
	}
}

pub fn setenv(name string, value string, overwrite bool) int {
	$if windows {
		format := '$name=$value'
		if overwrite {
			return C._putenv(format.str)
		}
		return -1
	} $else {
		return C.setenv(name.str, value.str, overwrite)
	}
}

pub fn unsetenv(name string) int {
	$if windows {
		format := '${name}='
		return C._putenv(format.str)
	} $else {
		return C.unsetenv(name.str)
	}
}

const (
	F_OK = 0
	X_OK = 1
	W_OK = 2
	R_OK = 4
)

// exists returns true if `path` exists.
pub fn exists(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), F_OK) != -1
	} $else {
		return C.access(path.str, F_OK) != -1
	}
}

// `is_executable` returns `true` if `path` is executable.
pub fn is_executable(path string) bool {
  $if windows {
    // NB: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/access-waccess?view=vs-2019
    // i.e. there is no X bit there, the modes can be:
    // 00 Existence only
    // 02 Write-only
    // 04 Read-only
    // 06 Read and write
    p := os.realpath( path )
    return ( os.exists( p ) && p.ends_with('.exe') )
  } $else {
    return C.access(path.str, X_OK) != -1
  }
}

// `is_writable` returns `true` if `path` is writable.
pub fn is_writable(path string) bool {
  $if windows {
    p := path.replace('/', '\\')
    return C._waccess(p.to_wide(), W_OK) != -1
  } $else {
    return C.access(path.str, W_OK) != -1
  }
}

// `is_readable` returns `true` if `path` is readable.
pub fn is_readable(path string) bool {
  $if windows {
    p := path.replace('/', '\\')
    return C._waccess(p.to_wide(), R_OK) != -1
  } $else {
    return C.access(path.str, R_OK) != -1
  }
}

[deprecated]
pub fn file_exists(_path string) bool {
	panic('Use `os.exists` instead of `os.file_exists`')
}

// rm removes file in `path`.
pub fn rm(path string) {
	$if windows {
		C._wremove(path.to_wide())
	} $else {
		C.remove(path.str)
	}
	// C.unlink(path.cstr())
}
// rmdir removes a specified directory.
pub fn rmdir(path string) {
	$if !windows {
		C.rmdir(path.str)
	} $else {
		C.RemoveDirectory(path.to_wide())
	}
}

[deprecated]
pub fn rmdir_recursive(path string) {
	panic('Use `os.rmdir_all` instead of `os.rmdir_recursive`')
}

pub fn rmdir_all(path string) {
	items := os.ls(path) or {
		return
	}
	for item in items {
		if os.is_dir(filepath.join(path,item)) {
			rmdir_all(filepath.join(path,item))
		}
		os.rm(filepath.join(path,item))
	}
	os.rmdir(path)
}

pub fn is_dir_empty(path string) bool {
	items := os.ls(path) or {
		return true
	}
	return items.len == 0
}

fn print_c_errno() {
	e := C.errno
	se := tos_clone(byteptr(C.strerror(C.errno)))
	println('errno=$e err=$se')
}

[deprecated]
pub fn ext(path string) string {
	panic('Use `filepath.ext` instead of `os.ext`')
}

[deprecated]
pub fn dir(path string) string {
	panic('Use `filepath.dir` instead of `os.dir`')
}

[deprecated]
pub fn basedir(path string) string {
	panic('Use `filepath.basedir` instead of `os.basedir`')
}

[deprecated]
pub fn filename(path string) string {
	panic('Use `filepath.filename` instead of `os.filename`')
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

// get_raw_line returns a one-line string from stdin along with '\n' if there is any
pub fn get_raw_line() string {
	$if windows {
		unsafe {
			max_line_chars := 256
			buf := malloc(max_line_chars * 2)
			h_input := C.GetStdHandle(STD_INPUT_HANDLE)
			mut bytes_read := 0
			if is_atty(0) > 0 {
				C.ReadConsole(h_input, buf, max_line_chars * 2, &bytes_read, 0)
				return string_from_wide2(&u16(buf), bytes_read)
			}
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, 1, &bytes_read, 0)
				if !res || bytes_read == 0 {
						break
				}
				if *pos == `\n` || *pos == `\r` {
					offset++
					break
				}
				offset++
			}
			return string(buf, offset)
		}
	} $else {
		max := size_t(0)
		mut buf := charptr(0)
		nr_chars := C.getline(&buf, &max, stdin)
		//defer { unsafe{ free(buf) } }
		if nr_chars == 0 || nr_chars == -1 {
			return ''
		}
		return tos3(buf)
		//res := tos_clone(buf)
		//return res
	}
}

pub fn get_lines() []string {
	mut line := ''
	mut inputstr := []string
	for {
		line = get_line()
		if (line.len <= 0) {
			break
		}
		line = line.trim_space()
		inputstr << line
	}
	return inputstr
}

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
		return 'mac'
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

// home_dir returns path to user's home directory.
pub fn home_dir() string {
	$if windows {
		return os.getenv('USERPROFILE') + filepath.separator
	} $else {
		return os.getenv('HOME') + filepath.separator
	}
}

// write_file writes `text` data to a file in `path`.
pub fn write_file(path, text string) {
	mut f := os.create(path) or {
		return
	}
	f.write(text)
	f.close()
}

// clear clears current terminal screen.
pub fn clear() {
	$if !windows {
		C.printf('\x1b[2J')
		C.printf('\x1b[H')
	}
}

pub fn on_segfault(f voidptr) {
	$if windows {
		return
	}
	$if macos {
		mut sa := C.sigaction{}
		C.memset(&sa, 0, sizeof(sigaction))
		C.sigemptyset(&sa.sa_mask)
		sa.sa_sigaction = f
		sa.sa_flags = C.SA_SIGINFO
		C.sigaction(C.SIGSEGV, &sa, 0)
	}
}

fn C.getpid() int


fn C.proc_pidpath(int, byteptr, int) int


fn C.readlink() int
// executable returns the path name of the executable that started the current
// process.
pub fn executable() string {
	$if linux {
		mut result := vcalloc(MAX_PATH)
		count := C.readlink('/proc/self/exe', result, MAX_PATH)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/self/exe to get exe path')
			return os.args[0]
		}
		return string(result)
	}
	$if windows {
		max := 512
		mut result := &u16(vcalloc(max * 2)) // MAX_PATH * sizeof(wchar_t)
		len := C.GetModuleFileName(0, result, max)
		return string_from_wide2(result, len)
	}
	$if macos {
		mut result := vcalloc(MAX_PATH)
		pid := C.getpid()
		ret := proc_pidpath(pid, result, MAX_PATH)
		if ret <= 0 {
			eprintln('os.executable() failed at calling proc_pidpath with pid: $pid . proc_pidpath returned $ret ')
			return os.args[0]
		}
		return string(result)
	}
	$if freebsd {
		mut result := vcalloc(MAX_PATH)
		mib := [1/* CTL_KERN */, 14/* KERN_PROC */, 12/* KERN_PROC_PATHNAME */, -1]
		size := MAX_PATH
		C.sysctl(mib.data, 4, result, &size, 0, 0)
		return string(result)
	}
	$if openbsd {
		// "Sadly there is no way to get the full path of the executed file in OpenBSD."
		// lol
		return os.args[0]
	}
	$if solaris {}
	$if haiku {}
	$if netbsd {
		mut result := vcalloc(MAX_PATH)
		count := C.readlink('/proc/curproc/exe', result, MAX_PATH)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/exe to get exe path')
			return os.args[0]
		}
		return string(result,count)
	}
	$if dragonfly {
		mut result := vcalloc(MAX_PATH)
		count := C.readlink('/proc/curproc/file', result, MAX_PATH)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/file to get exe path')
			return os.args[0]
		}
		return string(result,count)
	}
	return os.args[0]
}

[deprecated]
pub fn dir_exists(path string) bool {
	panic('Use `os.is_dir` instead of `os.dir_exists`')
}

// is_dir returns a boolean indicating whether the given path is a directory.
pub fn is_dir(path string) bool {
	$if windows {
		_path := path.replace('/', '\\')
		attr := C.GetFileAttributesW(_path.to_wide())
		if attr == u32(C.INVALID_FILE_ATTRIBUTES) {
			return false
		}
		if int(attr) & C.FILE_ATTRIBUTE_DIRECTORY != 0 {
			return true
		}
		return false
	} $else {
		statbuf := C.stat{}
		if C.stat(path.str, &statbuf) != 0 {
			return false
		}
		// ref: https://code.woboq.org/gcc/include/sys/stat.h.html
		return int(statbuf.st_mode) & S_IFMT == S_IFDIR
	}
}

// is_link returns a boolean indicating whether the given path is a link.
pub fn is_link(path string) bool {
	$if windows {
		return false // TODO
	} $else {
		statbuf := C.stat{}
		if C.lstat(path.str, &statbuf) != 0 {
			return false
		}
		return int(statbuf.st_mode) & S_IFMT == S_IFLNK
	}
}

// chdir changes the current working directory to the new directory path.
pub fn chdir(path string) {
	$if windows {
		C._wchdir(path.to_wide())
	} $else {
		C.chdir(path.str)
	}
}

// getwd returns the absolute path name of the current directory.
pub fn getwd() string {
	$if windows {
		max := 512 // MAX_PATH * sizeof(wchar_t)
		buf := &u16(vcalloc(max * 2))
		if C._wgetcwd(buf, max) == 0 {
			return ''
		}
		return string_from_wide(buf)
	} $else {
		buf := vcalloc(512)
		if C.getcwd(buf, 512) == 0 {
			return ''
		}
		return string(buf)
	}
}

// Returns the full absolute path for fpath, with all relative ../../, symlinks and so on resolved.
// See http://pubs.opengroup.org/onlinepubs/9699919799/functions/realpath.html
// Also https://insanecoding.blogspot.com/2007/11/pathmax-simply-isnt.html
// and https://insanecoding.blogspot.com/2007/11/implementing-realpath-in-c.html
// NB: this particular rabbit hole is *deep* ...
pub fn realpath(fpath string) string {
	mut fullpath := vcalloc(MAX_PATH)
	mut ret := charptr(0)
	$if windows {
		ret = C._fullpath(fullpath, fpath.str, MAX_PATH)
		if ret == 0 {
			return fpath
		}
	} $else {
		ret = C.realpath(fpath.str, fullpath)
		if ret == 0 {
			return fpath
		}
	}
	return string(fullpath)
}

// walk_ext returns a recursive list of all file paths ending with `ext`.
pub fn walk_ext(path, ext string) []string {
	if !os.is_dir(path) {
		return []
	}
	mut files := os.ls(path) or {
		return []
	}
	mut res := []string
	separator := if path.ends_with(filepath.separator) { '' } else { filepath.separator }
	for i, file in files {
		if file.starts_with('.') {
			continue
		}
		p := path + separator + file
		if os.is_dir(p) && !os.is_link(p) {
			res << walk_ext(p, ext)
		}
		else if file.ends_with(ext) {
			res << p
		}
	}
	return res
}

// walk recursively traverses the given directory path.
// When a file is encountred it will call the callback function with current file as argument.
pub fn walk(path string, f fn(path string)) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or {
		return
	}
	for file in files {
		p := path + filepath.separator + file
		if os.is_dir(p) && !os.is_link(p) {
			walk(p, f)
		}
		else if os.exists(p) {
			f(p)
		}
	}
	return
}

pub fn signal(signum int, handler voidptr) {
	C.signal(signum, handler)
}

fn C.fork() int


fn C.wait() int


pub fn fork() int {
	mut pid := -1
	$if !windows {
		pid = C.fork()
	}
	$if windows {
		panic('os.fork not supported in windows') // TODO
	}
	return pid
}

pub fn wait() int {
	mut pid := -1
	$if !windows {
		pid = C.wait(0)
	}
	$if windows {
		panic('os.wait not supported in windows') // TODO
	}
	return pid
}

pub fn file_last_mod_unix(path string) int {
	attr := C.stat{}
	// # struct stat attr;
	C.stat(path.str, &attr)
	// # stat(path.str, &attr);
	return attr.st_mtime
	// # return attr.st_mtime ;
}

pub fn log(s string) {
	println('os.log: ' + s)
}

[deprecated]
pub fn flush_stdout() {
	panic('Use `os.flush` instead of `os.flush_stdout`')
}

pub fn flush() {
	C.fflush(stdout)
}

pub fn mkdir_all(path string) {
	mut p := if path.starts_with(filepath.separator) { filepath.separator } else { '' }
	for subdir in path.split(filepath.separator) {
		p += subdir + filepath.separator
		if !os.is_dir(p) {
			os.mkdir(p) or {
				panic(err)
			}
		}
	}
}

[deprecated]
pub fn join(base string, dirs ...string) string {
	panic('Use `filepath.join` instead of `os.join`')
}

// cachedir returns the path to a *writable* user specific folder, suitable for writing non-essential data.
pub fn cachedir() string {
	// See: https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
	// There is a single base directory relative to which user-specific non-essential
	// (cached) data should be written. This directory is defined by the environment
	// variable $XDG_CACHE_HOME.
	// $XDG_CACHE_HOME defines the base directory relative to which user specific
	// non-essential data files should be stored. If $XDG_CACHE_HOME is either not set
	// or empty, a default equal to $HOME/.cache should be used.
	$if !windows {
		xdg_cache_home := os.getenv('XDG_CACHE_HOME')
		if xdg_cache_home != '' {
			return xdg_cache_home
		}
	}
	cdir := os.home_dir() + '.cache'
	if !os.is_dir(cdir) && !os.is_link(cdir) {
		os.mkdir(cdir) or {
			panic(err)
		}
	}
	return cdir
}

// tmpdir returns the path to a folder, that is suitable for storing temporary files
pub fn tmpdir() string {
	mut path := os.getenv('TMPDIR')
	$if windows {
		if path == '' {
			// TODO see Qt's implementation?
			// https://doc.qt.io/qt-5/qdir.html#tempPath
			// https://github.com/qt/qtbase/blob/e164d61ca8263fc4b46fdd916e1ea77c7dd2b735/src/corelib/io/qfilesystemengine_win.cpp#L1275
			path = os.getenv('TEMP')
			if path == '' {
				path = os.getenv('TMP')
			}
			if path == '' {
				path = 'C:/tmp'
			}
		}
	}
	if path == '' {
		path = os.cachedir()
	}
	if path == '' {
		path = '/tmp'
	}
	return path
}

pub fn chmod(path string, mode int) {
	C.chmod(path.str, mode)
}

pub const (
	wd_at_startup = getwd()
)

// resource_abs_path returns an absolute path, for the given `path`
// (the path is expected to be relative to the executable program)
// See https://discordapp.com/channels/592103645835821068/592294828432424960/630806741373943808
// It gives a convenient way to access program resources like images, fonts, sounds and so on,
// *no matter* how the program was started, and what is the current working directory.
pub fn resource_abs_path(path string) string {
	mut base_path := os.realpath(filepath.dir(os.executable()))
	vresource := os.getenv('V_RESOURCE_PATH')
	if vresource.len != 0 {
		base_path = vresource
	}
	return os.realpath( filepath.join( base_path, path ) )
}

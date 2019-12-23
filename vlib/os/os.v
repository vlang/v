// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
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

struct C.DIR {
}

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


pub fn (f File) is_opened() bool {
	return f.opened
}

// read_bytes reads an amount of bytes from the beginning of the file
pub fn (f mut File) read_bytes(size int) []byte {
	return f.read_bytes_at(size, 0)
}

// read_bytes_at reads an amount of bytes at the given position in the file
pub fn (f mut File) read_bytes_at(size, pos int) []byte {
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
	C.fseek(fp, 0, C.SEEK_END)
	fsize := C.ftell(fp)
	// C.fseek(fp, 0, SEEK_SET)  // same as `C.rewind(fp)` below
	C.rewind(fp)
	mut str := malloc(fsize + 1)
	C.fread(str, fsize, 1, fp)
	C.fclose(fp)
	str[fsize] = 0
	return string(str,fsize)
}

// file_size returns the size of the file located in `path`.
pub fn file_size(path string) int {
	mut s := C.stat{
	}
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

pub fn cp_r(osource_path, odest_path string, overwrite bool) ?bool {
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
		cp_r(sp, dp, overwrite) or {
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

fn vfopen(path, mode string) *C.FILE {
	$if windows {
		return C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		return C.fopen(charptr(path.str), charptr(mode.str))
	}
}

// read_lines reads the file in `path` into an array of lines.
pub fn read_lines(path string) ?[]string {
	mut res := []string
	mut buf_len := 1024
	mut buf := malloc(buf_len)
	mode := 'rb'
	mut fp := vfopen(path, mode)
	if isnil(fp) {
		return error('read_lines() failed to open file "$path"')
	}
	mut buf_index := 0
	for C.fgets(buf + buf_index, buf_len - buf_index, fp) != 0 {
		len := vstrlen(buf)
		if len == buf_len - 1 && buf[len - 1] != 10 {
			buf_len *= 2
			buf = C.realloc(buf, buf_len)
			if isnil(buf) {
				return error('could not reallocate the read buffer')
			}
			buf_index = len
			continue
		}
		if buf[len - 1] == 10 || buf[len - 1] == 13 {
			buf[len - 1] = `\0`
		}
		if len > 1 && buf[len - 2] == 13 {
			buf[len - 2] = `\0`
		}
		res << tos_clone(buf)
		buf_index = 0
	}
	C.fclose(fp)
	return res
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

pub fn open(path string) ?File {
	mut file := File{
	}
	$if windows {
		wpath := path.to_wide()
		mode := 'rb'
		file = File{
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str
		file = File{
			cfile: C.fopen(charptr(cpath), 'rb')
		}
	}
	if isnil(file.cfile) {
		return error('failed to open file "$path"')
	}
	file.opened = true
	return file
}

// create creates a file at a specified location and returns a writable `File` object.
pub fn create(path string) ?File {
	mut file := File{
	}
	$if windows {
		wpath := path.replace('/', '\\').to_wide()
		mode := 'wb'
		file = File{
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str
		file = File{
			cfile: C.fopen(charptr(cpath), 'wb')
		}
	}
	if isnil(file.cfile) {
		return error('failed to create file "$path"')
	}
	file.opened = true
	return file
}

pub fn open_append(path string) ?File {
	mut file := File{
	}
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

pub fn (f mut File) write(s string) {
	C.fputs(s.str, f.cfile)
	// C.fwrite(s.str, 1, s.len, f.cfile)
}
// convert any value to []byte (LittleEndian) and write it
// for example if we have write(7, 4), "07 00 00 00" gets written
// write(0x1234, 2) => "34 12"
pub fn (f mut File) write_bytes(data voidptr, size int) {
	C.fwrite(data, 1, size, f.cfile)
}

pub fn (f mut File) write_bytes_at(data voidptr, size, pos int) {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
}

pub fn (f mut File) writeln(s string) {
	if !f.opened {
		return
	}
	// C.fwrite(s.str, 1, s.len, f.cfile)
	// ss := s.clone()
	// TODO perf
	C.fputs(s.str, f.cfile)
	// ss.free()
	C.fputs('\n', f.cfile)
}

pub fn (f mut File) flush() {
	if !f.opened {
		return
	}
	C.fflush(f.cfile)
}

pub fn (f mut File) close() {
	if !f.opened {
		return
	}
	f.opened = false
	C.fflush(f.cfile)
	C.fclose(f.cfile)
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
		else {
		}}
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
			else {
			}}
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

// exists returns true if `path` exists.
pub fn exists(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), 0) != -1
	} $else {
		return C.access(path.str, 0) != -1
	}
}

[deprecated]
pub fn file_exists(_path string) bool {
	panic('use os.exists(path) instead of os.file_exists(path)')
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

fn print_c_errno() {
	// C.printf('errno=%d err="%s"\n', C.errno, C.strerror(C.errno))
}

[deprecated]
pub fn ext(path string) string {
	println('Use filepath.ext')
	return filepath.ext(path)
}

[deprecated]
pub fn dir(path string) string {
	println('Use filepath.dir')
	return filepath.ext(path)
}

[deprecated]
pub fn basedir(path string) string {
	println('Use filepath.basedir')
	return filepath.basedir(path)
}

[deprecated]
pub fn filename(path string) string {
	println('Use filepath.filename')
	return filepath.filename(path)
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
		max_line_chars := 256
		buf := malloc(max_line_chars * 2)
		if is_atty(0) > 0 {
			h_input := C.GetStdHandle(STD_INPUT_HANDLE)
			mut nr_chars := u32(0)
			C.ReadConsole(h_input, buf, max_line_chars * 2, voidptr(&nr_chars), 0)
			return string_from_wide2(&u16(buf), int(nr_chars))
		}
		res := C.fgetws(&u16(buf), max_line_chars, C.stdin)
		len := C.wcslen(&u16(buf))
		if !isnil(res) {
			return string_from_wide2(&u16(buf), len)
		}
		return ''
	} $else {
		max := size_t(256)
		buf := charptr(malloc(int(max)))
		nr_chars := C.getline(&buf, &max, stdin)
		if nr_chars == 0 {
			return ''
		}
		return string(byteptr(buf),nr_chars)
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
	mut home := os.getenv('HOME')
	$if windows {
		home = os.getenv('HOMEDRIVE')
		if home.len == 0 {
			home = os.getenv('SYSTEMDRIVE')
		}
		mut homepath := os.getenv('HOMEPATH')
		if homepath.len == 0 {
			homepath = '\\Users\\' + os.getenv('USERNAME')
		}
		home += homepath
	}
	home += path_separator
	return home
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
		mut sa := C.sigaction{
		}
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
		mut result := calloc(MAX_PATH)
		count := C.readlink('/proc/self/exe', result, MAX_PATH)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/self/exe to get exe path')
			return os.args[0]
		}
		return string(result)
	}
	$if windows {
		max := 512
		mut result := &u16(calloc(max * 2)) // MAX_PATH * sizeof(wchar_t)
		len := C.GetModuleFileName(0, result, max)
		return string_from_wide2(result, len)
	}
	$if macos {
		mut result := calloc(MAX_PATH)
		pid := C.getpid()
		ret := proc_pidpath(pid, result, MAX_PATH)
		if ret <= 0 {
			eprintln('os.executable() failed at calling proc_pidpath with pid: $pid . proc_pidpath returned $ret ')
			return os.args[0]
		}
		return string(result)
	}
	$if freebsd {
		mut result := calloc(MAX_PATH)
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
	$if solaris {
	}
	$if haiku {
	}
	$if netbsd {
		mut result := calloc(MAX_PATH)
		count := int(C.readlink('/proc/curproc/exe', result, MAX_PATH))
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/exe to get exe path')
			return os.args[0]
		}
		return string(result,count)
	}
	$if dragonfly {
		mut result := calloc(MAX_PATH)
		count := int(C.readlink('/proc/curproc/file', result, MAX_PATH))
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
	panic('use os.is_dir()')
	// return false
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
		statbuf := C.stat{
		}
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
		statbuf := C.stat{
		}
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
		buf := &u16(calloc(max * 2))
		if C._wgetcwd(buf, max) == 0 {
			return ''
		}
		return string_from_wide(buf)
	} $else {
		buf := calloc(512)
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
	mut fullpath := calloc(MAX_PATH)
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
		panic(err)
	}
	mut res := []string
	separator := if path.ends_with(path_separator) { '' } else { path_separator }
	for i, file in files {
		if file.starts_with('.') {
			continue
		}
		p := path + separator + file
		if os.is_dir(p) {
			res << walk_ext(p, ext)
		}
		else if file.ends_with(ext) {
			res << p
		}
	}
	return res
}

// walk recursively traverse the given directory path.
// When a file is encountred it will call the callback function with current file as argument.
pub fn walk(path string, fnc fn(path string)) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or {
		panic(err)
	}
	for file in files {
		p := path + os.path_separator + file
		if os.is_dir(p) {
			walk(p, fnc)
		}
		else if os.exists(p) {
			fnc(p)
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
	$if !windows {
		panic('os.wait not supported in windows') // TODO
	}
	return pid
}

pub fn file_last_mod_unix(path string) int {
	attr := C.stat{
	}
	// # struct stat attr;
	C.stat(path.str, &attr)
	// # stat(path.str, &attr);
	return attr.st_mtime
	// # return attr.st_mtime ;
}

pub fn log(s string) {
	println('os.log: ' + s)
}

pub fn flush_stdout() {
	C.fflush(stdout)
}

pub fn print_backtrace() {
	/*
	# void *buffer[100];
	nptrs := 0
	# nptrs = backtrace(buffer, 100);
	# printf("%d!!\n", nptrs);
	# backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO) ;
*/
}

pub fn mkdir_all(path string) {
	mut p := if path.starts_with(os.path_separator) { os.path_separator } else { '' }
	for subdir in path.split(os.path_separator) {
		p += subdir + os.path_separator
		if !os.is_dir(p) {
			os.mkdir(p) or {
				panic(err)
			}
		}
	}
}

pub fn join(base string, dirs ...string) string {
	println('use filepath.join')
	return filepath.join(base,dirs)
}

// tmpdir returns the path to a folder, that is suitable for storing temporary files
pub fn tmpdir() string {
	mut path := os.getenv('TMPDIR')
	$if linux {
		if path == '' {
			path = '/tmp'
		}
	}
	$if freebsd {
		if path == '' {
			path = '/tmp'
		}
	}
	$if macos {
		/*
		if path == '' {
			// TODO untested
			path = C.NSTemporaryDirectory()
		}
        */
		if path == '' {
			path = '/tmp'
		}
	}
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
	return path
}

pub fn chmod(path string, mode int) {
	C.chmod(path.str, mode)
}

pub const (
	wd_at_startup = getwd()
)


// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

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
	args = []string{}
	max_path_len = 4096
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


fn C.sigaction(int, voidptr, int)


fn C.open(charptr, int, int) int


fn C.fdopen(int, string) voidptr


pub fn (f File) is_opened() bool {
	return f.opened
}

/***************************** Write ops ****************************/

pub fn (mut f File) write(s string) {
	if !f.opened {
		return
	}
	/*
	$if linux {
		$if !android {
			C.syscall(sys_write, f.fd, s.str, s.len)
			return
		}
	}
	*/
	C.fputs(s.str, f.cfile)
}

pub fn (mut f File) writeln(s string) {
	if !f.opened {
		return
	}
	/*
	$if linux {
		$if !android {
			snl := s + '\n'
			C.syscall(sys_write, f.fd, snl.str, snl.len)
			return
		}
	}
	*/
	// TODO perf
	C.fputs(s.str, f.cfile)
	C.fputs('\n', f.cfile)
}

pub fn (mut f File) write_bytes(data voidptr, size int) {
	C.fwrite(data, 1, size, f.cfile)
}

pub fn (mut f File) write_bytes_at(data voidptr, size, pos int) {
	//$if linux {
	//}
	//$else {
	C.fseek(f.cfile, pos, C.SEEK_SET)
	C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, C.SEEK_END)
	//}
}

/***************************** Read ops  ****************************/


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

/***************************** Utility  ops ************************/
pub fn (mut f File) flush() {
	if !f.opened {
		return
	}
	C.fflush(f.cfile)
}

/***************************** OS ops ************************/
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
		w_old := old.replace('/', '\\')
		w_new := new.replace('/', '\\')
		C.CopyFile(w_old.to_wide(), w_new.to_wide(), false)
		result := C.GetLastError()
		if result == 0 {
			return true
		}
		else {
			return error_with_code('failed to copy $old to $new', int(result))
		}
	} $else {
		os.system('cp "$old" "$new"')
		return true // TODO make it return true or error when cp for linux is implemented
	}
}

[deprecated]
pub fn cp_r(osource_path, odest_path string, overwrite bool) ?bool {
	panic('Use `os.cp_all` instead of `os.cp_r`')
}

pub fn cp_all(osource_path, odest_path string, overwrite bool) ?bool {
	source_path := os.real_path(osource_path)
	dest_path := os.real_path(odest_path)
	if !os.exists(source_path) {
		return error("Source path doesn\'t exist")
	}
	// single file copy
	if !os.is_dir(source_path) {
		adjusted_path := if os.is_dir(dest_path) {os.join_path(dest_path,os.file_name(source_path)) } else { dest_path }
		if os.exists(adjusted_path) {
			if overwrite {
				os.rm(adjusted_path)
			}
			else {
				return error('Destination file path already exist')
			}
		}
		os.cp(source_path, adjusted_path) or {
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
		sp := os.join_path(source_path, file)
		dp := os.join_path(dest_path, file)
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

// vfopen returns an opened C file, given its path and open mode.
// NB: os.vfopen is useful for compatibility with C libraries, that expect `FILE *`.
// If you write pure V code, os.create or os.open are more convenient.
pub fn vfopen(path, mode string) &C.FILE {
	$if windows {
		return C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		return C.fopen(charptr(path.str), charptr(mode.str))
	}
}

// fileno returns the file descriptor of an opened C file
pub fn fileno(cfile voidptr) int {
	$if windows {
		return C._fileno(cfile)
	} $else {
		cfile_casted := &C.FILE(0) // FILE* cfile_casted = 0;
		cfile_casted = cfile
		// Required on FreeBSD/OpenBSD/NetBSD as stdio.h defines fileno(..) with a macro
		// that performs a field access on its argument without casting from void*.
		return C.fileno(cfile_casted)
	}
}

// read_lines reads the file in `path` into an array of lines.
pub fn read_lines(path string) ?[]string {
	buf := read_file(path) or {
		return error(err)
	}
	return buf.split_into_lines()
}

fn read_ulines(path string) ?[]ustring {
	lines := read_lines(path) or {
		return error(err)
	}
	// mut ulines := new_array(0, lines.len, sizeof(ustring))
	mut ulines := []ustring{}
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
			`r` { flags |= o_rdonly }
			`w` { flags |= o_create | o_trunc }
			`a` { flags |= o_create | o_append }
			`s` { flags |= o_sync }
			`n` { flags |= o_nonblock }
			`c` { flags |= o_noctty }
			`+` { flags |= o_rdwr }
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

struct Foo2 {
	x int

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
			// TODO dependent on platform
			// works only on x86/ARM/most others
			10 /*, 30, 16 */ {
				return 'SIGUSR1'
			}
			12 /*, 31, 17 */ {
				return 'SIGUSR2'
			}
			17 /*, 20, 18 */ {
				return 'SIGCHLD'
			}
			18 /*, 19, 25 */ {
				return 'SIGCONT'
			}
			19 /*, 17, 23 */ {
				return 'SIGSTOP'
			}
			20 /*, 18, 24 */ {
				return 'SIGTSTP'
			}
			21 /*, 26 */ {
				return 'SIGTTIN'
			}
			22 /*, 27 */ {
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

const (
	f_ok = 0
	x_ok = 1
	w_ok = 2
	r_ok = 4
)

// exists returns true if `path` exists.
pub fn exists(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), f_ok) != -1
	} $else {
		return C.access(path.str, f_ok) != -1
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
    p := os.real_path( path )
    return ( os.exists( p ) && p.ends_with('.exe') )
  }
  $if solaris {
    statbuf := C.stat{}
    if C.stat(path.str, &statbuf) != 0 {
      return false
    }
    return (int(statbuf.st_mode) & ( s_ixusr | s_ixgrp | s_ixoth )) != 0
  }
  return C.access(path.str, x_ok) != -1
}

// `is_writable_folder` - `folder` exists and is writable to the process
pub fn is_writable_folder(folder string) ?bool {
	if !os.exists(folder) {
		return error('`$folder` does not exist')
	}
	if !os.is_dir(folder) {
		return error('`folder` is not a folder')
	}
	tmp_perm_check := os.join_path(folder, 'tmp_perm_check')
	f := os.open_file(tmp_perm_check, 'w+', 0o700) or {
		return error('cannot write to folder `$folder`: $err')
	}
	f.close()
	os.rm(tmp_perm_check)
	return true
}

// `is_writable` returns `true` if `path` is writable.
pub fn is_writable(path string) bool {
  $if windows {
    p := path.replace('/', '\\')
    return C._waccess(p.to_wide(), w_ok) != -1
  } $else {
    return C.access(path.str, w_ok) != -1
  }
}

// `is_readable` returns `true` if `path` is readable.
pub fn is_readable(path string) bool {
  $if windows {
    p := path.replace('/', '\\')
    return C._waccess(p.to_wide(), r_ok) != -1
  } $else {
    return C.access(path.str, r_ok) != -1
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
		if os.is_dir(os.join_path(path, item)) {
			rmdir_all(os.join_path(path, item))
		}
		os.rm(os.join_path(path, item))
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

pub fn file_ext(path string) string {
	pos := path.last_index('.') or {
		return ''
	}
	return path[pos..]
}

pub fn dir(path string) string {
	pos := path.last_index(path_separator) or {
		return '.'
	}
	return path[..pos]
}

pub fn base_dir(path string) string {
	posx := path.last_index(path_separator) or {
		return path
	}
	// NB: *without* terminating /
	return path[..posx]
}

pub fn file_name(path string) string {
	return path.all_after_last(path_separator)
}

// input returns a one-line string from stdin, after printing a prompt
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

// get_raw_line returns a one-line string from stdin along with '\n' if there is any
pub fn get_raw_line() string {
	$if windows {
		unsafe {
			max_line_chars := 256
			buf := malloc(max_line_chars * 2)
			h_input := C.GetStdHandle(std_input_handle)
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
		nr_chars := C.getline(&buf, &max, C.stdin)
		//defer { unsafe{ free(buf) } }
		if nr_chars == 0 || nr_chars == -1 {
			return ''
		}
		return tos3(buf)
		//res := tos_clone(buf)
		//return res
	}
}

pub fn get_raw_stdin() []byte {
	$if windows {
		unsafe {
			block_bytes := 512
			mut buf := malloc(block_bytes)
			h_input := C.GetStdHandle(std_input_handle)
			mut bytes_read := 0
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, block_bytes, &bytes_read, 0)
				offset += bytes_read

				if !res {
					break
				}

				buf = v_realloc(buf, offset + block_bytes + (block_bytes-bytes_read))
			}

			C.CloseHandle(h_input)

			return array{element_size: 1 data: voidptr(buf) len: offset cap: offset }
		}
	} $else {
		panic('get_raw_stdin not implemented on this platform...')
	}
}

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
		return os.getenv('USERPROFILE') + os.path_separator
	} $else {
		//println('home_dir() call')
		//res:= os.getenv('HOME') + os.path_separator
		//println('res="$res"')
		return os.getenv('HOME') + os.path_separator
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
		C.printf("TODO")
		/*
		mut sa := C.sigaction{}
		C.memset(&sa, 0, sizeof(C.sigaction_size))
		C.sigemptyset(&sa.sa_mask)
		sa.sa_sigaction = f
		sa.sa_flags = C.SA_SIGINFO
		C.sigaction(C.SIGSEGV, &sa, 0)
		*/
	}
}

fn C.getpid() int


//fn C.proc_pidpath(int, byteptr, int) int


fn C.readlink() int
// executable returns the path name of the executable that started the current
// process.
pub fn executable() string {
	$if linux {
		mut result := vcalloc(max_path_len)
		count := C.readlink('/proc/self/exe', result, max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/self/exe to get exe path')
			return executable_fallback()
		}
		return string(result)
	}
	$if windows {
		max := 512
		mut result := &u16(vcalloc(max * 2)) // max_path_len * sizeof(wchar_t)
		len := C.GetModuleFileName(0, result, max)
		return string_from_wide2(result, len)
	}
	$if macos {
		mut result := vcalloc(max_path_len)
		pid := C.getpid()
		ret := proc_pidpath(pid, result, max_path_len)
		if ret <= 0 {
			eprintln('os.executable() failed at calling proc_pidpath with pid: $pid . proc_pidpath returned $ret ')
			return executable_fallback()
		}
		return string(result)
	}
	$if freebsd {
		mut result := vcalloc(max_path_len)
		mib := [1/* CTL_KERN */, 14/* KERN_PROC */, 12/* KERN_PROC_PATHNAME */, -1]
		size := max_path_len
		C.sysctl(mib.data, 4, result, &size, 0, 0)
		return string(result)
	}
	// "Sadly there is no way to get the full path of the executed file in OpenBSD."
	$if openbsd {}
	$if solaris {}
	$if haiku {}
	$if netbsd {
		mut result := vcalloc(max_path_len)
		count := C.readlink('/proc/curproc/exe', result, max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/exe to get exe path')
			return executable_fallback()
		}
		return string(result,count)
	}
	$if dragonfly {
		mut result := vcalloc(max_path_len)
		count := C.readlink('/proc/curproc/file', result, max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/file to get exe path')
			return executable_fallback()
		}
		return string(result,count)
	}
	return executable_fallback()
}

// executable_fallback is used when there is not a more platform specific and accurate implementation
// it relies on path manipulation of os.args[0] and os.wd_at_startup, so it may not work properly in
// all cases, but it should be better, than just using os.args[0] directly.
fn executable_fallback() string {
	if os.args.len == 0 {
		// we are early in the bootstrap, os.args has not been initialized yet :-|
		return ''
	}
	mut exepath := os.args[0]
	if !os.is_abs_path(exepath) {
		if exepath.contains( os.path_separator ) {
			exepath = os.join_path(os.wd_at_startup, exepath)
		}else{
			// no choice but to try to walk the PATH folders :-| ...
			foundpath := os.find_abs_path_of_executable(exepath) or { '' }
			if foundpath.len > 0 {
				exepath = foundpath
			}
		}
	}
	exepath = os.real_path(exepath)
	return exepath
}

// find_exe_path walks the environment PATH, just like most shell do, it returns
// the absolute path of the executable if found
pub fn find_abs_path_of_executable(exepath string) ?string {
	if os.is_abs_path(exepath) {
		return exepath
	}
	mut res := ''
	env_path_delimiter := if os.user_os() == 'windows' { ';' } else { ':' }
	paths := os.getenv('PATH').split(env_path_delimiter)
	for p in paths {
		found_abs_path := os.join_path( p, exepath )
		if os.exists( found_abs_path ) && os.is_executable( found_abs_path ) {
			res = found_abs_path
			break
		}
	}
	if res.len>0 {
		return res
	}
	return error('failed to find executable')
}

// exists_in_system_path returns true if prog exists in the system's path
pub fn exists_in_system_path(prog string) bool {
	os.find_abs_path_of_executable(prog) or {
		return false
	}
	return true
}

[deprecated]
pub fn dir_exists(path string) bool {
	panic('Use `os.is_dir` instead of `os.dir_exists`')
}

// is_dir returns a boolean indicating whether the given path is a directory.
pub fn is_dir(path string) bool {
	$if windows {
		w_path := path.replace('/', '\\')
		attr := C.GetFileAttributesW(w_path.to_wide())
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
		val:= int(statbuf.st_mode) & os.s_ifmt
		return val == s_ifdir
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
		return int(statbuf.st_mode) & s_ifmt == s_iflnk
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
		max := 512 // max_path_len * sizeof(wchar_t)
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
pub fn real_path(fpath string) string {
	mut fullpath := vcalloc(max_path_len)
	mut ret := charptr(0)
	$if windows {
		ret = charptr(C._fullpath(fullpath, fpath.str, max_path_len))
		if ret == 0 {
			return fpath
		}
	} $else {
		ret = charptr(C.realpath(fpath.str, fullpath))
		if ret == 0 {
			return fpath
		}
	}
	return string(fullpath)
}

// is_abs_path returns true if `path` is absolute.
pub fn is_abs_path(path string) bool {
	$if windows {
		return path[0] == `/` || // incase we're in MingGW bash
		(path[0].is_letter() && path[1] == `:`)
	}
	return path[0] == `/`
}

// join returns path as string from string parameter(s).
pub fn join_path(base string, dirs ...string) string {
	mut result := []string{}
	result << base.trim_right('\\/')
	for d in dirs {
		result << d
	}
	return result.join(path_separator)
}

// walk_ext returns a recursive list of all file paths ending with `ext`.
pub fn walk_ext(path, ext string) []string {
	if !os.is_dir(path) {
		return []
	}
	mut files := os.ls(path) or {
		return []
	}
	mut res := []string{}
	separator := if path.ends_with(os.path_separator) { '' } else { os.path_separator }
	for file in files {
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
		p := path + os.path_separator + file
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
	C.fflush(C.stdout)
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
pub fn temp_dir() string {
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
		path = os.cache_dir()
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
	mut base_path := os.real_path(os.dir(os.executable()))
	vresource := os.getenv('V_RESOURCE_PATH')
	if vresource.len != 0 {
		base_path = vresource
	}
	return os.real_path(os.join_path(base_path, path))
}


// open tries to open a file for reading and returns back a read-only `File` object
pub fn open(path string) ?File {
  /*
	$if linux {
		$if !android {
			fd := C.syscall(sys_open, path.str, 511)
			if fd == -1 {
				return error('failed to open file "$path"')
			}
			return File{
				fd: fd
				opened: true
			}
		}
	}
  */
	cfile := vfopen(path, 'rb')
	if cfile == voidptr(0) {
		return error('failed to open file "$path"')
	}
	fd := fileno(cfile)
	return File {
		cfile: cfile
		fd: fd
		opened: true
	}
}

// create creates or opens a file at a specified location and returns a write-only `File` object
pub fn create(path string) ?File {
  /*
	// NB: android/termux/bionic is also a kind of linux,
	// but linux syscalls there sometimes fail,
	// while the libc version should work.
	$if linux {
		$if !android {
			//$if macos {
			//	fd = C.syscall(398, path.str, 0x601, 0x1b6)
			//}
			//$if linux {
			fd = C.syscall(sys_creat, path.str, 511)
			//}
			if fd == -1 {
				return error('failed to create file "$path"')
			}
			file = File{
				fd: fd
				opened: true
			}
			return file
		}
	}
  */
	cfile := vfopen(path, 'wb')
	if cfile == voidptr(0) {
		return error('failed to create file "$path"')
	}
	fd := fileno(cfile)
	return File {
		cfile: cfile
		fd: fd
		opened: true
	}
}

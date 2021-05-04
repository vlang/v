module os

#include <sys/stat.h> // #include <signal.h>
#include <errno.h>

struct C.dirent {
	d_name [256]char
}

fn C.readdir(voidptr) &C.dirent

fn C.readlink(pathname &char, buf &char, bufsiz size_t) int

fn C.getline(voidptr, voidptr, voidptr) int

fn C.ftell(fp voidptr) int

fn C.sigaction(int, voidptr, int) int

fn C.open(&char, int, ...int) int

fn C.fdopen(fd int, mode &char) &C.FILE

fn C.ferror(stream &C.FILE) int

fn C.feof(stream &C.FILE) int

fn C.CopyFile(&u16, &u16, bool) int

// fn C.lstat(charptr, voidptr) u64

fn C._wstat64(&char, voidptr) u64

fn C.chown(&char, int, int) int

fn C.ftruncate(voidptr, u64) int

fn C._chsize_s(voidptr, u64) int

// fn C.proc_pidpath(int, byteptr, int) int
struct C.stat {
	st_size  u64
	st_mode  u32
	st_mtime int
}

struct C.__stat64 {
	st_size  u64
	st_mode  u32
	st_mtime int
}

struct C.DIR {
}

type FN_SA_Handler = fn (sig int)

struct C.sigaction {
mut:
	sa_mask      int
	sa_sigaction int
	sa_flags     int
	sa_handler   FN_SA_Handler
}

struct C.dirent {
	d_name &byte
}

// read_bytes returns all bytes read from file in `path`.
[manualfree]
pub fn read_bytes(path string) ?[]byte {
	mut fp := vfopen(path, 'rb') ?
	cseek := C.fseek(fp, 0, C.SEEK_END)
	if cseek != 0 {
		return error('fseek failed')
	}
	fsize := C.ftell(fp)
	if fsize < 0 {
		return error('ftell failed')
	}
	C.rewind(fp)
	mut res := []byte{len: fsize}
	nr_read_elements := int(C.fread(res.data, fsize, 1, fp))
	if nr_read_elements == 0 && fsize > 0 {
		return error('fread failed')
	}
	C.fclose(fp)
	fres := res[0..nr_read_elements * fsize].clone()
	unsafe { res.free() }
	return fres
}

// read_file reads the file in `path` and returns the contents.
pub fn read_file(path string) ?string {
	mode := 'rb'
	mut fp := vfopen(path, mode) ?
	defer {
		C.fclose(fp)
	}
	cseek := C.fseek(fp, 0, C.SEEK_END)
	if cseek != 0 {
		return error('fseek failed')
	}
	fsize := C.ftell(fp)
	if fsize < 0 {
		return error('ftell failed')
	}
	// C.fseek(fp, 0, SEEK_SET)  // same as `C.rewind(fp)` below
	C.rewind(fp)
	unsafe {
		mut str := malloc(fsize + 1)
		nelements := int(C.fread(str, fsize, 1, fp))
		is_eof := int(C.feof(fp))
		is_error := int(C.ferror(fp))
		if is_eof == 0 && is_error != 0 {
			free(str)
			return error('fread failed')
		}
		str[fsize] = 0
		if nelements == 0 {
			// It is highly likely that the file was a virtual file from
			// /sys or /proc, with information generated on the fly, so
			// fsize was not reliably reported. Using vstring() here is
			// slower (it calls strlen internally), but will return more
			// consistent results.
			// For example reading from /sys/class/sound/card0/id produces
			// a `PCH\n` string, but fsize is 4096, and otherwise you would
			// get a V string with .len = 4096 and .str = "PCH\n\\000".
			return str.vstring()
		}
		return str.vstring_with_len(fsize)
	}
}

// ***************************** OS ops ************************
//
// truncate changes the size of the file located in `path` to `len`.
// Note that changing symbolic links on Windows only works as admin.
pub fn truncate(path string, len u64) ? {
	fp := C.open(&char(path.str), o_wronly | o_trunc)
	defer {
		C.close(fp)
	}
	if fp < 0 {
		return error_with_code(posix_get_error_msg(C.errno), C.errno)
	}
	$if windows {
		if C._chsize_s(fp, len) != 0 {
			return error_with_code(posix_get_error_msg(C.errno), C.errno)
		}
	} $else {
		if C.ftruncate(fp, len) != 0 {
			return error_with_code(posix_get_error_msg(C.errno), C.errno)
		}
	}
}

// file_size returns the size of the file located in `path`.
// If an error occurs it returns 0.
// Note that use of this on symbolic links on Windows returns always 0.
pub fn file_size(path string) u64 {
	mut s := C.stat{}
	unsafe {
		$if x64 {
			$if windows {
				mut swin := C.__stat64{}
				if C._wstat64(&char(path.to_wide()), voidptr(&swin)) != 0 {
					eprintln('os.file_size() Cannot determine file-size: ' +
						posix_get_error_msg(C.errno))
					return 0
				}
				return swin.st_size
			} $else {
				if C.stat(&char(path.str), &s) != 0 {
					eprintln('os.file_size() Cannot determine file-size: ' +
						posix_get_error_msg(C.errno))
					return 0
				}
				return u64(s.st_size)
			}
		}
		$if x32 {
			$if debug {
				println('Using os.file_size() on 32bit systems may not work on big files.')
			}
			$if windows {
				if C._wstat(path.to_wide(), voidptr(&s)) != 0 {
					eprintln('os.file_size() Cannot determine file-size: ' +
						posix_get_error_msg(C.errno))
					return 0
				}
				return u64(s.st_size)
			} $else {
				if C.stat(&char(path.str), &s) != 0 {
					eprintln('os.file_size() Cannot determine file-size: ' +
						posix_get_error_msg(C.errno))
					return 0
				}
				return u64(s.st_size)
			}
		}
	}
	return 0
}

// mv moves files or folders from `src` to `dst`.
pub fn mv(src string, dst string) ? {
	mut rdst := dst
	if is_dir(rdst) {
		rdst = join_path(rdst.trim_right(path_separator), file_name(src.trim_right(path_separator)))
	}
	$if windows {
		w_src := src.replace('/', '\\')
		w_dst := rdst.replace('/', '\\')
		ret := C._wrename(w_src.to_wide(), w_dst.to_wide())
		if ret != 0 {
			return error_with_code('failed to rename $src to $dst', int(ret))
		}
	} $else {
		ret := C.rename(&char(src.str), &char(rdst.str))
		if ret != 0 {
			return error_with_code('failed to rename $src to $dst', int(ret))
		}
	}
}

// cp copies files or folders from `src` to `dst`.
pub fn cp(src string, dst string) ? {
	$if windows {
		w_src := src.replace('/', '\\')
		w_dst := dst.replace('/', '\\')
		if C.CopyFile(w_src.to_wide(), w_dst.to_wide(), false) == 0 {
			result := C.GetLastError()
			return error_with_code('failed to copy $src to $dst', int(result))
		}
	} $else {
		fp_from := C.open(&char(src.str), C.O_RDONLY)
		if fp_from < 0 { // Check if file opened
			return error_with_code('cp: failed to open $src', int(fp_from))
		}
		fp_to := C.open(&char(dst.str), C.O_WRONLY | C.O_CREAT | C.O_TRUNC, C.S_IWUSR | C.S_IRUSR)
		if fp_to < 0 { // Check if file opened (permissions problems ...)
			C.close(fp_from)
			return error_with_code('cp (permission): failed to write to $dst (fp_to: $fp_to)',
				int(fp_to))
		}
		// TODO use defer{} to close files in case of error or return.
		// Currently there is a C-Error when building.
		mut buf := [1024]byte{}
		mut count := 0
		for {
			count = C.read(fp_from, &buf[0], sizeof(buf))
			if count == 0 {
				break
			}
			if C.write(fp_to, &buf[0], count) < 0 {
				C.close(fp_to)
				C.close(fp_from)
				return error_with_code('cp: failed to write to $dst', int(-1))
			}
		}
		from_attr := C.stat{}
		unsafe {
			C.stat(&char(src.str), &from_attr)
		}
		if C.chmod(&char(dst.str), from_attr.st_mode) < 0 {
			C.close(fp_to)
			C.close(fp_from)
			return error_with_code('failed to set permissions for $dst', int(-1))
		}
		C.close(fp_to)
		C.close(fp_from)
	}
}

// vfopen returns an opened C file, given its path and open mode.
// NB: os.vfopen is useful for compatibility with C libraries, that expect `FILE *`.
// If you write pure V code, os.create or os.open are more convenient.
pub fn vfopen(path string, mode string) ?&C.FILE {
	if path.len == 0 {
		return error('vfopen called with ""')
	}
	mut fp := voidptr(0)
	$if windows {
		fp = C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		fp = C.fopen(&char(path.str), &char(mode.str))
	}
	if isnil(fp) {
		return error('failed to open file "$path"')
	} else {
		return fp
	}
}

// fileno returns the file descriptor of an opened C file.
pub fn fileno(cfile voidptr) int {
	$if windows {
		return C._fileno(cfile)
	} $else {
		mut cfile_casted := &C.FILE(0) // FILE* cfile_casted = 0;
		cfile_casted = cfile
		// Required on FreeBSD/OpenBSD/NetBSD as stdio.h defines fileno(..) with a macro
		// that performs a field access on its argument without casting from void*.
		return C.fileno(cfile_casted)
	}
}

// vpopen system starts the specified command, waits for it to complete, and returns its code.
fn vpopen(path string) voidptr {
	// *C.FILE {
	$if windows {
		mode := 'rb'
		wpath := path.to_wide()
		return C._wpopen(wpath, mode.to_wide())
	} $else {
		cpath := path.str
		return C.popen(&char(cpath), c'r')
	}
}

fn posix_wait4_to_exit_status(waitret int) (int, bool) {
	$if windows {
		return waitret, false
	} $else {
		mut ret := 0
		mut is_signaled := true
		// (see man system, man 2 waitpid: C macro WEXITSTATUS section)
		if C.WIFEXITED(waitret) {
			ret = C.WEXITSTATUS(waitret)
			is_signaled = false
		} else if C.WIFSIGNALED(waitret) {
			ret = C.WTERMSIG(waitret)
			is_signaled = true
		}
		return ret, is_signaled
	}
}

// posix_get_error_msg return error code representation in string.
pub fn posix_get_error_msg(code int) string {
	ptr_text := C.strerror(code) // voidptr?
	if ptr_text == 0 {
		return ''
	}
	return unsafe { tos3(ptr_text) }
}

// vpclose will close a file pointer opened with `vpopen`.
fn vpclose(f voidptr) int {
	$if windows {
		return C._pclose(f)
	} $else {
		ret, _ := posix_wait4_to_exit_status(C.pclose(f))
		return ret
	}
}

// system works like `exec`, but only returns a return code.
pub fn system(cmd string) int {
	// if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
	// TODO remove panic
	// panic(';, &&, || and \\n are not allowed in shell commands')
	// }
	mut ret := 0
	$if windows {
		// overcome bug in system & _wsystem (cmd) when first char is quote `"`
		wcmd := if cmd.len > 1 && cmd[0] == `"` && cmd[1] != `"` { '"$cmd"' } else { cmd }
		unsafe {
			ret = C._wsystem(wcmd.to_wide())
		}
	} $else {
		$if ios {
			unsafe {
				arg := [c'/bin/sh', c'-c', &byte(cmd.str), 0]
				pid := 0
				ret = C.posix_spawn(&pid, c'/bin/sh', 0, 0, arg.data, 0)
				status := 0
				ret = C.waitpid(pid, &status, 0)
				if C.WIFEXITED(status) {
					ret = C.WEXITSTATUS(status)
				}
			}
		} $else {
			unsafe {
				ret = C.system(&char(cmd.str))
			}
		}
	}
	if ret == -1 {
		print_c_errno()
	}
	$if !windows {
		pret, is_signaled := posix_wait4_to_exit_status(ret)
		if is_signaled {
			println('Terminated by signal ${ret:2d} (' + sigint_to_signal_name(pret) + ')')
		}
		ret = pret
	}
	return ret
}

// exists returns true if `path` (file or directory) exists.
pub fn exists(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), f_ok) != -1
	} $else {
		return C.access(&char(path.str), f_ok) != -1
	}
}

// is_executable returns `true` if `path` is executable.
pub fn is_executable(path string) bool {
	$if windows {
		// NB: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/access-waccess?view=vs-2019
		// i.e. there is no X bit there, the modes can be:
		// 00 Existence only
		// 02 Write-only
		// 04 Read-only
		// 06 Read and write
		p := real_path(path)
		return (exists(p) && p.ends_with('.exe'))
	}
	$if solaris {
		statbuf := C.stat{}
		unsafe {
			if C.stat(&char(path.str), &statbuf) != 0 {
				return false
			}
		}
		return (int(statbuf.st_mode) & (s_ixusr | s_ixgrp | s_ixoth)) != 0
	}
	return C.access(&char(path.str), x_ok) != -1
}

// is_writable returns `true` if `path` is writable.
pub fn is_writable(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), w_ok) != -1
	} $else {
		return C.access(&char(path.str), w_ok) != -1
	}
}

// is_readable returns `true` if `path` is readable.
pub fn is_readable(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		return C._waccess(p.to_wide(), r_ok) != -1
	} $else {
		return C.access(&char(path.str), r_ok) != -1
	}
}

// rm removes file in `path`.
pub fn rm(path string) ? {
	mut rc := 0
	$if windows {
		rc = C._wremove(path.to_wide())
	} $else {
		rc = C.remove(&char(path.str))
	}
	if rc == -1 {
		return error('Failed to remove "$path": ' + posix_get_error_msg(C.errno))
	}
	// C.unlink(path.cstr())
}

// rmdir removes a specified directory.
pub fn rmdir(path string) ? {
	$if windows {
		rc := C.RemoveDirectory(&char(path.to_wide()))
		if rc == 0 {
			// https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-removedirectorya - 0 is failure
			return error('Failed to remove "$path": ' + posix_get_error_msg(C.errno))
		}
	} $else {
		rc := C.rmdir(&char(path.str))
		if rc == -1 {
			return error(posix_get_error_msg(C.errno))
		}
	}
}

// print_c_errno will print the current value of `C.errno`.
fn print_c_errno() {
	e := C.errno
	se := unsafe { tos_clone(&byte(C.strerror(C.errno))) }
	println('errno=$e err=$se')
}

// get_raw_line returns a one-line string from stdin along with '\n' if there is any.
pub fn get_raw_line() string {
	$if windows {
		unsafe {
			max_line_chars := 256
			buf := malloc(max_line_chars * 2)
			h_input := C.GetStdHandle(C.STD_INPUT_HANDLE)
			mut bytes_read := u32(0)
			if is_atty(0) > 0 {
				x := C.ReadConsole(h_input, buf, max_line_chars * 2, &bytes_read, 0)
				if !x {
					return tos(buf, 0)
				}
				return string_from_wide2(&u16(buf), int(bytes_read))
			}
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, 1, C.LPDWORD(&bytes_read), 0)
				if !res && offset == 0 {
					return tos(buf, 0)
				}
				if !res || bytes_read == 0 {
					break
				}
				if *pos == `\n` || *pos == `\r` {
					offset++
					break
				}
				offset++
			}
			return buf.vstring_with_len(offset)
		}
	} $else {
		max := size_t(0)
		buf := &char(0)
		nr_chars := unsafe { C.getline(&buf, &max, C.stdin) }
		return unsafe { tos(&byte(buf), if nr_chars < 0 { 0 } else { nr_chars }) }
	}
}

// get_raw_stdin will get the raw input from stdin.
pub fn get_raw_stdin() []byte {
	$if windows {
		unsafe {
			block_bytes := 512
			mut old_size := block_bytes
			mut buf := malloc(block_bytes)
			h_input := C.GetStdHandle(C.STD_INPUT_HANDLE)
			mut bytes_read := 0
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, block_bytes, C.LPDWORD(&bytes_read), 0)
				offset += bytes_read
				if !res {
					break
				}
				new_size := offset + block_bytes + (block_bytes - bytes_read)
				buf = realloc_data(buf, old_size, new_size)
				old_size = new_size
			}
			return array{
				element_size: 1
				data: voidptr(buf)
				len: offset
				cap: offset
			}
		}
	} $else {
		max := size_t(0)
		buf := &char(0)
		nr_chars := unsafe { C.getline(&buf, &max, C.stdin) }
		return array{
			element_size: 1
			data: voidptr(buf)
			len: if nr_chars < 0 { 0 } else { nr_chars }
			cap: int(max)
		}
	}
}

// read_file_array reads an array of `T` values from file `path`.
pub fn read_file_array<T>(path string) []T {
	a := T{}
	tsize := int(sizeof(a))
	// prepare for reading, get current file size
	mut fp := vfopen(path, 'rb') or { return []T{} }
	C.fseek(fp, 0, C.SEEK_END)
	fsize := C.ftell(fp)
	C.rewind(fp)
	// read the actual data from the file
	len := fsize / tsize
	buf := unsafe { malloc(fsize) }
	C.fread(buf, fsize, 1, fp)
	C.fclose(fp)
	return unsafe {
		array{
			element_size: tsize
			data: buf
			len: len
			cap: len
		}
	}
}

pub fn on_segfault(f voidptr) {
	$if windows {
		return
	}
	$if macos {
		C.printf(c'TODO')
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

// executable returns the path name of the executable that started the current
// process.
[manualfree]
pub fn executable() string {
	$if linux {
		mut xresult := vcalloc(max_path_len)
		count := C.readlink(c'/proc/self/exe', &char(xresult), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/self/exe to get exe path')
			return executable_fallback()
		}
		res := unsafe { xresult.vstring() }.clone()
		unsafe { free(xresult) }
		return res
	}
	$if windows {
		max := 512
		size := max * 2 // max_path_len * sizeof(wchar_t)
		mut result := unsafe { &u16(vcalloc(size)) }
		len := C.GetModuleFileName(0, result, max)
		// determine if the file is a windows symlink
		attrs := C.GetFileAttributesW(result)
		is_set := attrs & 0x400 // FILE_ATTRIBUTE_REPARSE_POINT
		if is_set != 0 { // it's a windows symlink
			// gets handle with GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
			file := C.CreateFile(result, 0x80000000, 1, 0, 3, 0x80, 0)
			if file != voidptr(-1) {
				final_path := unsafe { &u16(vcalloc(size)) }
				// https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfinalpathnamebyhandlew
				final_len := C.GetFinalPathNameByHandleW(file, final_path, size, 0)
				if final_len < size {
					ret := unsafe { string_from_wide2(final_path, final_len) }
					// remove '\\?\' from beginning (see link above)
					return ret[4..]
				} else {
					eprintln('os.executable() saw that the executable file path was too long')
				}
			}
			C.CloseHandle(file)
		}
		return unsafe { string_from_wide2(result, len) }
	}
	$if macos {
		mut result := vcalloc(max_path_len)
		pid := C.getpid()
		ret := proc_pidpath(pid, result, max_path_len)
		if ret <= 0 {
			eprintln('os.executable() failed at calling proc_pidpath with pid: $pid . proc_pidpath returned $ret ')
			return executable_fallback()
		}
		return unsafe { result.vstring() }
	}
	$if freebsd {
		mut result := vcalloc(max_path_len)
		mib := [1 /* CTL_KERN */, 14 /* KERN_PROC */, 12 /* KERN_PROC_PATHNAME */, -1]
		size := max_path_len
		unsafe { C.sysctl(mib.data, 4, result, &size, 0, 0) }
		return unsafe { result.vstring() }
	}
	// "Sadly there is no way to get the full path of the executed file in OpenBSD."
	$if openbsd {
	}
	$if solaris {
	}
	$if haiku {
	}
	$if netbsd {
		mut result := vcalloc(max_path_len)
		count := C.readlink(c'/proc/curproc/exe', &char(result), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/exe to get exe path')
			return executable_fallback()
		}
		return unsafe { result.vstring_with_len(count) }
	}
	$if dragonfly {
		mut result := vcalloc(max_path_len)
		count := C.readlink(c'/proc/curproc/file', &char(result), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/file to get exe path')
			return executable_fallback()
		}
		return unsafe { result.vstring_with_len(count) }
	}
	return executable_fallback()
}

// is_dir returns a `bool` indicating whether the given `path` is a directory.
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
		if unsafe { C.stat(&char(path.str), &statbuf) } != 0 {
			return false
		}
		// ref: https://code.woboq.org/gcc/include/sys/stat.h.html
		val := int(statbuf.st_mode) & s_ifmt
		return val == s_ifdir
	}
}

// is_link returns a boolean indicating whether `path` is a link.
pub fn is_link(path string) bool {
	$if windows {
		return false // TODO
	} $else {
		statbuf := C.stat{}
		if C.lstat(&char(path.str), &statbuf) != 0 {
			return false
		}
		return int(statbuf.st_mode) & s_ifmt == s_iflnk
	}
}

// chdir changes the current working directory to the new directory in `path`.
pub fn chdir(path string) {
	$if windows {
		C._wchdir(path.to_wide())
	} $else {
		_ = C.chdir(&char(path.str))
	}
}

// getwd returns the absolute path of the current directory.
pub fn getwd() string {
	$if windows {
		max := 512 // max_path_len * sizeof(wchar_t)
		unsafe {
			buf := &u16(vcalloc(max * 2))
			if C._wgetcwd(buf, max) == 0 {
				free(buf)
				return ''
			}
			return string_from_wide(buf)
		}
	} $else {
		buf := vcalloc(512)
		unsafe {
			if C.getcwd(&char(buf), 512) == 0 {
				free(buf)
				return ''
			}
			res := buf.vstring().clone()
			free(buf)
			return res
		}
	}
}

// real_path returns the full absolute path for fpath, with all relative ../../, symlinks and so on resolved.
// See http://pubs.opengroup.org/onlinepubs/9699919799/functions/realpath.html
// Also https://insanecoding.blogspot.com/2007/11/pathmax-simply-isnt.html
// and https://insanecoding.blogspot.com/2007/11/implementing-realpath-in-c.html
// NB: this particular rabbit hole is *deep* ...
[manualfree]
pub fn real_path(fpath string) string {
	mut fullpath := &byte(0)
	defer {
		unsafe { free(fullpath) }
	}

	$if windows {
		fullpath = unsafe { &u16(vcalloc(max_path_len * 2)) }
		// TODO: check errors if path len is not enough
		ret := C.GetFullPathName(fpath.to_wide(), max_path_len, fullpath, 0)
		if ret == 0 {
			return fpath
		}
	} $else {
		fullpath = vcalloc(max_path_len)
		ret := &char(C.realpath(&char(fpath.str), &char(fullpath)))
		if ret == 0 {
			return fpath
		}
	}

	mut res := ''
	$if windows {
		res = unsafe { string_from_wide(fullpath) }
	} $else {
		res = unsafe { fullpath.vstring() }
	}
	nres := normalize_drive_letter(res)
	cres := nres.clone()
	return cres
}

fn normalize_drive_letter(path string) string {
	// normalize_drive_letter is needed, because a path like c:\nv\.bin (note the small `c`)
	// in %PATH is NOT recognized by cmd.exe (and probably other programs too)...
	// Capital drive letters do work fine.
	$if !windows {
		return path
	}
	if path.len > 2 && path[0] >= `a` && path[0] <= `z` && path[1] == `:`
		&& path[2] == path_separator[0] {
		unsafe {
			x := &path.str[0]
			(*x) = *x - 32
		}
	}
	return path
}

// signal will assign `handler` callback to be called when `signum` signal is received.
pub fn signal(signum int, handler voidptr) voidptr {
	res := unsafe { C.signal(signum, handler) }
	return res
}

// fork will fork the current system process and return the pid of the fork.
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

// wait blocks the calling process until one of its child processes exits or a signal is received.
// After child process terminates, parent continues its execution after wait system call instruction.
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

// file_last_mod_unix returns the "last modified" time stamp of file in `path`.
pub fn file_last_mod_unix(path string) int {
	attr := C.stat{}
	// # struct stat attr;
	unsafe { C.stat(&char(path.str), &attr) }
	// # stat(path.str, &attr);
	return attr.st_mtime
	// # return attr.st_mtime ;
}

// flush will flush the stdout buffer.
pub fn flush() {
	C.fflush(C.stdout)
}

// chmod change file access attributes of `path` to `mode`.
// Octals like `0o600` can be used.
pub fn chmod(path string, mode int) {
	if C.chmod(&char(path.str), mode) != 0 {
		panic('chmod failed: ' + posix_get_error_msg(C.errno))
	}
}

// chown changes the owner and group attributes of `path` to `owner` and `group`.
pub fn chown(path string, owner int, group int) ? {
	$if windows {
		return error('os.chown() not implemented for Windows')
	} $else {
		if C.chown(&char(path.str), owner, group) != 0 {
			return error_with_code(posix_get_error_msg(C.errno), C.errno)
		}
	}
}

// open_append opens `path` file for appending.
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
			cfile: C.fopen(&char(cpath), c'ab')
		}
	}
	if isnil(file.cfile) {
		return error('failed to create(append) file "$path"')
	}
	file.is_opened = true
	return file
}

// execvp - loads and executes a new child process, *in place* of the current process.
// The child process executable is located in `cmdpath`.
// The arguments, that will be passed to it are in `args`.
// NB: this function will NOT return when successfull, since
// the child process will take control over execution.
pub fn execvp(cmdpath string, args []string) ? {
	mut cargs := []&char{}
	cargs << &char(cmdpath.str)
	for i in 0 .. args.len {
		cargs << &char(args[i].str)
	}
	cargs << &char(0)
	mut res := int(0)
	$if windows {
		res = C._execvp(&char(cmdpath.str), cargs.data)
	} $else {
		res = C.execvp(&char(cmdpath.str), cargs.data)
	}
	if res == -1 {
		return error_with_code(posix_get_error_msg(C.errno), C.errno)
	}
	// just in case C._execvp returned ... that happens on windows ...
	exit(res)
}

// execve - loads and executes a new child process, *in place* of the current process.
// The child process executable is located in `cmdpath`.
// The arguments, that will be passed to it are in `args`.
// You can pass environment variables to through `envs`.
// NB: this function will NOT return when successfull, since
// the child process will take control over execution.
pub fn execve(cmdpath string, args []string, envs []string) ? {
	mut cargv := []&char{}
	mut cenvs := []&char{}
	cargv << &char(cmdpath.str)
	for i in 0 .. args.len {
		cargv << &char(args[i].str)
	}
	for i in 0 .. envs.len {
		cenvs << &char(envs[i].str)
	}
	cargv << &char(0)
	cenvs << &char(0)
	mut res := int(0)
	$if windows {
		res = C._execve(&char(cmdpath.str), cargv.data, cenvs.data)
	} $else {
		res = C.execve(&char(cmdpath.str), cargv.data, cenvs.data)
	}
	// NB: normally execve does not return at all.
	// If it returns, then something went wrong...
	if res == -1 {
		return error_with_code(posix_get_error_msg(C.errno), C.errno)
	}
}

module os

import strings

#include <sys/stat.h> // #include <signal.h>
#include <errno.h>

$if freebsd || openbsd {
	#include <sys/sysctl.h>
}

@[markused]
pub const args = arguments()

fn C.readdir(voidptr) &C.dirent

fn C.readlink(pathname &char, buf &char, bufsiz usize) int

fn C.getline(voidptr, voidptr, voidptr) int

fn C.sigaction(int, voidptr, int) int

fn C.open(&char, int, ...int) int

fn C._wopen(&u16, int, ...int) int

fn C.fdopen(fd int, mode &char) &C.FILE

fn C.ferror(stream &C.FILE) int

fn C.feof(stream &C.FILE) int

fn C.CopyFile(&u16, &u16, bool) int

// fn C.lstat(charptr, voidptr) u64

fn C._wstat64(&u16, voidptr) u64

fn C.chown(&char, int, int) int

fn C.ftruncate(voidptr, u64) int

fn C._chsize_s(voidptr, u64) int

// read_bytes returns all bytes read from file in `path`.
@[manualfree]
pub fn read_bytes(path string) ![]u8 {
	mut fp := vfopen(path, 'rb')!
	defer {
		C.fclose(fp)
	}
	fsize := find_cfile_size(fp)!
	if fsize == 0 {
		mut sb := slurp_file_in_builder(fp)!
		return unsafe { sb.reuse_as_plain_u8_array() }
	}
	mut res := []u8{len: fsize}
	nr_read_elements := int(C.fread(res.data, 1, fsize, fp))
	if nr_read_elements == 0 && fsize > 0 {
		return error('fread failed')
	}
	res.trim(nr_read_elements)
	return res
}

// write_bytes writes all the `bytes` to `path`.
@[manualfree]
pub fn write_bytes(path string, bytes []u8) ! {
	write_file_array(path, bytes)!
}

fn find_cfile_size(fp &C.FILE) !int {
	// NB: Musl's fseek returns -1 for virtual files, while Glibc's fseek returns 0
	cseek := C.fseek(fp, 0, C.SEEK_END)
	raw_fsize := C.ftell(fp)
	if raw_fsize != 0 && cseek != 0 {
		return error('fseek failed')
	}
	if cseek != 0 && raw_fsize < 0 {
		return error('ftell failed')
	}
	len := int(raw_fsize)
	// For files > 2GB, C.ftell can return values that, when cast to `int`, can result in values below 0.
	if i64(len) < raw_fsize {
		return error('int(${raw_fsize}) cast results in ${len}')
	}
	C.rewind(fp)
	return len
}

const buf_size = 4096

// slurp_file_in_builder reads an entire file into a strings.Builder chunk by chunk, without relying on its file size.
// It is intended for reading 0 sized files, or a dynamic files in a virtual filesystem like /proc/cpuinfo.
// For these, we can not allocate all memory in advance (since we do not know the final size), and so we have no choice
// but to read the file in `buf_size` chunks.
@[manualfree]
fn slurp_file_in_builder(fp &C.FILE) !strings.Builder {
	buf := [buf_size]u8{}
	mut sb := strings.new_builder(buf_size)
	for {
		mut read_bytes := fread(&buf[0], 1, buf_size, fp) or {
			if err is Eof {
				break
			}
			unsafe { sb.free() }
			return err
		}
		unsafe { sb.write_ptr(&buf[0], read_bytes) }
	}
	return sb
}

// read_file reads the file in `path` and returns the contents.
@[manualfree]
pub fn read_file(path string) !string {
	mode := 'rb'
	mut fp := vfopen(path, mode)!
	defer {
		C.fclose(fp)
	}
	allocate := find_cfile_size(fp)!
	if allocate == 0 {
		mut sb := slurp_file_in_builder(fp)!
		res := sb.str()
		unsafe { sb.free() }
		return res
	}
	unsafe {
		mut str := malloc_noscan(allocate + 1)
		nelements := int(C.fread(str, 1, allocate, fp))
		is_eof := int(C.feof(fp))
		is_error := int(C.ferror(fp))
		if is_eof == 0 && is_error != 0 {
			free(str)
			return error('fread failed')
		}
		str[nelements] = 0
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
		return str.vstring_with_len(nelements)
	}
}

// truncate changes the size of the file located in `path` to `len`.
// Note that changing symbolic links on Windows only works as admin.
pub fn truncate(path string, len u64) ! {
	fp := $if windows {
		C._wopen(path.to_wide(), o_wronly | o_trunc, 0)
	} $else {
		C.open(&char(path.str), o_wronly | o_trunc, 0)
	}
	if fp < 0 {
		return error_posix()
	}
	defer {
		C.close(fp)
	}
	$if windows {
		if C._chsize_s(fp, len) != 0 {
			return error_posix()
		}
	} $else {
		if C.ftruncate(fp, len) != 0 {
			return error_posix()
		}
	}
}

// file_size returns the size in bytes, of the file located in `path`.
// If an error occurs it returns 0.
// Note that use of this on symbolic links on Windows returns always 0.
pub fn file_size(path string) u64 {
	attr := stat(path) or {
		eprintln('os.file_size() Cannot determine file-size: ' + posix_get_error_msg(C.errno))
		return 0
	}
	return attr.size
}

// rename_dir renames the folder from `src` to `dst`.
// Use mv to move or rename a file in a platform independent manner.
pub fn rename_dir(src string, dst string) ! {
	$if windows {
		w_src := src.replace('/', '\\')
		w_dst := dst.replace('/', '\\')
		ret := C._wrename(w_src.to_wide(), w_dst.to_wide())
		if ret != 0 {
			return error_with_code('failed to rename ${src} to ${dst}', int(ret))
		}
	} $else {
		ret := C.rename(&char(src.str), &char(dst.str))
		if ret != 0 {
			return error_with_code('failed to rename ${src} to ${dst}', ret)
		}
	}
}

// rename renames the file or folder from `src` to `dst`.
// Use mv to move or rename a file in a platform independent manner.
pub fn rename(src string, dst string) ! {
	mut rdst := dst
	if is_dir(rdst) {
		rdst = join_path_single(rdst.trim_right(path_separator), file_name(src.trim_right(path_separator)))
	}
	$if windows {
		w_src := src.replace('/', '\\')
		w_dst := rdst.replace('/', '\\')
		ret := C._wrename(w_src.to_wide(), w_dst.to_wide())
		if ret != 0 {
			return error_with_code('failed to rename ${src} to ${dst}', int(ret))
		}
	} $else {
		ret := C.rename(&char(src.str), &char(rdst.str))
		if ret != 0 {
			return error_with_code('failed to rename ${src} to ${dst}', ret)
		}
	}
}

// cp copies files or folders from `src` to `dst`.
pub fn cp(src string, dst string) ! {
	$if windows {
		w_src := src.replace('/', '\\')
		w_dst := dst.replace('/', '\\')
		if C.CopyFile(w_src.to_wide(), w_dst.to_wide(), false) == 0 {
			// we must save error immediately, or it will be overwritten by other API function calls.
			code := int(C.GetLastError())
			return error_win32(
				msg:  'failed to copy ${src} to ${dst}'
				code: code
			)
		}
	} $else {
		fp_from := C.open(&char(src.str), C.O_RDONLY, 0)
		if fp_from < 0 { // Check if file opened
			return error_with_code('cp: failed to open ${src}', int(fp_from))
		}
		fp_to := C.open(&char(dst.str), C.O_WRONLY | C.O_CREAT | C.O_TRUNC, C.S_IWUSR | C.S_IRUSR)
		if fp_to < 0 { // Check if file opened (permissions problems ...)
			C.close(fp_from)
			return error_with_code('cp (permission): failed to write to ${dst} (fp_to: ${fp_to})',
				int(fp_to))
		}
		// TODO: use defer{} to close files in case of error or return.
		// Currently there is a C-Error when building.
		mut buf := [1024]u8{}
		mut count := 0
		for {
			count = C.read(fp_from, &buf[0], sizeof(buf))
			if count == 0 {
				break
			}
			if C.write(fp_to, &buf[0], count) < 0 {
				C.close(fp_to)
				C.close(fp_from)
				return error_with_code('cp: failed to write to ${dst}', int(-1))
			}
		}
		from_attr := stat(src)!
		if C.chmod(&char(dst.str), from_attr.mode) < 0 {
			C.close(fp_to)
			C.close(fp_from)
			return error_with_code('failed to set permissions for ${dst}', int(-1))
		}
		C.close(fp_to)
		C.close(fp_from)
	}
}

// vfopen returns an opened C file, given its path and open mode.
// Note: os.vfopen is useful for compatibility with C libraries, that expect `FILE *`.
// If you write pure V code, os.create or os.open are more convenient.
pub fn vfopen(path string, mode string) !&C.FILE {
	if path == '' {
		return error('vfopen called with ""')
	}
	mut fp := unsafe { nil }
	$if windows {
		fp = C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		fp = C.fopen(&char(path.str), &char(mode.str))
	}
	if isnil(voidptr(fp)) {
		return error_posix(msg: 'failed to open file "${path}"')
	} else {
		return fp
	}
}

// fileno returns the file descriptor of an opened C file.
pub fn fileno(cfile voidptr) int {
	$if windows {
		return C._fileno(cfile)
	} $else {
		mut cfile_casted := &C.FILE(unsafe { nil }) // FILE* cfile_casted = 0;
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

// posix_get_error_msg returns error code representation in string.
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
	// TODO: remove panic
	// panic(';, &&, || and \\n are not allowed in shell commands')
	// }
	mut ret := 0
	$if windows {
		// overcome bug in system & _wsystem (cmd) when first char is quote `"`
		wcmd := if cmd.len > 1 && cmd[0] == `"` && cmd[1] != `"` { '"${cmd}"' } else { cmd }
		flush_stdout()
		flush_stderr()
		unsafe {
			ret = C._wsystem(wcmd.to_wide())
		}
	} $else {
		$if ios {
			unsafe {
				arg := [c'/bin/sh', c'-c', &u8(cmd.str), 0]
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
		ret = pret
		if is_signaled {
			eprintln('Terminated by signal ${pret:2d} (' + sigint_to_signal_name(pret) + ')')
			ret = pret + 128
		}
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
// Warning: `is_executable()` is known to cause a TOCTOU vulnerability when used incorrectly
// (for more information: https://github.com/vlang/v/blob/master/vlib/os/README.md).
pub fn is_executable(path string) bool {
	$if windows {
		// Note: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/access-waccess?view=vs-2019
		// i.e. there is no X bit there, the modes can be:
		// 00 Existence only
		// 02 Write-only
		// 04 Read-only
		// 06 Read and write
		p := real_path(path)
		return exists(p) && (p.ends_with('.exe') || p.ends_with('.bat') || p.ends_with('.cmd'))
	}
	$if solaris {
		attr := stat(path) or { return false }
		return (int(attr.mode) & (s_ixusr | s_ixgrp | s_ixoth)) != 0
	}
	return C.access(&char(path.str), x_ok) != -1
}

// is_writable returns `true` if `path` is writable.
// Warning: `is_writable()` is known to cause a TOCTOU vulnerability when used incorrectly
// (for more information: https://github.com/vlang/v/blob/master/vlib/os/README.md).
@[manualfree]
pub fn is_writable(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		wp := p.to_wide()
		res := C._waccess(wp, w_ok) != -1
		unsafe { free(wp) } // &u16
		unsafe { p.free() }
		return res
	} $else {
		return C.access(&char(path.str), w_ok) != -1
	}
}

// is_readable returns `true` if `path` is readable.
// Warning: `is_readable()` is known to cause a TOCTOU vulnerability when used incorrectly
// (for more information: https://github.com/vlang/v/blob/master/vlib/os/README.md).
@[manualfree]
pub fn is_readable(path string) bool {
	$if windows {
		p := path.replace('/', '\\')
		wp := p.to_wide()
		res := C._waccess(wp, r_ok) != -1
		unsafe { free(wp) } // &u16
		unsafe { p.free() }
		return res
	} $else {
		return C.access(&char(path.str), r_ok) != -1
	}
}

// rm removes file in `path`.
pub fn rm(path string) ! {
	mut rc := 0
	$if windows {
		rc = C._wremove(path.to_wide())
	} $else {
		rc = C.remove(&char(path.str))
	}
	if rc == -1 {
		return error_posix(msg: 'Failed to remove "${path}": ' + posix_get_error_msg(C.errno))
	}
	// C.unlink(path.cstr())
}

// rmdir removes a specified directory.
pub fn rmdir(path string) ! {
	$if windows {
		rc := C.RemoveDirectory(path.to_wide())
		if !rc {
			// https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-removedirectorya - 0 == false, is failure
			// we must save error immediately, or it will be overwritten by other API function calls.
			code := int(C.GetLastError())
			return error_win32(
				msg:  'Failed to remove "${path}"'
				code: code
			)
		}
	} $else {
		rc := C.rmdir(&char(path.str))
		if rc == -1 {
			return error_posix()
		}
	}
}

// print_c_errno will print the current value of `C.errno`.
fn print_c_errno() {
	e := C.errno
	se := unsafe { tos_clone(&u8(C.strerror(e))) }
	eprintln('errno=${e} err=${se}')
}

// get_raw_line returns a one-line string from stdin along with `\n` if there is any.
@[manualfree]
pub fn get_raw_line() string {
	$if windows {
		is_console := is_atty(0) > 0
		wide_char_size := if is_console { 2 } else { 1 }
		h_input := C.GetStdHandle(C.STD_INPUT_HANDLE)
		if h_input == C.INVALID_HANDLE_VALUE {
			return ''
		}
		unsafe {
			initial_size := 256 * wide_char_size
			mut buf := malloc_noscan(initial_size)
			defer { buf.free() }
			mut capacity := initial_size
			mut offset := 0

			for {
				required_space := offset + wide_char_size
				if required_space > capacity {
					new_capacity := capacity * 2
					new_buf := realloc_data(buf, capacity, new_capacity)
					if new_buf == 0 {
						break
					}
					buf = new_buf
					capacity = new_capacity
				}

				pos := buf + offset
				mut bytes_read := u32(0)
				res := if is_console {
					C.ReadConsole(h_input, pos, 1, voidptr(&bytes_read), 0)
				} else {
					C.ReadFile(h_input, pos, 1, voidptr(&bytes_read), 0)
				}

				if !res || bytes_read == 0 {
					break
				}

				// check for `\n` and Ctrl+Z
				if is_console {
					read_char := *(&u16(pos))
					if read_char == `\n` {
						offset += wide_char_size
						break
					} else if read_char == 0x1A {
						break
					}
				} else {
					read_byte := *pos
					if read_byte == `\n` {
						offset += wide_char_size
						break
					} else if read_byte == 0x1A {
						break
					}
				}

				offset += wide_char_size
			}

			return if is_console {
				string_from_wide2(&u16(buf), offset / 2)
			} else {
				// let defer buf.free() to avoid memory leak
				buf.vstring_with_len(offset).clone()
			}
		}
	} $else {
		max := usize(0)
		buf := &u8(unsafe { nil })
		nr_chars := unsafe { C.getline(voidptr(&buf), &max, C.stdin) }
		str := unsafe { tos(buf, if nr_chars < 0 { 0 } else { nr_chars }) }
		ret := str.clone()
		$if !autofree {
			unsafe {
				if nr_chars > 0 && buf != 0 {
					C.free(buf)
				}
			}
		}
		return ret
	}
}

// get_raw_stdin will get the raw input from stdin.
pub fn get_raw_stdin() []u8 {
	$if windows {
		unsafe {
			block_bytes := 512
			mut old_size := block_bytes
			mut buf := malloc_noscan(block_bytes)
			h_input := C.GetStdHandle(C.STD_INPUT_HANDLE)
			mut bytes_read := 0
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, block_bytes, voidptr(&bytes_read), 0)
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
				data:         voidptr(buf)
				len:          offset
				cap:          offset
			}
		}
	} $else {
		max := usize(0)
		buf := &u8(unsafe { nil })
		nr_chars := unsafe { C.getline(voidptr(&buf), &max, C.stdin) }
		return array{
			element_size: 1
			data:         voidptr(buf)
			len:          if nr_chars < 0 { 0 } else { nr_chars }
			cap:          int(max)
		}
	}
}

// read_file_array reads an array of `T` values from file `path`.
pub fn read_file_array[T](path string) []T {
	a := T{}
	tsize := int(sizeof(a))
	// prepare for reading, get current file size
	mut fp := vfopen(path, 'rb') or { return []T{} }
	C.fseek(fp, 0, C.SEEK_END)
	fsize := C.ftell(fp)
	C.rewind(fp)
	// read the actual data from the file
	len := fsize / tsize
	allocate := int(fsize)
	// On some systems C.ftell can return values in the 64-bit range
	// that, when cast to `int`, can result in values below 0.
	if i64(allocate) < fsize {
		panic_n2('cast to int results in (fsize, int(fsize)):', i64(fsize), i64(int(fsize)))
	}
	buf := unsafe {
		malloc_noscan(allocate)
	}
	nread := C.fread(buf, tsize, len, fp)
	C.fclose(fp)
	return unsafe {
		array{
			element_size: tsize
			data:         buf
			len:          int(nread)
			cap:          int(len)
		}
	}
}

// executable returns the path name of the executable that started the current
// process.
@[manualfree]
pub fn executable() string {
	mut result := [max_path_buffer_size]u8{}
	$if windows {
		pu16_result := unsafe { &u16(&result[0]) }
		len := C.GetModuleFileName(0, pu16_result, 512)
		// determine if the file is a windows symlink
		attrs := C.GetFileAttributesW(pu16_result)
		is_set := attrs & 0x400 // FILE_ATTRIBUTE_REPARSE_POINT
		if is_set != 0 { // it's a windows symlink
			// gets handle with GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
			file := C.CreateFile(pu16_result, 0x80000000, 1, 0, 3, 0x80, 0)
			if file != voidptr(-1) {
				defer {
					C.CloseHandle(file)
				}
				final_path := [max_path_buffer_size]u8{}
				// https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfinalpathnamebyhandlew
				final_len := C.GetFinalPathNameByHandleW(file, unsafe { &u16(&final_path[0]) },
					max_path_buffer_size, 0)
				if final_len < u32(max_path_buffer_size) {
					sret := unsafe { string_from_wide2(&u16(&final_path[0]), int(final_len)) }
					defer {
						unsafe { sret.free() }
					}
					// remove '\\?\' from beginning (see link above)
					sret_slice := sret[4..]
					res := sret_slice.clone()
					return res
				} else {
					eprintln('os.executable() saw that the executable file path was too long')
				}
			}
		}
		res := unsafe { string_from_wide2(pu16_result, int(len)) }
		return res
	}
	$if macos {
		pid := C.getpid()
		ret := proc_pidpath(pid, &result[0], max_path_len)
		if ret <= 0 {
			eprintln('os.executable() failed at calling proc_pidpath with pid: ${pid} . proc_pidpath returned ${ret} ')
			return executable_fallback()
		}
		res := unsafe { tos_clone(&result[0]) }
		return res
	}
	$if freebsd {
		bufsize := usize(max_path_buffer_size)
		mib := [C.CTL_KERN, C.KERN_PROC, C.KERN_PROC_PATHNAME, -1]!
		unsafe { C.sysctl(&mib[0], mib.len, &result[0], &bufsize, 0, 0) }
		res := unsafe { tos_clone(&result[0]) }
		return res
	}
	$if openbsd {
		// Sadly, unlike on FreeBSD, there is still no reliable way, to get the full path of the
		// current process in OpenBSD. However, we can try our best, by first checking, if the passed
		// argv[0] to the process, contains an absolute path (starting with /), according to the kernel,
		// and only go use the slower PATH scanning fallback method, when it does not.
		// See also https://github.com/gpakosz/whereami/blob/master/src/whereami.c#L591
		// and https://github.com/ziglang/zig/issues/6718#issuecomment-711134120 .
		mut pbuf := unsafe { &&u8(&result[0]) }
		bufsize := usize(max_path_buffer_size)
		pid := C.getpid()
		mib := [C.CTL_KERN, C.KERN_PROC_ARGS, pid, C.KERN_PROC_ARGV]!
		if unsafe { C.sysctl(&mib[0], mib.len, C.NULL, &bufsize, C.NULL, 0) } == 0 {
			if bufsize > max_path_buffer_size {
				pbuf = unsafe { &&u8(malloc(int(bufsize))) }
				defer {
					unsafe { free(pbuf) }
				}
			}
			if unsafe { C.sysctl(&mib[0], mib.len, pbuf, &bufsize, C.NULL, 0) } == 0 {
				if unsafe { *pbuf[0] } == `/` {
					res := unsafe { tos_clone(pbuf[0]) }
					return res
				}
			}
		}
		return executable_fallback()
	}
	$if netbsd {
		count := C.readlink(c'/proc/curproc/exe', &char(&result[0]), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/exe to get exe path')
			return executable_fallback()
		}
		res := unsafe { tos_clone(&result[0]) }
		return res
	}
	$if dragonfly {
		count := C.readlink(c'/proc/curproc/file', &char(&result[0]), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/curproc/file to get exe path')
			return executable_fallback()
		}
		res := unsafe { tos_clone(&result[0]) }
		return res
	}
	$if linux {
		count := C.readlink(c'/proc/self/exe', &char(&result[0]), max_path_len)
		if count < 0 {
			eprintln('os.executable() failed at reading /proc/self/exe to get exe path')
			return executable_fallback()
		}
		res := unsafe { tos_clone(&result[0]) }
		return res
	}
	$if solaris {
	}
	$if haiku {
	}
	return executable_fallback()
}

struct PathKind {
mut:
	is_file bool
	is_dir  bool
	is_link bool
}

// chdir changes the current working directory to the new directory in `path`.
pub fn chdir(path string) ! {
	ret := $if windows { C._wchdir(path.to_wide()) } $else { C.chdir(&char(path.str)) }
	if ret == -1 {
		return error_posix()
	}
}

// getwd returns the absolute path of the current directory.
@[manualfree]
pub fn getwd() string {
	unsafe {
		buf := [max_path_buffer_size]u8{}
		$if windows {
			if C._wgetcwd(&u16(&buf[0]), max_path_len) == 0 {
				return ''
			}
			res := string_from_wide(&u16(&buf[0]))
			return res
		} $else {
			if C.getcwd(&char(&buf[0]), max_path_len) == 0 {
				return ''
			}
			res := tos_clone(&buf[0])
			return res
		}
	}
}

// real_path returns the full absolute path for fpath, with all relative ../../, symlinks and so on resolved.
// See http://pubs.opengroup.org/onlinepubs/9699919799/functions/realpath.html
// Also https://insanecoding.blogspot.com/2007/11/pathmax-simply-isnt.html
// and https://insanecoding.blogspot.com/2007/11/implementing-realpath-in-c.html
// Note: this particular rabbit hole is *deep* ...
@[manualfree]
pub fn real_path(fpath string) string {
	mut fullpath := [max_path_buffer_size]u8{}
	mut res := ''
	$if windows {
		pu16_fullpath := unsafe { &u16(&fullpath[0]) }
		// gets handle with GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
		// use C.CreateFile(fpath.to_wide(), 0x80000000, 1, 0, 3, 0x80, 0) instead of  get_file_handle
		// try to open the file to get symbolic link path
		fpath_wide := fpath.to_wide()
		defer {
			unsafe { free(voidptr(fpath_wide)) }
		}
		file := C.CreateFile(fpath_wide, 0x80000000, 1, 0, 3, 0x80, 0)
		if file != voidptr(-1) {
			defer {
				C.CloseHandle(file)
			}
			// https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfinalpathnamebyhandlew
			final_len := C.GetFinalPathNameByHandleW(file, pu16_fullpath, max_path_buffer_size,
				0)
			if final_len < u32(max_path_buffer_size) {
				rt := unsafe { string_from_wide2(pu16_fullpath, int(final_len)) }
				srt := rt[4..]
				unsafe { res.free() }
				res = srt.clone()
			} else {
				eprintln('os.real_path() saw that the file path was too long')
				unsafe { res.free() }
				return fpath.clone()
			}
		} else {
			// if it is not a file C.CreateFile doesn't gets a file handle, use GetFullPath instead
			ret := C.GetFullPathName(fpath_wide, max_path_len, pu16_fullpath, 0)
			if ret == 0 {
				// TODO: check errors if path len is not enough
				unsafe { res.free() }
				return fpath.clone()
			}
			unsafe { res.free() }
			res = unsafe { string_from_wide(pu16_fullpath) }
		}
	} $else {
		ret := &char(C.realpath(&char(fpath.str), &char(&fullpath[0])))
		if ret == 0 {
			unsafe { res.free() }
			return fpath.clone()
		}
		// Note: fullpath is much larger (usually ~4KB), than what C.realpath will
		// actually fill in the vast majority of the cases => it pays to copy the
		// resulting string from that buffer, to a shorter one, and then free the
		// 4KB fullpath buffer.
		unsafe { res.free() }
		res = unsafe { tos_clone(&fullpath[0]) }
	}
	unsafe { normalize_drive_letter(res) }
	return res
}

@[direct_array_access; manualfree; unsafe]
fn normalize_drive_letter(path string) {
	$if !windows {
		return
	}
	// normalize_drive_letter is needed, because
	// a path like c:\nv\.bin (note the small `c`) in %PATH,
	// is NOT recognized by cmd.exe (and probably other programs too)...
	// Capital drive letters do work fine.
	if path.len > 2 && path[0] >= `a` && path[0] <= `z` && path[1] == `:`
		&& path[2] == path_separator[0] {
		unsafe {
			x := &path.str[0]
			(*x) = *x - 32
		}
	}
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
		$if !emscripten ? {
			pid = C.wait(0)
		}
	}
	$if windows {
		panic('os.wait not supported in windows') // TODO
	}
	return pid
}

// file_last_mod_unix returns the "last modified" time stamp of file in `path`.
pub fn file_last_mod_unix(path string) i64 {
	if attr := stat(path) {
		return attr.mtime
	}
	return 0
}

// flush will flush the stdout buffer.
pub fn flush() {
	C.fflush(C.stdout)
}

// chmod change file access attributes of `path` to `mode`.
// Octals like `0o600` can be used.
pub fn chmod(path string, mode int) ! {
	if C.chmod(&char(path.str), mode) != 0 {
		return error_posix(msg: 'chmod failed: ' + posix_get_error_msg(C.errno))
	}
}

// chown changes the owner and group attributes of `path` to `owner` and `group`.
pub fn chown(path string, owner int, group int) ! {
	$if windows {
		return error('os.chown() not implemented for Windows')
	} $else {
		if C.chown(&char(path.str), owner, group) != 0 {
			return error_posix()
		}
	}
}

// open_append tries to open a file from a given path.
// If successful, it and returns a `File` for appending.
pub fn open_append(path string) !File {
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
		return error_posix(msg: 'failed to create(append) file "${path}"')
	}
	file.is_opened = true
	return file
}

// execvp - loads and executes a new child process, *in place* of the current process.
// The child process executable is located in `cmdpath`.
// The arguments, that will be passed to it are in `args`.
// Note: this function will NOT return when successful, since
// the child process will take control over execution.
pub fn execvp(cmdpath string, cmdargs []string) ! {
	mut cargs := []&char{}
	cargs << &char(cmdpath.str)
	for i in 0 .. cmdargs.len {
		cargs << &char(cmdargs[i].str)
	}
	cargs << &char(unsafe { nil })
	mut res := int(0)
	$if windows {
		res = C._execvp(&char(cmdpath.str), cargs.data)
	} $else {
		res = C.execvp(&char(cmdpath.str), cargs.data)
	}
	if res == -1 {
		return error_posix()
	}

	// just in case C._execvp returned ... that happens on windows ...
	exit(res)
}

// execve - loads and executes a new child process, *in place* of the current process.
// The child process executable is located in `cmdpath`.
// The arguments, that will be passed to it are in `args`.
// You can pass environment variables to through `envs`.
// Note: this function will NOT return when successful, since
// the child process will take control over execution.
pub fn execve(cmdpath string, cmdargs []string, envs []string) ! {
	mut cargv := []&char{}
	mut cenvs := []&char{}
	cargv << &char(cmdpath.str)
	for i in 0 .. cmdargs.len {
		cargv << &char(cmdargs[i].str)
	}
	for i in 0 .. envs.len {
		cenvs << &char(envs[i].str)
	}
	cargv << &char(unsafe { nil })
	cenvs << &char(unsafe { nil })
	mut res := int(0)
	$if windows {
		res = C._execve(&char(cmdpath.str), cargv.data, cenvs.data)
	} $else {
		res = C.execve(&char(cmdpath.str), cargv.data, cenvs.data)
	}
	// Note: normally execve does not return at all.
	// If it returns, then something went wrong...
	if res == -1 {
		return error_posix()
	}
}

// is_atty returns 1 if the `fd` file descriptor is open and refers to a terminal.
pub fn is_atty(fd int) int {
	$if windows {
		mut mode := u32(0)
		osfh := voidptr(C._get_osfhandle(fd))
		C.GetConsoleMode(osfh, voidptr(&mode))
		return int(mode)
	} $else {
		return C.isatty(fd)
	}
}

// write_file_array writes the data in `buffer` to a file in `path`.
pub fn write_file_array(path string, buffer array) ! {
	mut f := create(path)!
	unsafe { f.write_full_buffer(buffer.data, usize(buffer.len * buffer.element_size))! }
	f.close()
}

// glob function searches for all the pathnames matching patterns and returns a
// sorted list of found pathnames.
@[manualfree]
pub fn glob(patterns ...string) ![]string {
	mut matches := []string{}
	for pattern in patterns {
		native_glob_pattern(pattern, mut matches)!
	}
	matches.sort()
	return matches
}

// last_error returns a V error, formed by the last libc error (from
// `GetLastError()` on windows and from `errno` on !windows).
@[manualfree]
pub fn last_error() IError {
	$if windows {
		code := int(C.GetLastError())
		msg := get_error_msg(code)
		return error_with_code(msg, code)
	} $else {
		code := C.errno
		msg := posix_get_error_msg(code)
		return error_with_code(msg, code)
	}
}

// Magic constant because zero is used explicitly at times
pub const error_code_not_set = int(-1)

@[params]
pub struct SystemError {
pub:
	msg  string
	code int = error_code_not_set
}

// error_posix returns a POSIX error:
// Code defaults to last error (from C.errno)
// Message defaults to POSIX error message for the error code
@[inline; manualfree]
pub fn error_posix(e SystemError) IError {
	code := if e.code == error_code_not_set { C.errno } else { e.code }
	message := if e.msg == '' { posix_get_error_msg(code) } else { e.msg }
	return error_with_code(message, code)
}

// error_win32 returns a Win32 API error:
// example:
// ```
//    // save error code immediately, or it will be overwritten by other API
//    // function calls, even by `str_intp`.
//    code := int(C.GetLastError())
//    error_win32(
//       msg : 'some error'
//       code : code
//    )
// ```
// wrong usage:
// ```
//    error_win32(
//        msg : 'some error ${path}'		// this will overwrite error code
//        code : int(C.GetLastError())
//    )
// ```
// Message defaults to Win 32 API error message for the error code
@[inline; manualfree]
pub fn error_win32(e SystemError) IError {
	$if windows {
		if e.code == error_code_not_set {
			panic('before calling `error_win32`, you must set `e.code` first.')
		}
		message := if e.msg == '' { get_error_msg(e.code) } else { e.msg }
		return error_with_code(message, e.code)
	} $else {
		panic('Win32 API not available on this platform.')
	}
}

pub struct DiskUsage {
pub:
	total     u64
	available u64
	used      u64
}

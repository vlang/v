module os

import strings

#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/statvfs.h>
#include <utime.h>

// path_separator is the platform specific separator string, used between the folders and filenames in a path. It is '/' on POSIX, and '\\' on Windows.
pub const path_separator = '/'

// path_delimiter is the platform specific delimiter string, used between the paths in environment variables like PATH. It is ':' on POSIX, and ';' on Windows.
pub const path_delimiter = ':'

// path_devnull is a platform-specific file path of the null device.
// It is '/dev/null' on POSIX, and r'\\.\nul' on Windows.
pub const path_devnull = '/dev/null'

const executable_suffixes = ['']

const stdin_value = 0
const stdout_value = 1
const stderr_value = 2

// (Must be realized in Syscall) (Must be specified)
// ref: http://www.ccfit.nsu.ru/~deviv/courses/unix/unix/ng7c229.html
pub const s_ifmt = 0xF000 // type of file

pub const s_ifdir = 0x4000 // directory

pub const s_ifreg = 0x8000 // regular file

pub const s_iflnk = 0xa000 // link

pub const s_isuid = 0o4000 // SUID

pub const s_isgid = 0o2000 // SGID

pub const s_isvtx = 0o1000 // Sticky

pub const s_irusr = 0o0400 // Read by owner

pub const s_iwusr = 0o0200 // Write by owner

pub const s_ixusr = 0o0100 // Execute by owner

pub const s_irgrp = 0o0040 // Read by group

pub const s_iwgrp = 0o0020 // Write by group

pub const s_ixgrp = 0o0010 // Execute by group

pub const s_iroth = 0o0004 // Read by others

pub const s_iwoth = 0o0002 // Write by others

pub const s_ixoth = 0o0001

fn C.utime(&char, &C.utimbuf) int

fn C.uname(name &C.utsname) int

fn C.symlink(&char, &char) int

fn C.link(&char, &char) int

fn C.gethostname(&char, int) int

// Note: not available on Android fn C.getlogin_r(&char, int) int
fn C.getlogin() &char

fn C.getppid() int

fn C.getgid() int

fn C.getegid() int

enum GlobMatch {
	exact
	ends_with
	starts_with
	start_and_ends_with
	contains
	any
}

fn glob_match(dir string, pattern string, next_pattern string, mut matches []string) []string {
	mut subdirs := []string{}
	if is_file(dir) {
		return subdirs
	}
	mut files := ls(dir) or { return subdirs }
	mut mode := GlobMatch.exact
	mut pat := pattern
	if pat == '*' {
		mode = GlobMatch.any
		if next_pattern != pattern && next_pattern != '' {
			for file in files {
				if is_dir('${dir}/${file}') {
					subdirs << '${dir}/${file}'
				}
			}
			return subdirs
		}
	}
	if pat == '**' {
		files = walk_ext(dir, '')
		pat = next_pattern
	}
	if pat.starts_with('*') {
		mode = .ends_with
		pat = pat[1..]
	}
	if pat.ends_with('*') {
		mode = if mode == .ends_with { GlobMatch.contains } else { GlobMatch.starts_with }
		pat = pat[..pat.len - 1]
	}
	if pat.contains('*') {
		mode = .start_and_ends_with
	}
	for file in files {
		mut fpath := file
		f := if file.contains(path_separator) {
			pathwalk := file.split(path_separator)
			pathwalk[pathwalk.len - 1]
		} else {
			fpath = if dir == '.' { file } else { '${dir}/${file}' }
			file
		}
		if f in ['.', '..'] || f == '' {
			continue
		}
		hit := match mode {
			.any {
				true
			}
			.exact {
				f == pat
			}
			.starts_with {
				f.starts_with(pat)
			}
			.ends_with {
				f.ends_with(pat)
			}
			.start_and_ends_with {
				p := pat.split('*')
				f.starts_with(p[0]) && f.ends_with(p[1])
			}
			.contains {
				f.contains(pat)
			}
		}
		if hit {
			if is_dir(fpath) {
				subdirs << fpath
				if next_pattern == pattern && next_pattern != '' {
					matches << '${fpath}${path_separator}'
				}
			} else {
				matches << fpath
			}
		}
	}
	return subdirs
}

fn native_glob_pattern(pattern string, mut matches []string) ! {
	steps := pattern.split(path_separator)
	cwd := if pattern.starts_with(path_separator) { path_separator } else { '.' }
	mut subdirs := [cwd]
	for i := 0; i < steps.len; i++ {
		step := steps[i]
		step2 := if i + 1 == steps.len { step } else { steps[i + 1] }
		if step == '' {
			continue
		}
		if is_dir('${cwd}${path_separator}${step}') {
			dd := if cwd == '/' {
				step
			} else {
				if cwd == '.' || cwd == '' {
					step
				} else {
					if step == '.' || step == '/' { cwd } else { '${cwd}/${step}' }
				}
			}
			if i + 1 != steps.len {
				if dd !in subdirs {
					subdirs << dd
				}
			}
		}
		mut subs := []string{}
		for sd in subdirs {
			d := if cwd == '/' {
				sd
			} else {
				if cwd == '.' || cwd == '' {
					sd
				} else {
					if sd == '.' || sd == '/' { cwd } else { '${cwd}/${sd}' }
				}
			}
			subs << glob_match(d.replace('//', '/'), step, step2, mut matches)
		}
		subdirs = subs.clone()
	}
}

// utime changes the access and modification times of the inode specified by path.
// It returns POSIX error message, if it can not do so.
pub fn utime(path string, actime int, modtime int) ! {
	u := C.utimbuf{actime, modtime}
	if C.utime(&char(path.str), &u) != 0 {
		return error_with_code(posix_get_error_msg(C.errno), C.errno)
	}
}

// uname returns information about the platform on which the program is running.
// For example:
// os.Uname{
//    sysname: 'Linux'
//    nodename: 'nemesis'
//    release: '5.15.0-57-generic'
//    version: '#63~20.04.1-Ubuntu SMP Wed Nov 30 13:40:16 UTC 2022'
//    machine: 'x86_64'
// }
// where the fields have the following meaning:
//    sysname is the name of this implementation of the operating system
//    nodename is the name of this node within an implementation-dependent communications network
//    release is the current release level of this implementation
//    version is the current version level of this release
//    machine is the name of the hardware type, on which the system is running
// See also https://pubs.opengroup.org/onlinepubs/7908799/xsh/sysutsname.h.html
pub fn uname() Uname {
	mut u := Uname{}
	unsafe {
		d := &C.utsname(malloc_noscan(int(sizeof(C.utsname))))
		if C.uname(d) == 0 {
			u.sysname = cstring_to_vstring(d.sysname)
			u.nodename = cstring_to_vstring(d.nodename)
			u.release = cstring_to_vstring(d.release)
			u.version = cstring_to_vstring(d.version)
			u.machine = cstring_to_vstring(d.machine)
		}
		free(d)
	}
	return u
}

// hostname returns the hostname (system's DNS name) or POSIX error message if the hostname call fails.
pub fn hostname() !string {
	mut hstnme := ''
	size := 256
	buf := unsafe { &char(malloc_noscan(size)) }
	if C.gethostname(buf, size) == 0 {
		hstnme = unsafe { cstring_to_vstring(buf) }
		unsafe { free(buf) }
		return hstnme
	}
	return error(posix_get_error_msg(C.errno))
}

// loginname returns the name of the user logged in on the controlling terminal of the process.
// It returns a POSIX error message, if the getlogin call fails.
pub fn loginname() !string {
	x := C.getlogin()
	if !isnil(x) {
		return unsafe { cstring_to_vstring(x) }
	}
	return error(posix_get_error_msg(C.errno))
}

// ls returns ![]string of the files and dirs in the given `path` ( os.ls uses C.readdir ). Symbolic links are returned to be files. For recursive list see os.walk functions.
// See also: `os.walk`, `os.walk_ext`, `os.is_dir`, `os.is_file`
// Example: https://github.com/vlang/v/blob/master/examples/readdir.v
// ```
//   entries := os.ls(os.home_dir()) or { [] }
//   for entry in entries {
//     if os.is_dir(os.join_path(os.home_dir(), entry)) {
//       println('dir: $entry')
//     } else {
//       println('file: $entry')
//     }
//   }
// ```
@[manualfree]
pub fn ls(path string) ![]string {
	if path == '' {
		return error('ls() expects a folder, not an empty string')
	}
	mut res := []string{cap: 50}
	dir := unsafe { C.opendir(&char(path.str)) }
	if isnil(dir) {
		return error('ls() couldnt open dir "${path}"')
	}
	mut ent := &C.dirent(unsafe { nil })
	// mut ent := &C.dirent{!}
	for {
		ent = C.readdir(dir)
		if isnil(ent) {
			break
		}
		unsafe {
			bptr := &u8(&ent.d_name[0])
			if bptr[0] == 0 || (bptr[0] == `.` && bptr[1] == 0)
				|| (bptr[0] == `.` && bptr[1] == `.` && bptr[2] == 0) {
				continue
			}
			res << tos_clone(bptr)
		}
	}
	C.closedir(dir)
	return res
}

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string, params MkdirParams) ! {
	if path == '.' {
		return
	}
	apath := real_path(path)
	r := unsafe { C.mkdir(&char(apath.str), params.mode) }
	if r == -1 {
		return error(posix_get_error_msg(C.errno))
	}
}

// execute starts the specified command, waits for it to complete, and returns its output.
@[manualfree]
pub fn execute(cmd string) Result {
	pcmd := 'exec 2>&1;${cmd}'
	defer {
		unsafe { pcmd.free() }
	}
	f := vpopen(pcmd)
	if isnil(f) {
		return Result{
			exit_code: -1
			output:    'exec("${cmd}") failed'
		}
	}
	fd := fileno(f)
	mut res := strings.new_builder(1024)
	defer {
		unsafe { res.free() }
	}
	buf := [4096]u8{}
	unsafe {
		pbuf := &buf[0]
		for {
			len := C.read(fd, pbuf, 4096)
			if len == 0 {
				break
			}
			res.write_ptr(pbuf, len)
		}
	}
	soutput := res.str()
	exit_code := vpclose(f)
	return Result{
		exit_code: exit_code
		output:    soutput
	}
}

// raw_execute does the same as `execute` on Unix platforms.
// On Windows raw_execute starts the specified command, waits for it to complete, and returns its output.
// It's marked as `unsafe` to help emphasize the problems that may arise by allowing, for example,
// user provided escape sequences.
@[unsafe]
pub fn raw_execute(cmd string) Result {
	return execute(cmd)
}

@[manualfree]
pub fn (mut c Command) start() ! {
	pcmd := c.path + ' 2>&1'
	defer {
		unsafe { pcmd.free() }
	}
	c.f = vpopen(pcmd)
	if isnil(c.f) {
		return error('exec("${c.path}") failed')
	}
}

@[manualfree]
pub fn (mut c Command) read_line() string {
	buf := [4096]u8{}
	mut res := strings.new_builder(1024)
	defer {
		unsafe { res.free() }
	}
	unsafe {
		bufbp := &buf[0]
		for C.fgets(&char(bufbp), 4096, c.f) != 0 {
			len := vstrlen(bufbp)
			for i in 0 .. len {
				if bufbp[i] == `\n` {
					res.write_ptr(bufbp, i)
					final := res.str()
					return final
				}
			}
			res.write_ptr(bufbp, len)
		}
	}
	c.eof = true
	final := res.str()
	return final
}

pub fn (mut c Command) close() ! {
	c.exit_code = vpclose(c.f)
	if c.exit_code == 127 {
		return error_with_code('error', 127)
	}
}

// symlink creates a symbolic link named target, which points to origin.
// It returns a POSIX error message, if it can not do so.
pub fn symlink(origin string, target string) ! {
	res := C.symlink(&char(origin.str), &char(target.str))
	if res == 0 {
		return
	}
	return error(posix_get_error_msg(C.errno))
}

// link creates a new link (also known as a hard link) to an existing file.
// It returns a POSIX error message, if it can not do so.
pub fn link(origin string, target string) ! {
	res := C.link(&char(origin.str), &char(target.str))
	if res == 0 {
		return
	}
	return error(posix_get_error_msg(C.errno))
}

// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
	return posix_get_error_msg(code)
}

pub fn (mut f File) close() {
	if !f.is_opened {
		return
	}
	f.is_opened = false
	C.fflush(f.cfile)
	C.fclose(f.cfile)
}

fn C.mkstemp(stemplate &u8) int

// ensure_folder_is_writable checks that `folder` exists, and is writable to the process
// by creating an empty file in it, then deleting it.
@[manualfree]
pub fn ensure_folder_is_writable(folder string) ! {
	if !exists(folder) {
		return error_with_code('`${folder}` does not exist', 1)
	}
	if !is_dir(folder) {
		return error_with_code('`${folder}` is not a folder', 2)
	}
	tmp_perm_check := join_path_single(folder, 'XXXXXX')
	defer {
		unsafe { tmp_perm_check.free() }
	}
	unsafe {
		x := C.mkstemp(&char(tmp_perm_check.str))
		if -1 == x {
			return error_with_code('folder `${folder}` is not writable', 3)
		}
		C.close(x)
	}
	rm(tmp_perm_check)!
}

// getpid returns the process ID (PID) of the calling process.
@[inline]
pub fn getpid() int {
	return C.getpid()
}

// getppid returns the process ID of the parent of the calling process.
@[inline]
pub fn getppid() int {
	return C.getppid()
}

// getuid returns the real user ID of the calling process.
@[inline]
pub fn getuid() int {
	return C.getuid()
}

// geteuid returns the effective user ID of the calling process.
@[inline]
pub fn geteuid() int {
	return C.geteuid()
}

// getgid returns the real group ID of the calling process.
@[inline]
pub fn getgid() int {
	return C.getgid()
}

// getegid returns the effective group ID of the calling process.
@[inline]
pub fn getegid() int {
	return C.getegid()
}

// Turns the given bit on or off, depending on the `enable` parameter.
pub fn posix_set_permission_bit(path_s string, mode u32, enable bool) {
	mut new_mode := u32(0)
	if s := stat(path_s) {
		new_mode = s.mode
	}
	match enable {
		true { new_mode |= mode }
		false { new_mode &= (0o7777 - mode) }
	}
	C.chmod(&char(path_s.str), int(new_mode))
}

// get_long_path has no meaning for *nix, but has for windows, where `c:\folder\some~1` for example
// can be the equivalent of `c:\folder\some spa ces`. On *nix, it just returns a copy of the input path.
fn get_long_path(path string) !string {
	return path
}

fn C.sysconf(name int) i64

// page_size returns the page size in bytes.
pub fn page_size() int {
	return int(C.sysconf(C._SC_PAGESIZE))
}

struct C.statvfs {
	f_bsize  usize
	f_blocks usize
	f_bfree  usize
	f_bavail usize
}

// disk_usage returns disk usage of `path`.
@[manualfree]
pub fn disk_usage(path string) !DiskUsage {
	mpath := if path == '' { '.' } else { path }
	defer { unsafe { mpath.free() } }
	mut vfs := C.statvfs{}
	ret := unsafe { C.statvfs(&char(mpath.str), &vfs) }
	if ret == -1 {
		return error('cannot get disk usage of path')
	}
	f_bsize := u64(vfs.f_bsize)
	f_blocks := u64(vfs.f_blocks)
	f_bavail := u64(vfs.f_bavail)
	f_bfree := u64(vfs.f_bfree)
	return DiskUsage{
		total:     f_bsize * f_blocks
		available: f_bsize * f_bavail
		used:      f_bsize * (f_blocks - f_bfree)
	}
}

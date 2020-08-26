module os

import strings

#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/utsname.h>
pub const (
	path_separator = '/'
	path_delimiter = ':'
)

const (
	stdin_value  = 0
	stdout_value = 1
	stderr_value = 2
)

struct C.utsname {
mut:
	sysname  charptr
	nodename charptr
	release  charptr
	version  charptr
	machine  charptr
}

fn C.uname(name voidptr) int

fn C.symlink(arg_1, arg_2 charptr) int

pub fn uname() Uname {
	mut u := Uname{}
	utsize := sizeof(C.utsname)
	x := malloc(int(utsize))
	d := &C.utsname(x)
	if C.uname(d) == 0 {
		u.sysname = cstring_to_vstring(byteptr(d.sysname))
		u.nodename = cstring_to_vstring(byteptr(d.nodename))
		u.release = cstring_to_vstring(byteptr(d.release))
		u.version = cstring_to_vstring(byteptr(d.version))
		u.machine = cstring_to_vstring(byteptr(d.machine))
	}
	free(d)
	return u
}

fn init_os_args(argc int, argv &&byte) []string {
	mut args := []string{}
	// mut args := []string(make(0, argc, sizeof(string)))
	// mut args := []string{len:argc}
	for i in 0 .. argc {
		// args [i] = argv[i].vstring()
		unsafe {
			args << byteptr(argv[i]).vstring()
		}
	}
	return args
}

pub fn ls(path string) ?[]string {
	mut res := []string{}
	dir := C.opendir(charptr(path.str))
	if isnil(dir) {
		return error('ls() couldnt open dir "$path"')
	}
	mut ent := &C.dirent(0)
	// mut ent := &C.dirent{!}
	for {
		ent = C.readdir(dir)
		if isnil(ent) {
			break
		}
		name := tos_clone(byteptr(ent.d_name))
		if name != '.' && name != '..' && name != '' {
			res << name
		}
	}
	C.closedir(dir)
	return res
}

/*
pub fn is_dir(path string) bool {
	//$if linux {
		//C.syscall(4, path.str) // sys_newstat
	//}
	dir := C.opendir(path.str)
	res := !isnil(dir)
	if res {
		C.closedir(dir)
	}
	return res
}
*/
/*
pub fn (mut f File) fseek(pos, mode int) {
}
*/
// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) ?bool {
	if path == '.' {
		return true
	}
	/*
	mut k := 0
	defer {
		k = 1
	}
	*/
	apath := real_path(path)
	/*
	$if linux {
		$if !android {
			ret := C.syscall(sys_mkdir, apath.str, 511)
			if ret == -1 {
				return error(posix_get_error_msg(C.errno))
			}
			return true
		}
	}
	*/
	r := unsafe {
		C.mkdir(charptr(apath.str), 511)
	}
	
	if r == -1 {
		return error(posix_get_error_msg(C.errno))
	}
	return true
}

// exec starts the specified command, waits for it to complete, and returns its output.
pub fn exec(cmd string) ?Result {
	// if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
	// return error(';, &&, || and \\n are not allowed in shell commands')
	// }
	pcmd := '$cmd 2>&1'
	f := vpopen(pcmd)
	if isnil(f) {
		return error('exec("$cmd") failed')
	}
	buf := [4096]byte{}
	mut res := strings.new_builder(1024)
	for C.fgets(charptr(buf), 4096, f) != 0 {
		bufbp := byteptr(buf)
		res.write_bytes(bufbp, vstrlen(bufbp))
	}
	soutput := res.str()
	// res.free()
	exit_code := vpclose(f)
	if exit_code == 127 {
		return error_with_code(soutput, 127)
	}
	return Result{
		exit_code: exit_code
		output: soutput
	}
}

pub fn symlink(origin, target string) ?bool {
	res := C.symlink(charptr(origin.str), charptr(target.str))
	if res == 0 {
		return true
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
	/*
	$if linux {
		$if !android {
			C.syscall(sys_close, f.fd)
			return
		}
	}
	*/
	C.fflush(f.cfile)
	C.fclose(f.cfile)
}

pub fn debugger_present() bool {
	return false
}

fn C.mkstemp(stemplate byteptr) int
// `is_writable_folder` - `folder` exists and is writable to the process
pub fn is_writable_folder(folder string) ?bool {
	if !os.exists(folder) {
		return error('`$folder` does not exist')
	}
	if !os.is_dir(folder) {
		return error('`folder` is not a folder')
	}
	tmp_perm_check := os.join_path(folder, 'XXXXXX')
	unsafe {
		x := C.mkstemp(tmp_perm_check.str)
		if -1 == x {
			return error('folder `$folder` is not writable')
		}
		C.close(x)
	}
	os.rm(tmp_perm_check)
	return true
}

[inline]
pub fn getpid() int {
	return C.getpid()
}

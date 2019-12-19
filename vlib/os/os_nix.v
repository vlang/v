module os

#include <dirent.h>
#include <unistd.h>
pub const (
	path_separator = '/'
)

pub fn init_os_args(argc int, argv &byteptr) []string {
	mut args := []string
	for i in 0 .. argc {
		args << string(argv[i])
	}
	return args
}

// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
	ptr_text := C.strerror(code) // voidptr?
	if ptr_text == 0 {
		return ''
	}
	return tos3(ptr_text)
}

pub fn ls(path string) ?[]string {
	mut res := []string
	dir := C.opendir(path.str)
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

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) ?bool {
	if path == '.' {
		return true
	}
	apath := os.realpath(path)
	r := C.mkdir(apath.str, 511)
	if r == -1 {
		return error(get_error_msg(C.errno))
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
	buf := [1000]byte
	mut res := ''
	for C.fgets(charptr(buf), 1000, f) != 0 {
		res += tos(buf, vstrlen(buf))
	}
	res = res.trim_space()
	exit_code := vpclose(f)
	// if exit_code != 0 {
	// return error(res)
	// }
	return Result{
		output: res
		exit_code: exit_code
	}
}


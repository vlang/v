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
	_ptr_text := C.strerror(code) // voidptr?
	if _ptr_text == 0 {
		return ''
	}
	return tos(_ptr_text, vstrlen(_ptr_text))
}

pub fn ls(path string) ?[]string {
	mut res := []string
	dir := C.opendir(path.str)
	if isnil(dir) {
		return error('ls() couldnt open dir "$path"')
	}
	mut ent := &C.dirent{!}
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

pub fn dir_exists(path string) bool {
	dir := C.opendir(path.str)
	res := !isnil(dir)
	if res {
		C.closedir(dir)
	}
	return res
}

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) {
	C.mkdir(path.str, 511)// S_IRWXU | S_IRWXG | S_IRWXO
}

// exec starts the specified command, waits for it to complete, and returns its output.
pub fn exec(cmd string) ?Result {
	if cmd.contains(';') || cmd.contains('&&') || cmd.contains('||') || cmd.contains('\n') {
		return error(';, &&, || and \\n are not allowed in shell commands')
	}
	pcmd := '$cmd 2>&1'
	f := vpopen(pcmd)
	if isnil(f) {
		return error('exec("$cmd") failed')
	}
	buf := [1000]byte
	mut res := ''
	for C.fgets(*char(buf), 1000, f) != 0 {
		res += tos(buf, vstrlen(buf))
	}
	res = res.trim_space()
	exit_code := vpclose(f)
	//if exit_code != 0 {
		//return error(res)
	//}
	return Result {
		output: res
		exit_code: exit_code
	}
}

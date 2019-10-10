module os

#include <dirent.h>
#include <unistd.h>

const (
	PathSeparator = '/'
)

fn init_os_args(argc int, argv &byteptr) []string {
	mut args := []string
	for i := 0; i < argc; i++ {
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

pub fn ls(path string) []string {
	mut res := []string
	dir := C.opendir(path.str)
	if isnil(dir) {
		println('ls() couldnt open dir "$path"')
		print_c_errno()
		return res
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



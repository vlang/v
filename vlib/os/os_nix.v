module os 

#include <dirent.h>
#include <unistd.h>

const (
	PathSeparator = '/' 
)

struct C.dirent {
	d_name byteptr 
} 

fn C.readdir(voidptr) C.dirent 


// get_error_msg return error code representation in string.
pub fn get_error_msg(code int) string {
	_ptr_text := C.strerror(code) // voidptr?
	if _ptr_text == 0 {
		return ''
	}
	return tos(_ptr_text, C.strlen(_ptr_text))
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
		name := tos_clone(ent.d_name)
		if name != '.' && name != '..' && name != '' {
			res << name
		}
	}
	C.closedir(dir)
	return res
}

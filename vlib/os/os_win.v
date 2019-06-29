// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module os

#include <dirent.h>
#include <sys/types.h>

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

pub fn getwd() string {
	mut buffer := malloc(512)
	
	buffer = C._getcwd(0, 0)
	// A NULL return value indicates an error
	if isnil(buffer) {
		return ''
	}
	return string(buffer)
}

const (
	FILE_ATTRIBUTE_DIRECTORY = 16
)

fn is_dir(path string) bool {
	val := int(C.GetFileAttributes(path.cstr()))
	return val &FILE_ATTRIBUTE_DIRECTORY > 0
}

fn chdir(path string) {
	C._chdir(path.cstr())
}

fn log(s string) {
}


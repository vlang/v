// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module os

//#include <execinfo.h> // for backtrace_symbols_fd 
#include <signal.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
// import darwin
fn log(s string) {
}


pub fn is_dir(path string) bool {
	statbuf := C.stat{}
	cstr := path.cstr()
	if C.stat(cstr, &statbuf) != 0 {
		return false
	}
	return statbuf.st_mode & S_IFMT == S_IFDIR
}

fn chdir(path string) {
	C.chdir(path.cstr())
}

pub fn getwd() string {
	cwd := malloc(512)
	if C.getcwd(cwd, 512) == 0 {
		return ''
	}
	return string(cwd)
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

fn print_backtrace() {
/* 
	# void *buffer[100];
	nptrs := 0
	# nptrs = backtrace(buffer, 100);
	# printf("%d!!\n", nptrs);
	# backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO) ;
*/ 
}


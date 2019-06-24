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

fn is_dir(path string) bool {
	# struct stat statbuf;
	cstr := path.cstr()
	# if (stat(cstr, &statbuf) != 0)
	{
		return false
	}
	# return S_ISDIR(statbuf.st_mode);
	return false
}

fn chdir(path string) {
	C.chdir(path.cstr())
}

fn getwd() string {
	cwd := malloc(1024)
	# if (getcwd(cwd, 1024)) return tos2(cwd);
	return ''
}

fn ls(path string) []string {
	mut res := []string
	# DIR *dir;
	# struct dirent *ent;
	# if ((dir = opendir (path.str)) == NULL)
	{
		println('ls() couldnt open dir "$path"')
		print_c_errno()
		return res
	}
	// print all the files and directories within directory */
	# while ((ent = readdir (dir)) != NULL) {
	name := ''
	# name = tos_clone(ent->d_name);//, strlen(ent->d_name));
	// # printf ("printf ls() %s\n", ent->d_name);
	// println(name)
	if name != '.' && name != '..' && name != '' {
		res << name
	}
	# }
	# closedir (dir);
	// res.sort()
	// println('sorted res')
	// print_strings(res)
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


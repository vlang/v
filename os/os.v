// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module os

#include <sys/stat.h>
const (
	args = []string
)

struct FILE {
}

struct File {
	cfile *FILE
}

struct FileInfo {
	name string
	size int
}

import const (
	SEEK_SET
	SEEK_END
)

fn C.getline(voidptr, voidptr, voidptr) int

fn C.ftell(fp voidptr) int

fn todo_remove(){}

fn init_os_args(argc int, _argv *byteptr) []string {
	mut args := []string
	# char** argv = (char**) _argv;
	for i := 0; i < argc; i++ {
		arg := ''
		//arg := tos(argv[i], strlen(argv[i])) 
		# arg = tos((char**)(argv[i]), strlen((char**)(argv[i])));
		args << arg
	}
	return args
}

fn parse_windows_cmd_line(cmd byteptr) []string {
	s := tos2(cmd)
	return s.split(' ')
}

// read_file reads the file in `path` and returns the contents.
pub fn read_file(path string) ?string {
	mut res := ''
	cpath := path.cstr()
	fp := C.fopen(cpath, 'r')
	if isnil(fp) {
		return error('failed to open file "$path"')
	}
	C.fseek(fp, 0, SEEK_END)
	fsize := C.ftell(fp)
	// C.fseek(fp, 0, SEEK_SET)  // same as C.rewind(fp) below
	C.rewind(fp)
	mut str := malloc(fsize + 1)
	C.fread(str, fsize, 1, fp)
	C.fclose(fp)
	str[fsize] = 0
	res = tos(str, fsize)
	return res
}

/* 
// TODO 
fn (f File) read_rune() string {
	# if (!f.cfile) return tos("", 0);
	c := malloc(1)
	C.fread(c, 1, 1, f.cfile)
	return tos(c, 1)
}
*/ 

// file_size returns the size of the file located in `path`.
pub fn file_size(path string) int {
	# struct stat s;
	# stat(path.str, &s);
	// # if (S_ISLNK(s.st_mode)) return -1;
	# return s.st_size;
	// //////////////////////
	# FILE *f = fopen(path.str, "r");
	# if (!f) return 0;
	# fseek(f, 0, SEEK_END);
	# long fsize = ftell(f);
	// # fseek(f, 0, SEEK_SET);  //same as rewind(f);
	# rewind(f);
	# return fsize;
	return 0
}

pub fn mv(old, new string) {
	C.rename(old.cstr(), new.cstr())
}

/* 
pub fn file_last_mod_unix(path string) int {
	# struct stat attr;
	# stat(path.str, &attr);
	# return attr.st_mtime ;
	return 0
}

pub fn file_last_mod_time(path string) time.Time {
	return time.now()
	q := C.tm{}
	# struct stat attr;
	# stat(path.str, &attr);
	// # q = attr.st_mtime;
	# struct tm * now = localtime(&attr.st_mtime);
	# q = *now;
	# printf("Last modified time: %s", ctime(&attr.st_mtime));
	return time.convert_ctime(q)
}
*/
// read_lines reads the file in `path` into an array of lines.
// TODO return `?[]string` TODO implement `?[]` support
pub fn read_lines(path string) []string {
	mut res := []string
	mut buf := [1000]byte
	cpath := path.cstr()
	fp := C.fopen(cpath, 'rb')
	if isnil(fp) {
		// TODO
		// return error('failed to open file "$path"')
		return res
	}
	for C.fgets(buf, 1000, fp) != 0 {
		mut val := ''
		buf[C.strlen(buf) - 1] = `\0` // eat the newline fgets() stores
		$if windows {
			if buf[strlen(buf)-2] == 13 {
				buf[strlen(buf) - 2] = `\0`
			}
		}
		res << tos_clone(buf)
	}
	C.fclose(fp)
	return res
}

fn read_ulines(path string) []ustring {
	lines := read_lines(path)
	// mut ulines := new_array(0, lines.len, sizeof(ustring))
	mut ulines := []ustring
	for myline in lines {
		// ulines[i] = ustr
		ulines << myline.ustring()
	}
	return ulines
}

/* 
struct Reader {
	fp *FILE
}
*/ 


// fn open(file string) File? {
// return open_file(file)
// }
pub fn open(path string) File {
	return open_file(path)
}

fn open_file(file string) File {
	return create_file2(file, 'r')
}

// `create` creates a file at a specified location and returns a writable `File` object.
pub fn create(path string) File {
	return create_file(path)
}

pub fn open_append(path string) File {
	return create_file(path)
}

// TODO remove
fn create_file(file string) File {
	return create_file2(file, 'w')
}

fn create_file_a(file string) File {
	return create_file2(file, 'a')
}

fn open_file_a(file string) File {
	return create_file2(file, 'a')
}

fn create_file2(file, mode string) File {
	res := File {
		cfile: C.fopen(file.cstr(), mode.cstr())
	}
	if isnil(res.cfile) {
		println('coudlnt create file "$file"')
	}
	return res
}

fn (f File) append(s string) {
	ss := s.clone()
	C.fputs(ss.cstr(), f.cfile)
	// ss.free()
	// C.fwrite(s.str, 1, s.len, f.cfile)
}

// convert any value to []byte (LittleEndian) and write it
// for example if we have write(7, 4), "07 00 00 00" gets written
// write(0x1234, 2) => "34 12"
fn (f File) write_bytes(data voidptr, size int) {
	C.fwrite(data, 1, size, f.cfile)
}

fn (f File) write_bytes_at(data voidptr, size, pos int) {
	C.fseek(f.cfile, pos, SEEK_SET)
	C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, SEEK_END)
}

pub fn (f File) appendln(s string) {
	// C.fwrite(s.str, 1, s.len, f.cfile)
	// ss := s.clone()
	// TODO perf
	C.fputs(s.cstr(), f.cfile)
	// ss.free()
	C.fputs('\n', f.cfile)
}

pub fn (f File) close() {
	C.fclose(f.cfile)
}

fn close_file(fp *FILE) {
	$if windows {
	}
	# if (fp)
	C.fclose(fp)
}

// system starts the specified command, waits for it to complete, and returns its code.
pub fn system(cmd string) int {
	ret := C.system(cmd.cstr()) 
	if ret == -1 {
		os.print_c_errno()
	}
	return ret
}

fn popen(path string) *FILE {
	cpath := path.cstr()
	$if windows {
		return C._popen(cpath, 'r')
	}
	$else {
		return C.popen(cpath, 'r')
	}
}

// exec starts the specified command, waits for it to complete, and returns its output.
pub fn exec(cmd string) string {
	cmd = '$cmd 2>&1'
	f := popen(cmd) 
	if isnil(f) {
		// TODO optional or error code 
		println('popen $cmd failed')
		return '' 
	}
	buf := [1000]byte 
	mut res := ''
	for C.fgets(buf, 1000, f) != 0 { 
		res += tos(buf, strlen(buf)) 
	}
	return res.trim_space()
}

/* 
// TODO 
fn system_into_lines(s string) []string {
	mut res := []string
	cmd := '$s 2>&1'
	max := 5000
	$if windows {
		# FILE* f = _popen(cmd.str, "r");
	}
	$else {
		# FILE* f = popen(cmd.str, "r");
	}
	# char * buf = malloc(sizeof(char) * max);
	# while (fgets(buf, max, f) != NULL)
	{
		val := ''
		# buf[strlen(buf) - 1] = '\0'; // eat the newline fgets() stores
		# val=tos_clone(buf);
		res << val
	}
	return res
}
*/ 

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {
	s := C.getenv(key.cstr())
	if isnil(s) {
		return ''
	}
	return tos2(s)
}

// `file_exists` returns true if `path` exists.
pub fn file_exists(path string) bool {
	res := false
	$if windows {
		# res = _access( path.str, 0 ) != -1 ;
	}
	$else {
		# res = access( path.str, 0 ) != -1 ;
	}
	return res
}

pub fn dir_exists(path string) bool {
	dir := C.opendir(path.cstr())
	res := !isnil(dir)
	if res {
		C.closedir(dir)
	}
	return res
}

// `mkdir` creates a new directory with the specified path.
pub fn mkdir(path string) {
	$if windows {
		path = path.replace('/', '\\')
		C.CreateDirectory(path.cstr(), 0)
	}
	$else {
		C.mkdir(path.cstr(), 511)// S_IRWXU | S_IRWXG | S_IRWXO
	}
}

// `rm` removes file in `path`.
pub fn rm(path string) {
	$if windows {
		// os.system2('del /f $path')
	}
	$else {
		C.remove(path.cstr())
	}
	// C.unlink(path.cstr())
}

/*
// TODO
fn rmdir(path, guard string) {
	if !path.contains(guard) {
		println('rmdir canceled because the path doesnt contain $guard')
		return
	}
	$if !windows {
	}
	$else {
	}
}
*/

fn print_c_errno() {
	//C.printf('errno=%d err="%s"\n', errno, C.strerror(errno)) 
}


pub fn ext(path string) string {
	pos := path.last_index('.')
	if pos == -1 {
		return ''
	}
	return path.right(pos)
}

fn path_sans_ext(path string) string {
	pos := path.last_index('.')
	if pos == -1 {
		return path
	}
	return path.left(pos)
}


pub fn basedir(path string) string {
	pos := path.last_index('/')
	if pos == -1 {
		return path
	}
	return path.left(pos + 1)
}

pub fn filename(path string) string {
	return path.all_after('/')
}

// get_line returns a one-line string from stdin 
pub fn get_line() string {
	max := 256
	buf := malloc(max)
	nr_chars := C.getline(&buf, &max, stdin)
	if nr_chars == 0 {
		return ''
	}
	return tos(buf, nr_chars - 1)
}

pub fn user_os() string {
	$if linux {
		return 'linux'
	}
	$if mac {
		return 'mac'
	}
	$if windows {
		return 'windows'
	}
	return 'unknown'
}

// home_dir returns path to user's home directory.
pub fn home_dir() string {
	mut home := os.getenv('HOME')
	$if windows {
		home = os.getenv('HOMEDRIVE')
		home += os.getenv('HOMEPATH')
	}
	home += '/'
	return home
}

// write_file writes text data to a file in `path`. 
pub fn write_file(path, text string) {
	f := os.create(path)
	f.appendln(text)
	f.close()
}

fn on_segfault(f voidptr) {
	$if windows {
		return
	}
	$if mac {
		# struct sigaction sa;
		# memset(&sa, 0, sizeof(struct sigaction));
		# sigemptyset(&sa.sa_mask);
		# sa.sa_sigaction = f;
		# sa.sa_flags   = SA_SIGINFO;
		# sigaction(SIGSEGV, &sa, 0);
	}
}


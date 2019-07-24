// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module os

#include <sys/stat.h>
#include <signal.h>
#include <errno.h>

//#include <execinfo.h> // for backtrace_symbols_fd 

/* 
struct dirent {
               d_ino int 
               d_off int 
	d_reclen u16 
	d_type byte 
	d_name [256]byte 
} 
*/ 

struct C.dirent {
	d_name byteptr 
} 

const (
	args = []string
	MAX_PATH = 4096
)

// Windows 
import const (
	FILE_ATTRIBUTE_DIRECTORY
	INVALID_FILE_ATTRIBUTES
)

struct C.FILE {
	
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
	SA_SIGINFO
	S_IFMT
	S_IFDIR
	SIGABRT
	SIGFPE
	SIGILL
	SIGINT
	SIGSEGV
	SIGTERM
)

struct C.stat {
	st_size int
	st_mode int
	st_mtime int 
}

struct C.DIR {

}

//struct C.dirent {
	//d_name byteptr

//}

struct C.sigaction {
mut:
	sa_mask int
	sa_sigaction int
	sa_flags   int
}

fn C.getline(voidptr, voidptr, voidptr) int
fn C.ftell(fp voidptr) int
fn C.getenv(byteptr) byteptr
fn C.sigaction(int, voidptr, int)

fn todo_remove(){}

fn init_os_args(argc int, argv *byteptr) []string {
	mut args := []string
	$if windows {
		mut args_list := &voidptr(0)
		mut args_count := 0
		args_list = C.CommandLineToArgvW(C.GetCommandLine(), &args_count)
		for i := 0; i < args_count; i++ {
			args << string_from_wide(&u16(args_list[i]))
		}
	} $else {
		for i := 0; i < argc; i++ {
			args << string(argv[i])
		}		
	}
	return args
}

fn parse_windows_cmd_line(cmd byteptr) []string {
	s := string(cmd)
	return s.split(' ')
}

// read_file reads the file in `path` and returns the contents.
pub fn read_file(path string) ?string { 
	mode := 'rb'
	mut fp := &C.FILE{}
	$if windows {
		fp = C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		cpath := path.str
		fp = C.fopen(cpath, mode.str) 
	}
	if isnil(fp) {
		return error('failed to open file "$path"')
	}
	C.fseek(fp, 0, SEEK_END)
	fsize := C.ftell(fp)
	// C.fseek(fp, 0, SEEK_SET)  // same as `C.rewind(fp)` below
	C.rewind(fp)
	mut str := malloc(fsize + 1)
	C.fread(str, fsize, 1, fp)
	C.fclose(fp)
	str[fsize] = 0
	return string(str, fsize)
}

// file_size returns the size of the file located in `path`.
pub fn file_size(path string) int {
	mut s := C.stat{}
	$if windows {
		C._wstat(path.to_wide(), &s)
	} $else {
		C.stat(path.str, &s)
	}
	return s.st_size
}

pub fn mv(old, new string) {
	$if windows {
		C._wrename(old.to_wide(), new.to_wide())
	} $else {
		C.rename(old.str, new.str)
	}
}

// read_lines reads the file in `path` into an array of lines.
// TODO return `?[]string` TODO implement `?[]` support
pub fn read_lines(path string) []string {
	mut res := []string
	mut buf_len := 1024
	mut buf := malloc(buf_len)

	mode := 'rb'
	mut fp := &C.FILE{}
	$if windows {
		fp = C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		fp = C.fopen(path.str, mode.str) 
	}
	if isnil(fp) {
		// TODO
		// return error('failed to open file "$path"')
		return res
	}

	mut buf_index := 0
	for C.fgets(buf + buf_index, buf_len - buf_index, fp) != 0 {
		len := C.strlen(buf)
		if len == buf_len - 1 && buf[len - 1] != 10 {
			buf_len *= 2
			buf = C.realloc(buf, buf_len)
			if isnil(buf) {
				panic('Could not reallocate the read buffer')
			}
			buf_index = len
			continue
		}
		if buf[len - 1] == 10 {
			buf[len - 1] = `\0`
		}
		$if windows {
			if buf[len - 2] == 13 {
				buf[len - 2] = `\0`
			}
		}
		res << tos_clone(buf)
		buf_index = 0
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

pub fn open(path string) ?File {
	mut file := File{}
	$if windows {
		wpath := path.to_wide()
		mode := 'rb'
		file = File {			
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str 
		file = File {
			cfile: C.fopen(cpath, 'rb') 
		}
	}
	if isnil(file.cfile) {
		return error('failed to open file "$path"')
	}
	return file 
}

// create creates a file at a specified location and returns a writable `File` object.
pub fn create(path string) ?File {
	mut file := File{}
	$if windows {
		wpath := path.replace('/', '\\').to_wide()
		mode := 'wb'
		file = File {			
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str 
		file = File {
			cfile: C.fopen(cpath, 'wb') 
		}
	}
	if isnil(file.cfile) {
		return error('failed to create file "$path"')
	}
	return file 
}

pub fn open_append(path string) ?File {
	mut file := File{}
	$if windows {
		wpath := path.replace('/', '\\').to_wide()
		mode := 'ab'
		file = File {			
			cfile: C._wfopen(wpath, mode.to_wide())
		}
	} $else {
		cpath := path.str 
		file = File {
			cfile: C.fopen(cpath, 'ab') 
		}
	}
	if isnil(file.cfile) {
		return error('failed to create(append) file "$path"')
	}
	return file 
}

pub fn (f File) write(s string) {
	ss := s.clone() // TODO is clone() needed here? 
	C.fputs(ss.str, f.cfile)
	// ss.free()
	// C.fwrite(s.str, 1, s.len, f.cfile)
}

// convert any value to []byte (LittleEndian) and write it
// for example if we have write(7, 4), "07 00 00 00" gets written
// write(0x1234, 2) => "34 12"
pub fn (f File) write_bytes(data voidptr, size int) {
	C.fwrite(data, 1, size, f.cfile)
}

pub fn (f File) write_bytes_at(data voidptr, size, pos int) {
	C.fseek(f.cfile, pos, SEEK_SET)
	C.fwrite(data, 1, size, f.cfile)
	C.fseek(f.cfile, 0, SEEK_END)
}

pub fn (f File) writeln(s string) {
	// C.fwrite(s.str, 1, s.len, f.cfile)
	// ss := s.clone()
	// TODO perf
	C.fputs(s.str, f.cfile)
	// ss.free()
	C.fputs('\n', f.cfile)
}

pub fn (f File) flush() {
	C.fflush(f.cfile)
}

pub fn (f File) close() {
	C.fclose(f.cfile)
}

// system starts the specified command, waits for it to complete, and returns its code.
pub fn system(cmd string) int {
	mut ret := int(0)
	$if windows {
		ret = C._wsystem(cmd.to_wide())
	} $else {
		ret = C.system(cmd.str) 
	}
	if ret == -1 {
		os.print_c_errno()
	}
	return ret
}

fn popen(path string) *FILE {
	$if windows {
		mode := 'rb'
		wpath := path.to_wide()
		return C._wpopen(wpath, mode.to_wide())
	}
	$else {
		cpath := path.str
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

// `getenv` returns the value of the environment variable named by the key.
pub fn getenv(key string) string {	
	$if windows {
		s := C._wgetenv(key.to_wide())
		if isnil(s) {
			return ''
		}
		return string_from_wide(s)
	} $else {
		s := C.getenv(key.str)
		if isnil(s) {
			return ''
		}
		return string(s)
	}
}

pub fn setenv(name string, value string, overwrite bool) int {
	$if windows {
		format := '$name=$value'

		if overwrite {
			return C._putenv(format.str)
		}

		return -1
	} 
	$else { 
		return C.setenv(name.str, value.str, overwrite)
	} 
}

pub fn unsetenv(name string) int {
	$if windows {
		format := '${name}='
		
		return C._putenv(format.str)
	} 
	$else { 
		return C.unsetenv(name.str)
	} 
}

// `file_exists` returns true if `path` exists.
pub fn file_exists(path string) bool {
	$if windows {
		path = path.replace('/', '\\')
		return C._waccess( path.to_wide(), 0 ) != -1
	} $else {
		return C.access( path.str, 0 ) != -1
	}
}

pub fn dir_exists(path string) bool {
	$if windows {
		path = path.replace('/', '\\')
		attr := int(C.GetFileAttributes(path.to_wide()))
		if attr == INVALID_FILE_ATTRIBUTES {
			return false
		}
		if (attr & FILE_ATTRIBUTE_DIRECTORY) != 0 {
			return true
		}
		return false
	} 
	$else { 
		dir := C.opendir(path.str)
		res := !isnil(dir)
		if res {
			C.closedir(dir)
		}
		return res
	} 
}

// mkdir creates a new directory with the specified path.
pub fn mkdir(path string) {
	$if windows {
		path = path.replace('/', '\\')
		// Windows doesnt recursively create the folders
		// so we need to help it out here
		if path.last_index('\\') != -1 {
			mkdir(path.all_before_last('\\'))
		}
		C.CreateDirectory(path.to_wide(), 0)
	}
	$else {
		C.mkdir(path.str, 511)// S_IRWXU | S_IRWXG | S_IRWXO
	}
}

// rm removes file in `path`.
pub fn rm(path string) {
	$if windows {
		C._wremove(path.to_wide())
	}
	$else {
		C.remove(path.str)
	}
	// C.unlink(path.cstr())
}


// rmdir removes a specified directory.
pub fn rmdir(path string) {
	$if !windows {
		C.rmdir(path.str)		
	}
	$else {
		C.RemoveDirectory(path.to_wide())
	}
}


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


// dir returns all but the last element of path, typically the path's directory.  
pub fn dir(path string) string {
	mut pos := -1
	// TODO PathSeparator defined in os_win.v doesn't work when building V, 
	// because v.c is generated for a nix system. 
	$if windows { 
		pos = path.last_index('\\') 
	} 
	$else { 
		pos = path.last_index(PathSeparator) 
	} 
	if pos == -1 {
		return '.' 
	} 
	return path.left(pos) 
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
    str := get_raw_line()
    if str[str.len - 1] == `\n` {
        return str.substr(0, str.len - 1)
    }

    return str
}

// get_raw_line returns a one-line string from stdin along with '\n' if there is any
pub fn get_raw_line() string {
	$if windows {
		max := 512 // MAX_PATH * sizeof(wchar_t)
		buf := &u16(malloc(max))
		h_input := C.GetStdHandle(STD_INPUT_HANDLE)
		if h_input == INVALID_HANDLE_VALUE {
			panic('get_raw_line() error getting input handle.')
		}
		mut nr_chars := 0
		C.ReadConsole(h_input, buf, max, &nr_chars, 0)
		if nr_chars == 0 {
			return ''
		}
		return string_from_wide2(buf, nr_chars)
	}
	$else {
		//u64 is used because C.getline needs a size_t as second argument
		//Otherwise, it would cause a valgrind warning and may be dangerous
		//Malloc takes an int as argument so a cast has to be made
		max := u64(256)
		buf := malloc(int(max))
		nr_chars := C.getline(&buf, &max, stdin)
		if nr_chars == 0 {
			return ''
		}
		return string(buf, nr_chars)
	} 
}

pub fn get_lines() []string {
        mut line := ''
        mut inputstr := []string
        for {
                line = get_line()
                if(line.len <= 0) {
                        break
                }
                line = line.trim_space()
                inputstr << line
        }
        return inputstr
}

pub fn get_lines_joined() string {
        mut line := ''
        mut inputstr := ''
        for {
                line = get_line()
                if(line.len <= 0) {
                        break
                }
                line = line.trim_space()
                inputstr += line
        }
        return inputstr
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
	$if freebsd {
		return 'freebsd' 
	} 
	$if openbsd {
		return 'openbsd' 
	} 
	$if netbsd {
		return 'netbsd' 
	} 
	$if dragonfly {
		return 'dragonfly' 
	}
	// $if msvc {
	// 	return 'windows'
	// }
	return 'unknown'
}

// home_dir returns path to user's home directory.
pub fn home_dir() string {
	mut home := os.getenv('HOME')
	$if windows {
		home = os.getenv('HOMEDRIVE')
		if home.len == 0 {
			home = os.getenv('SYSTEMDRIVE')
		}
		mut homepath := os.getenv('HOMEPATH')
		if homepath.len == 0 {
			homepath = '\\Users\\' + os.getenv('USERNAME')
		}
		home += homepath
	}
	home += '/'
	return home
}

// write_file writes `text` data to a file in `path`. 
pub fn write_file(path, text string) {
	f := os.create(path) or {
		return 
	} 
	f.write(text)
	f.close()
}

pub fn clear() {
	C.printf('\x1b[2J')
	C.printf('\x1b[H')
}

fn on_segfault(f voidptr) {
	$if windows {
		return
	}
	$if mac {
		mut sa := C.sigaction{}
		C.memset(&sa, 0, sizeof(sigaction))
		C.sigemptyset(&sa.sa_mask)
		sa.sa_sigaction = f
		sa.sa_flags   = SA_SIGINFO
		C.sigaction(SIGSEGV, &sa, 0)
	}
}

pub fn executable() string {
	$if linux {
		mut result := malloc(MAX_PATH)
		count := int(C.readlink('/proc/self/exe', result, MAX_PATH ))
		if count < 0 {
			panic('error reading /proc/self/exe to get exe path')
		}
		return string(result, count)
	}
	$if windows {
		mut result := &u16(malloc(512)) // MAX_PATH * sizeof(wchar_t)
		len := int(C.GetModuleFileName( 0, result, MAX_PATH ))
		return string_from_wide2(result, len)
	}
	$if mac {
		mut result := malloc(MAX_PATH)
		pid := C.getpid() 
		ret := C.proc_pidpath (pid, result, MAX_PATH) 
		if ret <= 0  {
			println('os.executable() failed') 
			return '.'  
		}  
		return string(result) 
	}
	$if freebsd {
		mut result := malloc(MAX_PATH)
		mut mib := [1 /* CTL_KERN */, 14 /* KERN_PROC */, 12 /* KERN_PROC_PATHNAME */, -1]!! 
		size := MAX_PATH 
		C.sysctl(mib, 4, result, &size, 0, 0) 
		return string(result) 
	} 
	$if openbsd {
		// "Sadly there is no way to get the full path of the executed file in OpenBSD." 
		// lol 
		return os.args[0] 
	} 
	$if netbsd {
		mut result := malloc(MAX_PATH)
		count := int(C.readlink('/proc/curproc/exe', result, MAX_PATH ))
		if count < 0 {
			panic('error reading /proc/curproc/exe to get exe path')
		}
		return string(result, count)
	} 
	$if dragonfly {
		mut result := malloc(MAX_PATH)
		count := int(C.readlink('/proc/curproc/file', result, MAX_PATH ))
		if count < 0 {
			panic('error reading /proc/curproc/file to get exe path')
		}
		return string(result, count)
	} 
	return '.' 
}

pub fn is_dir(path string) bool {
	$if windows {
		return dir_exists(path) 
		//val := int(C.GetFileAttributes(path.to_wide()))
		// Note: this return is broke (wrong). we have dir_exists already how will this differ?
		//return (val &FILE_ATTRIBUTE_DIRECTORY) > 0
	} 
	$else { 
		statbuf := C.stat{}
		cstr := path.str
		if C.stat(cstr, &statbuf) != 0 {
			return false
		}
		return statbuf.st_mode & S_IFMT == S_IFDIR
	} 
}

pub fn chdir(path string) {
	$if windows {
		C._wchdir(path.to_wide())
	}
	$else { 
		C.chdir(path.str)
	} 
}

pub fn getwd() string {	
	$if windows {
		max := 1024 // MAX_PATH * sizeof(wchar_t)
		buf := &u16(malloc(max))
		if C._wgetcwd(buf, max/2) == 0 {
			return ''
		}
		return string_from_wide(buf)
	}
	$else {
		buf := malloc(512) 
		if C.getcwd(buf, 512) == 0 {
			return ''
		}
		return string(buf)
	}
}

// win: FILETIME
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-filetime
struct filetime {
  dwLowDateTime u32
  dwHighDateTime u32
}

// win: WIN32_FIND_DATA
// https://docs.microsoft.com/en-us/windows/win32/api/minwinbase/ns-minwinbase-_win32_find_dataw
struct win32finddata {
mut:
    dwFileAttributes u32
    ftCreationTime filetime
  	ftLastAccessTime filetime
  	ftLastWriteTime filetime
	nFileSizeHigh u32
	nFileSizeLow u32
	dwReserved0 u32
	dwReserved1 u32
	cFileName [260]u16 // MAX_PATH = 260 
	cAlternateFileName [14]u16 // 14
  	dwFileType u32
  	dwCreatorType u32
  	wFinderFlags u16
}

pub fn ls(path string) []string {
	$if windows {
		mut find_file_data := win32finddata{}
		mut dir_files := []string
		// We can also check if the handle is valid. but using dir_exists instead
		// h_find_dir := C.FindFirstFile(path.str, &find_file_data)
		// if (INVALID_HANDLE_VALUE == h_find_dir) {
		//     return dir_files
		// }
		// C.FindClose(h_find_dir)
		if !dir_exists(path) {
			println('ls() couldnt open dir "$path" (does not exist).')
			return dir_files
		}
		// NOTE: Should eventually have path struct & os dependant path seperator (eg os.PATH_SEPERATOR)
		// we need to add files to path eg. c:\windows\*.dll or :\windows\*
		path_files := '$path\\*' 
		// NOTE:TODO: once we have a way to convert utf16 wide character to utf8
		// we should use FindFirstFileW and FindNextFileW
		h_find_files := C.FindFirstFile(path_files.to_wide(), &find_file_data)
		first_filename := string_from_wide(&u16(find_file_data.cFileName))
		if first_filename != '.' && first_filename != '..' {
			dir_files << first_filename
		}
		for C.FindNextFile(h_find_files, &find_file_data) {
			filename := string_from_wide(&u16(find_file_data.cFileName))
			if filename != '.' && filename != '..' {
				dir_files << filename.clone()
			}
		}
		C.FindClose(h_find_files)
		return dir_files
	} 
	$else { 
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
}

pub fn signal(signum int, handler voidptr) {
	C.signal(signum, handler)
}

pub fn fork() int {
	$if !windows {
		pid := C.fork()
		return pid
	}
}

pub fn wait() int {
	$if !windows {
		pid := C.wait(0)
		return pid
	}
}

pub fn file_last_mod_unix(path string) int {
	attr := C.stat{} 
	//# struct stat attr;
	C.stat(path.str, &attr) 
	//# stat(path.str, &attr);
	return attr.st_mtime 
	//# return attr.st_mtime ;
}
 

fn log(s string) {
}

pub fn flush_stdout() {
	C.fflush(stdout)
} 

pub fn print_backtrace() {
/* 
	# void *buffer[100];
	nptrs := 0
	# nptrs = backtrace(buffer, 100);
	# printf("%d!!\n", nptrs);
	# backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO) ;
*/ 
}

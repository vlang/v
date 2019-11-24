module builtin


// <string.h>
fn C.memcpy(byteptr, byteptr, int) voidptr
fn C.memmove(byteptr, byteptr, int) voidptr

//fn C.malloc(int) byteptr
fn C.realloc(a byteptr, b int) byteptr

fn C.qsort(voidptr, int, int, voidptr)

fn C.sprintf(a ...voidptr) byteptr
fn C.strlen(s byteptr) int
fn C.isdigit(s byteptr) bool

// stdio.h
fn C.popen(c byteptr, t byteptr) voidptr

// <execinfo.h>
fn backtrace(a voidptr, b int) int
fn backtrace_symbols(voidptr, int) &byteptr
fn backtrace_symbols_fd(voidptr, int, int)

// <libproc.h>
fn proc_pidpath(int, voidptr, int) int

fn C.realpath(byteptr, byteptr) &char




fn C.chmod(byteptr, int)
fn C.printf(byteptr, ...byteptr)
fn C.fputs(byteptr)
fn C.fflush(byteptr) int
// TODO define args in these functions
fn C.fseek() int
fn C.fopen() int
fn C.fwrite() int
fn C.fclose() int
fn C.pclose() int
fn C.system() int
fn C.setenv() int
fn C.unsetenv() int
fn C.access() int
fn C.remove() int
fn C.rmdir() int
fn C.chdir() int
fn C.fread() int
fn C.rewind() int
fn C.stat() int
fn C.rename() int
fn C.fgets() int
fn C.memset() int
fn C.sigemptyset() int
fn C.getcwd() int
fn C.signal() int
fn C.mktime() int
fn C.gettimeofday() int
fn C.sleep() int
fn C.usleep() int
fn C.opendir() voidptr
fn C.closedir() int
fn C.mkdir() int
fn C.srand() int
fn C.atof() int
fn C.tolower() int
fn C.toupper() int
fn C.getchar() int
fn C.strerror() *C.char
fn C.snprintf() int
fn C.fprintf(byteptr, ...byteptr)

fn C.WIFEXITED() bool
fn C.WEXITSTATUS() int
fn C.WIFSIGNALED() bool
fn C.WTERMSIG() int
fn C.DEFAULT_LE() bool
fn C.DEFAULT_EQ() bool
fn C.DEFAULT_GT() bool
fn C.DEFAULT_EQUAL() bool
fn C.DEFAULT_NOT_EQUAL() bool
fn C.DEFAULT_LT() bool
fn C.DEFAULT_GE() bool
fn C.isatty() int

fn C.syscall() int





// Windows
fn C._setmode(int, int)
fn C._fileno(int) int
fn C._get_osfhandle(fd int) C.intptr_t
fn C.GetModuleFileNameW(hModule voidptr, lpFilename &u16, nSize u32) u32
fn C.CreatePipe(hReadPipe &voidptr, hWritePipe &voidptr, lpPipeAttributes voidptr, nSize u32) bool
fn C.SetHandleInformation(hObject voidptr, dwMask u32, dwFlags u32) bool
fn C.ExpandEnvironmentStringsW(lpSrc &u16, lpDst &u16, nSize u32) u32
fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr, bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr, lpProcessInformation voidptr) bool
fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead voidptr, lpOverlapped voidptr) bool
fn C.GetFileAttributesW(lpFileName byteptr) u32
fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lpReserved &u32, lpType &u32, lpData byteptr, lpcbData &u32) int
fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int
fn C.RemoveDirectory() int
fn C.GetStdHandle()
fn C.SetConsoleMode()
fn C._putsws()
fn C.wprintf()

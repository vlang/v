module builtin

// <string.h>
fn C.memcpy(byteptr, byteptr, int) voidptr


fn C.memmove(byteptr, byteptr, int) voidptr
fn C.calloc(int)  byteptr
fn C.malloc(int) byteptr
fn C.realloc(a byteptr, b int) byteptr
fn C.free(ptr voidptr)
fn C.exit(code int)


fn C.qsort(voidptr, int, int, qsort_callback_func)


fn C.sprintf(a ...voidptr) int


fn C.strlen(s byteptr) int


fn C.isdigit(s byteptr) bool
// stdio.h
fn C.popen(c byteptr, t byteptr) voidptr

// <execinfo.h>
fn C.backtrace(a &voidptr, size int) int
fn C.backtrace_symbols(a &voidptr, size int)  &charptr
fn C.backtrace_symbols_fd(a &voidptr, size int, fd int)

// <libproc.h>
pub fn proc_pidpath(int, voidptr, int) int


fn C.realpath(byteptr, byteptr) &char


fn C.chmod(byteptr, int) int


fn C.printf(byteptr, ...byteptr) int


fn C.fputs(byteptr) int


fn C.fflush(byteptr) int
// TODO define args in these functions
fn C.fseek() int


fn C.fopen() voidptr


fn C.fileno(voidptr) int


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


fn C.lstat() int


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


fn C.strerror(int) charptr


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


fn C.sysctl() int


fn C._fileno(int) int


fn C._get_osfhandle(fd int) C.intptr_t


fn C.GetModuleFileNameW(hModule voidptr, lpFilename &u16, nSize u32) u32


fn C.CreatePipe(hReadPipe &voidptr, hWritePipe &voidptr, lpPipeAttributes voidptr, nSize u32) bool


fn C.SetHandleInformation(hObject voidptr, dwMask u32, dw_flags u32) bool


fn C.ExpandEnvironmentStringsW(lpSrc &u16, lpDst &u16, nSize u32) u32


fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr, bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr, lpProcessInformation voidptr) bool


fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead voidptr, lpOverlapped voidptr) bool


fn C.GetFileAttributesW(lpFileName byteptr) u32


fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData byteptr, lpcbData &u32) int


fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int


fn C.RegCloseKey()


fn C.RegQueryValueEx() voidptr


fn C.RemoveDirectory() int


//fn C.GetStdHandle() voidptr
fn C.GetStdHandle(u32) voidptr


//fn C.SetConsoleMode()
fn C.SetConsoleMode(voidptr, u32)


//fn C.GetConsoleMode() int
fn C.GetConsoleMode(voidptr, &u32) int


fn C.wprintf()


//fn C.setbuf()
fn C.setbuf(voidptr, charptr)


fn C.SymCleanup()


fn C.MultiByteToWideChar() int


fn C.wcslen() int


fn C.WideCharToMultiByte() int


fn C._wstat()


fn C._wrename()


fn C._wfopen() voidptr


fn C._wpopen() voidptr


fn C._pclose() int


fn C._wsystem() int


fn C._wgetenv() voidptr


fn C._putenv() int


fn C._waccess() int


fn C._wremove()


fn C.ReadConsole() voidptr


fn C.WriteConsole() voidptr


fn C.WriteFile() voidptr


fn C.GetModuleFileName() int


fn C._wchdir()


fn C._wgetcwd() int


fn C._fullpath() int


fn C.GetCommandLine() voidptr


fn C.LocalFree()


fn C.FindFirstFileW() voidptr


fn C.FindFirstFile() voidptr


fn C.FindNextFile() int


fn C.FindClose()


fn C.MAKELANGID() int


fn C.FormatMessage() voidptr


fn C.CloseHandle()


fn C.GetExitCodeProcess()


fn C.RegOpenKeyEx() voidptr


fn C.GetTickCount() i64


fn C.Sleep()


fn C.WSAStartup(u16, &voidptr) int


fn C.WSAGetLastError() int


fn C.closesocket(int) int


fn C.vschannel_init(&C.TlsContext)


fn C.request(&C.TlsContext, int, &u16, byteptr, &byteptr)


fn C.vschannel_cleanup(&C.TlsContext)


fn C.URLDownloadToFile(int, &u16, &u16, int, int)


fn C.GetLastError() u32


fn C.CreateDirectory(byteptr, int) bool


fn C.BCryptGenRandom(int, voidptr, int, int) int


fn C.CreateMutex(int, bool, byteptr) voidptr


fn C.WaitForSingleObject(voidptr, int) int


fn C.ReleaseMutex(voidptr) bool

// pthread.h

fn C.pthread_mutex_init(voidptr, voidptr) int
fn C.pthread_mutex_lock(voidptr) int
fn C.pthread_mutex_unlock(voidptr) int


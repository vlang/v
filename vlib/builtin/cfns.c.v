module builtin

// <string.h>
fn C.memcpy(dest byteptr, src byteptr, n int) voidptr

fn C.memcmp(byteptr, byteptr, int) int

fn C.memmove(byteptr, byteptr, int) voidptr

fn C.calloc(int) byteptr

fn C.malloc(int) byteptr

fn C.realloc(a byteptr, b int) byteptr

fn C.free(ptr voidptr)

fn C.exit(code int)

fn C.qsort(base voidptr, items size_t, item_size size_t, cb qsort_callback_func)

fn C.sprintf(a ...voidptr) int

fn C.strlen(s charptr) int

fn C.sscanf(byteptr, byteptr, ...byteptr) int

fn C.isdigit(s byteptr) bool

// stdio.h
fn C.popen(c charptr, t charptr) voidptr

// <execinfo.h>
fn C.backtrace(a &voidptr, size int) int

fn C.backtrace_symbols(a &voidptr, size int) &charptr

fn C.backtrace_symbols_fd(a &voidptr, size int, fd int)

// <libproc.h>
pub fn proc_pidpath(int, voidptr, int) int

fn C.realpath(charptr, charptr) &char

fn C.chmod(byteptr, int) int

fn C.printf(byteptr, ...byteptr) int

fn C.puts(byteptr) int

fn C.fputs(byteptr) int

fn C.fflush(byteptr) int

// TODO define args in these functions
fn C.fseek() int

fn C.fopen() voidptr

fn C.fileno(voidptr) int

fn C.fread(ptr voidptr, item_size size_t, items size_t, stream &C.FILE) size_t

fn C.fwrite(ptr voidptr, item_size size_t, items size_t, stream &C.FILE) size_t

fn C.fclose() int

fn C.pclose() int

// process execution, os.process:
fn C.getpid() int

fn C.system() int

fn C.posix_spawn(child_pid &int, path charptr, file_actions voidptr, attrp voidptr, argv &charptr, envp &charptr) int

fn C.posix_spawnp(child_pid &int, exefile charptr, file_actions voidptr, attrp voidptr, argv &charptr, envp &charptr) int

fn C.execve(cmd_path charptr, args voidptr, envs voidptr) int

fn C.fork() int

fn C.wait(status &int) int

fn C.waitpid(pid int, status &int, options int) int

fn C.kill(pid int, sig int) int

fn C.setenv(charptr) int

fn C.unsetenv(charptr) int

fn C.access() int

fn C.remove() int

fn C.rmdir() int

fn C.chdir() int

fn C.rewind() int

fn C.stat(charptr) int

fn C.lstat() int

fn C.rename() int

fn C.fgets() int

fn C.memset() int

fn C.sigemptyset() int

fn C.getcwd() int

fn C.signal(signal int, handlercb voidptr) voidptr

fn C.mktime() int

fn C.gettimeofday() int

[trusted]
fn C.sleep(int) int

fn C.usleep() int

fn C.opendir() voidptr

fn C.closedir() int

fn C.mkdir() int

fn C.srand() int

fn C.atof() int

fn C.tolower() int

fn C.toupper() int

[trusted]
fn C.getchar() int

[trusted]
fn C.strerror(int) charptr

fn C.snprintf() int

fn C.fprintf(byteptr, ...byteptr)

fn C.WIFEXITED() bool

fn C.WEXITSTATUS() int

fn C.WIFSIGNALED() bool

fn C.WTERMSIG() int

fn C.isatty() int

fn C.syscall() int

fn C.sysctl() int

fn C._fileno(int) int

fn C._get_osfhandle(fd int) C.intptr_t

fn C.GetModuleFileName() int

fn C.GetModuleFileNameW(hModule voidptr, lpFilename &u16, nSize u32) u32

fn C.CreateFile() voidptr

fn C.CreateFileW(lpFilename &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes &u16, dwCreationDisposition u32, dwFlagsAndAttributes u32, hTemplateFile voidptr) u32

fn C.GetFinalPathNameByHandleW(hFile voidptr, lpFilePath &u16, nSize u32, dwFlags u32) int

fn C.CreatePipe(hReadPipe &voidptr, hWritePipe &voidptr, lpPipeAttributes voidptr, nSize u32) bool

fn C.SetHandleInformation(hObject voidptr, dwMask u32, dw_flags u32) bool

fn C.ExpandEnvironmentStringsW(lpSrc &u16, lpDst &u16, nSize u32) u32

fn C.SendMessageTimeout() u32

fn C.SendMessageTimeoutW(hWnd voidptr, Msg u32, wParam &u16, lParam &u32, fuFlags u32, uTimeout u32, lpdwResult &u64) u32

fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr, bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr, lpProcessInformation voidptr) bool

fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead C.LPDWORD, lpOverlapped voidptr) bool

fn C.GetFileAttributesW(lpFileName byteptr) u32

fn C.RegQueryValueEx() voidptr

fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData byteptr, lpcbData &u32) int

fn C.RegOpenKeyEx() voidptr

fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int

fn C.RegSetValueEx() voidptr

fn C.RegSetValueExW(hKey voidptr, lpValueName &u16, Reserved u32, dwType u32, lpData byteptr, lpcbData u32) int

fn C.RegCloseKey()

fn C.RemoveDirectory() int

// fn C.GetStdHandle() voidptr
fn C.GetStdHandle(u32) voidptr

// fn C.SetConsoleMode()
fn C.SetConsoleMode(voidptr, u32)

// fn C.GetConsoleMode() int
fn C.GetConsoleMode(voidptr, &u32) int

fn C.GetCurrentProcessId() int

fn C.wprintf()

// fn C.setbuf()
fn C.setbuf(voidptr, charptr)

fn C.SymCleanup()

fn C.MultiByteToWideChar() int

fn C.wcslen() int

fn C.WideCharToMultiByte() int

fn C._wstat()

fn C._wrename() int

fn C._wfopen() voidptr

fn C._wpopen() voidptr

fn C._pclose() int

fn C._wsystem() int

fn C._wgetenv() voidptr

fn C._putenv() int

fn C._waccess() int

fn C._wremove() int

fn C.ReadConsole() voidptr

fn C.WriteConsole() voidptr

fn C.WriteFile() voidptr

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

fn C.CloseHandle(voidptr) int

fn C.GetExitCodeProcess()

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

// win crypto
fn C.BCryptGenRandom(int, voidptr, int, int) int

// win synchronization
fn C.CreateMutex(int, bool, byteptr) voidptr

fn C.WaitForSingleObject(voidptr, int) int

fn C.ReleaseMutex(voidptr) bool

fn C.CreateEvent(int, bool, bool, byteptr) voidptr

fn C.SetEvent(voidptr) int

fn C.CreateSemaphore(voidptr, int, int, voidptr) voidptr

fn C.ReleaseSemaphore(voidptr, int, voidptr) voidptr

fn C.InitializeSRWLock(voidptr)

fn C.AcquireSRWLockShared(voidptr)

fn C.AcquireSRWLockExclusive(voidptr)

fn C.ReleaseSRWLockShared(voidptr)

fn C.ReleaseSRWLockExclusive(voidptr)

// pthread.h
fn C.pthread_mutex_init(voidptr, voidptr) int

fn C.pthread_mutex_lock(voidptr) int

fn C.pthread_mutex_unlock(voidptr) int

fn C.pthread_mutex_destroy(voidptr) int

fn C.pthread_rwlockattr_init(voidptr) int

fn C.pthread_rwlockattr_setkind_np(voidptr, int) int

fn C.pthread_rwlockattr_setpshared(voidptr, int) int

fn C.pthread_rwlock_init(voidptr, voidptr) int

fn C.pthread_rwlock_rdlock(voidptr) int

fn C.pthread_rwlock_wrlock(voidptr) int

fn C.pthread_rwlock_unlock(voidptr) int

fn C.pthread_condattr_init(voidptr) int

fn C.pthread_condattr_setpshared(voidptr, int) int

fn C.pthread_condattr_destroy(voidptr) int

fn C.pthread_cond_init(voidptr, voidptr) int

fn C.pthread_cond_signal(voidptr) int

fn C.pthread_cond_wait(voidptr, voidptr) int

fn C.pthread_cond_timedwait(voidptr, voidptr, voidptr) int

fn C.pthread_cond_destroy(voidptr) int

fn C.sem_init(voidptr, int, u32) int

fn C.sem_post(voidptr) int

fn C.sem_wait(voidptr) int

fn C.sem_trywait(voidptr) int

fn C.sem_timedwait(voidptr, voidptr) int

fn C.sem_destroy(voidptr) int

// MacOS semaphore functions
fn C.dispatch_semaphore_create(i64) voidptr

fn C.dispatch_semaphore_signal(voidptr) i64

fn C.dispatch_semaphore_wait(voidptr, u64) i64

fn C.dispatch_time(u64, i64) u64

fn C.dispatch_release(voidptr)

// file descriptor based reading/writing
fn C.read(fd int, buf voidptr, count size_t) int

fn C.write(fd int, buf voidptr, count size_t) int

fn C.close(fd int) int

// pipes
fn C.pipe(pipefds &int) int

fn C.dup2(oldfd int, newfd int) int

// used by gl, stbi, freetype
fn C.glTexImage2D()

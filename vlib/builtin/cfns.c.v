module builtin

// struct C.FILE {}

// <string.h>
fn C.memcpy(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memcmp(const_s1 voidptr, const_s2 voidptr, n usize) int

fn C.memmove(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memset(str voidptr, c int, n usize) voidptr

[trusted]
fn C.calloc(int, int) &u8

fn C.atoi(&char) int

fn C.malloc(int) &u8

fn C.realloc(a &u8, b int) &u8

fn C.free(ptr voidptr)

[noreturn; trusted]
fn C.exit(code int)

fn C.qsort(base voidptr, items usize, item_size usize, cb C.qsort_callback_func)

fn C.sprintf(a ...voidptr) int

fn C.strlen(s &char) int

fn C.sscanf(&u8, &u8, ...&u8) int

[trusted]
fn C.isdigit(c int) bool

// stdio.h
fn C.popen(c &char, t &char) voidptr

// <libproc.h>
pub fn proc_pidpath(int, voidptr, int) int

fn C.realpath(&char, &char) &char

// fn C.chmod(byteptr, mode_t) int
fn C.chmod(&char, u32) int

fn C.printf(&char, ...voidptr) int

fn C.puts(&char) int
fn C.abs(f64) f64

fn C.fputs(str &char, stream &C.FILE) int

fn C.fflush(&C.FILE) int

// TODO define args in these functions
fn C.fseek(stream &C.FILE, offset int, whence int) int

fn C.fopen(filename &char, mode &char) &C.FILE

fn C.fileno(&C.FILE) int

fn C.fread(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fwrite(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fclose(stream &C.FILE) int

fn C.pclose(stream &C.FILE) int

fn C.strrchr(s &char, c int) &char
fn C.strchr(s &char, c int) &char

// process execution, os.process:
[trusted]
fn C.getpid() int

fn C.getuid() int

fn C.geteuid() int

fn C.system(cmd &char) int

fn C.posix_spawn(child_pid &int, path &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) int

fn C.posix_spawnp(child_pid &int, exefile &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) int

fn C.execve(cmd_path &char, args voidptr, envs voidptr) int

fn C.execvp(cmd_path &char, args &&char) int

fn C._execve(cmd_path &char, args voidptr, envs voidptr) int

fn C._execvp(cmd_path &char, args &&char) int

fn C.strcmp(s1 &char, s2 &char) int

[trusted]
fn C.fork() int

fn C.wait(status &int) int

fn C.waitpid(pid int, status &int, options int) int

[trusted]
fn C.kill(pid int, sig int) int

fn C.setenv(&char, &char, int) int

fn C.unsetenv(&char) int

fn C.access(path &char, amode int) int

fn C.remove(filename &char) int

fn C.rmdir(path &char) int

fn C.chdir(path &char) int

fn C.rewind(stream &C.FILE) int

fn C.ftell(&C.FILE) int

fn C.stat(&char, voidptr) int

fn C.lstat(path &char, buf &C.stat) int

fn C.rename(old_filename &char, new_filename &char) int

fn C.fgets(str &char, n int, stream &C.FILE) int

[trusted]
fn C.sigemptyset() int

fn C.getcwd(buf &char, size usize) &char

[trusted]
fn C.mktime() int

fn C.gettimeofday(tv &C.timeval, tz &C.timezone) int

[trusted]
fn C.sleep(seconds u32) u32

// fn C.usleep(usec useconds_t) int
[trusted]
fn C.usleep(usec u32) int

[typedef]
struct C.DIR {
}

fn C.opendir(&char) &C.DIR

fn C.closedir(dirp &C.DIR) int

// fn C.mkdir(path &char, mode mode_t) int
fn C.mkdir(path &char, mode u32) int

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
[trusted]
fn C.rand() int

// C.srand seeds the internal PRNG with the given value.
[trusted]
fn C.srand(seed u32)

fn C.atof(str &char) f64

[trusted]
fn C.tolower(c int) int

[trusted]
fn C.toupper(c int) int

[trusted]
fn C.isspace(c int) int

fn C.strchr(s &char, c int) &char

[trusted]
fn C.getchar() int

fn C.strdup(s &char) &char

fn C.strncasecmp(s &char, s2 &char, n int) int

fn C.strcasecmp(s &char, s2 &char) int

fn C.strncmp(s &char, s2 &char, n int) int

[trusted]
fn C.strerror(int) &char

fn C.snprintf(str &char, size usize, format &char, opt ...voidptr) int

fn C.fprintf(voidptr, &char, ...voidptr)

[trusted]
fn C.WIFEXITED(status int) bool

[trusted]
fn C.WEXITSTATUS(status int) int

[trusted]
fn C.WIFSIGNALED(status int) bool

[trusted]
fn C.WTERMSIG(status int) int

[trusted]
fn C.isatty(fd int) int

fn C.syscall(number int, va ...voidptr) int

fn C.sysctl(name &int, namelen u32, oldp voidptr, oldlenp voidptr, newp voidptr, newlen usize) int

[trusted]
fn C._fileno(int) int

fn C._get_osfhandle(fd int) C.intptr_t

fn C.GetModuleFileName(hModule voidptr, lpFilename &u16, nSize u32) int

fn C.GetModuleFileNameW(hModule voidptr, lpFilename &u16, nSize u32) u32

fn C.CreateFile(lpFilename &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes &u16, dwCreationDisposition u32, dwFlagsAndAttributes u32, hTemplateFile voidptr) voidptr

fn C.CreateFileW(lpFilename &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes &u16, dwCreationDisposition u32, dwFlagsAndAttributes u32, hTemplateFile voidptr) u32

fn C.GetFinalPathNameByHandleW(hFile voidptr, lpFilePath &u16, nSize u32, dwFlags u32) int

fn C.CreatePipe(hReadPipe &voidptr, hWritePipe &voidptr, lpPipeAttributes voidptr, nSize u32) bool

fn C.SetHandleInformation(hObject voidptr, dwMask u32, dw_flags u32) bool

fn C.ExpandEnvironmentStringsW(lpSrc &u16, lpDst &u16, nSize u32) u32

fn C.GetComputerNameW(&u16, &u32) bool

fn C.GetUserNameW(&u16, &u32) bool

[trusted]
fn C.SendMessageTimeout() u32

fn C.SendMessageTimeoutW(hWnd voidptr, msg u32, wParam &u16, lParam &u32, fuFlags u32, uTimeout u32, lpdwResult &u64) u32

fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr, bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr, lpProcessInformation voidptr) bool

fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead C.LPDWORD, lpOverlapped voidptr) bool

fn C.GetFileAttributesW(lpFileName &u8) u32

fn C.RegQueryValueEx(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) voidptr

fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) int

fn C.RegOpenKeyEx(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) voidptr

fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int

fn C.RegSetValueEx() voidptr

fn C.RegSetValueExW(hKey voidptr, lpValueName &u16, reserved u32, dwType u32, lpData &u8, lpcbData u32) int

fn C.RegCloseKey(hKey voidptr)

fn C.RemoveDirectory(lpPathName &u16) int

// fn C.GetStdHandle() voidptr
fn C.GetStdHandle(u32) voidptr

// fn C.SetConsoleMode()
fn C.SetConsoleMode(voidptr, u32) int

// fn C.GetConsoleMode() int
fn C.GetConsoleMode(voidptr, &u32) int

[trusted]
fn C.GetCurrentProcessId() int

fn C.wprintf()

// fn C.setbuf()
fn C.setbuf(voidptr, &char)

fn C.SymCleanup(hProcess voidptr)

fn C.MultiByteToWideChar(codePage u32, dwFlags u32, lpMultiMyteStr &char, cbMultiByte int, lpWideCharStr &u16, cchWideChar int) int

fn C.wcslen(str &u16) int

fn C.WideCharToMultiByte(codePage u32, dwFlags u32, lpWideCharStr &u16, cchWideChar int, lpMultiByteStr &char, cbMultiByte int, lpDefaultChar &char, lpUsedDefaultChar &int) int

fn C._wstat(path &u16, buffer &C._stat) int

fn C._wrename(oldname &u16, newname &u16) int

fn C._wfopen(filename &u16, mode &u16) voidptr

fn C._wpopen(command &u16, mode &u16) voidptr

fn C._pclose(stream &C.FILE) int

fn C._wsystem(command &u16) int

fn C._wgetenv(varname &u16) voidptr

fn C._putenv(envstring &char) int

fn C._waccess(path &u16, mode int) int

fn C._wremove(path &u16) int

fn C.ReadConsole(in_input_handle voidptr, out_buffer voidptr, in_chars_to_read u32, out_read_chars &u32, in_input_control voidptr) bool

fn C.WriteConsole() voidptr

fn C.WriteFile() voidptr

fn C._wchdir(dirname &u16)

fn C._wgetcwd(buffer &u16, maxlen int) int

fn C._fullpath() int

fn C.GetFullPathName(voidptr, u32, voidptr, voidptr) u32

[trusted]
fn C.GetCommandLine() voidptr

fn C.LocalFree()

fn C.FindFirstFileW(lpFileName &u16, lpFindFileData voidptr) voidptr

fn C.FindFirstFile(lpFileName &u8, lpFindFileData voidptr) voidptr

fn C.FindNextFile(hFindFile voidptr, lpFindFileData voidptr) int

fn C.FindClose(hFindFile voidptr)

// macro
fn C.MAKELANGID(lgid voidptr, srtid voidptr) int

fn C.FormatMessage(dwFlags u32, lpSource voidptr, dwMessageId u32, dwLanguageId u32, lpBuffer voidptr, nSize int, arguments ...voidptr) voidptr

fn C.CloseHandle(voidptr) int

fn C.GetExitCodeProcess(hProcess voidptr, lpExitCode &u32)

[trusted]
fn C.GetTickCount() i64

[trusted]
fn C.Sleep(dwMilliseconds u32)

fn C.WSAStartup(u16, &voidptr) int

[trusted]
fn C.WSAGetLastError() int

fn C.closesocket(int) int

fn C.vschannel_init(&C.TlsContext)

fn C.request(&C.TlsContext, int, &u16, &u8, &&u8) int

fn C.vschannel_cleanup(&C.TlsContext)

fn C.URLDownloadToFile(int, &u16, &u16, int, int)

[trusted]
fn C.GetLastError() u32

fn C.CreateDirectory(&u8, int) bool

// win crypto
fn C.BCryptGenRandom(int, voidptr, int, int) int

// win synchronization
fn C.CreateMutex(int, bool, &u8) voidptr

fn C.WaitForSingleObject(voidptr, int) int

fn C.ReleaseMutex(voidptr) bool

fn C.CreateEvent(int, bool, bool, &u8) voidptr

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
[trusted]
fn C.dispatch_semaphore_create(i64) voidptr

fn C.dispatch_semaphore_signal(voidptr) i64

fn C.dispatch_semaphore_wait(voidptr, u64) i64

[trusted]
fn C.dispatch_time(u64, i64) u64

fn C.dispatch_release(voidptr)

// file descriptor based reading/writing
fn C.read(fd int, buf voidptr, count usize) int

fn C.write(fd int, buf voidptr, count usize) int

fn C.close(fd int) int

// pipes
fn C.pipe(pipefds &int) int

fn C.dup2(oldfd int, newfd int) int

// used by gl, stbi, freetype
fn C.glTexImage2D()

// used by ios for println
fn C.WrappedNSLog(str &u8)

// absolute value
fn C.abs(number int) int

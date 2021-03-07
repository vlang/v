module builtin

// <string.h>
fn C.memcpy(dest byteptr, src byteptr, n int) voidptr

fn C.memcmp(byteptr, byteptr, int) int

fn C.memmove(byteptr, byteptr, int) voidptr

[trusted]
fn C.calloc(int, int) byteptr

fn C.malloc(int) byteptr

fn C.realloc(a byteptr, b int) byteptr

fn C.free(ptr voidptr)

[trusted]
fn C.exit(code int)

fn C.qsort(base voidptr, items size_t, item_size size_t, cb qsort_callback_func)

fn C.sprintf(a ...voidptr) int

fn C.strlen(s charptr) int

fn C.sscanf(byteptr, byteptr, ...byteptr) int

[trusted]
fn C.isdigit(c int) bool

// stdio.h
fn C.popen(c charptr, t charptr) voidptr

// <execinfo.h>
fn C.backtrace(a &voidptr, size int) int

fn C.backtrace_symbols(a &voidptr, size int) &charptr

fn C.backtrace_symbols_fd(a &voidptr, size int, fd int)

// <libproc.h>
pub fn proc_pidpath(int, voidptr, int) int

fn C.realpath(charptr, charptr) &char

// fn C.chmod(byteptr, mode_t) int
fn C.chmod(byteptr, u32) int

fn C.printf(byteptr, ...voidptr) int

fn C.puts(byteptr) int

fn C.fputs(str byteptr, stream &C.FILE) int

fn C.fflush(&C.FILE) int

// TODO define args in these functions
fn C.fseek(stream &C.FILE, offset int, whence int) int

fn C.fopen(filename charptr, mode charptr) &C.FILE

fn C.fileno(&C.FILE) int

fn C.fread(ptr voidptr, item_size size_t, items size_t, stream &C.FILE) size_t

fn C.fwrite(ptr voidptr, item_size size_t, items size_t, stream &C.FILE) size_t

fn C.fclose(stream &C.FILE) int

fn C.pclose(stream &C.FILE) int

// process execution, os.process:
fn C.getpid() int

fn C.system(cmd charptr) int

fn C.posix_spawn(child_pid &int, path charptr, file_actions voidptr, attrp voidptr, argv &charptr, envp &charptr) int

fn C.posix_spawnp(child_pid &int, exefile charptr, file_actions voidptr, attrp voidptr, argv &charptr, envp &charptr) int

fn C.execve(cmd_path charptr, args voidptr, envs voidptr) int

[trusted]
fn C.fork() int

fn C.wait(status &int) int

fn C.waitpid(pid int, status &int, options int) int

fn C.kill(pid int, sig int) int

fn C.setenv(charptr, charptr, int) int

fn C.unsetenv(charptr) int

fn C.access(path charptr, amode int) int

fn C.remove(filename charptr) int

fn C.rmdir(path charptr) int

fn C.chdir(path charptr) int

fn C.rewind(stream &C.FILE) int

fn C.stat(charptr, voidptr) int

fn C.lstat(path charptr, buf &C.stat) int

fn C.rename(old_filename charptr, new_filename charptr) int

fn C.fgets(str charptr, n int, stream &C.FILE) int

fn C.memset(str voidptr, c int, n size_t) int

fn C.sigemptyset() int

fn C.getcwd(buf charptr, size size_t) charptr

fn C.signal(signal int, handlercb voidptr) voidptr

fn C.mktime() int

fn C.gettimeofday(tv &C.timeval, tz &C.timezone) int

[trusted]
fn C.sleep(seconds u32) u32

// fn C.usleep(usec useconds_t) int
fn C.usleep(usec u32) int

fn C.opendir(charptr) voidptr

fn C.closedir(dirp &C.DIR) int

// fn C.mkdir(path charptr, mode mode_t) int
fn C.mkdir(path charptr, mode u32) int

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
[trusted]
fn C.rand() int

// C.srand seeds the internal PRNG with the given value.
[trusted]
fn C.srand(seed u32)

fn C.atof(str charptr) f64

fn C.tolower(c int) int

fn C.toupper(c int) int

[trusted]
fn C.getchar() int

[trusted]
fn C.strerror(int) charptr

fn C.snprintf(str charptr, size size_t, format charptr, opt ...voidptr) int

fn C.fprintf(byteptr, ...byteptr)

fn C.WIFEXITED(status int) bool

fn C.WEXITSTATUS(status int) int

fn C.WIFSIGNALED(status int) bool

fn C.WTERMSIG(status int) int

fn C.isatty(fd int) int

fn C.syscall(number int, va ...voidptr) int

fn C.sysctl(name &int, namelen u32, oldp voidptr, oldlenp voidptr, newp voidptr, newlen size_t) int

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

fn C.SendMessageTimeout() u32

fn C.SendMessageTimeoutW(hWnd voidptr, Msg u32, wParam &u16, lParam &u32, fuFlags u32, uTimeout u32, lpdwResult &u64) u32

fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr, bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr, lpProcessInformation voidptr) bool

fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead C.LPDWORD, lpOverlapped voidptr) bool

fn C.GetFileAttributesW(lpFileName byteptr) u32

fn C.RegQueryValueEx(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData byteptr, lpcbData &u32) voidptr

fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData byteptr, lpcbData &u32) int

fn C.RegOpenKeyEx(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) voidptr

fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int

fn C.RegSetValueEx() voidptr

fn C.RegSetValueExW(hKey voidptr, lpValueName &u16, Reserved u32, dwType u32, lpData byteptr, lpcbData u32) int

fn C.RegCloseKey(hKey voidptr)

fn C.RemoveDirectory(lpPathName charptr) int

// fn C.GetStdHandle() voidptr
fn C.GetStdHandle(u32) voidptr

// fn C.SetConsoleMode()
fn C.SetConsoleMode(voidptr, u32) int

// fn C.GetConsoleMode() int
fn C.GetConsoleMode(voidptr, &u32) int

fn C.GetCurrentProcessId() int

fn C.wprintf()

// fn C.setbuf()
fn C.setbuf(voidptr, charptr)

fn C.SymCleanup(hProcess voidptr)

fn C.MultiByteToWideChar(codePage u32, dwFlags u32, lpMultiMyteStr charptr, cbMultiByte int, lpWideCharStr &u16, cchWideChar int) int

fn C.wcslen(str &u16) int

fn C.WideCharToMultiByte(codePage u32, dwFlags u32, lpWideCharStr &u16, cchWideChar int, lpMultiByteStr charptr, cbMultiByte int, lpDefaultChar charptr, lpUsedDefaultChar &int) int

fn C._wstat(path &u16, buffer &C._stat)

fn C._wrename(oldname &u16, newname &u16) int

fn C._wfopen(filename &u16, mode &u16) voidptr

fn C._wpopen(command &u16, mode &u16) voidptr

fn C._pclose(stream &C.FILE) int

fn C._wsystem(command &u16) int

fn C._wgetenv(varname &u16) voidptr

fn C._putenv(envstring charptr) int

fn C._waccess(path &u16, mode int) int

fn C._wremove(path &u16) int

fn C.ReadConsole(in_input_handle voidptr, out_buffer voidptr, in_chars_to_read u32, out_read_chars &u32, in_input_control voidptr) bool

fn C.WriteConsole() voidptr

fn C.WriteFile() voidptr

fn C._wchdir(dirname &u16)

fn C._wgetcwd(buffer &u16, maxlen int) int

fn C._fullpath() int

fn C.GetFullPathName(voidptr, u32, voidptr, voidptr) u32

fn C.GetCommandLine() voidptr

fn C.LocalFree()

fn C.FindFirstFileW(lpFileName &u16, lpFindFileData voidptr) voidptr

fn C.FindFirstFile(lpFileName byteptr, lpFindFileData voidptr) voidptr

fn C.FindNextFile(hFindFile voidptr, lpFindFileData voidptr) int

fn C.FindClose(hFindFile voidptr)

// macro
fn C.MAKELANGID(lgid voidptr, srtid voidptr) int

fn C.FormatMessage(dwFlags u32, lpSource voidptr, dwMessageId u32, dwLanguageId u32, lpBuffer voidptr, nSize int, Arguments ...voidptr) voidptr

fn C.CloseHandle(voidptr) int

fn C.GetExitCodeProcess(hProcess voidptr, lpExitCode &u32)

fn C.GetTickCount() i64

fn C.Sleep(dwMilliseconds u32)

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

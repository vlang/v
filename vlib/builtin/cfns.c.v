module builtin

@[typedef]
pub struct C.FILE {}

// <string.h>
fn C.memcpy(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memcmp(const_s1 voidptr, const_s2 voidptr, n usize) i32

fn C.memmove(dest voidptr, const_src voidptr, n usize) voidptr

fn C.memset(str voidptr, c i32, n usize) voidptr

fn C.memchr(str voidptr, c i32, n usize) voidptr

fn C.memmem(haystack voidptr, haystacklen usize, needle voidptr, needlelen usize) voidptr

fn C.mempcpy(dest voidptr, src voidptr, n usize) voidptr

@[trusted]
fn C.calloc(usize, usize) voidptr

fn C.atoi(&char) i32

fn C.malloc(usize) voidptr

fn C.realloc(a voidptr, b usize) voidptr

fn C.free(ptr voidptr)

fn C.mmap(addr_length voidptr, length usize, prot i32, flags i32, fd i32, offset isize) voidptr
fn C.mprotect(addr_length voidptr, len usize, prot i32) i32

fn C.aligned_alloc(align usize, size usize) voidptr

// windows aligned memory functions
fn C._aligned_malloc(size isize, align isize) voidptr
fn C._aligned_free(voidptr)
fn C._aligned_realloc(voidptr, size isize, align isize) voidptr
fn C._aligned_offset_malloc(size isize, align isize, offset isize) voidptr
fn C._aligned_offset_realloc(voidptr, size isize, align isize, offset isize) voidptr
fn C._aligned_msize(voidptr, align isize, offset isize) isize
fn C._aligned_recalloc(voidptr, num isize, size isize, align isize) voidptr

fn C.VirtualAlloc(voidptr, isize, u32, u32) voidptr
fn C.VirtualProtect(voidptr, isize, u32, &u32) bool

@[noreturn; trusted]
fn C.exit(code i32)

fn C.qsort(base voidptr, items usize, item_size usize, cb C.qsort_callback_func)

fn C.strlen(s &char) i32

@[trusted]
fn C.isdigit(c i32) bool

// stdio.h
fn C.popen(c &char, t &char) voidptr

// <libproc.h>
fn C.proc_pidpath(i32, voidptr, i32) i32

fn C.realpath(const_path &char, resolved_path &char) &char

// fn C.chmod(byteptr, mode_t) int
fn C.chmod(path &char, mode u32) i32

fn C.printf(const_format &char, opt ...voidptr) i32
fn C.dprintf(fd i32, const_format &char, opt ...voidptr) i32
fn C.fprintf(fstream &C.FILE, const_format &char, opt ...voidptr) i32
fn C.sprintf(str &char, const_format &char, opt ...voidptr) i32
fn C.snprintf(str &char, size usize, const_format &char, opt ...voidptr) i32
fn C.wprintf(const_format &u16, opt ...voidptr) i32

// used by Android for (e)println to output to the Android log system / logcat
pub fn C.android_print(fstream voidptr, format &char, opt ...voidptr)

fn C.sscanf(str &char, const_format &char, opt ...voidptr) i32
fn C.scanf(const_format &char, opt ...voidptr) i32

fn C.puts(msg &char) i32
@[trusted]
fn C.abs(f64) f64

fn C.fputs(msg &char, fstream &C.FILE) i32

fn C.fflush(fstream &C.FILE) i32

// TODO: define args in these functions
fn C.fseek(stream &C.FILE, offset i32, whence i32) i32

fn C.fopen(filename &char, mode &char) &C.FILE

fn C.fileno(&C.FILE) i32

fn C.fread(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fwrite(ptr voidptr, item_size usize, items usize, stream &C.FILE) usize

fn C.fclose(stream &C.FILE) i32

fn C.pclose(stream &C.FILE) i32

fn C.open(path &char, flags i32, mode ...int) i32
fn C.close(fd i32) i32

fn C.strrchr(s &char, c i32) &char
fn C.strchr(s &char, c i32) &char

// process execution, os.process:
@[trusted]
fn C.GetCurrentProcessId() u32
@[trusted]
fn C._getpid() i32
@[trusted]
fn C.getpid() i32

@[trusted]
fn C.GetCurrentThreadId() u32
@[trusted]
fn C.gettid() u32

@[trusted]
fn C.getuid() i32

@[trusted]
fn C.geteuid() i32

fn C.system(cmd &char) i32

fn C.posix_spawn(child_pid &int, path &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) i32

fn C.posix_spawnp(child_pid &int, exefile &char, file_actions voidptr, attrp voidptr, argv &&char, envp &&char) i32

fn C.execve(cmd_path &char, args voidptr, envs voidptr) i32

fn C.execvp(cmd_path &char, args &&char) i32

fn C._execve(cmd_path &char, args voidptr, envs voidptr) i32

fn C._execvp(cmd_path &char, args &&char) i32

fn C.strcmp(s1 &char, s2 &char) i32

@[trusted]
fn C.fork() i32

fn C.wait(status &int) i32

fn C.waitpid(pid i32, status &int, options i32) i32

@[trusted]
fn C.kill(pid i32, sig i32) i32

fn C.setenv(&char, &char, i32) i32

fn C.unsetenv(&char) i32

fn C.access(path &char, amode i32) i32

fn C.remove(filename &char) i32

fn C.rmdir(path &char) i32

fn C.chdir(path &char) i32

fn C.rewind(stream &C.FILE) i32

fn C.ftell(&C.FILE) isize

fn C.stat(&char, voidptr) i32

fn C.lstat(path &char, buf &C.stat) i32

fn C.statvfs(const_path &char, buf &C.statvfs) i32

fn C.rename(old_filename &char, new_filename &char) i32

fn C.fgets(str &char, n i32, stream &C.FILE) i32

fn C.fgetpos(&C.FILE, voidptr) i32

@[trusted]
fn C.sigemptyset() i32

fn C.getcwd(buf &char, size usize) &char

@[trusted]
fn C.mktime() i32

fn C.gettimeofday(tv &C.timeval, tz &C.timezone) i32

@[trusted]
fn C.sleep(seconds u32) u32

// fn C.usleep(usec useconds_t) int
@[trusted]
fn C.usleep(usec u32) i32

@[typedef]
pub struct C.DIR {
}

fn C.opendir(&char) &C.DIR

fn C.closedir(dirp &C.DIR) i32

// fn C.mkdir(path &char, mode mode_t) int
fn C.mkdir(path &char, mode u32) i32

// C.rand returns a pseudorandom integer from 0 (inclusive) to C.RAND_MAX (exclusive)
@[trusted]
fn C.rand() i32

// C.srand seeds the internal PRNG with the given value.
@[trusted]
fn C.srand(seed u32)

fn C.atof(str &char) f64

@[trusted]
fn C.tolower(c i32) i32

@[trusted]
fn C.toupper(c i32) i32

@[trusted]
fn C.isspace(c i32) i32

fn C.strchr(s &char, c i32) &char

@[trusted]
fn C.getchar() i32

@[trusted]
fn C.putchar(i32) i32

fn C.strdup(s &char) &char

fn C.strncasecmp(s &char, s2 &char, n i32) i32

fn C.strcasecmp(s &char, s2 &char) i32

fn C.strncmp(s &char, s2 &char, n i32) i32

@[trusted]
fn C.strerror(i32) &char

@[trusted]
fn C.WIFEXITED(status i32) bool

@[trusted]
fn C.WEXITSTATUS(status i32) i32

@[trusted]
fn C.WIFSIGNALED(status i32) bool

@[trusted]
fn C.WTERMSIG(status i32) i32

@[trusted]
fn C.isatty(fd i32) i32

fn C.syscall(number i32, va ...voidptr) i32

fn C.sysctl(name &int, namelen u32, oldp voidptr, oldlenp voidptr, newp voidptr, newlen usize) i32

@[trusted]
fn C._fileno(i32) i32

pub type C.intptr_t = voidptr

fn C._get_osfhandle(fd i32) C.intptr_t

fn C.GetModuleFileName(hModule voidptr, lpFilename &u16, nSize u32) u32

fn C.GetModuleFileNameW(hModule voidptr, lpFilename &u16, nSize u32) u32

fn C.CreateFile(lpFilename &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes &u16, dwCreationDisposition u32,
	dwFlagsAndAttributes u32, hTemplateFile voidptr) voidptr

fn C.CreateFileW(lpFilename &u16, dwDesiredAccess u32, dwShareMode u32, lpSecurityAttributes &u16, dwCreationDisposition u32,
	dwFlagsAndAttributes u32, hTemplateFile voidptr) voidptr

fn C.GetFinalPathNameByHandleW(hFile voidptr, lpFilePath &u16, nSize u32, dwFlags u32) u32

fn C.CreatePipe(hReadPipe &voidptr, hWritePipe &voidptr, lpPipeAttributes voidptr, nSize u32) bool

fn C.SetHandleInformation(hObject voidptr, dwMask u32, dw_flags u32) bool

fn C.ExpandEnvironmentStringsW(lpSrc &u16, lpDst &u16, nSize u32) u32

fn C.GetComputerNameW(&u16, &u32) bool

fn C.GetUserNameW(&u16, &u32) bool

@[trusted]
fn C.SendMessageTimeout() isize

fn C.SendMessageTimeoutW(hWnd voidptr, msg u32, wParam &u16, lParam &u32, fuFlags u32, uTimeout u32, lpdwResult &u64) isize

fn C.CreateProcessW(lpApplicationName &u16, lpCommandLine &u16, lpProcessAttributes voidptr, lpThreadAttributes voidptr,
	bInheritHandles bool, dwCreationFlags u32, lpEnvironment voidptr, lpCurrentDirectory &u16, lpStartupInfo voidptr,
	lpProcessInformation voidptr) bool

fn C.ReadFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToRead u32, lpNumberOfBytesRead &u32, lpOverlapped voidptr) bool

fn C.GetFileAttributesW(lpFileName &u8) u32

fn C.RegQueryValueEx(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) i32

fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) i32

fn C.RegOpenKeyEx(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) i32

fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) i32

fn C.RegSetValueEx(hKey voidptr, lpValueName &u16, dwType u32, lpData &u16, cbData u32) i32

fn C.RegSetValueExW(hKey voidptr, lpValueName &u16, reserved u32, dwType u32, const_lpData &u8, cbData u32) i32

fn C.RegCloseKey(hKey voidptr) i32

fn C.RemoveDirectory(lpPathName &u16) bool

fn C.RemoveDirectoryW(lpPathName &u16) bool

fn C.GetStdHandle(u32) voidptr

fn C.SetConsoleMode(voidptr, u32) bool

fn C.GetConsoleMode(voidptr, &u32) bool

@[trusted]
fn C.GetCurrentProcessId() u32

// fn C.setbuf()
fn C.setbuf(voidptr, &char)

fn C.SymCleanup(hProcess voidptr)

fn C.MultiByteToWideChar(codePage u32, dwFlags u32, lpMultiMyteStr &char, cbMultiByte i32, lpWideCharStr &u16,
	cchWideChar i32) i32

fn C.wcslen(str voidptr) usize

fn C.WideCharToMultiByte(codePage u32, dwFlags u32, lpWideCharStr &u16, cchWideChar i32, lpMultiByteStr &char,
	cbMultiByte i32, lpDefaultChar &char, lpUsedDefaultChar &int) i32

fn C._wstat(path &u16, buffer &C._stat) i32

fn C._wrename(oldname &u16, newname &u16) i32

fn C._wfopen(filename &u16, mode &u16) voidptr

fn C._wpopen(command &u16, mode &u16) voidptr

fn C._pclose(stream &C.FILE) i32

fn C._wsystem(command &u16) i32

fn C._wgetenv(varname &u16) voidptr

fn C._putenv(envstring &char) i32
fn C._wputenv(envstring &u16) i32

fn C._waccess(path &u16, mode i32) i32

fn C._wremove(path &u16) i32

fn C.ReadConsole(in_input_handle voidptr, out_buffer voidptr, in_chars_to_read u32, out_read_chars &u32,
	in_input_control voidptr) bool

fn C.WriteConsole() voidptr

fn C.WriteFile(hFile voidptr, lpBuffer voidptr, nNumberOfBytesToWrite u32, lpNumberOfBytesWritten &u32, lpOverlapped voidptr) bool

fn C._wchdir(dirname &u16) i32

fn C._wgetcwd(buffer &u16, maxlen i32) i32

fn C._fullpath() i32

fn C.GetFullPathName(voidptr, u32, voidptr, voidptr) u32

@[trusted]
fn C.GetCommandLine() voidptr

fn C.LocalFree(voidptr)

fn C.FindFirstFileW(lpFileName &u16, lpFindFileData voidptr) voidptr

fn C.FindFirstFile(lpFileName &u8, lpFindFileData voidptr) voidptr

fn C.FindNextFile(hFindFile voidptr, lpFindFileData voidptr) i32

fn C.FindClose(hFindFile voidptr)

// macro
fn C.MAKELANGID(lgid voidptr, srtid voidptr) i32

fn C.FormatMessageW(dwFlags u32, lpSource voidptr, dwMessageId u32, dwLanguageId u32, lpBuffer voidptr,
	nSize u32, arguments ...voidptr) u32

fn C.CloseHandle(voidptr) i32

fn C.GetExitCodeProcess(hProcess voidptr, lpExitCode &u32)

@[trusted]
fn C.GetTickCount() i64

@[trusted]
fn C.Sleep(dwMilliseconds u32)

fn C.WSAStartup(u16, &voidptr) i32

@[trusted]
fn C.WSAGetLastError() i32

fn C.closesocket(i32) i32

fn C.vschannel_init(&C.TlsContext)

fn C.request(&C.TlsContext, i32, &u16, &u8, u32, &&u8, fn (voidptr, isize) voidptr) i32

fn C.vschannel_cleanup(&C.TlsContext)

fn C.URLDownloadToFile(i32, &u16, &u16, i32, i32)

@[trusted]
fn C.GetLastError() u32

fn C.CreateDirectory(&u8, i32) bool

// win crypto
fn C.BCryptGenRandom(i32, voidptr, i32, i32) i32

// win synchronization
fn C.CreateMutex(i32, bool, &u8) voidptr

fn C.WaitForSingleObject(voidptr, i32) i32

fn C.ReleaseMutex(voidptr) bool

fn C.CreateEvent(i32, bool, bool, &u8) voidptr

fn C.SetEvent(voidptr) i32

fn C.CreateSemaphore(voidptr, i32, i32, voidptr) voidptr

fn C.ReleaseSemaphore(voidptr, i32, voidptr) voidptr

fn C.InitializeSRWLock(voidptr)

fn C.AcquireSRWLockShared(voidptr)

fn C.AcquireSRWLockExclusive(voidptr)

fn C.ReleaseSRWLockShared(voidptr)

fn C.ReleaseSRWLockExclusive(voidptr)

// pthread.h
fn C.pthread_self() usize
fn C.pthread_mutex_init(voidptr, voidptr) i32

fn C.pthread_mutex_lock(voidptr) i32

fn C.pthread_mutex_unlock(voidptr) i32

fn C.pthread_mutex_destroy(voidptr) i32

fn C.pthread_rwlockattr_init(voidptr) i32

fn C.pthread_rwlockattr_setkind_np(voidptr, i32) i32

fn C.pthread_rwlockattr_setpshared(voidptr, i32) i32

fn C.pthread_rwlock_init(voidptr, voidptr) i32

fn C.pthread_rwlock_rdlock(voidptr) i32

fn C.pthread_rwlock_wrlock(voidptr) i32

fn C.pthread_rwlock_unlock(voidptr) i32

fn C.pthread_condattr_init(voidptr) i32

fn C.pthread_condattr_setpshared(voidptr, i32) i32

fn C.pthread_condattr_destroy(voidptr) i32

fn C.pthread_cond_init(voidptr, voidptr) i32

fn C.pthread_cond_signal(voidptr) i32

fn C.pthread_cond_wait(voidptr, voidptr) i32

fn C.pthread_cond_timedwait(voidptr, voidptr, voidptr) i32

fn C.pthread_cond_destroy(voidptr) i32

fn C.sem_init(voidptr, i32, u32) i32

fn C.sem_post(voidptr) i32

fn C.sem_wait(voidptr) i32

fn C.sem_trywait(voidptr) i32

fn C.sem_timedwait(voidptr, voidptr) i32

fn C.sem_destroy(voidptr) i32

// MacOS semaphore functions
@[trusted]
fn C.dispatch_semaphore_create(i64) voidptr

fn C.dispatch_semaphore_signal(voidptr) i64

fn C.dispatch_semaphore_wait(voidptr, u64) i64

@[trusted]
fn C.dispatch_time(u64, i64) u64

fn C.dispatch_release(voidptr)

// file descriptor based reading/writing
fn C.read(fd i32, buf voidptr, count usize) i32

fn C.write(fd i32, buf voidptr, count usize) i32

fn C.close(fd i32) i32

// pipes
fn C.pipe(pipefds &int) i32

fn C.dup2(oldfd i32, newfd i32) i32

// used by gl, stbi, freetype
fn C.glTexImage2D()

// used by ios for println
fn C.WrappedNSLog(str &u8)

// absolute value
@[trusted]
fn C.abs(number i32) i32

fn C.GetDiskFreeSpaceExA(const_path &char, free_bytes_available_to_caller &u64, total_number_of_bytes &u64, total_number_of_free_bytes &u64) bool

fn C.GetNativeSystemInfo(voidptr)

fn C.sysconf(name i32) i32

// C.SYSTEM_INFO contains information about the current computer system. This includes the architecture and type of the processor, the number of processors in the system, the page size, and other such information.
@[typedef]
pub struct C.SYSTEM_INFO {
	// workaround: v doesn't support a truely C anon union/struct here
	// union {
	dwOemId u32
	// struct {
	wProcessorArchitecture u16
	wReserved              u16
	//	}
	//}
	dwPageSize                  u32
	lpMinimumApplicationAddress voidptr
	lpMaximumApplicationAddress voidptr
	dwActiveProcessorMask       u32
	dwNumberOfProcessors        u32
	dwProcessorType             u32
	dwAllocationGranularity     u32
	wProcessorLevel             u16
	wProcessorRevision          u16
}

fn C.GetSystemInfo(&C.SYSTEM_INFO)

@[typedef]
pub struct C.SRWLOCK {}

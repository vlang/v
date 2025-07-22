module builtin

// windows aligned memory functions
fn C._aligned_malloc(size isize, align isize) voidptr
fn C._aligned_free(voidptr)
fn C._aligned_realloc(voidptr, size isize, align isize) voidptr
fn C._aligned_offset_malloc(size isize, align isize, offset isize) voidptr
fn C._aligned_offset_realloc(voidptr, size isize, align isize, offset isize) voidptr
fn C._aligned_msize(voidptr, align isize, offset isize) isize
fn C._aligned_recalloc(voidptr, num isize, size isize, align isize) voidptr

@[trusted]
fn C._fileno(int) int

pub type C.intptr_t = voidptr

fn C._get_osfhandle(fd int) C.intptr_t

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

fn C.RegQueryValueEx(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) int

fn C.RegQueryValueExW(hKey voidptr, lpValueName &u16, lp_reserved &u32, lpType &u32, lpData &u8, lpcbData &u32) int

fn C.RegOpenKeyEx(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int

fn C.RegOpenKeyExW(hKey voidptr, lpSubKey &u16, ulOptions u32, samDesired u32, phkResult voidptr) int

fn C.RegSetValueEx(hKey voidptr, lpValueName &u16, dwType u32, lpData &u16, cbData u32) int

fn C.RegSetValueExW(hKey voidptr, lpValueName &u16, reserved u32, dwType u32, const_lpData &u8, cbData u32) int

fn C.RegCloseKey(hKey voidptr) int

fn C.RemoveDirectory(lpPathName &u16) bool

fn C.RemoveDirectoryW(lpPathName &u16) bool

fn C.GetStdHandle(u32) voidptr

fn C.SetConsoleMode(voidptr, u32) bool

fn C.GetConsoleMode(voidptr, &u32) bool

@[trusted]
fn C.GetCurrentProcessId() u32

fn C.SymCleanup(hProcess voidptr)

fn C.MultiByteToWideChar(codePage u32, dwFlags u32, lpMultiMyteStr &char, cbMultiByte int, lpWideCharStr &u16,
	cchWideChar int) int

fn C.WideCharToMultiByte(codePage u32, dwFlags u32, lpWideCharStr &u16, cchWideChar int, lpMultiByteStr &char,
	cbMultiByte int, lpDefaultChar &char, lpUsedDefaultChar &int) int

fn C._wstat(path &u16, buffer &C._stat) int

fn C._wrename(oldname &u16, newname &u16) int

fn C._wfopen(filename &u16, mode &u16) voidptr

fn C._wpopen(command &u16, mode &u16) voidptr

fn C._wsystem(command &u16) int

fn C._wgetenv(varname &u16) voidptr

fn C._wputenv(envstring &u16) int

fn C._waccess(path &u16, mode int) int

fn C._wremove(path &u16) int

fn C.ReadConsole(in_input_handle voidptr, out_buffer voidptr, in_chars_to_read u32, out_read_chars &u32,
	in_input_control voidptr) bool

fn C.WriteConsole() voidptr

fn C.WriteFile() voidptr

fn C._wgetcwd(buffer &u16, maxlen int) int

fn C._fullpath() int

fn C.GetFullPathName(voidptr, u32, voidptr, voidptr) u32

@[trusted]
fn C.GetCommandLine() voidptr

fn C.LocalFree(voidptr)

fn C.FindFirstFileW(lpFileName &u16, lpFindFileData voidptr) voidptr

fn C.FindFirstFile(lpFileName &u8, lpFindFileData voidptr) voidptr

fn C.FindNextFile(hFindFile voidptr, lpFindFileData voidptr) int

fn C.FindClose(hFindFile voidptr)

// macro
fn C.MAKELANGID(lgid voidptr, srtid voidptr) int

fn C.FormatMessageW(dwFlags u32, lpSource voidptr, dwMessageId u32, dwLanguageId u32, lpBuffer voidptr,
	nSize u32, arguments ...voidptr) u32

fn C.CloseHandle(voidptr) int

fn C.GetExitCodeProcess(hProcess voidptr, lpExitCode &u32)

@[trusted]
fn C.GetTickCount() i64

@[trusted]
fn C.Sleep(dwMilliseconds u32)

fn C.WSAStartup(u16, &voidptr) int

@[trusted]
fn C.WSAGetLastError() int

fn C.closesocket(int) int

fn C.vschannel_init(&C.TlsContext)

fn C.request(&C.TlsContext, int, &u16, &u8, u32, &&u8) int

fn C.vschannel_cleanup(&C.TlsContext)

fn C.URLDownloadToFile(int, &u16, &u16, int, int)

@[trusted]
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

fn C.GetDiskFreeSpaceExA(const_path &char, free_bytes_available_to_caller &u64, total_number_of_bytes &u64, total_number_of_free_bytes &u64) bool

// C.SYSTEM_INFO contains information about the current computer system. This includes the architecture and type of the processor, the number of processors in the system, the page size, and other such information.
@[typedef]
pub struct C.SYSTEM_INFO {
	dwNumberOfProcessors u32
	dwPageSize           u32
}

fn C.GetSystemInfo(&C.SYSTEM_INFO)

@[typedef]
pub struct C.SRWLOCK {}

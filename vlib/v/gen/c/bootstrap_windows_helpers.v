module c

// bootstrap_windows_runtime_helpers supplies the Windows runtime helpers that a
// relocatable compiler bootstrap cannot obtain from a compiled object.
//
// `cmd/v` and parts of vlib declare these as `fn C.` and ship their bodies as C
// sources. A normal build compiles those sources and links them (`makev.bat`'s
// V_STAGE_NATIVE passes them to its stage compile), so the V views carry a
// `#flag ... .o` rather than an include, and the definitions never reach a
// single-file snapshot. That snapshot links nothing external, so the bodies are
// emitted here instead.
//
// Each header is emitted under the scope macro its own `.c` file sets, which is
// empty: the prototypes cgen generates for these symbols are non-static, so a
// `static` definition would clash with them.
//
// The SDK includes each source opens with are dropped, because this path runs
// after `headerless_compiler_platform_helpers`, which declares the Win32
// surface the bodies need -- and including `<windows.h>` here contradicts it,
// the SDK spelling `DWORD` as `unsigned long` where the headerless preamble
// spells it `u32`. What the headerless surface does not yet carry is declared
// below, under the same `#if !defined(...)` guards it uses so a real SDK header
// still wins if one is ever included.
fn (mut g FlatGen) bootstrap_windows_runtime_helpers() {
	g.writeln('#ifdef _WIN32')
	g.writeln('#ifndef V_BOOTSTRAP_WIN_LONG')
	g.writeln('#define V_BOOTSTRAP_WIN_LONG')
	g.writeln('typedef long LONG;')
	g.writeln('typedef void* HMODULE;')
	g.writeln('typedef unsigned long long ULONGLONG;')
	g.writeln('typedef union LARGE_INTEGER { struct { DWORD LowPart; LONG HighPart; }; ULONGLONG QuadPart; } LARGE_INTEGER;')
	g.writeln('typedef struct _EXCEPTION_POINTERS { void* ExceptionRecord; void* ContextRecord; } EXCEPTION_POINTERS;')
	g.writeln('typedef LONG (WINAPI *LPTOP_LEVEL_EXCEPTION_FILTER)(EXCEPTION_POINTERS*);')
	g.writeln('typedef struct _BY_HANDLE_FILE_INFORMATION { DWORD dwFileAttributes; FILETIME ftCreationTime; FILETIME ftLastAccessTime; FILETIME ftLastWriteTime; DWORD dwVolumeSerialNumber; DWORD nFileSizeHigh; DWORD nFileSizeLow; DWORD nNumberOfLinks; DWORD nFileIndexHigh; DWORD nFileIndexLow; } BY_HANDLE_FILE_INFORMATION;')
	g.writeln('typedef BY_HANDLE_FILE_INFORMATION* LPBY_HANDLE_FILE_INFORMATION;')
	g.writeln('typedef void* LPVOID;')
	g.writeln('typedef void* PSID;')
	g.writeln('typedef void* PACL;')
	g.writeln('typedef void* PSECURITY_DESCRIPTOR;')
	g.writeln('typedef DWORD SECURITY_INFORMATION;')
	g.writeln('typedef int WELL_KNOWN_SID_TYPE;')
	g.writeln('typedef const unsigned short* LPCWSTR;')
	g.writeln('typedef unsigned short* LPWSTR;')
	g.writeln('typedef DWORD* PDWORD;')
	g.writeln('typedef DWORD* LPDWORD;')
	g.writeln('typedef HANDLE* PHANDLE;')
	g.writeln('typedef void* FARPROC;')
	g.writeln('#define STACK_SIZE_PARAM_IS_A_RESERVATION 0x00010000')
	g.writeln('#define WAIT_OBJECT_0 0')
	g.writeln('#define WAIT_TIMEOUT 258')
	g.writeln('#define WAIT_FAILED 0xffffffffU')
	g.writeln('#define FILE_TYPE_DISK 0x0001')
	g.writeln('#define ERROR_SUCCESS 0')
	g.writeln('#define ERROR_PROC_NOT_FOUND 127')
	g.writeln('#endif')
	// Function prototypes are not repeated here: cgen generates them from the
	// `fn C.` declarations, and a second prototype with a different spelling
	// clashes with them. Only the types and constants those headers assume, and
	// that the headerless surface does not yet carry, are declared above.
	g.writeln('#ifndef V_BOOTSTRAP_WIN_ACL')
	g.writeln('#define V_BOOTSTRAP_WIN_ACL')
	// What `toolcache_windows_helpers.h` assumes from `<aclapi.h>`/`<accctrl.h>`
	// when those headers are present. The header itself declares the two items
	// below as an enum when they are not; everything else here has no fallback,
	// so it is kept beside the helpers that use it rather than in a system
	// header the bootstrap never reads.
	g.writeln('typedef enum { TokenUser = 1 } TOKEN_INFORMATION_CLASS;')
	g.writeln('typedef struct _SID_AND_ATTRIBUTES { PSID Sid; DWORD Attributes; } SID_AND_ATTRIBUTES;')
	g.writeln('typedef struct _TOKEN_USER { SID_AND_ATTRIBUTES User; } TOKEN_USER;')
	g.writeln('typedef enum { AclSizeInformation = 0 } ACL_INFORMATION_CLASS;')
	g.writeln('typedef struct _ACL_SIZE_INFORMATION { DWORD AceCount; DWORD AceBytesInUse; DWORD AceBytesFree; } ACL_SIZE_INFORMATION;')
	g.writeln('typedef struct _ACE_HEADER { unsigned char AceType; unsigned char AceFlags; u16 AceSize; } ACE_HEADER;')
	g.writeln('typedef struct _ACCESS_ALLOWED_ACE { ACE_HEADER Header; DWORD Mask; DWORD SidStart; } ACCESS_ALLOWED_ACE;')
	g.writeln('#define ACCESS_ALLOWED_ACE_TYPE 0')
	g.writeln('#define ACCESS_ALLOWED_COMPOUND_ACE_TYPE 4')
	g.writeln('#define ACCESS_ALLOWED_OBJECT_ACE_TYPE 5')
	g.writeln('#define ACCESS_ALLOWED_CALLBACK_ACE_TYPE 6')
	g.writeln('#define ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE 7')
	g.writeln('#define WinCreatorOwnerSid 11')
	g.writeln('#define WinLocalSystemSid 22')
	g.writeln('#define WinBuiltinAdministratorsSid 26')
	g.writeln('#define TOKEN_QUERY 0x0008')
	g.writeln('#define OWNER_SECURITY_INFORMATION 0x00000001')
	g.writeln('#define DACL_SECURITY_INFORMATION 0x00000004')
	g.writeln('#define GENERIC_ALL 0x10000000')
	g.writeln('#define WRITE_DAC 0x00040000')
	g.writeln('#define WRITE_OWNER 0x00080000')
	g.writeln('#define DELETE 0x00010000')
	g.writeln('#define FILE_WRITE_DATA 0x0002')
	g.writeln('#define FILE_APPEND_DATA 0x0004')
	g.writeln('#define FILE_WRITE_EA 0x0010')
	g.writeln('#define FILE_DELETE_CHILD 0x0040')
	g.writeln('#define _O_RDONLY 0x0000')
	g.writeln('#define _O_WRONLY 0x0001')
	g.writeln('#endif')
	// These are provided by `<windows.h>` in a normal build and by the fallback
	// declarations in `toolcache_windows_helpers.h` when `aclapi.h` is missing,
	// so they appear here only as what the helpers below call and cgen does not
	// already declare. Their spellings follow the headerless preamble, not the
	// SDK, and a second identical prototype is legal C if one is ever added.
	g.writeln('#ifdef _WIN32')
	g.writeln('HMODULE WINAPI GetModuleHandleA(const char*);')
	g.writeln('HMODULE WINAPI GetModuleHandleW(const unsigned short*);')
	g.writeln('void* WINAPI GetProcAddress(HMODULE, const char*);')
	g.writeln('DWORD WINAPI GetTickCount(void);')
	g.writeln('ULONGLONG WINAPI GetTickCount64(void);')
	// cgen also declares these two, and its spelling wins here too.
	g.writeln('int WINAPI QueryPerformanceFrequency(u64*);')
	g.writeln('int WINAPI QueryPerformanceCounter(u64*);')
	g.writeln('DWORD WINAPI GetCurrentProcessId(void);')
	// cgen also declares this one, so its spelling is matched exactly.
	g.writeln('u32 WINAPI GetCurrentThreadId(void);')
	g.writeln('DWORD WINAPI GetThreadId(HANDLE);')
	g.writeln('HANDLE WINAPI GetProcessHeap(void);')
	g.writeln('void* WINAPI HeapAlloc(HANDLE, DWORD, size_t);')
	g.writeln('BOOL WINAPI HeapFree(HANDLE, DWORD, void*);')
	g.writeln('i32 WINAPI CloseHandle(void*);')
	g.writeln('void WINAPI LocalFree(void*);')
	g.writeln('void* WINAPI CreateFileW(u16* lpFilename, u32 dwDesiredAccess, u32 dwShareMode, void* lpSecurityAttributes, u32 dwCreationDisposition, u32 dwFlagsAndAttributes, void* hTemplateFile);')
	g.writeln('i32 WINAPI MoveFileExW(u16* existing_file_name, u16* new_file_name, u32 flags);')
	g.writeln('i32 WINAPI GetFileInformationByHandle(void* handle, void* information);')
	g.writeln('BOOL WINAPI ReadFile(void* hFile, void* lpBuffer, u32 nNumberOfBytesToRead, u32* lpNumberOfBytesRead, void* lpOverlapped);')
	g.writeln('void* WINAPI GetCurrentProcess(void);')
	g.writeln('void* WINAPI GetStdHandle(u32);')
	g.writeln('BOOL WINAPI OpenProcessToken(HANDLE, DWORD, HANDLE*);')
	g.writeln('BOOL WINAPI GetTokenInformation(HANDLE, TOKEN_INFORMATION_CLASS, void*, DWORD, DWORD*);')
	g.writeln('BOOL WINAPI GetAclInformation(PACL, void*, DWORD, ACL_INFORMATION_CLASS);')
	g.writeln('BOOL WINAPI GetAce(PACL, DWORD, void**);')
	g.writeln('void WINAPI DebugBreak(void);')
	g.writeln('LPTOP_LEVEL_EXCEPTION_FILTER WINAPI SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER);')
	g.writeln('DWORD WINAPI GetFileType(HANDLE);')
	g.writeln('#endif')
	// `v_builtin_windows_stream` hands out the standard streams. They are
	// `FILE*` in the CRT this snapshot links against, and only the pointer is
	// carried, so declaring them as opaque handles is enough to link.
	g.emit_bootstrap_windows_helper_sources()
	g.writeln('#endif')
	g.emit_bootstrap_windows_atomics()
}

// emit_bootstrap_windows_atomics writes the atomic helpers for a Windows
// snapshot. The shared atomic block skips Windows under TCC and MSVC on the
// assumption that the system header provides them, and the system-header path
// never runs for a relocatable bootstrap, so without this nothing defines them.
//
// tcc takes the sized `__atomic_*_N` libcalls from its own libtcc1.a (its
// generic `_n` form does not exist), MSVC the `Interlocked*` family, and
// everything else the generic builtins.
fn (mut g FlatGen) emit_bootstrap_windows_atomics() {
	g.writeln('#ifdef _WIN32')
	g.writeln('#ifdef __TINYC__')
	g.writeln('static inline byte atomic_fetch_add_byte(void* ptr, byte delta) { return (byte)__atomic_fetch_add_1((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_add_u16(void* ptr, u16 delta) { return (u16)__atomic_fetch_add_2((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_add_u32(void* ptr, u32 delta) { return (u32)__atomic_fetch_add_4((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_add_u64(void* ptr, u64 delta) { return (u64)__atomic_fetch_add_8((u64*)ptr, delta, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void* atomic_fetch_add_ptr(void* ptr, void* delta) { return (void*)(size_t)__atomic_fetch_add_4((u32*)ptr, (u32)(size_t)delta, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_fetch_add_ptr(void* ptr, void* delta) { return (void*)(size_t)__atomic_fetch_add_8((u64*)ptr, (u64)(size_t)delta, 5); }')
	g.writeln('#endif')
	g.writeln('static inline byte atomic_fetch_sub_byte(void* ptr, byte delta) { return (byte)__atomic_fetch_sub_1((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_sub_u16(void* ptr, u16 delta) { return (u16)__atomic_fetch_sub_2((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_sub_u32(void* ptr, u32 delta) { return (u32)__atomic_fetch_sub_4((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_sub_u64(void* ptr, u64 delta) { return (u64)__atomic_fetch_sub_8((u64*)ptr, delta, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void* atomic_fetch_sub_ptr(void* ptr, void* delta) { return (void*)(size_t)__atomic_fetch_sub_4((u32*)ptr, (u32)(size_t)delta, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_fetch_sub_ptr(void* ptr, void* delta) { return (void*)(size_t)__atomic_fetch_sub_8((u64*)ptr, (u64)(size_t)delta, 5); }')
	g.writeln('#endif')
	g.writeln('static inline byte atomic_load_byte(void* ptr) { return __atomic_load_1((byte*)ptr, 5); }')
	g.writeln('static inline u16 atomic_load_u16(void* ptr) { return __atomic_load_2((u16*)ptr, 5); }')
	g.writeln('static inline u32 atomic_load_u32(void* ptr) { return __atomic_load_4((u32*)ptr, 5); }')
	g.writeln('static inline u64 atomic_load_u64(void* ptr) { return __atomic_load_8((u64*)ptr, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return (void*)(size_t)__atomic_load_4((u32*)ptr, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return (void*)(size_t)__atomic_load_8((u64*)ptr, 5); }')
	g.writeln('#endif')
	g.writeln('static inline byte atomic_exchange_byte(void* ptr, byte val) { return __atomic_exchange_1((byte*)ptr, val, 5); }')
	g.writeln('static inline u16 atomic_exchange_u16(void* ptr, u16 val) { return __atomic_exchange_2((u16*)ptr, val, 5); }')
	g.writeln('static inline u32 atomic_exchange_u32(void* ptr, u32 val) { return __atomic_exchange_4((u32*)ptr, val, 5); }')
	g.writeln('static inline u64 atomic_exchange_u64(void* ptr, u64 val) { return __atomic_exchange_8((u64*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_1((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_2((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_4((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_8((u64*)ptr, val, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return (void*)(size_t)__atomic_exchange_4((u32*)ptr, (u32)(size_t)val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_4((u32*)ptr, (u32)(size_t)val, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return (void*)(size_t)__atomic_exchange_8((u64*)ptr, (u64)(size_t)val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_8((u64*)ptr, (u64)(size_t)val, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_strong_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_1((byte*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_2((u16*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_4((u32*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_8((u64*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_1((byte*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_2((u16*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_4((u32*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_8((u64*)ptr, expected, desired, 5, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_4((u32*)ptr, (u32*)expected, (u32)desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_4((u32*)ptr, (u32*)expected, (u32)desired, 5, 5); }')
	g.writeln('#else')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_8((u64*)ptr, (u64*)expected, (u64)desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_8((u64*)ptr, (u64*)expected, (u64)desired, 5, 5); }')
	g.writeln('#endif')
	g.writeln('static inline void cpu_relax(void) { }')
	g.writeln('#else')
	g.writeln('static inline byte atomic_fetch_add_byte(void* ptr, byte delta) { return __atomic_fetch_add((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_add_u16(void* ptr, u16 delta) { return __atomic_fetch_add((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_add_u32(void* ptr, u32 delta) { return __atomic_fetch_add((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_add_u64(void* ptr, u64 delta) { return __atomic_fetch_add((u64*)ptr, delta, 5); }')
	g.writeln('static inline void* atomic_fetch_add_ptr(void* ptr, void* delta) { return (void*)(uintptr_t)__atomic_fetch_add((uintptr_t*)ptr, (uintptr_t)delta, 5); }')
	g.writeln('static inline byte atomic_fetch_sub_byte(void* ptr, byte delta) { return __atomic_fetch_sub((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_sub_u16(void* ptr, u16 delta) { return __atomic_fetch_sub((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_sub_u32(void* ptr, u32 delta) { return __atomic_fetch_sub((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_sub_u64(void* ptr, u64 delta) { return __atomic_fetch_sub((u64*)ptr, delta, 5); }')
	g.writeln('static inline void* atomic_fetch_sub_ptr(void* ptr, void* delta) { return (void*)(uintptr_t)__atomic_fetch_sub((uintptr_t*)ptr, (uintptr_t)delta, 5); }')
	g.writeln('static inline byte atomic_load_byte(void* ptr) { return __atomic_load_n((byte*)ptr, 5); }')
	g.writeln('static inline u16 atomic_load_u16(void* ptr) { return __atomic_load_n((u16*)ptr, 5); }')
	g.writeln('static inline u32 atomic_load_u32(void* ptr) { return __atomic_load_n((u32*)ptr, 5); }')
	g.writeln('static inline u64 atomic_load_u64(void* ptr) { return __atomic_load_n((u64*)ptr, 5); }')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return __atomic_load_n((void**)ptr, 5); }')
	g.writeln('static inline byte atomic_exchange_byte(void* ptr, byte val) { return __atomic_exchange_n((byte*)ptr, val, 5); }')
	g.writeln('static inline u16 atomic_exchange_u16(void* ptr, u16 val) { return __atomic_exchange_n((u16*)ptr, val, 5); }')
	g.writeln('static inline u32 atomic_exchange_u32(void* ptr, u32 val) { return __atomic_exchange_n((u32*)ptr, val, 5); }')
	g.writeln('static inline u64 atomic_exchange_u64(void* ptr, u64 val) { return __atomic_exchange_n((u64*)ptr, val, 5); }')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return __atomic_exchange_n((void**)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_n((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_n((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_n((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_n((u64*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_n((void**)ptr, val, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_n((byte*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_n((u64*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_n((void**)ptr, (void**)expected, (void*)desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_n((byte*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_n((u64*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_n((void**)ptr, (void**)expected, (void*)desired, 1, 5, 5); }')
	g.writeln('static inline void cpu_relax(void) { __asm__ __volatile__("" ::: "memory"); }')
	g.writeln('#endif')
	g.writeln('#endif')
}

// emit_bootstrap_windows_helper_sources writes the bodies of the four Windows
// core runtime sources, with the SDK includes each carries dropped for the
// reason `bootstrap_windows_runtime_helpers` describes.
//
// `$embed_file` takes a literal path, so each header is read here rather than
// through a helper that takes a name.
fn (mut g FlatGen) emit_bootstrap_windows_helper_sources() {
	// `v_builtin_windows_stream` hands out the standard streams, which arrive
	// through `<stdio.h>` in a normal build. There is no SDK header on this
	// path, and the headerless preamble routes `stdin` through that function
	// itself, so it is built here from the console handles instead.
	g.writeln('int _open_osfhandle(intptr_t osfhandle, int flags);')
	g.writeln('void* _fdopen(int fd, const char* mode);')
	g.writeln('static void* v_bootstrap_stdio_stream(unsigned int index) {')
	g.writeln('	void* handle = GetStdHandle(index == 0 ? STD_INPUT_HANDLE : index == 1 ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE);')
	g.writeln('	int fd = _open_osfhandle((intptr_t)handle, index == 0 ? _O_RDONLY : _O_WRONLY);')
	g.writeln('	return _fdopen(fd, index == 0 ? "r" : "w");')
	g.writeln('}')
	g.write($embed_file('@VEXEROOT/vlib/builtin/core_windows.c').to_string()
		.replace('#include <windows.h>\n', '')
		.replace('#include <stdio.h>\n', '')
		.replace('(void *)stdin', 'v_bootstrap_stdio_stream(0)')
		.replace('(void *)stdout', 'v_bootstrap_stdio_stream(1)')
		.replace('(void *)stderr', 'v_bootstrap_stdio_stream(2)'))
	g.writeln('#define V_TIME_TICKS_SCOPE')
	g.write($embed_file('@VEXEROOT/vlib/time/ticks_windows.c').to_string()
		.replace('#include "ticks_windows.h"', $embed_file('@VEXEROOT/vlib/time/ticks_windows.h').to_string()
			.replace('#include <windows.h>\n', '')))
	g.writeln('#define V3_WINTHREAD_SCOPE')
	g.write($embed_file('@VEXEROOT/vlib/v/winthread_helper_windows.c').to_string()
		.replace('#include "winthread_helper.h"', $embed_file('@VEXEROOT/vlib/v/winthread_helper.h').to_string()
			.replace('#include <windows.h>\n', '')
			.replace('#include <stddef.h>\n', '')))
	g.writeln('#define V_TOOLCACHE_WINDOWS_SCOPE')
	// The header below brings its own `aclapi.h` detection: when the toolchain
	// ships that header it includes it, and that drags the whole SDK back in
	// behind the headerless preamble. Strip both the include and the detection
	// that would re-enable it, so the header's own fallback declarations stand.
	toolcache_header := $embed_file('@VEXEROOT/cmd/v/toolcache_windows_helpers.h').to_string()
		.replace('#include <windows.h>\n', '')
		.replace('#include <aclapi.h>', '')
		.replace('#define V_TOOLCACHE_HAS_ACLAPI_H 1', '/* bootstrap: keep the fallback declarations */')
	g.write($embed_file('@VEXEROOT/cmd/v/toolcache_windows_helpers.c').to_string()
		.replace('#include "toolcache_windows_helpers.h"', toolcache_header))
}

// TCC's bundled <windows.h> does not include <fileapi.h>, and TCC's own
// <fileapi.h> cannot be used either because it pulls in <apiset.h>, a header
// TCC does not ship. As a result GetFinalPathNameByHandleW has no declaration
// in scope when TCC compiles code that uses it (e.g. os.real_path).
//
// Provide the declaration for TCC only. GCC/MSVC get it from the Windows SDK
// (via <windows.h> -> <fileapi.h>); emitting a V-side extern there would
// conflict with the SDK's `DWORD WINAPI` signature, since DWORD is
// `unsigned long`, not `unsigned int`.
#ifdef __TINYC__
#ifndef GetFinalPathNameByHandleW
extern unsigned int GetFinalPathNameByHandleW(void *hFile, unsigned short *lpFilePath,
	unsigned int nSize, unsigned int dwFlags);
#endif
#endif

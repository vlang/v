// Stack overflow handling must run before stack unwinding and needs reserved stack
// space on every thread. Other first-chance exceptions continue to their handlers.
#ifndef V_SEGFAULT_HANDLER_WINDOWS_H
#define V_SEGFAULT_HANDLER_WINDOWS_H

#include <windows.h>

#if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__) || defined(__SANITIZE_HWADDRESS__)
#define V_SEGFAULT_HANDLER_SANITIZED 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer) || __has_feature(thread_sanitizer) || __has_feature(memory_sanitizer) || __has_feature(hwaddress_sanitizer)
#define V_SEGFAULT_HANDLER_SANITIZED 1
#endif
#endif

typedef BOOL (WINAPI *v_windows_stack_guarantee_fn)(PULONG);

static inline void v_windows_set_stack_guarantee(void) {
	// Bundled TCC's kernel32 import library omits this API even though its Windows
	// headers declare it. Resolve the system export without changing that library.
	v_windows_stack_guarantee_fn guarantee = (v_windows_stack_guarantee_fn)GetProcAddress(
		GetModuleHandleW(L"kernel32.dll"), "SetThreadStackGuarantee");
	if (guarantee) {
		ULONG size = 64 * 1024;
		guarantee(&size);
	}
}

#if !defined(V_SEGFAULT_HANDLER_SANITIZED)
static LONG CALLBACK v_windows_stack_overflow_handler(EXCEPTION_POINTERS* exception) {
	if (exception->ExceptionRecord->ExceptionCode != EXCEPTION_STACK_OVERFLOW) {
		return EXCEPTION_CONTINUE_SEARCH;
	}
	// Write directly to stderr without allocation, CRT buffering, or a backtrace.
	static const char message[] = "V panic: stack overflow\n";
	DWORD written;
	WriteFile(GetStdHandle(STD_ERROR_HANDLE), message, sizeof(message) - 1, &written, NULL);
	// The stack is exhausted: do not run CRT or DLL cleanup on this thread.
	TerminateProcess(GetCurrentProcess(), 1);
	return EXCEPTION_CONTINUE_SEARCH;
}
#endif

static void v_install_windows_stack_overflow_handler(void) {
	v_windows_set_stack_guarantee();
#if !defined(V_SEGFAULT_HANDLER_SANITIZED)
	// Sanitizers report stack overflows themselves; do not intercept their faults.
	AddVectoredExceptionHandler(1, v_windows_stack_overflow_handler);
#endif
}

#endif

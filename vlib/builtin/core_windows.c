#include <windows.h>
#include <stdio.h>

static int (*v_builtin_windows_exception_callback)(void *);

/* V3's callback adapter has the ordinary C calling convention. The SDK calls
 * this real WINAPI thunk, which preserves the x86 stack on 32-bit Windows. */
static LONG WINAPI v_builtin_windows_exception_filter(EXCEPTION_POINTERS *exception) {
    return v_builtin_windows_exception_callback((void *)exception);
}

void *v_set_unhandled_exception_filter(int (*handler)(void *)) {
    v_builtin_windows_exception_callback = handler;
    return (void *)SetUnhandledExceptionFilter(
        handler == NULL ? NULL : v_builtin_windows_exception_filter);
}

void *v_builtin_windows_stream(unsigned int index) {
    return index == 0 ? (void *)stdin : index == 1 ? (void *)stdout : (void *)stderr;
}

void v_builtin_windows_debugbreak(void) {
    DebugBreak();
}

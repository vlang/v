#include <execinfo.h>
#include <stdint.h>
#include <unwind.h>

extern int32_t v_trace_frame(uintptr_t, uintptr_t);
static _Unwind_Reason_Code frame(struct _Unwind_Context *ctx, void *state) {
    return v_trace_frame((uintptr_t)state, _Unwind_GetIP(ctx)) ? _URC_END_OF_STACK : _URC_NO_REASON;
}
int backtrace(void **buffer, int size) {
    /* State is shared with BareTraceState. The first callback is this function. */
    struct { uintptr_t buffer; int32_t capacity, count; } state = {(uintptr_t)buffer, size, -1};
    if (size > 0) _Unwind_Backtrace(frame, &state);
    return state.count < 0 ? 0 : state.count;
}
extern uintptr_t v_trace_symbols(uintptr_t, int32_t);
extern void v_trace_symbols_fd(uintptr_t, int32_t, int32_t);
char **backtrace_symbols(void *const *buffer, int size) {
    return (char **)v_trace_symbols((uintptr_t)buffer, size);
}
void backtrace_symbols_fd(void *const *buffer, int size, int fd) {
    v_trace_symbols_fd((uintptr_t)buffer, size, fd);
}

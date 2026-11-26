#include "libc_compat.h"
#include <stdarg.h>
#include <stdio.h>
#include <syslog.h>

/* Native varargs only. Logging policy, locking, transport and ownership are V. */
extern void v_freestanding_syslog(int32_t, uintptr_t, uintptr_t);
extern void v_freestanding_openlog(uintptr_t, int32_t, int32_t);
extern void v_freestanding_closelog(void);
extern int32_t v_freestanding_setlogmask(int32_t);

int v_freestanding_log_format(uintptr_t output, uintptr_t format, uintptr_t arguments) {
    return vasprintf((char **)output, (const char *)format, *(va_list *)arguments);
}
void vsyslog(int priority, const char *format, va_list arguments) {
    va_list copy;
    va_copy(copy, arguments);
    v_freestanding_syslog(priority, (uintptr_t)format, (uintptr_t)&copy);
    va_end(copy);
}
void syslog(int priority, const char *format, ...) {
    va_list arguments;
    va_start(arguments, format);
    vsyslog(priority, format, arguments);
    va_end(arguments);
}
void openlog(const char *ident, int options, int facility) {
    v_freestanding_openlog((uintptr_t)ident, options, facility);
}
void closelog(void) { v_freestanding_closelog(); }
int setlogmask(int mask) { return v_freestanding_setlogmask(mask); }

#include <sys/socket.h>
int32_t v_freestanding_log_connect(int32_t fd,uintptr_t address,uint32_t length) { return connect(fd,(const struct sockaddr *)address,(socklen_t)length); }

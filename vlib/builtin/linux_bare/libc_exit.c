#include "libc_compat.h"
#include <stdlib.h>
#include <unistd.h>
/* Standalone executables omit crtbegin. A supplied CRT's strong definition
 * takes precedence; hidden visibility keeps each image's identity private. */
__attribute__((weak, visibility("hidden"))) void *__dso_handle = &__dso_handle;

struct v_exit_callback {
    struct v_exit_callback *next;
    union {
        void (*plain)(void);
        void (*argument)(void *);
    } call;
    void *argument, *dso;
    int with_argument;
};
static int v_exit_lock;
static struct v_exit_callback *v_exit_callbacks;
static int v_register_exit(void (*plain)(void), void (*argument)(void *), void *data, void *dso) {
    struct v_exit_callback *entry = malloc(sizeof *entry);
    if (!entry) {
        errno = ENOMEM;
        return -1;
    }
    entry->argument = data;
    entry->dso = dso;
    entry->with_argument = argument != NULL;
    if (argument)
        entry->call.argument = argument;
    else
        entry->call.plain = plain;
    v_linux_lock(&v_exit_lock);
    entry->next = v_exit_callbacks;
    v_exit_callbacks = entry;
    v_linux_unlock(&v_exit_lock);
    return 0;
}
int atexit(void (*callback)(void)) { return v_register_exit(callback, NULL, NULL, NULL); }
int __cxa_atexit(void (*callback)(void *), void *argument, void *dso) {
    return v_register_exit(NULL, callback, argument, dso);
}
void __cxa_finalize(void *dso) {
    for (;;) {
        v_linux_lock(&v_exit_lock);
        struct v_exit_callback **position = &v_exit_callbacks;
        while (*position && dso && (*position)->dso != dso)
            position = &(*position)->next;
        struct v_exit_callback *entry = *position;
        if (!entry) {
            v_linux_unlock(&v_exit_lock);
            return;
        }
        *position = entry->next;
        v_linux_unlock(&v_exit_lock);
        struct v_exit_callback call = *entry;
        free(entry);
        if (call.with_argument)
            call.call.argument(call.argument);
        else
            call.call.plain();
    }
}
void v_freestanding_exit_callbacks(void) { __cxa_finalize(NULL); }

_Noreturn void _Exit(int status) {
    V_RAW1(SYS_exit_group, status);
    __builtin_unreachable();
}
_Noreturn void _exit(int status) { _Exit(status); }

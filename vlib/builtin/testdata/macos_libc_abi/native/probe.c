/* Native SDK consumer; runtime policy remains entirely in the production V. */
#include <assert.h>
#include <crt_externs.h>
#include <errno.h>
#include <limits.h>
#include <mach/vm_statistics.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include "../../../macos_bare/kernel_services.h"

static ssize_t (*volatile read_api)(int, void *, size_t) = read;
static ssize_t (*volatile write_api)(int, const void *, size_t) = write;
static int (*volatile isatty_api)(int) = isatty;
static void *(*volatile find_api)(const void *, int, size_t) = memchr;
/* Volatile calls cannot be folded into traps or builtins by Clang. */
static void (*volatile abort_api)(void) = abort;
static void (*volatile assert_api)(const char *, const char *, int, const char *) = __assert_rtn;
static _Thread_local int handler_cookie;
static _Atomic unsigned abort_callers;

static void mark(char byte) { (void)write_api(STDOUT_FILENO, &byte, 1); }
static void normal_exit_callback(void) { mark('A'); }

static void receive_abort(int number) {
    if (number != SIGABRT || handler_cookie != 123 || errno != 94) _Exit(91);
    mark('H');
}

int v_macos_libc_native_mode(void) {
    return *_NSGetArgc() > 1 ? (unsigned char)(*_NSGetArgv())[1][0] : '0';
}

/* This runs before _vinit, so the assertion export may not initialize V or
 * allocate. Startup has established the native ABI, including GS, first. */
__attribute__((constructor)) static void early_assert(void) {
    if (v_macos_libc_native_mode() == 'e')
        assert_api(NULL, "constructor.c", 17, "early assertion");
}

static int check_memory(void) {
    unsigned char bytes[] = {1, 0, 0x80, 0xff, 2};
    if (find_api(NULL, 0, 0) != NULL || find_api(bytes, 0, sizeof bytes) != bytes + 1 ||
        find_api(bytes, -1, sizeof bytes) != bytes + 3 ||
        find_api(bytes, 0x180, sizeof bytes) != bytes + 2 ||
        find_api(bytes, 3, sizeof bytes) != NULL) return 20;
    uint64_t page = 0, address = 0, result = 0;
    if (v_macos_kernel_page_size(&page) || !page ||
        v_macos_kernel_vm_allocate(&address, page * 2, VM_FLAGS_ANYWHERE)) return 21;
    /* mprotect's BSD service is a fixture operation, not a production shim. */
    if (v_macos_kernel_bsd_call6(74, address + page, page, PROT_NONE, 0, 0, 0, &result)) return 22;
    unsigned char *boundary = (void *)(uintptr_t)(address + page - 1);
    *boundary = 0xff;
    if (find_api(boundary, 255, SIZE_MAX) != boundary || find_api(boundary, 1, 1) != NULL) return 23;
    return v_macos_kernel_vm_deallocate(address, page * 2) ? 24 : 0;
}

int v_macos_libc_native_probe(int mode) {
    if (mode != '0') {
        handler_cookie = 123;
        if (atexit(normal_exit_callback)) _Exit(92);
        if (signal(SIGABRT, mode == 'i' ? SIG_IGN : mode == 'h' || mode == 'c' ? receive_abort : SIG_DFL) == SIG_ERR) _Exit(93);
        /* abort must unblock its calling thread even when inherited blocked. */
        uint32_t mask = 1U << (SIGABRT - 1);
        if (v_macos_kernel_bsd_call6(329, SIG_BLOCK, (uintptr_t)&mask, 0, 0, 0, 0, NULL)) _Exit(94);
        errno = 94;
        if (mode == 'c') {
            atomic_fetch_add_explicit(&abort_callers, 1, memory_order_release);
            while (atomic_load_explicit(&abort_callers, memory_order_acquire) != 2)
                __asm__ volatile("pause" ::: "memory");
        }
        if (mode == 'n') assert_api(NULL, "native.c", 123, "value != 0");
        else if (mode == 'l') assert_api((const char *)(uintptr_t)UINTPTR_MAX, "native.c", INT_MIN, "value != 0");
        else if (mode == 'f') assert_api("native_function", "native.c", 0, "value != 0");
        else abort_api();
        mark('R');
        _Exit(95);
    }
    char bytes[8] = {0};
    errno = 77;
    if (read_api(-1, bytes, 8) != -1 || errno != EBADF) return 1;
    errno = 77;
    if (read_api(STDIN_FILENO, bytes, 8) != 3 || errno != 77 || memcmp(bytes, "xyz", 3)) return 2;
    if (write_api(STDOUT_FILENO, bytes, 3) != 3 || errno != 77) return 3;
    if (write_api(-1, bytes, 3) != -1 || errno != EBADF) return 4;
    errno = 78;
    if (isatty_api(-1) != 0 || errno != EBADF) return 5;
    if (isatty_api(STDIN_FILENO) != 0 || errno != ENOTTY) return 6;
    errno = 79;
    if (isatty_api(3) != 1 || errno != 79) return 7;
    return check_memory();
}

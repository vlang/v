/* Checks the __builtin_* shims that tcc-termux-stdlimits.diff adds to
   include/tccdefs.h. check_tcc_termux_builtins.sh extracts the shims from the
   patch into shim.h and compiles this file with the chosen C compiler. */
#include <stddef.h>
#include <stdio.h>
#include "shim.h"

static int fails;

#define CHECK(c)                                                    \
    do {                                                            \
        if (!(c)) {                                                 \
            fprintf(stderr, "FAIL line %d: %s\n", __LINE__, #c);    \
            fails++;                                                \
        }                                                           \
    } while (0)

static int calls;
static unsigned short u16_arg(void) { calls++; return 0x1122; }
static unsigned u32_arg(void) { calls++; return 0x11223344u; }
static unsigned long long u64_arg(void) { calls++; return 0x1122334455667788ULL; }

static char buf[16];
static int pcalls;
static char *side_ptr(void) { pcalls++; return buf; }
static char *unknown_ptr(void) { return (char *)(size_t)0x1000; }

int main(void) {
    /* Byte swaps: correct value, argument evaluated exactly once. */
    calls = 0;
    CHECK(__builtin_bswap16(u16_arg()) == 0x2211);
    CHECK(calls == 1);
    calls = 0;
    CHECK(__builtin_bswap32(u32_arg()) == 0x44332211u);
    CHECK(calls == 1);
    calls = 0;
    CHECK(__builtin_bswap64(u64_arg()) == 0x8877665544332211ULL);
    CHECK(calls == 1);

    /* Object size of an unknown object: modes 0/1 give (size_t)-1, modes 2/3 give 0. */
    CHECK(__builtin_object_size(unknown_ptr(), 0) == (size_t)-1);
    CHECK(__builtin_object_size(unknown_ptr(), 1) == (size_t)-1);
    CHECK(__builtin_object_size(unknown_ptr(), 2) == 0);
    CHECK(__builtin_object_size(unknown_ptr(), 3) == 0);

    /* The pointer argument is not evaluated. */
    pcalls = 0;
    (void)__builtin_object_size(side_ptr(), 0);
    CHECK(pcalls == 0);

    if (fails == 0) {
        puts("tcc_termux_builtins_test: ok");
        return 0;
    }
    fprintf(stderr, "tcc_termux_builtins_test: %d check(s) failed\n", fails);
    return 1;
}

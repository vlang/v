// This oracle executes the compiler-generated V byte operations and C ABI exports
// on Linux. It deliberately does not emulate Darwin startup or system calls.
#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <sys/mman.h>
#include <unistd.h>

#include "generated_memory.c"

static void *(*volatile copy_bytes)(void *, const void *, size_t) = memcpy;
static void *(*volatile move_bytes)(void *, const void *, size_t) = memmove;
static void *(*volatile fill_bytes)(void *, int, size_t) = memset;
static int (*volatile compare_bytes)(const void *, const void *, size_t) = memcmp;
static size_t (*volatile text_length)(const char *) = strlen;
static void (*volatile zero_bytes)(void *, size_t) = bzero;
static void (*volatile internal_zero_bytes)(void *, size_t) = __bzero;
static int32_t *(*volatile errno_pointer)(void) = __error;

static unsigned char pattern(size_t i) { return (unsigned char)(i * 73 + 19); }

static void memory_cases(void) {
    unsigned char source[1024], actual[1024], expected[1024];
    for (size_t i = 0; i < sizeof source; ++i) source[i] = pattern(i);
    for (size_t length = 0; length <= 512; ++length) {
        for (size_t alignment = 0; alignment < 16; ++alignment) {
            for (size_t i = 0; i < sizeof actual; ++i) actual[i] = expected[i] = 0xa5;
            assert(copy_bytes(actual + alignment, source + 15 - alignment, length) == actual + alignment);
            for (size_t i = 0; i < length; ++i) expected[alignment + i] = source[15 - alignment + i];
            for (size_t i = 0; i < sizeof actual; ++i) assert(actual[i] == expected[i]);
            assert(compare_bytes(actual + alignment, source + 15 - alignment, length) == 0);
            assert(fill_bytes(actual + alignment, 0x1e3, length) == actual + alignment);
            for (size_t i = 0; i < length; ++i) expected[alignment + i] = 0xe3;
            for (size_t i = 0; i < sizeof actual; ++i) assert(actual[i] == expected[i]);
            zero_bytes(actual + alignment, length);
            for (size_t i = 0; i < length; ++i) assert(actual[alignment + i] == 0);
            fill_bytes(actual + alignment, 0xff, length);
            internal_zero_bytes(actual + alignment, length);
            for (size_t i = 0; i < length; ++i) assert(actual[alignment + i] == 0);
        }
    }
    for (size_t length = 0; length <= 256; ++length) {
        for (size_t from = 0; from < 32; ++from) {
            for (size_t to = 0; to < 32; ++to) {
                for (size_t i = 0; i < sizeof actual; ++i) actual[i] = expected[i] = pattern(i);
                for (size_t i = 0; i < length; ++i) expected[to + i] = source[from + i];
                assert(move_bytes(actual + to, actual + from, length) == actual + to);
                for (size_t i = 0; i < sizeof actual; ++i) assert(actual[i] == expected[i]);
            }
        }
    }
    const unsigned char low[] = {0, 0x7f, 0x80, 0xff};
    for (size_t a = 0; a < sizeof low; ++a) {
        for (size_t b = 0; b < sizeof low; ++b) {
            int value = compare_bytes(low + a, low + b, 1);
            assert((value > 0) - (value < 0) == (low[a] > low[b]) - (low[a] < low[b]));
        }
    }
    static const char readonly_text[] = "read-only source";
    assert(text_length(readonly_text) == sizeof readonly_text - 1);
    assert(text_length("") == 0);
}

static void guard_cases(void) {
    long page = sysconf(_SC_PAGESIZE);
    assert(page > 0);
    size_t size = (size_t)page;
    unsigned char *source = mmap(0, 3 * size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    unsigned char *target = mmap(0, 3 * size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    assert(source != MAP_FAILED && target != MAP_FAILED);
    assert(mprotect(source + size, size, PROT_READ | PROT_WRITE) == 0);
    assert(mprotect(target + size, size, PROT_READ | PROT_WRITE) == 0);
    for (size_t i = 0; i < size; ++i) source[size + i] = pattern(i);
    source[2 * size - 1] = 0;
    assert(mprotect(source + size, size, PROT_READ) == 0);
    for (size_t n = 1; n <= 257; ++n) {
        unsigned char *end_source = source + 2 * size - n;
        unsigned char *end_target = target + 2 * size - n;
        assert(copy_bytes(end_target, end_source, n) == end_target);
        assert(compare_bytes(end_target, end_source, n) == 0);
        assert(copy_bytes(target + size, source + size, n) == target + size);
        assert(compare_bytes(target + size, source + size, n) == 0);
        assert(move_bytes(end_target, end_target, n) == end_target);
        zero_bytes(end_target, n);
        internal_zero_bytes(target + size, n);
    }
    // The final byte is a NUL; the next page must never be read by strlen.
    assert(text_length((const char *)source + 2 * size - 1) == 0);
    assert(text_length((const char *)source + 2 * size - 2) <= 1);
    assert(munmap(source, 3 * size) == 0);
    assert(munmap(target, 3 * size) == 0);
}

static pthread_barrier_t errno_barrier;
static void *thread_errno(void *argument) {
    intptr_t id = (intptr_t)argument;
    int32_t *native = errno_pointer();
    assert(*native == 0 && bf_last_errno() == 0);
    *native = (int32_t)(100 + id);
    bf_last_errno_cell = 200 + id;
    int status = pthread_barrier_wait(&errno_barrier);
    assert(status == 0 || status == PTHREAD_BARRIER_SERIAL_THREAD);
    assert(native == errno_pointer());
    assert(*native == 100 + id && bf_last_errno() == 200 + id);
    return 0;
}

static void errno_cases(void) {
    *errno_pointer() = 71;
    bf_last_errno_cell = 72;
    assert((void *)errno_pointer() != (void *)&bf_last_errno_cell);
    assert(sizeof *errno_pointer() == 4);
    assert(pthread_barrier_init(&errno_barrier, 0, 5) == 0);
    pthread_t workers[4];
    for (intptr_t i = 0; i < 4; ++i) assert(pthread_create(&workers[i], 0, thread_errno, (void *)i) == 0);
    int status = pthread_barrier_wait(&errno_barrier);
    assert(status == 0 || status == PTHREAD_BARRIER_SERIAL_THREAD);
    assert(*errno_pointer() == 71 && bf_last_errno() == 72);
    for (size_t i = 0; i < 4; ++i) assert(pthread_join(workers[i], 0) == 0);
    assert(pthread_barrier_destroy(&errno_barrier) == 0);
}

int main(void) {
    memory_cases();
    guard_cases();
    errno_cases();
    puts("generated V macOS memory C ABI and errno passed");
    return 0;
}

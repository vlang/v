#ifndef VGC_SCAN_H
#define VGC_SCAN_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#if defined(_MSC_VER)
#include <intrin.h>
#endif

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define VGC_SCAN_ASAN 1
#endif
#if __has_feature(memory_sanitizer)
#define VGC_SCAN_MSAN 1
#endif
#endif
#if defined(__SANITIZE_ADDRESS__)
#define VGC_SCAN_ASAN 1
#endif
#if defined(__SANITIZE_MEMORY__)
#define VGC_SCAN_MSAN 1
#endif

// Declare the public sanitizer interfaces here: not every compiler installation
// provides their headers, even when it supports the instrumentation.
#ifdef __cplusplus
extern "C" {
#endif
#ifdef VGC_SCAN_ASAN
extern void* __asan_region_is_poisoned(void* begin, size_t size);
extern void* __asan_get_current_fake_stack(void);
extern void* __asan_addr_is_in_fake_stack(void* fake_stack, void* address, void** begin, void** end);
#endif
#ifdef VGC_SCAN_MSAN
extern intptr_t __msan_test_shadow(const volatile void* begin, size_t size);
#endif
#ifdef __cplusplus
}
#endif

// Conservative scans include padding and unused stack/object slots. Such bytes
// cannot hold an initialized live pointer. Skip them without changing sanitizer
// shadow state or disabling instrumentation of the collector or program.
static inline uintptr_t vgc_read_candidate(const void* address) {
#ifdef VGC_SCAN_ASAN
    if (__asan_region_is_poisoned((void*)address, sizeof(uintptr_t)) != NULL) return 0;
#endif
#ifdef VGC_SCAN_MSAN
    if (__msan_test_shadow(address, sizeof(uintptr_t)) != -1) return 0;
#endif
    uintptr_t candidate;
    memcpy(&candidate, address, sizeof(candidate));
    return candidate;
}

static inline unsigned vgc_ctz64(uint64_t x) {
#if defined(_MSC_VER) && defined(_M_X64)
    unsigned long index = 0;
    _BitScanForward64(&index, x);
    return (unsigned)index;
#elif defined(__GNUC__) || defined(__clang__)
    return (unsigned)__builtin_ctzll(x);
#else
    unsigned n = 0;
    while ((x & 1u) == 0) {
        x >>= 1;
        ++n;
    }
    return n;
#endif
}

static inline int vgc_bitmap_atomic_test_set(uint8_t* bits, uint32_t index) {
    // The mark bitmaps live in span metadata, whose `bits` array is 8-byte
    // aligned, so 32-bit words are safe to compare-and-swap on all targets.
    // Bit `index` lives in byte `index>>3`, bit `index&7`, so on big-endian
    // targets the word mask must mirror the byte order (byte 0 is the most
    // significant) instead of using the little-endian `index&31` position.
    uint32_t* word = &((uint32_t*)bits)[index >> 5];
#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
    const uint32_t mask = (uint32_t)1u << ((3u - ((index >> 3) & 3u)) * 8u + (index & 7u));
#elif defined(__BIG_ENDIAN__) || defined(_BIG_ENDIAN) || defined(__ARMEB__) || defined(__MIPSEB__) || defined(__s390x__) || defined(__zarch__)
    const uint32_t mask = (uint32_t)1u << ((3u - ((index >> 3) & 3u)) * 8u + (index & 7u));
#else
    const uint32_t mask = 1u << (index & 31u);
#endif
#if defined(__TINYC__)
    extern _Bool __atomic_compare_exchange_4(uint32_t*, uint32_t*, uint32_t, int, int, int);
    uint32_t expected = *word;
    for (;;) {
        if (expected & mask) return 1;
        if (__atomic_compare_exchange_4(word, &expected, expected | mask, 0, 5, 5)) return 0;
    }
#elif defined(_MSC_VER)
    volatile long* target = (volatile long*)word;
    long expected = (long)*target;
    for (;;) {
        if (expected & (long)mask) return 1;
        long current = _InterlockedCompareExchange(target, expected | (long)mask, expected);
        if (current == expected) return 0;
        expected = current;
    }
#else
    return (__atomic_fetch_or(word, mask, __ATOMIC_ACQ_REL) & mask) != 0 ? 1 : 0;
#endif
}

static inline void* vgc_current_fake_stack(void) {
#ifdef VGC_SCAN_ASAN
    return __asan_get_current_fake_stack();
#else
    return NULL;
#endif
}

// ASan can relocate the register-spill buffer, just like a program local. Its
// fake address must never become a bound of the physical stack scan.
static inline uintptr_t vgc_real_stack_address(void* fake_stack, uintptr_t address) {
#ifdef VGC_SCAN_ASAN
    void* real = __asan_addr_is_in_fake_stack(fake_stack, (void*)address, NULL, NULL);
    if (real != NULL) return (uintptr_t)real;
#else
    (void)fake_stack;
#endif
    return address;
}

// The ASan GC interface accepts another live thread's handle. VGC holds its
// cache lock while marking, so the owner cannot exit during this query.
static inline uintptr_t vgc_fake_frame_range(void* fake_stack, uintptr_t address,
                                           uintptr_t* begin, uintptr_t* end) {
#ifdef VGC_SCAN_ASAN
    void* frame_begin = NULL;
    void* frame_end = NULL;
    void* real = __asan_addr_is_in_fake_stack(fake_stack, (void*)address, &frame_begin, &frame_end);
    if (real != NULL) {
        *begin = (uintptr_t)frame_begin;
        *end = (uintptr_t)frame_end;
    }
    return (uintptr_t)real;
#else
    (void)fake_stack;
    (void)address;
    (void)begin;
    (void)end;
    return 0;
#endif
}

#endif

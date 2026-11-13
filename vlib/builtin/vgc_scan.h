#ifndef VGC_SCAN_H
#define VGC_SCAN_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

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

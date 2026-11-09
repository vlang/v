#ifndef V_PREALLOC_PRESSURE_TEST_H
#define V_PREALLOC_PRESSURE_TEST_H

static int v_prealloc_pressure_has_shadow_memory(void) {
#if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_MEMORY__) || defined(__SANITIZE_THREAD__) || defined(__SANITIZE_HWADDRESS__)
    return 1;
#elif defined(__has_feature)
#if __has_feature(address_sanitizer) || __has_feature(memory_sanitizer) || __has_feature(thread_sanitizer) || __has_feature(hwaddress_sanitizer)
    return 1;
#endif
#endif
    return 0;
}

#endif

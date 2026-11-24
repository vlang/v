#include <stdint.h>
int v_native_runtime_capabilities(void) {
    int result = 0;
#ifdef CUSTOM_DEFINE_freestanding
    result |= 1;
#endif
#ifdef CUSTOM_DEFINE_v_bare_no_startup
    result |= 2;
#endif
#if defined(__STDC_HOSTED__) && __STDC_HOSTED__
    result |= 4;
#endif
#if defined(__SSP__) || defined(__SSP_ALL__) || defined(__SSP_STRONG__)
    result |= 8;
#endif
    /* Keep stack storage observable when testing a toolchain's stack-protector
       defaults. The freestanding policy must explicitly disable that runtime. */
    volatile unsigned char storage[64];
    storage[0] = (unsigned char)result;
    return storage[0];
}

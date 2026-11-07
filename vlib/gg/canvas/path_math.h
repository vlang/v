// Sine and cosine of one angle, using a combined call where available.
// Clang combines std::sin and std::cos of the same float argument into one
// sincos call, whose results can differ from separate sinf/cosf calls in the
// last bit (Apple's __sincosf_stret).
#include <math.h>

static inline void gg_canvas_sincosf(float x, float* s, float* c) {
#if defined(__APPLE__)
    struct __float2 r = __sincosf_stret(x);
    *s = r.__sinval;
    *c = r.__cosval;
#elif defined(_WIN32)
    // The Windows CRT has no sincosf declaration. Calling it without a
    // prototype promotes x to double, corrupting the float argument in TCC.
    *s = sinf(x);
    *c = cosf(x);
#else
    sincosf(x, s, c);
#endif
}

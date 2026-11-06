// Sine and cosine of one angle as Skia's compiled code computes them: clang
// combines std::sin and std::cos of the same float argument into one sincos
// call, whose results can differ from separate sinf/cosf calls in the last
// bit (Apple's __sincosf_stret).
#include <math.h>

// glibc exports sincosf, but only declares the GNU extension when GNU feature
// macros were set before the first system header. V also compiles in strict C
// modes, so provide its declaration when math.h did not expose it.
#if defined(__GLIBC__) && !defined(__USE_GNU)
extern void sincosf(float, float*, float*);
#endif

static inline void gg_canvas_sincosf(float x, float* s, float* c) {
#if defined(__APPLE__)
    struct __float2 r = __sincosf_stret(x);
    *s = r.__sinval;
    *c = r.__cosval;
#else
    sincosf(x, s, c);
#endif
}

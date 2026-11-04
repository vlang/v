// This implementation belongs to the native owner in a cached build. A parallel
// C build repeats the native directives in every unit, and only its first unit
// may define these functions; sokol_v_post.h declares them for the others.
#if !defined(V_PARALLEL_CC) || defined(V_PARALLEL_CC_OUT_0)
#include "sokol_v.post.h"
#endif

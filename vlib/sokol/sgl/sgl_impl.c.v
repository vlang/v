module sgl

#flag -I @VEXEROOT/thirdparty/sokol
#flag -I @VEXEROOT/thirdparty/sokol/util

// sokol_gl.h is only needed by programs that actually use sokol.sgl, so it is
// included here instead of in sokol.c/declaration.c.v. That keeps it out of
// builds that only use sokol.gfx/sokol.sapp (including gg, which uses its
// own pure-V renderer in vlib/gg/render).
@[use_once]
#include "util/sokol_gl.h"

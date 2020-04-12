module c

pub const (
  used_import = 1
)

#flag -I @VROOT/thirdparty/sokol
#flag -I @VROOT/thirdparty/sokol/util

#flag darwin -fobjc-arc
#flag linux -lX11 -lGL

#flag windows -lgdi32

// METAL
#flag darwin -DSOKOL_METAL
#flag darwin -framework Metal -framework Cocoa -framework MetalKit -framework QuartzCore

// OPENGL
#flag linux -DSOKOL_GLCORE33
#flag windows -DSOKOL_GLCORE33
//#flag darwin -framework OpenGL -framework Cocoa -framework QuartzCore


// for simplicity, all header includes are here because import order matters and we dont have any way
// to ensure import order with V yet
#define SOKOL_IMPL
#define SOKOL_NO_ENTRY
#include "sokol_app.h"

#define SOKOL_IMPL
#define SOKOL_NO_DEPRECATED
#include "sokol_gfx.h"

#define SOKOL_GL_IMPL
#include "util/sokol_gl.h"

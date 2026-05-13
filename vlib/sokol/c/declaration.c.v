module c

import sokol.memory as _

#flag -I @VEXEROOT/thirdparty/sokol
#flag -I @VEXEROOT/thirdparty/sokol/util
#flag freebsd -I /usr/local/include
#flag darwin -fobjc-arc

// Platform-specific library linking
// X11 is the default on Linux
// Use `-d sokol_wayland` to enable Wayland support
#flag linux -DSOKOL_GLCORE
$if sokol_wayland ? {
	#flag linux -lwayland-client -lwayland-egl -lxkbcommon -lxkbcommon-x11 -lEGL -lGL -lpthread -lm -ldl -lX11 -lXi -lXcursor
} $else {
	// EGL is used instead of GLX on X11 to get consistent vsync under XWayland.
	// GLX vsync is often ignored by XWayland compositors, causing frame pacing
	// jitter. EGL honours the swap interval reliably on both native X11 and
	// XWayland sessions.
	#flag linux -lX11 -lXi -lXcursor -lEGL -lGL -lpthread -lm -ldl
}
#flag freebsd -DSOKOL_GLCORE
#flag freebsd -L/usr/local/lib -lX11 -lGL -lXcursor -lXi
#flag openbsd -DSOKOL_GLCORE
#flag openbsd -I/usr/X11R6/include -L/usr/X11R6/lib -lX11 -lGL -lXcursor -lXi
#flag windows -DSOKOL_GLCORE
#flag windows -lgdi32

$if windows {
	#flag windows -lopengl32
	$if msvc {
		$if livemain ? {
			#define SOKOL_DLL
		}
		$if sharedlive ? {
			#define SOKOL_DLL
		}
	}
}

// Note that -lm is needed *only* for sokol_gl.h's usage of sqrtf(),
// but without -lm, this fails:
// `v -cc gcc ~/.vmodules/sdl/examples/sdl_opengl_and_sokol/`
// With tcc, this succeeds with or without -lm:
// `v ~/.vmodules/sdl/examples/sdl_opengl_and_sokol/`
$if !tinyc {
	#flag linux -lm
}

// METAL
$if macos {
	$if darwin_sokol_glcore33 ? {
		#flag darwin -DSOKOL_GLCORE -framework OpenGL -framework Cocoa -framework QuartzCore
	} $else {
		#flag -DSOKOL_METAL
		#flag -framework Metal -framework Cocoa -framework MetalKit -framework QuartzCore
	}
}
$if ios {
	#flag -DSOKOL_METAL
	#flag -framework Foundation -framework Metal -framework MetalKit -framework UIKit
}

$if emscripten ? {
	#flag -DSOKOL_GLES3
	#flag -DSOKOL_NO_ENTRY
	#flag -s ERROR_ON_UNDEFINED_SYMBOLS=0
	#flag -s USE_WEBGL2
	$if !prod {
		#flag -s ASSERTIONS=1
	}
	$if prod {
		#flag -s ASSERTIONS=0
	}
	// See https://emscripten.org/docs/tools_reference/settings_reference.html#modularize
	// Note that it makes it impossible to use `v -os wasm32_emscripten -o file.html program.v` , due to:
	// https://github.com/emscripten-core/emscripten/issues/7950
	//	#flag -s MODULARIZE
}

// D3D
//#flag windows -DSOKOL_D3D11
#flag windows -DSOKOL_GLCORE

// for simplicity, all header includes are here because import order matters and we dont have any way
// to ensure import order with V yet

// TODO: should not be defined for android graphic (apk/aab using sokol) builds, but we have no ways to undefine
//#define SOKOL_NO_ENTRY
#flag linux   -DSOKOL_NO_ENTRY
#flag darwin  -DSOKOL_NO_ENTRY
#flag windows -DSOKOL_NO_ENTRY
#flag windows -DSOKOL_WIN32_FORCE_MAIN
#flag freebsd -DSOKOL_NO_ENTRY
#flag openbsd -DSOKOL_NO_ENTRY
#flag solaris -DSOKOL_NO_ENTRY
// TODO: end

#flag linux -ldl

// To allow for thirdparty initializing window / acceleration contexts
// but still be able to use sokol.gfx e.g. SDL+sokol_gfx
$if !no_sokol_app ? {
	$if linux && sokol_wayland ? {
		// Enable Wayland on Linux (when explicitly enabled with -d sokol_wayland)
		#define SOKOL_WAYLAND
		#flag -I@VEXEROOT/thirdparty/sokol
	} $else $if linux {
		// Explicitly disable Wayland on Linux when not using sokol_wayland
		#define SOKOL_DISABLE_WAYLAND
		// Force EGL instead of GLX so that eglSwapBuffers provides consistent
		// vsync pacing on both native X11 and XWayland sessions.
		#define SOKOL_FORCE_EGL
	}

	$if macos {
		$if sharedlive ? {
		} $else {
			// The live-reload dylib should reuse the host app's sokol_app symbols on macOS.
			#define SOKOL_APP_IMPL
		}
	} $else $if windows {
		$if sharedlive ? {
		} $else {
			// On Windows, the live-reload DLL links back to the host executable's
			// import library, so it should not embed its own sokol_app backend.
			#define SOKOL_APP_IMPL
		}
	} $else {
		#define SOKOL_APP_IMPL
	}

	@[use_once]
	#include "sokol_app.h"
}

$if windows && sharedlive ? {
} $else {
	@[use_once]
	#define SOKOL_GFX_IMPL
}
#define SOKOL_NO_DEPRECATED
#include "sokol_gfx.h"

$if windows && sharedlive ? {
} $else {
	@[use_once]
	#define SOKOL_IMPL
}
#include "util/sokol_gl.h"

#include "sokol_v.post.h"

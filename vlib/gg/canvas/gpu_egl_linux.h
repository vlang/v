#ifndef V_CANVAS_EGL_LINUX_H
#define V_CANVAS_EGL_LINUX_H

#include <EGL/egl.h>
#include <EGL/eglext.h>
#ifndef GL_GLEXT_PROTOTYPES
#define GL_GLEXT_PROTOTYPES
#endif
#include <GL/gl.h>
#include <GL/glext.h>
#include <stdlib.h>
#include <string.h>

/*
 * Some Mesa DRI drivers retain process-lifetime allocations. When EGL closes
 * the driver after eglTerminate(), LeakSanitizer loses the driver symbols and
 * reports those allocations with frames from "<unknown module>". Keep only
 * those DRI libraries mapped in ASan builds so their static roots remain
 * visible; still destroy every EGL context, surface, and display normally.
 * See https://bugzilla.mozilla.org/show_bug.cgi?id=1702394.
 */
#if defined(__SANITIZE_ADDRESS__)
#define V_CANVAS_EGL_ASAN 1
#elif defined(__clang__)
#if __has_feature(address_sanitizer)
#define V_CANVAS_EGL_ASAN 1
#endif
#endif

#if defined(V_CANVAS_EGL_ASAN) && defined(__GLIBC__)
#include <dlfcn.h>
#include <link.h>

static int v_canvas_egl_keep_driver_loaded(struct dl_phdr_info* info, size_t size, void* data) {
    (void)size;
    (void)data;
    const char* path = info->dlpi_name;
    if (!path || !*path) {
        return 0;
    }
    const char* name = strrchr(path, '/');
    name = name ? name + 1 : path;
    // Mesa loads swrast_dri.so on older systems and libgallium-*.so on newer ones.
    if (!strstr(name, "_dri.so") && !strstr(name, "libgallium")) {
        return 0;
    }
    void* handle = dlopen(path, RTLD_LAZY | RTLD_NOLOAD | RTLD_NODELETE);
    if (handle) {
        dlclose(handle);
    }
    return 0;
}

static void v_canvas_egl_keep_drivers_loaded(void) {
    dl_iterate_phdr(v_canvas_egl_keep_driver_loaded, NULL);
}
#endif

typedef struct {
    EGLDisplay display;
    EGLContext context;
    EGLSurface surface;
    int owns_display;
    EGLenum previous_api;
    EGLDisplay previous_display;
    EGLContext previous_context;
    EGLSurface previous_draw;
    EGLSurface previous_read;
} v_canvas_egl_context;

static int v_canvas_gl_has_storage_buffers(void) {
    GLint vertex_blocks = 0;
    GLint fragment_blocks = 0;
    GLint bindings = 0;
    glGetIntegerv(GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS, &vertex_blocks);
    glGetIntegerv(GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS, &fragment_blocks);
    glGetIntegerv(GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS, &bindings);
    // Sokol maps eight slots per stage and clears all sixteen during setup.
    return vertex_blocks > 0 && fragment_blocks > 0 && bindings >= 16;
}

static void v_canvas_egl_destroy(void* opaque) {
    v_canvas_egl_context* ctx = (v_canvas_egl_context*)opaque;
    if (!ctx) {
        return;
    }
    if (ctx->display != EGL_NO_DISPLAY) {
        if (ctx->context != EGL_NO_CONTEXT && eglGetCurrentContext() == ctx->context) {
            eglMakeCurrent(ctx->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
        }
        if (ctx->context != EGL_NO_CONTEXT) {
            eglDestroyContext(ctx->display, ctx->context);
        }
        if (ctx->surface != EGL_NO_SURFACE) {
            eglDestroySurface(ctx->display, ctx->surface);
        }
    }
    eglBindAPI(ctx->previous_api);
    if (ctx->previous_display != EGL_NO_DISPLAY) {
        eglMakeCurrent(ctx->previous_display, ctx->previous_draw, ctx->previous_read,
            ctx->previous_context);
    }
    if (ctx->owns_display) {
        eglTerminate(ctx->display);
    }
    free(ctx);
}

static int v_canvas_egl_has_extension(const char* extensions, const char* name) {
    if (!extensions) {
        return 0;
    }
    const size_t length = strlen(name);
    const char* match = extensions;
    while ((match = strstr(match, name)) != NULL) {
        if ((match == extensions || match[-1] == ' ') &&
            (match[length] == ' ' || match[length] == '\0')) {
            return 1;
        }
        match += length;
    }
    return 0;
}

// Prefer Mesa's surfaceless display so offscreen drawing does not need X11
// or Wayland. Other EGL implementations can use their default display.
static void* v_canvas_egl_create(char** message) {
    v_canvas_egl_context* ctx = (v_canvas_egl_context*)calloc(1, sizeof(*ctx));
    *message = "could not allocate an EGL context";
    if (!ctx) {
        return NULL;
    }
    ctx->previous_api = eglQueryAPI();
    ctx->previous_display = eglGetCurrentDisplay();
    ctx->previous_context = eglGetCurrentContext();
    ctx->previous_draw = eglGetCurrentSurface(EGL_DRAW);
    ctx->previous_read = eglGetCurrentSurface(EGL_READ);
    const char* extensions = eglQueryString(EGL_NO_DISPLAY, EGL_EXTENSIONS);
    if (v_canvas_egl_has_extension(extensions, "EGL_MESA_platform_surfaceless")) {
        PFNEGLGETPLATFORMDISPLAYEXTPROC get_platform_display =
            (PFNEGLGETPLATFORMDISPLAYEXTPROC)eglGetProcAddress("eglGetPlatformDisplayEXT");
        if (get_platform_display) {
            ctx->display = get_platform_display(EGL_PLATFORM_SURFACELESS_MESA,
                EGL_DEFAULT_DISPLAY, NULL);
        } else {
            PFNEGLGETPLATFORMDISPLAYPROC get_core_display =
                (PFNEGLGETPLATFORMDISPLAYPROC)eglGetProcAddress("eglGetPlatformDisplay");
            if (get_core_display) {
                ctx->display = get_core_display(EGL_PLATFORM_SURFACELESS_MESA,
                    EGL_DEFAULT_DISPLAY, NULL);
            }
        }
    }
    if (ctx->display == EGL_NO_DISPLAY) {
        ctx->display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    }
    *message = "could not initialize EGL for offscreen OpenGL rendering";
    if (ctx->display == EGL_NO_DISPLAY) {
        goto fail;
    }
    // EGL displays are shared even when their contexts are not current.
    // Terminating an already initialized display would destroy its owner's
    // retained contexts and surfaces, so only initialize and own a new one.
    if (!eglQueryString(ctx->display, EGL_VERSION)) {
        eglGetError(); // consume the probe's EGL_NOT_INITIALIZED error
        if (!eglInitialize(ctx->display, NULL, NULL)) {
            goto fail;
        }
        ctx->owns_display = 1;
    }
    *message = "EGL does not support desktop OpenGL";
    if (!eglBindAPI(EGL_OPENGL_API)) {
        goto fail;
    }
    const EGLint config_attributes[] = {
        EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        EGL_RED_SIZE, 8, EGL_GREEN_SIZE, 8, EGL_BLUE_SIZE, 8, EGL_ALPHA_SIZE, 8,
        EGL_NONE
    };
    EGLConfig config;
    EGLint count = 0;
    *message = "EGL has no suitable offscreen OpenGL configuration";
    if (!eglChooseConfig(ctx->display, config_attributes, &config, 1, &count) || count == 0) {
        goto fail;
    }
    const EGLint context_attributes[] = {
        EGL_CONTEXT_MAJOR_VERSION_KHR, 4,
        EGL_CONTEXT_MINOR_VERSION_KHR, 3,
        EGL_CONTEXT_OPENGL_PROFILE_MASK_KHR, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT_KHR,
        EGL_NONE
    };
    ctx->context = eglCreateContext(ctx->display, config, EGL_NO_CONTEXT, context_attributes);
    *message = "OpenGL 4.3 is required for canvas storage buffers";
    if (ctx->context == EGL_NO_CONTEXT) {
        goto fail;
    }
    const EGLint surface_attributes[] = {EGL_WIDTH, 1, EGL_HEIGHT, 1, EGL_NONE};
    ctx->surface = eglCreatePbufferSurface(ctx->display, config, surface_attributes);
    *message = "could not create the EGL offscreen surface";
    if (ctx->surface == EGL_NO_SURFACE) {
        goto fail;
    }
    *message = "could not make the EGL OpenGL context current";
    if (!eglMakeCurrent(ctx->display, ctx->surface, ctx->surface, ctx->context)) {
        goto fail;
    }
#if defined(V_CANVAS_EGL_ASAN) && defined(__GLIBC__)
    v_canvas_egl_keep_drivers_loaded();
#endif
    return ctx;

fail:
    v_canvas_egl_destroy(ctx);
    return NULL;
}

#endif

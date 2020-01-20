#ifndef SOKOL_GL_INCLUDED
/*
    sokol_gl.h -- OpenGL 1.x style rendering on top of sokol_gfx.h

    Project URL: https://github.com/floooh/sokol

    Do this:
        #define SOKOL_GL_IMPL
    before you include this file in *one* C or C++ file to create the
    implementation.

    The following defines are used by the implementation to select the
    platform-specific embedded shader code (these are the same defines as
    used by sokol_gfx.h and sokol_app.h):

    SOKOL_GLCORE33
    SOKOL_GLES2
    SOKOL_GLES3
    SOKOL_D3D11
    SOKOL_METAL

    ...optionally provide the following macros to override defaults:

    SOKOL_ASSERT(c)     - your own assert macro (default: assert(c))
    SOKOL_MALLOC(s)     - your own malloc function (default: malloc(s))
    SOKOL_FREE(p)       - your own free function (default: free(p))
    SOKOL_API_DECL      - public function declaration prefix (default: extern)
    SOKOL_API_IMPL      - public function implementation prefix (default: -)
    SOKOL_LOG(msg)      - your own logging function (default: puts(msg))
    SOKOL_UNREACHABLE() - a guard macro for unreachable code (default: assert(false))

    If sokol_gl.h is compiled as a DLL, define the following before
    including the declaration or implementation:

    SOKOL_DLL

    On Windows, SOKOL_DLL will define SOKOL_API_DECL as __declspec(dllexport)
    or __declspec(dllimport) as needed.

    Include the following headers before including sokol_gl.h:

        sokol_gfx.h

    Matrix functions have been taken from MESA and Regal.

    FEATURE OVERVIEW:
    =================
    sokol_gl.h implements a subset of the OpenGLES 1.x feature set useful for
    when you just want to quickly render a bunch of colored triangles or
    lines without having to mess with buffers and
    shaders.

    The current feature set is mostly useful for debug visualizations
    and simple UI-style 2D rendering:

    What's implemented:
        - vertex components:
            - position (x, y, z)
            - 2D texture coords (u, v)
            - color (r, g, b, a)
        - primitive types:
            - triangle list and strip
            - line list and strip
            - quad list (TODO: quad strips)
            - point list (TODO: point size)
        - one texture layer (no multi-texturing)
        - viewport and scissor-rect with selectable origin (top-left or bottom-left)
        - all GL 1.x matrix stack functions, and additionally equivalent
          functions for gluPerspective and gluLookat

    Notable GLES 1.x features that are *NOT* implemented:
        - vertex lighting (this is the most likely GL feature that might be added later)
        - vertex arrays (although providing whole chunks of vertex data at once
          might be a useful feature for a later version)
        - texture coordinate generation
        - point size and line width
        - all pixel store functions
        - no ALPHA_TEST
        - no clear functions (clearing is handled by the sokol-gfx render pass)
        - fog

    Notable differences to GL:
        - No "enum soup" for render states etc, instead there's a
          'pipeline stack', this is similar to GL's matrix stack,
          but for pipeline-state-objects. The pipeline object at
          the top of the pipeline stack defines the active set of render states
        - All angles are in radians, not degrees (note the sgl_rad() and
          sgl_deg() conversion functions)
        - No enable/disable state for scissor test, this is always enabled

    STEP BY STEP:
    =============
    --- To initialize sokol-gl, call:

            sgl_setup(const sgl_desc_t* desc)

        NOTE that sgl_setup() must be called *after* initializing sokol-gfx
        (via sg_setup). This is because sgl_setup() needs to create
        sokol-gfx resource objects.

        sgl_setup() needs to know the attributes of the sokol-gfx render pass
        where sokol-gl rendering will happen through the passed-in sgl_desc_t
        struct:

            sg_pixel_format color_format    - color pixel format of render pass
            sg_pixel_format depth_format    - depth pixel format of render pass
            int sample_count                - MSAA sample count of render pass

        These values have the same defaults as sokol_gfx.h and sokol_app.h,
        to use the default values, leave them zero-initialized.

        You can adjust the maximum number of vertices and drawing commands
        per frame through the members:

            int max_vertices    - default is 65536
            int max_commands    - default is 16384

        You can adjust the size of the internal pipeline state object pool
        with:

            int pipeline_pool_size  - default is 64

        Finally you can change the face winding for front-facing triangles
        and quads:

            sg_face_winding face_winding    - default is SG_FACEWINDING_CCW

        The default winding for front faces is counter-clock-wise. This is
        the same as OpenGL's default, but different from sokol-gfx.

    --- Optionally create pipeline-state-objects if you need render state
        that differs from sokol-gl's default state:

            sgl_pipeline pip = sgl_make_pipeline(const sg_pipeline_desc* desc)

        The similarity with sokol_gfx.h's sg_pipeline type and sg_make_pipeline()
        function is intended. sgl_make_pipeline() also takes a standard
        sokol-gfx sg_pipeline_desc object to describe the render state, but
        without:
            - shader
            - vertex layout
            - color- and depth-pixel-formats
            - primitive type (lines, triangles, ...)
            - MSAA sample count
        Those will be filled in by sgl_make_pipeline(). Note that each
        call to sgl_make_pipeline() needs to create several sokol-gfx
        pipeline objects (one for each primitive type).

    --- if you need to destroy sgl_pipeline objects before sgl_shutdown():

            sgl_destroy_pipeline(sgl_pipeline pip)

    --- After sgl_setup() you can call any of the sokol-gl functions anywhere
        in a frame, *except* sgl_draw(). The 'vanilla' functions
        will only change internal sokol-gl state, and not call any sokol-gfx
        functions.

    --- Unlike OpenGL, sokol-gl has a function to reset internal state to
        a known default. This is useful at the start of a sequence of
        rendering operations:

            void sgl_defaults(void)

        This will set the following default state:

            - current texture coordinate to u=0.0f, v=0.0f
            - current color to white (rgba all 1.0f)
            - unbind the current texture and texturing will be disabled
            - *all* matrices will be set to identity (also the projection matrix)
            - the default render state will be set by loading the 'default pipeline'
              into the top of the pipeline stack

        The current matrix- and pipeline-stack-depths will not be changed by
        sgl_defaults().

    --- change the currently active renderstate through the
        pipeline-stack functions, this works similar to the
        traditional GL matrix stack:

            ...load the default pipeline state on the top of the pipeline stack:

                sgl_default_pipeline()

            ...load a specific pipeline on the top of the pipeline stack:

                sgl_load_pipeline(sgl_pipeline pip)

            ...push and pop the pipeline stack:
                sgl_push_pipeline()
                sgl_pop_pipeline()

    --- control texturing with:

            sgl_enable_texture()
            sgl_disable_texture()
            sgl_texture(sg_image img)

    --- set the current viewport and scissor rect with:

            sgl_viewport(int x, int y, int w, int h, bool origin_top_left)
            sgl_scissor_rect(int x, int y, int w, int h, bool origin_top_left)

        ...these calls add a new command to the internal command queue, so
        that the viewport or scissor rect are set at the right time relative
        to other sokol-gl calls.

    --- adjust the transform matrices, matrix manipulation works just like
        the OpenGL matrix stack:

        ...set the current matrix mode:

            sgl_matrix_mode_modelview()
            sgl_matrix_mode_projection()
            sgl_matrix_mode_texture()

        ...load the identity matrix into the current matrix:

            sgl_load_identity()

        ...translate, rotate and scale the current matrix:

            sgl_translate(float x, float y, float z)
            sgl_rotate(float angle_rad, float x, float y, float z)
            sgl_scale(float x, float y, float z)

        NOTE that all angles in sokol-gl are in radians, not in degree.
        Convert between radians and degree with the helper functions:

            float sgl_rad(float deg)        - degrees to radians
            float sgl_deg(float rad)        - radians to degrees

        ...directly load the current matrix from a float[16] array:

            sgl_load_matrix(const float m[16])
            sgl_load_transpose_matrix(const float m[16])

        ...directly multiply the current matrix from a float[16] array:

            sgl_mult_matrix(const float m[16])
            sgl_mult_transpose_matrix(const float m[16])

        The memory layout of those float[16] arrays is the same as in OpenGL.

        ...more matrix functions:

            sgl_frustum(float left, float right, float bottom, float top, float near, float far)
            sgl_ortho(float left, float right, float bottom, float top, float near, float far)
            sgl_perspective(float fov_y, float aspect, float near, float far)
            sgl_lookat(float eye_x, float eye_y, float eye_z, float center_x, float center_y, float center_z, float up_x, float up_y, float up_z)

        These functions work the same as glFrustum(), glOrtho(), gluPerspective()
        and gluLookAt().

        ...and finally to push / pop the current matrix stack:

            sgl_push_matrix(void)
            sgl_pop_matrix(void)

        Again, these work the same as glPushMatrix() and glPopMatrix().

    --- perform primitive rendering:

        ...set the current texture coordinate and color 'registers' with:

            sgl_t2f(float u, float v)   - set current texture coordinate
            sgl_c*(...)                 - set current color

        There are several functions for setting the color (as float values,
        unsigned byte values, packed as unsigned 32-bit integer, with
        and without alpha).

        NOTE that these are the only functions that can be called both inside
        sgl_begin_*() / sgl_end() and outside.

        ...start a primitive vertex sequence with:

            sgl_begin_points()
            sgl_begin_lines()
            sgl_begin_line_strip()
            sgl_begin_triangles()
            sgl_begin_triangle_strip()
            sgl_begin_quads()

        ...after sgl_begin_*() specifiy vertices:

            sgl_v*(...)
            sgl_v*_t*(...)
            sgl_v*_c*(...)
            sgl_v*_t*_c*(...)

        These functions write a new vertex to sokol-gl's internal vertex buffer,
        optionally with texture-coords and color. If the texture coordinate
        and/or color is missing, it will be taken from the current texture-coord
        and color 'register'.

        ...finally, after specifying vertices, call:

            sgl_end()

        This will record a new draw command in sokol-gl's internal command
        list, or it will extend the previous draw command if no relevant
        state has changed since the last sgl_begin/end pair.

    --- inside a sokol-gfx rendering pass, call:

            sgl_draw()

        This will render everything that has been recorded since the last
        call to sgl_draw() through sokol-gfx, and will 'rewind' the internal
        vertex-, uniform- and command-buffers.

    --- sokol-gl tracks a single internal error code which can be
        queried with

            sgl_error_t sgl_error(void)

        ...which can return the following error codes:

        SGL_NO_ERROR                - all OK, no error occurred since last sgl_draw()
        SGL_ERROR_VERTICES_FULL     - internal vertex buffer is full (checked in sgl_end())
        SGL_ERROR_UNIFORMS_FULL     - the internal uniforms buffer is full (checked in sgl_end())
        SGL_ERROR_COMMANDS_FULL     - the internal command buffer is full (checked in sgl_end())
        SGL_ERROR_STACK_OVERFLOW    - matrix- or pipeline-stack overflow
        SGL_ERROR_STACK_UNDERFLOW   - matrix- or pipeline-stack underflow

        ...if sokol-gl is in an error-state, sgl_draw() will skip any rendering,
        and reset the error code to SGL_NO_ERROR.

    UNDER THE HOOD:
    ===============
    sokol_gl.h works by recording vertex data and rendering commands into
    memory buffers, and then drawing the recorded commands via sokol_gfx.h

    The only functions which call into sokol_gfx.h are:
        - sgl_setup()
        - sgl_shutdown()
        - sgl_draw()

    sgl_setup() must be called after initializing sokol-gfx.
    sgl_shutdown() must be called before shutting down sokol-gfx.
    sgl_draw() must be called once per frame inside a sokol-gfx render pass.

    All other sokol-gl function can be called anywhere in a frame, since
    they just record data into memory buffers owned by sokol-gl.

    What happens in:

        sgl_setup():
            - 3 memory buffers are allocated, one for vertex data,
              one for uniform data, and one for commands
            - sokol-gfx resources are created: a (dynamic) vertex buffer,
              a shader object (using embedded shader source or byte code),
              and an 8x8 all-white default texture

            One vertex is 24 bytes:
                - float3 position
                - float2 texture coords
                - uint32_t color

            One uniform block is 128 bytes:
                - mat4 model-view-projection matrix
                - mat4 texture matrix

            One draw command is ca. 24 bytes for the actual
            command code plus command arguments.

            Each sgl_end() consumes one command, and one uniform block
            (only when the matrices have changed).
            The required size for one sgl_begin/end pair is (at most):

                (152 + 24 * num_verts) bytes

        sgl_shutdown():
            - all sokol-gfx resources (buffer, shader, default-texture and
              all pipeline objects) are destroyed
            - the 3 memory buffers are freed

        sgl_draw():
            - copy all recorded vertex data into the dynamic sokol-gfx buffer
              via a call to sg_update_buffer()
            - for each recorded command:
                - if it's a viewport command, call sg_apply_viewport()
                - if it's a scissor-rect command, call sg_apply_scissor_rect()
                - if it's a draw command:
                    - depending on what has changed since the last draw command,
                      call sg_apply_pipeline(), sg_apply_bindings() and
                      sg_apply_uniforms()
                    - finally call sg_draw()

    All other functions only modify the internally tracked state, add
    data to the vertex, uniform and command buffers, or manipulate
    the matrix stack.

    ON DRAW COMMAND MERGING
    =======================
    Not every call to sgl_end() will automatically record a new draw command.
    If possible, the previous draw command will simply be extended,
    resulting in fewer actual draw calls later in sgl_draw().

    A draw command will be merged with the previous command if "no relevant
    state has changed" since the last sgl_end(), meaning:

    - no calls to sgl_apply_viewport() and sgl_apply_scissor_rect()
    - the primitive type hasn't changed
    - the primitive type isn't a 'strip type' (no line or triangle strip)
    - the pipeline state object hasn't changed
    - none of the matrices has changed
    - none of the texture state has changed

    Merging a draw command simply means that the number of vertices
    to render in the previous draw command will be incremented by the
    number of vertices in the new draw command.

    LICENSE
    =======
    zlib/libpng license

    Copyright (c) 2018 Andre Weissflog

    This software is provided 'as-is', without any express or implied warranty.
    In no event will the authors be held liable for any damages arising from the
    use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

        1. The origin of this software must not be misrepresented; you must not
        claim that you wrote the original software. If you use this software in a
        product, an acknowledgment in the product documentation would be
        appreciated but is not required.

        2. Altered source versions must be plainly marked as such, and must not
        be misrepresented as being the original software.

        3. This notice may not be removed or altered from any source
        distribution.
*/
#define SOKOL_GL_INCLUDED (1)
#include <stdint.h>
#include <stdbool.h>

#if !defined(SOKOL_GFX_INCLUDED)
#error "Please include sokol_gfx.h before sokol_gl.h"
#endif

#ifndef SOKOL_API_DECL
#if defined(_WIN32) && defined(SOKOL_DLL) && defined(SOKOL_IMPL)
#define SOKOL_API_DECL __declspec(dllexport)
#elif defined(_WIN32) && defined(SOKOL_DLL)
#define SOKOL_API_DECL __declspec(dllimport)
#else
#define SOKOL_API_DECL extern
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* sokol_gl pipeline handle (created with sgl_make_pipeline()) */
typedef struct sgl_pipeline { uint32_t id; } sgl_pipeline;

/*
    sgl_error_t

    Errors are reset each frame after calling sgl_draw(),
    get the last error code with sgl_error()
*/
typedef enum sgl_error_t {
    SGL_NO_ERROR = 0,
    SGL_ERROR_VERTICES_FULL,
    SGL_ERROR_UNIFORMS_FULL,
    SGL_ERROR_COMMANDS_FULL,
    SGL_ERROR_STACK_OVERFLOW,
    SGL_ERROR_STACK_UNDERFLOW,
} sgl_error_t;

typedef struct sgl_desc_t {
    int max_vertices;       /* size for vertex buffer */
    int max_commands;       /* size of uniform- and command-buffers */
    int pipeline_pool_size; /* size of the internal pipeline pool, default is 64 */
    sg_pixel_format color_format;
    sg_pixel_format depth_format;
    int sample_count;
    sg_face_winding face_winding; /* default front face winding is CCW */
} sgl_desc_t;

/* setup/shutdown/misc */
SOKOL_API_DECL void sgl_setup(const sgl_desc_t* desc);
SOKOL_API_DECL void sgl_shutdown(void);
SOKOL_API_DECL sgl_error_t sgl_error(void);
SOKOL_API_DECL void sgl_defaults(void);
SOKOL_API_DECL float sgl_rad(float deg);
SOKOL_API_DECL float sgl_deg(float rad);

/* create and destroy pipeline objects */
SOKOL_API_DECL sgl_pipeline sgl_make_pipeline(const sg_pipeline_desc* desc);
SOKOL_API_DECL void sgl_destroy_pipeline(sgl_pipeline pip);

/* render state functions */
SOKOL_API_DECL void sgl_viewport(int x, int y, int w, int h, bool origin_top_left);
SOKOL_API_DECL void sgl_scissor_rect(int x, int y, int w, int h, bool origin_top_left);
SOKOL_API_DECL void sgl_enable_texture(void);
SOKOL_API_DECL void sgl_disable_texture(void);
SOKOL_API_DECL void sgl_texture(sg_image img);

/* pipeline stack functions */
SOKOL_API_DECL void sgl_default_pipeline(void);
SOKOL_API_DECL void sgl_load_pipeline(sgl_pipeline pip);
SOKOL_API_DECL void sgl_push_pipeline(void);
SOKOL_API_DECL void sgl_pop_pipeline(void);

/* matrix stack functions */
SOKOL_API_DECL void sgl_matrix_mode_modelview(void);
SOKOL_API_DECL void sgl_matrix_mode_projection(void);
SOKOL_API_DECL void sgl_matrix_mode_texture(void);
SOKOL_API_DECL void sgl_load_identity(void);
SOKOL_API_DECL void sgl_load_matrix(const float m[16]);
SOKOL_API_DECL void sgl_load_transpose_matrix(const float m[16]);
SOKOL_API_DECL void sgl_mult_matrix(const float m[16]);
SOKOL_API_DECL void sgl_mult_transpose_matrix(const float m[16]);
SOKOL_API_DECL void sgl_rotate(float angle_rad, float x, float y, float z);
SOKOL_API_DECL void sgl_scale(float x, float y, float z);
SOKOL_API_DECL void sgl_translate(float x, float y, float z);
SOKOL_API_DECL void sgl_frustum(float l, float r, float b, float t, float n, float f);
SOKOL_API_DECL void sgl_ortho(float l, float r, float b, float t, float n, float f);
SOKOL_API_DECL void sgl_perspective(float fov_y, float aspect, float z_near, float z_far);
SOKOL_API_DECL void sgl_lookat(float eye_x, float eye_y, float eye_z, float center_x, float center_y, float center_z, float up_x, float up_y, float up_z);
SOKOL_API_DECL void sgl_push_matrix(void);
SOKOL_API_DECL void sgl_pop_matrix(void);

/* these functions only set the internal 'current texcoord / color' (valid inside or outside begin/end) */
SOKOL_API_DECL void sgl_t2f(float u, float v);
SOKOL_API_DECL void sgl_c3f(float r, float g, float b);
SOKOL_API_DECL void sgl_c4f(float r, float g, float b, float a);
SOKOL_API_DECL void sgl_c3b(uint8_t r, uint8_t g, uint8_t b);
SOKOL_API_DECL void sgl_c4b(uint8_t r, uint8_t g, uint8_t b, uint8_t a);
SOKOL_API_DECL void sgl_c1i(uint32_t rgba);

/* define primitives, each begin/end is one draw command */
SOKOL_API_DECL void sgl_begin_points(void);
SOKOL_API_DECL void sgl_begin_lines(void);
SOKOL_API_DECL void sgl_begin_line_strip(void);
SOKOL_API_DECL void sgl_begin_triangles(void);
SOKOL_API_DECL void sgl_begin_triangle_strip(void);
SOKOL_API_DECL void sgl_begin_quads(void);
SOKOL_API_DECL void sgl_v2f(float x, float y);
SOKOL_API_DECL void sgl_v3f(float x, float y, float z);
SOKOL_API_DECL void sgl_v2f_t2f(float x, float y, float u, float v);
SOKOL_API_DECL void sgl_v3f_t2f(float x, float y, float z, float u, float v);
SOKOL_API_DECL void sgl_v2f_c3f(float x, float y, float r, float g, float b);
SOKOL_API_DECL void sgl_v2f_c3b(float x, float y, uint8_t r, uint8_t g, uint8_t b);
SOKOL_API_DECL void sgl_v2f_c4f(float x, float y, float r, float g, float b, float a);
SOKOL_API_DECL void sgl_v2f_c4b(float x, float y, uint8_t r, uint8_t g, uint8_t b, uint8_t a);
SOKOL_API_DECL void sgl_v2f_c1i(float x, float y, uint32_t rgba);
SOKOL_API_DECL void sgl_v3f_c3f(float x, float y, float z, float r, float g, float b);
SOKOL_API_DECL void sgl_v3f_c3b(float x, float y, float z, uint8_t r, uint8_t g, uint8_t b);
SOKOL_API_DECL void sgl_v3f_c4f(float x, float y, float z, float r, float g, float b, float a);
SOKOL_API_DECL void sgl_v3f_c4b(float x, float y, float z, uint8_t r, uint8_t g, uint8_t b, uint8_t a);
SOKOL_API_DECL void sgl_v3f_c1i(float x, float y, float z, uint32_t rgba);
SOKOL_API_DECL void sgl_v2f_t2f_c3f(float x, float y, float u, float v, float r, float g, float b);
SOKOL_API_DECL void sgl_v2f_t2f_c3b(float x, float y, float u, float v, uint8_t r, uint8_t g, uint8_t b);
SOKOL_API_DECL void sgl_v2f_t2f_c4f(float x, float y, float u, float v, float r, float g, float b, float a);
SOKOL_API_DECL void sgl_v2f_t2f_c4b(float x, float y, float u, float v, uint8_t r, uint8_t g, uint8_t b, uint8_t a);
SOKOL_API_DECL void sgl_v2f_t2f_c1i(float x, float y, float u, float v, uint32_t rgba);
SOKOL_API_DECL void sgl_v3f_t2f_c3f(float x, float y, float z, float u, float v, float r, float g, float b);
SOKOL_API_DECL void sgl_v3f_t2f_c3b(float x, float y, float z, float u, float v, uint8_t r, uint8_t g, uint8_t b);
SOKOL_API_DECL void sgl_v3f_t2f_c4f(float x, float y, float z, float u, float v, float r, float g, float b, float a);
SOKOL_API_DECL void sgl_v3f_t2f_c4b(float x, float y, float z, float u, float v, uint8_t r, uint8_t g, uint8_t b, uint8_t a);
SOKOL_API_DECL void sgl_v3f_t2f_c1i(float x, float y, float z, float u, float v, uint32_t rgba);
SOKOL_API_DECL void sgl_end(void);

/* render everything */
SOKOL_API_DECL void sgl_draw(void);

#ifdef __cplusplus
} /* extern "C" */
#endif
#endif /* SOKOL_GL_INCLUDED */

/*-- IMPLEMENTATION ----------------------------------------------------------*/
#ifdef SOKOL_GL_IMPL
#define SOKOL_GL_IMPL_INCLUDED (1)

#include <stddef.h> /* offsetof */
#include <string.h> /* memset */
#include <math.h> /* M_PI, sqrtf, sinf, cosf */

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327
#endif

#ifndef SOKOL_API_IMPL
    #define SOKOL_API_IMPL
#endif
#ifndef SOKOL_DEBUG
    #ifndef NDEBUG
        #define SOKOL_DEBUG (1)
    #endif
#endif
#ifndef SOKOL_ASSERT
    #include <assert.h>
    #define SOKOL_ASSERT(c) assert(c)
#endif
#ifndef SOKOL_MALLOC
    #include <stdlib.h>
    #define SOKOL_MALLOC(s) malloc(s)
    #define SOKOL_FREE(p) free(p)
#endif
#ifndef SOKOL_LOG
    #ifdef SOKOL_DEBUG
        #include <stdio.h>
        #define SOKOL_LOG(s) { SOKOL_ASSERT(s); puts(s); }
    #else
        #define SOKOL_LOG(s)
    #endif
#endif
#ifndef SOKOL_UNREACHABLE
    #define SOKOL_UNREACHABLE SOKOL_ASSERT(false)
#endif

#define _sgl_def(val, def) (((val) == 0) ? (def) : (val))
#define _SGL_INIT_COOKIE (0xABCDABCD)

#if defined(SOKOL_GLCORE33)
static const char* _sgl_vs_src =
    "#version 330\n"
    "uniform mat4 mvp;\n"
    "uniform mat4 tm;\n"
    "in vec4 position;\n"
    "in vec2 texcoord0;\n"
    "in vec4 color0;\n"
    "out vec4 uv;\n"
    "out vec4 color;\n"
    "void main() {\n"
    "    gl_Position = mvp * position;\n"
    "    uv = tm * vec4(texcoord0, 0.0, 1.0);\n"
    "    color = color0;\n"
    "}\n";
static const char* _sgl_fs_src =
    "#version 330\n"
    "uniform sampler2D tex;\n"
    "in vec4 uv;\n"
    "in vec4 color;\n"
    "out vec4 frag_color;\n"
    "void main() {\n"
    "    frag_color = texture(tex, uv.xy) * color;\n"
    "}\n";
#elif defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
static const char* _sgl_vs_src =
    "uniform mat4 mvp;\n"
    "uniform mat4 tm;\n"
    "attribute vec4 position;\n"
    "attribute vec2 texcoord0;\n"
    "attribute vec4 color0;\n"
    "varying vec4 uv;\n"
    "varying vec4 color;\n"
    "void main() {\n"
    "    gl_Position = mvp * position;\n"
    "    uv = tm * vec4(texcoord0, 0.0, 1.0);\n"
    "    color = color0;\n"
    "}\n";
static const char* _sgl_fs_src =
    "precision mediump float;\n"
    "uniform sampler2D tex;\n"
    "varying vec4 uv;\n"
    "varying vec4 color;\n"
    "void main() {\n"
    "    gl_FragColor = texture2D(tex, uv.xy) * color;\n"
    "}\n";
#elif defined(SOKOL_METAL)
static const char* _sgl_vs_src =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "struct params_t {\n"
    "  float4x4 mvp;\n"
    "  float4x4 tm;\n"
    "};\n"
    "struct vs_in {\n"
    "  float4 pos [[attribute(0)]];\n"
    "  float2 uv [[attribute(1)]];\n"
    "  float4 color [[attribute(2)]];\n"
    "};\n"
    "struct vs_out {\n"
    "  float4 pos [[position]];\n"
    "  float4 uv;\n"
    "  float4 color;\n"
    "};\n"
    "vertex vs_out _main(vs_in in [[stage_in]], constant params_t& params [[buffer(0)]]) {\n"
    "  vs_out out;\n"
    "  out.pos = params.mvp * in.pos;\n"
    "  out.uv = params.tm * float4(in.uv, 0.0, 1.0);\n"
    "  out.color = in.color;\n"
    "  return out;\n"
    "}\n";
static const char* _sgl_fs_src =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "struct fs_in {\n"
    "  float4 uv;\n"
    "  float4 color;\n"
    "};\n"
    "fragment float4 _main(fs_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]]) {\n"
    "  return tex.sample(smp, in.uv.xy) * in.color;\n"
    "}\n";
#elif defined(SOKOL_D3D11)
/*
    Shader blobs for D3D11, compiled with:

    fxc.exe /T vs_5_0 /Fh vs.h /Gec /O3 vs.hlsl
    fxc.exe /T ps_5_0 /Fh fs.h /Gec /O3 fs.hlsl

    Vertex shader source:

        cbuffer params: register(b0) {
            float4x4 mvp;
            float4x4 tm;
        };
        struct vs_in {
            float4 pos: POSITION;
            float2 uv: TEXCOORD0;
            float4 color: COLOR0;
        };
        struct vs_out {
            float4 uv: TEXCOORD0;
            float4 color: COLOR0;
            float4 pos: SV_Position;
        };
        vs_out main(vs_in inp) {
            vs_out outp;
            outp.pos = mul(mvp, inp.pos);
            outp.uv = mul(tm, float4(inp.uv, 0.0, 1.0));
            outp.color = inp.color;
            return outp;
        };

    Pixel shader source:

        Texture2D<float4> tex: register(t0);
        sampler smp: register(s0);
        float4 main(float4 uv: TEXCOORD0, float4 color: COLOR0): SV_Target0 {
            return tex.Sample(smp, uv.xy) * color;
        }
*/
static const uint8_t _sgl_vs_bin[] = {
     68,  88,  66,  67, 239, 161,
      1, 229, 179,  68, 206,  40,
     34,  15,  57, 169, 103, 117,
    134, 191,   1,   0,   0,   0,
    120,   4,   0,   0,   5,   0,
      0,   0,  52,   0,   0,   0,
    104,   1,   0,   0, 216,   1,
      0,   0,  76,   2,   0,   0,
    220,   3,   0,   0,  82,  68,
     69,  70,  44,   1,   0,   0,
      1,   0,   0,   0, 100,   0,
      0,   0,   1,   0,   0,   0,
     60,   0,   0,   0,   0,   5,
    254, 255,   0, 145,   0,   0,
      3,   1,   0,   0,  82,  68,
     49,  49,  60,   0,   0,   0,
     24,   0,   0,   0,  32,   0,
      0,   0,  40,   0,   0,   0,
     36,   0,   0,   0,  12,   0,
      0,   0,   0,   0,   0,   0,
     92,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      1,   0,   0,   0,   1,   0,
      0,   0, 112,  97, 114,  97,
    109, 115,   0, 171,  92,   0,
      0,   0,   2,   0,   0,   0,
    124,   0,   0,   0, 128,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0, 204,   0,
      0,   0,   0,   0,   0,   0,
     64,   0,   0,   0,   2,   0,
      0,   0, 220,   0,   0,   0,
      0,   0,   0,   0, 255, 255,
    255, 255,   0,   0,   0,   0,
    255, 255, 255, 255,   0,   0,
      0,   0,   0,   1,   0,   0,
     64,   0,   0,   0,  64,   0,
      0,   0,   2,   0,   0,   0,
    220,   0,   0,   0,   0,   0,
      0,   0, 255, 255, 255, 255,
      0,   0,   0,   0, 255, 255,
    255, 255,   0,   0,   0,   0,
    109, 118, 112,   0, 102, 108,
    111,  97, 116,  52, 120,  52,
      0, 171, 171, 171,   3,   0,
      3,   0,   4,   0,   4,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
    208,   0,   0,   0, 116, 109,
      0,  77, 105,  99, 114, 111,
    115, 111, 102, 116,  32,  40,
     82,  41,  32,  72,  76,  83,
     76,  32,  83, 104,  97, 100,
    101, 114,  32,  67, 111, 109,
    112, 105, 108, 101, 114,  32,
     49,  48,  46,  49,   0, 171,
     73,  83,  71,  78, 104,   0,
      0,   0,   3,   0,   0,   0,
      8,   0,   0,   0,  80,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   3,   0,
      0,   0,   0,   0,   0,   0,
     15,  15,   0,   0,  89,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   3,   0,
      0,   0,   1,   0,   0,   0,
      3,   3,   0,   0,  98,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   3,   0,
      0,   0,   2,   0,   0,   0,
     15,  15,   0,   0,  80,  79,
     83,  73,  84,  73,  79,  78,
      0,  84,  69,  88,  67,  79,
     79,  82,  68,   0,  67,  79,
     76,  79,  82,   0,  79,  83,
     71,  78, 108,   0,   0,   0,
      3,   0,   0,   0,   8,   0,
      0,   0,  80,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   3,   0,   0,   0,
      0,   0,   0,   0,  15,   0,
      0,   0,  89,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   3,   0,   0,   0,
      1,   0,   0,   0,  15,   0,
      0,   0,  95,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   3,   0,   0,   0,
      2,   0,   0,   0,  15,   0,
      0,   0,  84,  69,  88,  67,
     79,  79,  82,  68,   0,  67,
     79,  76,  79,  82,   0,  83,
     86,  95,  80, 111, 115, 105,
    116, 105, 111, 110,   0, 171,
     83,  72,  69,  88, 136,   1,
      0,   0,  80,   0,   1,   0,
     98,   0,   0,   0, 106,   8,
      0,   1,  89,   0,   0,   4,
     70, 142,  32,   0,   0,   0,
      0,   0,   8,   0,   0,   0,
     95,   0,   0,   3, 242,  16,
     16,   0,   0,   0,   0,   0,
     95,   0,   0,   3,  50,  16,
     16,   0,   1,   0,   0,   0,
     95,   0,   0,   3, 242,  16,
     16,   0,   2,   0,   0,   0,
    101,   0,   0,   3, 242,  32,
     16,   0,   0,   0,   0,   0,
    101,   0,   0,   3, 242,  32,
     16,   0,   1,   0,   0,   0,
    103,   0,   0,   4, 242,  32,
     16,   0,   2,   0,   0,   0,
      1,   0,   0,   0, 104,   0,
      0,   2,   1,   0,   0,   0,
     56,   0,   0,   8, 242,   0,
     16,   0,   0,   0,   0,   0,
     86,  21,  16,   0,   1,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   5,   0,
      0,   0,  50,   0,   0,  10,
    242,   0,  16,   0,   0,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   4,   0,
      0,   0,   6,  16,  16,   0,
      1,   0,   0,   0,  70,  14,
     16,   0,   0,   0,   0,   0,
      0,   0,   0,   8, 242,  32,
     16,   0,   0,   0,   0,   0,
     70,  14,  16,   0,   0,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   7,   0,
      0,   0,  54,   0,   0,   5,
    242,  32,  16,   0,   1,   0,
      0,   0,  70,  30,  16,   0,
      2,   0,   0,   0,  56,   0,
      0,   8, 242,   0,  16,   0,
      0,   0,   0,   0,  86,  21,
     16,   0,   0,   0,   0,   0,
     70, 142,  32,   0,   0,   0,
      0,   0,   1,   0,   0,   0,
     50,   0,   0,  10, 242,   0,
     16,   0,   0,   0,   0,   0,
     70, 142,  32,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      6,  16,  16,   0,   0,   0,
      0,   0,  70,  14,  16,   0,
      0,   0,   0,   0,  50,   0,
      0,  10, 242,   0,  16,   0,
      0,   0,   0,   0,  70, 142,
     32,   0,   0,   0,   0,   0,
      2,   0,   0,   0, 166,  26,
     16,   0,   0,   0,   0,   0,
     70,  14,  16,   0,   0,   0,
      0,   0,  50,   0,   0,  10,
    242,  32,  16,   0,   2,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   3,   0,
      0,   0, 246,  31,  16,   0,
      0,   0,   0,   0,  70,  14,
     16,   0,   0,   0,   0,   0,
     62,   0,   0,   1,  83,  84,
     65,  84, 148,   0,   0,   0,
      9,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      6,   0,   0,   0,   7,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0
};
static uint8_t _sgl_fs_bin[] = {
     68,  88,  66,  67, 145, 182,
     34, 101, 114, 183,  46,   3,
    176, 243, 147, 199, 109,  42,
    196, 114,   1,   0,   0,   0,
    176,   2,   0,   0,   5,   0,
      0,   0,  52,   0,   0,   0,
    232,   0,   0,   0,  56,   1,
      0,   0, 108,   1,   0,   0,
     20,   2,   0,   0,  82,  68,
     69,  70, 172,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   2,   0,   0,   0,
     60,   0,   0,   0,   0,   5,
    255, 255,   0, 145,   0,   0,
    132,   0,   0,   0,  82,  68,
     49,  49,  60,   0,   0,   0,
     24,   0,   0,   0,  32,   0,
      0,   0,  40,   0,   0,   0,
     36,   0,   0,   0,  12,   0,
      0,   0,   0,   0,   0,   0,
    124,   0,   0,   0,   3,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      1,   0,   0,   0,   1,   0,
      0,   0, 128,   0,   0,   0,
      2,   0,   0,   0,   5,   0,
      0,   0,   4,   0,   0,   0,
    255, 255, 255, 255,   0,   0,
      0,   0,   1,   0,   0,   0,
     13,   0,   0,   0, 115, 109,
    112,   0, 116, 101, 120,   0,
     77, 105,  99, 114, 111, 115,
    111, 102, 116,  32,  40,  82,
     41,  32,  72,  76,  83,  76,
     32,  83, 104,  97, 100, 101,
    114,  32,  67, 111, 109, 112,
    105, 108, 101, 114,  32,  49,
     48,  46,  49,   0,  73,  83,
     71,  78,  72,   0,   0,   0,
      2,   0,   0,   0,   8,   0,
      0,   0,  56,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   3,   0,   0,   0,
      0,   0,   0,   0,  15,   3,
      0,   0,  65,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   3,   0,   0,   0,
      1,   0,   0,   0,  15,  15,
      0,   0,  84,  69,  88,  67,
     79,  79,  82,  68,   0,  67,
     79,  76,  79,  82,   0, 171,
     79,  83,  71,  78,  44,   0,
      0,   0,   1,   0,   0,   0,
      8,   0,   0,   0,  32,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   3,   0,
      0,   0,   0,   0,   0,   0,
     15,   0,   0,   0,  83,  86,
     95,  84,  97, 114, 103, 101,
    116,   0, 171, 171,  83,  72,
     69,  88, 160,   0,   0,   0,
     80,   0,   0,   0,  40,   0,
      0,   0, 106,   8,   0,   1,
     90,   0,   0,   3,   0,  96,
     16,   0,   0,   0,   0,   0,
     88,  24,   0,   4,   0, 112,
     16,   0,   0,   0,   0,   0,
     85,  85,   0,   0,  98,  16,
      0,   3,  50,  16,  16,   0,
      0,   0,   0,   0,  98,  16,
      0,   3, 242,  16,  16,   0,
      1,   0,   0,   0, 101,   0,
      0,   3, 242,  32,  16,   0,
      0,   0,   0,   0, 104,   0,
      0,   2,   1,   0,   0,   0,
     69,   0,   0, 139, 194,   0,
      0, 128,  67,  85,  21,   0,
    242,   0,  16,   0,   0,   0,
      0,   0,  70,  16,  16,   0,
      0,   0,   0,   0,  70, 126,
     16,   0,   0,   0,   0,   0,
      0,  96,  16,   0,   0,   0,
      0,   0,  56,   0,   0,   7,
    242,  32,  16,   0,   0,   0,
      0,   0,  70,  14,  16,   0,
      0,   0,   0,   0,  70,  30,
     16,   0,   1,   0,   0,   0,
     62,   0,   0,   1,  83,  84,
     65,  84, 148,   0,   0,   0,
      3,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      3,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   1,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0
};
#elif defined(SOKOL_DUMMY_BACKEND)
static const char* _sgl_vs_src = "";
static const char* _sgl_fs_src = "";
#endif

typedef enum {
    SGL_PRIMITIVETYPE_POINTS = 0,
    SGL_PRIMITIVETYPE_LINES,
    SGL_PRIMITIVETYPE_LINE_STRIP,
    SGL_PRIMITIVETYPE_TRIANGLES,
    SGL_PRIMITIVETYPE_TRIANGLE_STRIP,
    SGL_PRIMITIVETYPE_QUADS,
    SGL_NUM_PRIMITIVE_TYPES,
} _sgl_primitive_type_t;

typedef struct {
    uint32_t id;
    sg_resource_state state;
} _sgl_slot_t;

typedef struct {
    int size;
    int queue_top;
    uint32_t* gen_ctrs;
    int* free_queue;
} _sgl_pool_t;

typedef struct {
    _sgl_slot_t slot;
    sg_pipeline pip[SGL_NUM_PRIMITIVE_TYPES];
} _sgl_pipeline_t;

typedef struct {
    _sgl_pool_t pool;
    _sgl_pipeline_t* pips;
} _sgl_pipeline_pool_t;

typedef enum {
    SGL_MATRIXMODE_MODELVIEW,
    SGL_MATRIXMODE_PROJECTION,
    SGL_MATRIXMODE_TEXTURE,
    SGL_NUM_MATRIXMODES
} _sgl_matrix_mode_t;

typedef struct {
    float pos[3];
    float uv[2];
    uint32_t rgba;
} _sgl_vertex_t;

typedef struct {
    float v[4][4];
} _sgl_matrix_t;

typedef struct {
    _sgl_matrix_t mvp;  /* model-view-projection matrix */
    _sgl_matrix_t tm;   /* texture matrix */
} _sgl_uniform_t;

typedef enum {
    SGL_COMMAND_DRAW,
    SGL_COMMAND_VIEWPORT,
    SGL_COMMAND_SCISSOR_RECT,
} _sgl_command_type_t;

typedef struct {
    sg_pipeline pip;
    sg_image img;
    int base_vertex;
    int num_vertices;
    int uniform_index;
} _sgl_draw_args_t;

typedef struct {
    int x, y, w, h;
    bool origin_top_left;
} _sgl_viewport_args_t;

typedef struct {
    int x, y, w, h;
    bool origin_top_left;
} _sgl_scissor_rect_args_t;

typedef union {
    _sgl_draw_args_t draw;
    _sgl_viewport_args_t viewport;
    _sgl_scissor_rect_args_t scissor_rect;
} _sgl_args_t;

typedef struct {
    _sgl_command_type_t cmd;
    _sgl_args_t args;
} _sgl_command_t;

#define _SGL_INVALID_SLOT_INDEX (0)
#define _SGL_MAX_STACK_DEPTH (64)
#define _SGL_DEFAULT_PIPELINE_POOL_SIZE (64)
#define _SGL_DEFAULT_MAX_VERTICES (1<<16)
#define _SGL_DEFAULT_MAX_COMMANDS (1<<14)
#define _SGL_SLOT_SHIFT (16)
#define _SGL_MAX_POOL_SIZE (1<<_SGL_SLOT_SHIFT)
#define _SGL_SLOT_MASK (_SGL_MAX_POOL_SIZE-1)

typedef struct {
    uint32_t init_cookie;
    sgl_desc_t desc;

    int num_vertices;
    int num_uniforms;
    int num_commands;
    int cur_vertex;
    int cur_uniform;
    int cur_command;
    _sgl_vertex_t* vertices;
    _sgl_uniform_t* uniforms;
    _sgl_command_t* commands;

    /* state tracking */
    int base_vertex;
    int vtx_count;          /* number of times vtx function has been called, used for non-triangle primitives */
    sgl_error_t error;
    bool in_begin;
    float u, v;
    uint32_t rgba;
    _sgl_primitive_type_t cur_prim_type;
    sg_image cur_img;
    bool texturing_enabled;
    bool matrix_dirty;      /* reset in sgl_end(), set in any of the matrix stack functions */

    /* sokol-gfx resources */
    sg_buffer vbuf;
    sg_image def_img;   /* a default white texture */
    sg_shader shd;
    sg_bindings bind;
    sgl_pipeline def_pip;
    _sgl_pipeline_pool_t pip_pool;

    /* pipeline stack */
    int pip_tos;
    sgl_pipeline pip_stack[_SGL_MAX_STACK_DEPTH];

    /* matrix stacks */
    _sgl_matrix_mode_t cur_matrix_mode;
    int matrix_tos[SGL_NUM_MATRIXMODES];
    _sgl_matrix_t matrix_stack[SGL_NUM_MATRIXMODES][_SGL_MAX_STACK_DEPTH];
} _sgl_t;
static _sgl_t _sgl;

/*== PRIVATE FUNCTIONS =======================================================*/

static void _sgl_init_pool(_sgl_pool_t* pool, int num) {
    SOKOL_ASSERT(pool && (num >= 1));
    /* slot 0 is reserved for the 'invalid id', so bump the pool size by 1 */
    pool->size = num + 1;
    pool->queue_top = 0;
    /* generation counters indexable by pool slot index, slot 0 is reserved */
    size_t gen_ctrs_size = sizeof(uint32_t) * pool->size;
    pool->gen_ctrs = (uint32_t*) SOKOL_MALLOC(gen_ctrs_size);
    SOKOL_ASSERT(pool->gen_ctrs);
    memset(pool->gen_ctrs, 0, gen_ctrs_size);
    /* it's not a bug to only reserve 'num' here */
    pool->free_queue = (int*) SOKOL_MALLOC(sizeof(int)*num);
    SOKOL_ASSERT(pool->free_queue);
    /* never allocate the zero-th pool item since the invalid id is 0 */
    for (int i = pool->size-1; i >= 1; i--) {
        pool->free_queue[pool->queue_top++] = i;
    }
}

static void _sgl_discard_pool(_sgl_pool_t* pool) {
    SOKOL_ASSERT(pool);
    SOKOL_ASSERT(pool->free_queue);
    SOKOL_FREE(pool->free_queue);
    pool->free_queue = 0;
    SOKOL_ASSERT(pool->gen_ctrs);
    SOKOL_FREE(pool->gen_ctrs);
    pool->gen_ctrs = 0;
    pool->size = 0;
    pool->queue_top = 0;
}

static int _sgl_pool_alloc_index(_sgl_pool_t* pool) {
    SOKOL_ASSERT(pool);
    SOKOL_ASSERT(pool->free_queue);
    if (pool->queue_top > 0) {
        int slot_index = pool->free_queue[--pool->queue_top];
        SOKOL_ASSERT((slot_index > 0) && (slot_index < pool->size));
        return slot_index;
    }
    else {
        /* pool exhausted */
        return _SGL_INVALID_SLOT_INDEX;
    }
}

static void _sgl_pool_free_index(_sgl_pool_t* pool, int slot_index) {
    SOKOL_ASSERT((slot_index > _SGL_INVALID_SLOT_INDEX) && (slot_index < pool->size));
    SOKOL_ASSERT(pool);
    SOKOL_ASSERT(pool->free_queue);
    SOKOL_ASSERT(pool->queue_top < pool->size);
    #ifdef SOKOL_DEBUG
    /* debug check against double-free */
    for (int i = 0; i < pool->queue_top; i++) {
        SOKOL_ASSERT(pool->free_queue[i] != slot_index);
    }
    #endif
    pool->free_queue[pool->queue_top++] = slot_index;
    SOKOL_ASSERT(pool->queue_top <= (pool->size-1));
}

static void _sgl_reset_pipeline(_sgl_pipeline_t* pip) {
    SOKOL_ASSERT(pip);
    memset(pip, 0, sizeof(_sgl_pipeline_t));
}

static void _sgl_setup_pipeline_pool(const sgl_desc_t* desc) {
    SOKOL_ASSERT(desc);
    /* note: the pools here will have an additional item, since slot 0 is reserved */
    SOKOL_ASSERT((desc->pipeline_pool_size > 0) && (desc->pipeline_pool_size < _SGL_MAX_POOL_SIZE));
    _sgl_init_pool(&_sgl.pip_pool.pool, desc->pipeline_pool_size);
    size_t pool_byte_size = sizeof(_sgl_pipeline_t) * _sgl.pip_pool.pool.size;
    _sgl.pip_pool.pips = (_sgl_pipeline_t*) SOKOL_MALLOC(pool_byte_size);
    SOKOL_ASSERT(_sgl.pip_pool.pips);
    memset(_sgl.pip_pool.pips, 0, pool_byte_size);
}

static void _sgl_discard_pipeline_pool(void) {
    SOKOL_FREE(_sgl.pip_pool.pips); _sgl.pip_pool.pips = 0;
    _sgl_discard_pool(&_sgl.pip_pool.pool);
}

/* allocate the slot at slot_index:
    - bump the slot's generation counter
    - create a resource id from the generation counter and slot index
    - set the slot's id to this id
    - set the slot's state to ALLOC
    - return the resource id
*/
static uint32_t _sgl_slot_alloc(_sgl_pool_t* pool, _sgl_slot_t* slot, int slot_index) {
    /* FIXME: add handling for an overflowing generation counter,
       for now, just overflow (another option is to disable
       the slot)
    */
    SOKOL_ASSERT(pool && pool->gen_ctrs);
    SOKOL_ASSERT((slot_index > _SGL_INVALID_SLOT_INDEX) && (slot_index < pool->size));
    SOKOL_ASSERT((slot->state == SG_RESOURCESTATE_INITIAL) && (slot->id == SG_INVALID_ID));
    uint32_t ctr = ++pool->gen_ctrs[slot_index];
    slot->id = (ctr<<_SGL_SLOT_SHIFT)|(slot_index & _SGL_SLOT_MASK);
    slot->state = SG_RESOURCESTATE_ALLOC;
    return slot->id;
}

/* extract slot index from id */
static int _sgl_slot_index(uint32_t id) {
    int slot_index = (int) (id & _SGL_SLOT_MASK);
    SOKOL_ASSERT(_SGL_INVALID_SLOT_INDEX != slot_index);
    return slot_index;
}

/* get pipeline pointer without id-check */
static _sgl_pipeline_t* _sgl_pipeline_at(uint32_t pip_id) {
    SOKOL_ASSERT(SG_INVALID_ID != pip_id);
    int slot_index = _sgl_slot_index(pip_id);
    SOKOL_ASSERT((slot_index > _SGL_INVALID_SLOT_INDEX) && (slot_index < _sgl.pip_pool.pool.size));
    return &_sgl.pip_pool.pips[slot_index];
}

/* get pipeline pointer with id-check, returns 0 if no match */
static _sgl_pipeline_t* _sgl_lookup_pipeline(uint32_t pip_id) {
    if (SG_INVALID_ID != pip_id) {
        _sgl_pipeline_t* pip = _sgl_pipeline_at(pip_id);
        if (pip->slot.id == pip_id) {
            return pip;
        }
    }
    return 0;
}

static sgl_pipeline _sgl_alloc_pipeline(void) {
    sgl_pipeline res;
    int slot_index = _sgl_pool_alloc_index(&_sgl.pip_pool.pool);
    if (_SGL_INVALID_SLOT_INDEX != slot_index) {
        res.id =_sgl_slot_alloc(&_sgl.pip_pool.pool, &_sgl.pip_pool.pips[slot_index].slot, slot_index);
    }
    else {
        /* pool is exhausted */
        res.id = SG_INVALID_ID;
    }
    return res;
}

static void _sgl_init_pipeline(sgl_pipeline pip_id, const sg_pipeline_desc* in_desc) {
    SOKOL_ASSERT((pip_id.id != SG_INVALID_ID) && in_desc);

    /* create a new desc with 'patched' shader and pixel format state */
    sg_pipeline_desc desc = *in_desc;
    desc.layout.buffers[0].stride = sizeof(_sgl_vertex_t);
    {
        sg_vertex_attr_desc* pos = &desc.layout.attrs[0];
        pos->offset = offsetof(_sgl_vertex_t, pos);
        pos->format = SG_VERTEXFORMAT_FLOAT3;
    }
    {
        sg_vertex_attr_desc* uv = &desc.layout.attrs[1];
        uv->offset = offsetof(_sgl_vertex_t, uv);
        uv->format = SG_VERTEXFORMAT_FLOAT2;
    }
    {
        sg_vertex_attr_desc* rgba = &desc.layout.attrs[2];
        rgba->offset = offsetof(_sgl_vertex_t, rgba);
        rgba->format = SG_VERTEXFORMAT_UBYTE4N;
    }
    if (in_desc->shader.id == SG_INVALID_ID) {
        desc.shader = _sgl.shd;
    }
    desc.index_type = SG_INDEXTYPE_NONE;
    desc.blend.color_format = _sgl.desc.color_format;
    desc.blend.depth_format = _sgl.desc.depth_format;
    desc.rasterizer.sample_count = _sgl.desc.sample_count;
    if (desc.rasterizer.face_winding == _SG_FACEWINDING_DEFAULT) {
        desc.rasterizer.face_winding = _sgl.desc.face_winding;
    }
    if (desc.blend.color_write_mask == _SG_COLORMASK_DEFAULT) {
        desc.blend.color_write_mask = SG_COLORMASK_RGB;
    }

    _sgl_pipeline_t* pip = _sgl_lookup_pipeline(pip_id.id);
    SOKOL_ASSERT(pip && (pip->slot.state == SG_RESOURCESTATE_ALLOC));
    pip->slot.state = SG_RESOURCESTATE_VALID;
    for (int i = 0; i < SGL_NUM_PRIMITIVE_TYPES; i++) {
        switch (i) {
            case SGL_PRIMITIVETYPE_POINTS:
                desc.primitive_type = SG_PRIMITIVETYPE_POINTS;
                break;
            case SGL_PRIMITIVETYPE_LINES:
                desc.primitive_type = SG_PRIMITIVETYPE_LINES;
                break;
            case SGL_PRIMITIVETYPE_LINE_STRIP:
                desc.primitive_type = SG_PRIMITIVETYPE_LINE_STRIP;
                break;
            case SGL_PRIMITIVETYPE_TRIANGLES:
                desc.primitive_type = SG_PRIMITIVETYPE_TRIANGLES;
                break;
            case SGL_PRIMITIVETYPE_TRIANGLE_STRIP:
            case SGL_PRIMITIVETYPE_QUADS:
                desc.primitive_type = SG_PRIMITIVETYPE_TRIANGLE_STRIP;
                break;
        }
        if (SGL_PRIMITIVETYPE_QUADS == i) {
            /* quads are emulated via triangles, use the same pipeline object */
            pip->pip[i] = pip->pip[SGL_PRIMITIVETYPE_TRIANGLES];
        }
        else {
            pip->pip[i] = sg_make_pipeline(&desc);
            if (pip->pip[i].id == SG_INVALID_ID) {
                SOKOL_LOG("sokol_gl.h: failed to create pipeline object");
                pip->slot.state = SG_RESOURCESTATE_FAILED;
            }
        }
    }
}

static sgl_pipeline _sgl_make_pipeline(const sg_pipeline_desc* desc) {
    SOKOL_ASSERT(desc);
    sgl_pipeline pip_id = _sgl_alloc_pipeline();
    if (pip_id.id != SG_INVALID_ID) {
        _sgl_init_pipeline(pip_id, desc);
    }
    else {
        SOKOL_LOG("sokol_gl.h: pipeline pool exhausted!");
    }
    return pip_id;
}

static void _sgl_destroy_pipeline(sgl_pipeline pip_id) {
    _sgl_pipeline_t* pip = _sgl_lookup_pipeline(pip_id.id);
    if (pip) {
        for (int i = 0; i < SGL_NUM_PRIMITIVE_TYPES; i++) {
            if (i != SGL_PRIMITIVETYPE_QUADS) {
                sg_destroy_pipeline(pip->pip[i]);
            }
        }
        _sgl_reset_pipeline(pip);
        _sgl_pool_free_index(&_sgl.pip_pool.pool, _sgl_slot_index(pip_id.id));
    }
}

static sg_pipeline _sgl_get_pipeline(sgl_pipeline pip_id, _sgl_primitive_type_t prim_type) {
    _sgl_pipeline_t* pip = _sgl_lookup_pipeline(pip_id.id);
    if (pip) {
        return pip->pip[prim_type];
    }
    else {
        sg_pipeline dummy_pip;
        dummy_pip.id = SG_INVALID_ID;
        return dummy_pip;
    }
}

static inline void _sgl_begin(_sgl_primitive_type_t mode) {
    _sgl.in_begin = true;
    _sgl.base_vertex = _sgl.cur_vertex;
    _sgl.vtx_count = 0;
    _sgl.cur_prim_type = mode;
}

static void _sgl_rewind(void) {
    _sgl.base_vertex = 0;
    _sgl.cur_vertex = 0;
    _sgl.cur_uniform = 0;
    _sgl.cur_command = 0;
    _sgl.error = SGL_NO_ERROR;
    _sgl.matrix_dirty = true;
}

static inline _sgl_vertex_t* _sgl_next_vertex(void) {
    if (_sgl.cur_vertex < _sgl.num_vertices) {
        return &_sgl.vertices[_sgl.cur_vertex++];
    }
    else {
        _sgl.error = SGL_ERROR_VERTICES_FULL;
        return 0;
    }
}

static inline _sgl_uniform_t* _sgl_next_uniform(void) {
    if (_sgl.cur_uniform < _sgl.num_uniforms) {
        return &_sgl.uniforms[_sgl.cur_uniform++];
    }
    else {
        _sgl.error = SGL_ERROR_UNIFORMS_FULL;
        return 0;
    }
}

static inline _sgl_command_t* _sgl_prev_command(void) {
    if (_sgl.cur_command > 0) {
        return &_sgl.commands[_sgl.cur_command - 1];
    }
    else {
        return 0;
    }
}

static inline _sgl_command_t* _sgl_next_command(void) {
    if (_sgl.cur_command < _sgl.num_commands) {
        return &_sgl.commands[_sgl.cur_command++];
    }
    else {
        _sgl.error = SGL_ERROR_COMMANDS_FULL;
        return 0;
    }
}

static inline uint32_t _sgl_pack_rgbab(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    return (uint32_t)(((uint32_t)a<<24)|((uint32_t)b<<16)|((uint32_t)g<<8)|r);
}

static inline float _sgl_clamp(float v, float lo, float hi) {
    if (v < lo) return lo;
    else if (v > hi) return hi;
    else return v;
}

static inline uint32_t _sgl_pack_rgbaf(float r, float g, float b, float a) {
    uint8_t r_u8 = (uint8_t) (_sgl_clamp(r, 0.0f, 1.0f) * 255.0f);
    uint8_t g_u8 = (uint8_t) (_sgl_clamp(g, 0.0f, 1.0f) * 255.0f);
    uint8_t b_u8 = (uint8_t) (_sgl_clamp(b, 0.0f, 1.0f) * 255.0f);
    uint8_t a_u8 = (uint8_t) (_sgl_clamp(a, 0.0f, 1.0f) * 255.0f);
    return _sgl_pack_rgbab(r_u8, g_u8, b_u8, a_u8);
}

static inline void _sgl_vtx(float x, float y, float z, float u, float v, uint32_t rgba) {
    SOKOL_ASSERT(_sgl.in_begin);
    _sgl_vertex_t* vtx;
    /* handle non-native primitive types */
    if ((_sgl.cur_prim_type == SGL_PRIMITIVETYPE_QUADS) && ((_sgl.vtx_count & 3) == 3)) {
        /* for quads, before writing the last quad vertex, reuse
           the first and third vertex to start the second triangle in the quad
        */
        vtx = _sgl_next_vertex();
        if (vtx) { *vtx = *(vtx - 3); }
        vtx = _sgl_next_vertex();
        if (vtx) { *vtx = *(vtx - 2); }
    }
    vtx = _sgl_next_vertex();
    if (vtx) {
        vtx->pos[0] = x; vtx->pos[1] = y; vtx->pos[2] = z;
        vtx->uv[0] = u; vtx->uv[1] = v;
        vtx->rgba = rgba;
    }
    _sgl.vtx_count++;
}

static void _sgl_identity(_sgl_matrix_t* m) {
    for (int c = 0; c < 4; c++) {
        for (int r = 0; r < 4; r++) {
            m->v[c][r] = (r == c) ? 1.0f : 0.0f;
        }
    }
}

static void _sgl_transpose(_sgl_matrix_t* dst, const _sgl_matrix_t* m) {
    SOKOL_ASSERT(dst != m);
    for (int c = 0; c < 4; c++) {
        for (int r = 0; r < 4; r++) {
            dst->v[r][c] = m->v[c][r];
        }
    }
}

/* _sgl_rotate, _sgl_frustum, _sgl_ortho from MESA m_matric.c */
static void _sgl_matmul4(_sgl_matrix_t* p, const _sgl_matrix_t* a, const _sgl_matrix_t* b) {
    for (int r = 0; r < 4; r++) {
        float ai0=a->v[0][r], ai1=a->v[1][r], ai2=a->v[2][r], ai3=a->v[3][r];
        p->v[0][r] = ai0*b->v[0][0] + ai1*b->v[0][1] + ai2*b->v[0][2] + ai3*b->v[0][3];
        p->v[1][r] = ai0*b->v[1][0] + ai1*b->v[1][1] + ai2*b->v[1][2] + ai3*b->v[1][3];
        p->v[2][r] = ai0*b->v[2][0] + ai1*b->v[2][1] + ai2*b->v[2][2] + ai3*b->v[2][3];
        p->v[3][r] = ai0*b->v[3][0] + ai1*b->v[3][1] + ai2*b->v[3][2] + ai3*b->v[3][3];
    }
}

static void _sgl_mul(_sgl_matrix_t* dst, const _sgl_matrix_t* m) {
    _sgl_matmul4(dst, dst, m);
}

static void _sgl_rotate(_sgl_matrix_t* dst, float a, float x, float y, float z) {

    float s = sinf(a);
    float c = cosf(a);

    float mag = sqrtf(x*x + y*y + z*z);
    if (mag < 1.0e-4F) {
        return;
    }
    x /= mag;
    y /= mag;
    z /= mag;
    float xx = x * x;
    float yy = y * y;
    float zz = z * z;
    float xy = x * y;
    float yz = y * z;
    float zx = z * x;
    float xs = x * s;
    float ys = y * s;
    float zs = z * s;
    float one_c = 1.0f - c;

    _sgl_matrix_t m;
    m.v[0][0] = (one_c * xx) + c;
    m.v[1][0] = (one_c * xy) - zs;
    m.v[2][0] = (one_c * zx) + ys;
    m.v[3][0] = 0.0f;
    m.v[0][1] = (one_c * xy) + zs;
    m.v[1][1] = (one_c * yy) + c;
    m.v[2][1] = (one_c * yz) - xs;
    m.v[3][1] = 0.0f;
    m.v[0][2] = (one_c * zx) - ys;
    m.v[1][2] = (one_c * yz) + xs;
    m.v[2][2] = (one_c * zz) + c;
    m.v[3][2] = 0.0f;
    m.v[0][3] = 0.0f;
    m.v[1][3] = 0.0f;
    m.v[2][3] = 0.0f;
    m.v[3][3] = 1.0f;
    _sgl_mul(dst, &m);
}

static void _sgl_scale(_sgl_matrix_t* dst, float x, float y, float z) {
    for (int r = 0; r < 4; r++) {
        dst->v[0][r] *= x;
        dst->v[1][r] *= y;
        dst->v[2][r] *= z;
    }
}

static void _sgl_translate(_sgl_matrix_t* dst, float x, float y, float z) {
    for (int r = 0; r < 4; r++) {
        dst->v[3][r] = dst->v[0][r]*x + dst->v[1][r]*y + dst->v[2][r]*z + dst->v[3][r];
    }
}

static void _sgl_frustum(_sgl_matrix_t* dst, float left, float right, float bottom, float top, float znear, float zfar) {
    float x = (2.0f * znear) / (right - left);
    float y = (2.0f * znear) / (top - bottom);
    float a = (right + left) / (right - left);
    float b = (top + bottom) / (top - bottom);
    float c = -(zfar + znear) / (zfar - znear);
    float d = -(2.0f * zfar * znear) / (zfar - znear);
    _sgl_matrix_t m;
    m.v[0][0] = x;    m.v[0][1] = 0.0f; m.v[0][2] = 0.0f; m.v[0][3] = 0.0f;
    m.v[1][0] = 0.0f; m.v[1][1] = y;    m.v[1][2] = 0.0f; m.v[1][3] = 0.0f;
    m.v[2][0] = a;    m.v[2][1] = b;    m.v[2][2] = c;    m.v[2][3] = -1.0f;
    m.v[3][0] = 0.0f; m.v[3][1] = 0.0f; m.v[3][2] = d;    m.v[3][3] = 0.0f;
    _sgl_mul(dst, &m);
}

static void _sgl_ortho(_sgl_matrix_t* dst, float left, float right, float bottom, float top, float znear, float zfar) {
    _sgl_matrix_t m;
    m.v[0][0] = 2.0f / (right - left);
    m.v[1][0] = 0.0f;
    m.v[2][0] = 0.0f;
    m.v[3][0] = -(right + left) / (right - left);
    m.v[0][1] = 0.0f;
    m.v[1][1] = 2.0f / (top - bottom);
    m.v[2][1] = 0.0f;
    m.v[3][1] = -(top + bottom) / (top - bottom);
    m.v[0][2] = 0.0f;
    m.v[1][2] = 0.0f;
    m.v[2][2] = -2.0f / (zfar - znear);
    m.v[3][2] = -(zfar + znear) / (zfar - znear);
    m.v[0][3] = 0.0f;
    m.v[1][3] = 0.0f;
    m.v[2][3] = 0.0f;
    m.v[3][3] = 1.0f;

    _sgl_mul(dst, &m);
}

/* _sgl_perspective, _sgl_lookat from Regal project.c */
static void _sgl_perspective(_sgl_matrix_t* dst, float fovy, float aspect, float znear, float zfar) {
    float sine = sinf(fovy / 2.0f);
    float delta_z = zfar - znear;
    if ((delta_z == 0.0f) || (sine == 0.0f) || (aspect == 0.0f)) {
        return;
    }
    float cotan = cosf(fovy / 2.0f) / sine;
    _sgl_matrix_t m;
    _sgl_identity(&m);
    m.v[0][0] = cotan / aspect;
    m.v[1][1] = cotan;
    m.v[2][2] = -(zfar + znear) / delta_z;
    m.v[2][3] = -1.0f;
    m.v[3][2] = -2.0f * znear * zfar / delta_z;
    m.v[3][3] = 0.0f;
    _sgl_mul(dst, &m);
}

static void _sgl_normalize(float v[3]) {
    float r = sqrtf(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    if (r == 0.0f) {
        return;
    }
    v[0] /= r;
    v[1] /= r;
    v[2] /= r;
}

static void _sgl_cross(float v1[3], float v2[3], float res[3]) {
    res[0] = v1[1]*v2[2] - v1[2]*v2[1];
    res[1] = v1[2]*v2[0] - v1[0]*v2[2];
    res[2] = v1[0]*v2[1] - v1[1]*v2[0];
}

static void _sgl_lookat(_sgl_matrix_t* dst,
                        float eye_x, float eye_y, float eye_z,
                        float center_x, float center_y, float center_z,
                        float up_x, float up_y, float up_z)
{
    float fwd[3], side[3], up[3];

    fwd[0] = center_x - eye_x; fwd[1] = center_y - eye_y; fwd[2] = center_z - eye_z;
    up[0] = up_x; up[1] = up_y; up[2] = up_z;
    _sgl_normalize(fwd);
    _sgl_cross(fwd, up, side);
    _sgl_normalize(side);
    _sgl_cross(side, fwd, up);

    _sgl_matrix_t m;
    _sgl_identity(&m);
    m.v[0][0] = side[0];
    m.v[1][0] = side[1];
    m.v[2][0] = side[2];
    m.v[0][1] = up[0];
    m.v[1][1] = up[1];
    m.v[2][1] = up[2];
    m.v[0][2] = -fwd[0];
    m.v[1][2] = -fwd[1];
    m.v[2][2] = -fwd[2];
    _sgl_mul(dst, &m);
    _sgl_translate(dst, -eye_x, -eye_y, -eye_z);
}

/* current top-of-stack projection matrix */
static inline _sgl_matrix_t* _sgl_matrix_projection(void) {
    return &_sgl.matrix_stack[SGL_MATRIXMODE_PROJECTION][_sgl.matrix_tos[SGL_MATRIXMODE_PROJECTION]];
}

/* get top-of-stack modelview matrix */
static inline _sgl_matrix_t* _sgl_matrix_modelview(void) {
    return &_sgl.matrix_stack[SGL_MATRIXMODE_MODELVIEW][_sgl.matrix_tos[SGL_MATRIXMODE_MODELVIEW]];
}

/* get top-of-stack texture matrix */
static inline _sgl_matrix_t* _sgl_matrix_texture(void) {
    return &_sgl.matrix_stack[SGL_MATRIXMODE_TEXTURE][_sgl.matrix_tos[SGL_MATRIXMODE_TEXTURE]];
}

/* get pointer to current top-of-stack of current matrix mode */
static inline _sgl_matrix_t* _sgl_matrix(void) {
    return &_sgl.matrix_stack[_sgl.cur_matrix_mode][_sgl.matrix_tos[_sgl.cur_matrix_mode]];
}

/*== PUBLIC FUNCTIONS ========================================================*/
SOKOL_API_IMPL void sgl_setup(const sgl_desc_t* desc) {
    SOKOL_ASSERT(desc);
    memset(&_sgl, 0, sizeof(_sgl));
    _sgl.init_cookie = _SGL_INIT_COOKIE;
    _sgl.desc = *desc;
    _sgl.desc.pipeline_pool_size = _sgl_def(_sgl.desc.pipeline_pool_size, _SGL_DEFAULT_PIPELINE_POOL_SIZE);
    _sgl.desc.max_vertices = _sgl_def(_sgl.desc.max_vertices, _SGL_DEFAULT_MAX_VERTICES);
    _sgl.desc.max_commands = _sgl_def(_sgl.desc.max_commands, _SGL_DEFAULT_MAX_COMMANDS);
    _sgl.desc.face_winding = _sgl_def(_sgl.desc.face_winding, SG_FACEWINDING_CCW);

    /* allocate buffers and pools */
    _sgl.num_vertices = _sgl.desc.max_vertices;
    _sgl.num_uniforms = _sgl.desc.max_commands;
    _sgl.num_commands = _sgl.num_uniforms;
    _sgl.vertices = (_sgl_vertex_t*) SOKOL_MALLOC(_sgl.num_vertices * sizeof(_sgl_vertex_t));
    SOKOL_ASSERT(_sgl.vertices);
    _sgl.uniforms = (_sgl_uniform_t*) SOKOL_MALLOC(_sgl.num_uniforms * sizeof(_sgl_uniform_t));
    SOKOL_ASSERT(_sgl.uniforms);
    _sgl.commands = (_sgl_command_t*) SOKOL_MALLOC(_sgl.num_commands * sizeof(_sgl_command_t));
    SOKOL_ASSERT(_sgl.commands);
    _sgl_setup_pipeline_pool(&_sgl.desc);

    /* create sokol-gfx resource objects */
    sg_push_debug_group("sokol-gl");

    sg_buffer_desc vbuf_desc;
    memset(&vbuf_desc, 0, sizeof(vbuf_desc));
    vbuf_desc.size = _sgl.num_vertices * sizeof(_sgl_vertex_t);
    vbuf_desc.type = SG_BUFFERTYPE_VERTEXBUFFER;
    vbuf_desc.usage = SG_USAGE_STREAM;
    vbuf_desc.label = "sgl-vertex-buffer";
    _sgl.vbuf = sg_make_buffer(&vbuf_desc);
    SOKOL_ASSERT(SG_INVALID_ID != _sgl.vbuf.id);

    uint32_t pixels[64];
    for (int i = 0; i < 64; i++) {
        pixels[i] = 0xFFFFFFFF;
    }
    sg_image_desc img_desc;
    memset(&img_desc, 0, sizeof(img_desc));
    img_desc.type = SG_IMAGETYPE_2D;
    img_desc.width = 8;
    img_desc.height = 8;
    img_desc.num_mipmaps = 1;
    img_desc.pixel_format = SG_PIXELFORMAT_RGBA8;
    img_desc.min_filter = SG_FILTER_NEAREST;
    img_desc.mag_filter = SG_FILTER_NEAREST;
    img_desc.content.subimage[0][0].ptr = pixels;
    img_desc.content.subimage[0][0].size = sizeof(pixels);
    img_desc.label = "sgl-default-texture";
    _sgl.def_img = sg_make_image(&img_desc);
    SOKOL_ASSERT(SG_INVALID_ID != _sgl.def_img.id);
    _sgl.cur_img = _sgl.def_img;

    sg_shader_desc shd_desc;
    memset(&shd_desc, 0, sizeof(shd_desc));
    shd_desc.attrs[0].name = "position";
    shd_desc.attrs[1].name = "texcoord0";
    shd_desc.attrs[2].name = "color0";
    shd_desc.attrs[0].sem_name = "POSITION";
    shd_desc.attrs[1].sem_name = "TEXCOORD";
    shd_desc.attrs[2].sem_name = "COLOR";
    sg_shader_uniform_block_desc* ub = &shd_desc.vs.uniform_blocks[0];
    ub->size = sizeof(_sgl_uniform_t);
    ub->uniforms[0].name = "mvp";
    ub->uniforms[0].type = SG_UNIFORMTYPE_MAT4;
    ub->uniforms[1].name = "tm";
    ub->uniforms[1].type = SG_UNIFORMTYPE_MAT4;
    shd_desc.fs.images[0].name = "tex";
    shd_desc.fs.images[0].type = SG_IMAGETYPE_2D;
    #if defined(SOKOL_D3D11)
        shd_desc.vs.byte_code = _sgl_vs_bin;
        shd_desc.vs.byte_code_size = sizeof(_sgl_vs_bin);
        shd_desc.fs.byte_code = _sgl_fs_bin;
        shd_desc.fs.byte_code_size = sizeof(_sgl_fs_bin);
    #else
        shd_desc.vs.source = _sgl_vs_src;
        shd_desc.fs.source = _sgl_fs_src;
    #endif
    shd_desc.label = "sgl-shader";
    _sgl.shd = sg_make_shader(&shd_desc);

    /* create default pipeline object */
    sg_pipeline_desc def_pip_desc;
    memset(&def_pip_desc, 0, sizeof(def_pip_desc));
    def_pip_desc.depth_stencil.depth_write_enabled = true;
    _sgl.def_pip = _sgl_make_pipeline(&def_pip_desc);
    sg_pop_debug_group();

    /* default state */
    _sgl.rgba = 0xFFFFFFFF;
    for (int i = 0; i < SGL_NUM_MATRIXMODES; i++) {
        _sgl_identity(&_sgl.matrix_stack[i][0]);
    }
    _sgl.pip_stack[0] = _sgl.def_pip;
    _sgl.matrix_dirty = true;
}

SOKOL_API_IMPL void sgl_shutdown(void) {
    SOKOL_ASSERT(_sgl.init_cookie == 0xABCDABCD);
    SOKOL_FREE(_sgl.vertices); _sgl.vertices = 0;
    SOKOL_FREE(_sgl.uniforms); _sgl.uniforms = 0;
    SOKOL_FREE(_sgl.commands); _sgl.commands = 0;
    sg_destroy_buffer(_sgl.vbuf);
    sg_destroy_image(_sgl.def_img);
    sg_destroy_shader(_sgl.shd);
    _sgl_destroy_pipeline(_sgl.def_pip);
    // FIXME: need to destroy ALL valid pipeline objects in pool here
    _sgl_discard_pipeline_pool();
    _sgl.init_cookie = 0;
}

SOKOL_API_IMPL sgl_error_t sgl_error(void) {
    return _sgl.error;
}

SOKOL_API_IMPL float sgl_rad(float deg) {
    return (deg * (float)M_PI) / 180.0f;
}

SOKOL_API_IMPL float sgl_deg(float rad) {
    return (rad * 180.0f) / (float)M_PI;
}

SOKOL_API_IMPL sgl_pipeline sgl_make_pipeline(const sg_pipeline_desc* desc) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    return _sgl_make_pipeline(desc);
}

SOKOL_API_IMPL void sgl_destroy_pipeline(sgl_pipeline pip_id) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl_destroy_pipeline(pip_id);
}

SOKOL_API_IMPL void sgl_load_pipeline(sgl_pipeline pip_id) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT((_sgl.pip_tos >= 0) && (_sgl.pip_tos < _SGL_MAX_STACK_DEPTH));
    _sgl.pip_stack[_sgl.pip_tos] = pip_id;
}

SOKOL_API_IMPL void sgl_default_pipeline(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT((_sgl.pip_tos >= 0) && (_sgl.pip_tos < _SGL_MAX_STACK_DEPTH));
    _sgl.pip_stack[_sgl.pip_tos] = _sgl.def_pip;
}

SOKOL_API_IMPL void sgl_push_pipeline(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    if (_sgl.pip_tos < (_SGL_MAX_STACK_DEPTH - 1)) {
        _sgl.pip_tos++;
        _sgl.pip_stack[_sgl.pip_tos] = _sgl.pip_stack[_sgl.pip_tos-1];
    }
    else {
        _sgl.error = SGL_ERROR_STACK_OVERFLOW;
    }
}

SOKOL_API_IMPL void sgl_pop_pipeline(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    if (_sgl.pip_tos > 0) {
        _sgl.pip_tos--;
    }
    else {
        _sgl.error = SGL_ERROR_STACK_UNDERFLOW;
    }
}

SOKOL_API_IMPL void sgl_defaults(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl.u = 0.0f; _sgl.v = 0.0f;
    _sgl.rgba = 0xFFFFFFFF;
    _sgl.texturing_enabled = false;
    _sgl.cur_img = _sgl.def_img;
    sgl_default_pipeline();
    _sgl_identity(_sgl_matrix_texture());
    _sgl_identity(_sgl_matrix_modelview());
    _sgl_identity(_sgl_matrix_projection());
    _sgl.cur_matrix_mode = SGL_MATRIXMODE_MODELVIEW;
    _sgl.matrix_dirty = true;
}

SOKOL_API_IMPL void sgl_viewport(int x, int y, int w, int h, bool origin_top_left) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_command_t* cmd = _sgl_next_command();
    if (cmd) {
        cmd->cmd = SGL_COMMAND_VIEWPORT;
        cmd->args.viewport.x = x;
        cmd->args.viewport.y = y;
        cmd->args.viewport.w = w;
        cmd->args.viewport.h = h;
        cmd->args.viewport.origin_top_left = origin_top_left;
    }
}

SOKOL_API_IMPL void sgl_scissor_rect(int x, int y, int w, int h, bool origin_top_left) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_command_t* cmd = _sgl_next_command();
    if (cmd) {
        cmd->cmd = SGL_COMMAND_SCISSOR_RECT;
        cmd->args.scissor_rect.x = x;
        cmd->args.scissor_rect.y = y;
        cmd->args.scissor_rect.w = w;
        cmd->args.scissor_rect.h = h;
        cmd->args.scissor_rect.origin_top_left = origin_top_left;
    }
}

SOKOL_API_IMPL void sgl_enable_texture(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl.texturing_enabled = true;
}

SOKOL_API_IMPL void sgl_disable_texture(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl.texturing_enabled = false;
}

SOKOL_API_IMPL void sgl_texture(sg_image img) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    if (SG_INVALID_ID != img.id) {
        _sgl.cur_img = img;
    }
    else {
        _sgl.cur_img = _sgl.def_img;
    }
}

SOKOL_API_IMPL void sgl_begin_points(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_POINTS);
}

SOKOL_API_IMPL void sgl_begin_lines(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_LINES);
}

SOKOL_API_IMPL void sgl_begin_line_strip(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_LINE_STRIP);
}

SOKOL_API_IMPL void sgl_begin_triangles(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_TRIANGLES);
}

SOKOL_API_IMPL void sgl_begin_triangle_strip(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_TRIANGLE_STRIP);
}

SOKOL_API_IMPL void sgl_begin_quads(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(!_sgl.in_begin);
    _sgl_begin(SGL_PRIMITIVETYPE_QUADS);
}

SOKOL_API_IMPL void sgl_end(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT(_sgl.in_begin);
    _sgl.in_begin = false;
    if (_sgl.base_vertex == _sgl.cur_vertex) {
        return;
    }
    bool matrix_dirty = _sgl.matrix_dirty;
    if (matrix_dirty) {
        _sgl.matrix_dirty = false;
        _sgl_uniform_t* uni = _sgl_next_uniform();
        if (uni) {
            _sgl_matmul4(&uni->mvp, _sgl_matrix_projection(), _sgl_matrix_modelview());
            uni->tm = *_sgl_matrix_texture();
        }
    }
    /* check if command can be merged with previous command */
    sg_pipeline pip = _sgl_get_pipeline(_sgl.pip_stack[_sgl.pip_tos], _sgl.cur_prim_type);
    sg_image img = _sgl.texturing_enabled ? _sgl.cur_img : _sgl.def_img;
    _sgl_command_t* prev_cmd = _sgl_prev_command();
    bool merge_cmd = false;
    if (prev_cmd) {
        if ((prev_cmd->cmd == SGL_COMMAND_DRAW) &&
            (_sgl.cur_prim_type != SGL_PRIMITIVETYPE_LINE_STRIP) &&
            (_sgl.cur_prim_type != SGL_PRIMITIVETYPE_TRIANGLE_STRIP) &&
            !matrix_dirty &&
            (prev_cmd->args.draw.img.id == img.id) &&
            (prev_cmd->args.draw.pip.id == pip.id))
        {
            merge_cmd = true;
        }
    }
    if (merge_cmd) {
        /* draw command can be merged with the previous command */
        prev_cmd->args.draw.num_vertices += _sgl.cur_vertex - _sgl.base_vertex;
    }
    else {
        /* append a new draw command */
        _sgl_command_t* cmd = _sgl_next_command();
        if (cmd) {
            SOKOL_ASSERT(_sgl.cur_uniform > 0);
            cmd->cmd = SGL_COMMAND_DRAW;
            cmd->args.draw.img = img;
            cmd->args.draw.pip = _sgl_get_pipeline(_sgl.pip_stack[_sgl.pip_tos], _sgl.cur_prim_type);
            cmd->args.draw.base_vertex = _sgl.base_vertex;
            cmd->args.draw.num_vertices = _sgl.cur_vertex - _sgl.base_vertex;
            cmd->args.draw.uniform_index = _sgl.cur_uniform - 1;
        }
    }
}

SOKOL_API_IMPL void sgl_t2f(float u, float v) {
    _sgl.u = u; _sgl.v = v;
}

SOKOL_API_IMPL void sgl_c3f(float r, float g, float b) {
    _sgl.rgba = _sgl_pack_rgbaf(r, g, b, 1.0f);
}

SOKOL_API_IMPL void sgl_c4f(float r, float g, float b, float a) {
    _sgl.rgba = _sgl_pack_rgbaf(r, g, b, a);
}

SOKOL_API_IMPL void sgl_c3b(uint8_t r, uint8_t g, uint8_t b) {
    _sgl.rgba = _sgl_pack_rgbab(r, g, b, 255);
}

SOKOL_API_IMPL void sgl_c4b(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    _sgl.rgba = _sgl_pack_rgbab(r, g, b, a);
}

SOKOL_API_IMPL void sgl_c1i(uint32_t rgba) {
    _sgl.rgba = rgba;
}

SOKOL_API_IMPL void sgl_v2f(float x, float y) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, _sgl.rgba);
}

SOKOL_API_IMPL void sgl_v3f(float x, float y, float z) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, _sgl.rgba);
}

SOKOL_API_IMPL void sgl_v2f_t2f(float x, float y, float u, float v) {
    _sgl_vtx(x, y, 0.0f, u, v, _sgl.rgba);
}

SOKOL_API_IMPL void sgl_v3f_t2f(float x, float y, float z, float u, float v) {
    _sgl_vtx(x, y, z, u, v, _sgl.rgba);
}

SOKOL_API_IMPL void sgl_v2f_c3f(float x, float y, float r, float g, float b) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, _sgl_pack_rgbaf(r, g, b, 1.0f));
}

SOKOL_API_IMPL void sgl_v2f_c3b(float x, float y, uint8_t r, uint8_t g, uint8_t b) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, _sgl_pack_rgbab(r, g, b, 255));
}

SOKOL_API_IMPL void sgl_v2f_c4f(float x, float y, float r, float g, float b, float a) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, _sgl_pack_rgbaf(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v2f_c4b(float x, float y, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, _sgl_pack_rgbab(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v2f_c1i(float x, float y, uint32_t rgba) {
    _sgl_vtx(x, y, 0.0f, _sgl.u, _sgl.v, rgba);
}

SOKOL_API_IMPL void sgl_v3f_c3f(float x, float y, float z, float r, float g, float b) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, _sgl_pack_rgbaf(r, g, b, 1.0f));
}

SOKOL_API_IMPL void sgl_v3f_c3b(float x, float y, float z, uint8_t r, uint8_t g, uint8_t b) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, _sgl_pack_rgbab(r, g, b, 255));
}

SOKOL_API_IMPL void sgl_v3f_c4f(float x, float y, float z, float r, float g, float b, float a) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, _sgl_pack_rgbaf(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v3f_c4b(float x, float y, float z, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, _sgl_pack_rgbab(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v3f_c1i(float x, float y, float z, uint32_t rgba) {
    _sgl_vtx(x, y, z, _sgl.u, _sgl.v, rgba);
}

SOKOL_API_IMPL void sgl_v2f_t2f_c3f(float x, float y, float u, float v, float r, float g, float b) {
    _sgl_vtx(x, y, 0.0f, u, v, _sgl_pack_rgbaf(r, g, b, 1.0f));
}

SOKOL_API_IMPL void sgl_v2f_t2f_c3b(float x, float y, float u, float v, uint8_t r, uint8_t g, uint8_t b) {
    _sgl_vtx(x, y, 0.0f, u, v, _sgl_pack_rgbab(r, g, b, 255));
}

SOKOL_API_IMPL void sgl_v2f_t2f_c4f(float x, float y, float u, float v, float r, float g, float b, float a) {
    _sgl_vtx(x, y, 0.0f, u, v, _sgl_pack_rgbaf(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v2f_t2f_c4b(float x, float y, float u, float v, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    _sgl_vtx(x, y, 0.0f, u, v, _sgl_pack_rgbab(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v2f_t2f_c1i(float x, float y, float u, float v, uint32_t rgba) {
    _sgl_vtx(x, y, 0.0f, u, v, rgba);
}

SOKOL_API_IMPL void sgl_v3f_t2f_c3f(float x, float y, float z, float u, float v, float r, float g, float b) {
    _sgl_vtx(x, y, z, u, v, _sgl_pack_rgbaf(r, g, b, 1.0f));
}

SOKOL_API_IMPL void sgl_v3f_t2f_c3b(float x, float y, float z, float u, float v, uint8_t r, uint8_t g, uint8_t b) {
    _sgl_vtx(x, y, z, u, v, _sgl_pack_rgbab(r, g, b, 255));
}

SOKOL_API_IMPL void sgl_v3f_t2f_c4f(float x, float y, float z, float u, float v, float r, float g, float b, float a) {
    _sgl_vtx(x, y, z, u, v, _sgl_pack_rgbaf(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v3f_t2f_c4b(float x, float y, float z, float u, float v, uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    _sgl_vtx(x, y, z, u, v, _sgl_pack_rgbab(r, g, b, a));
}

SOKOL_API_IMPL void sgl_v3f_t2f_c1i(float x, float y, float z, float u, float v, uint32_t rgba) {
    _sgl_vtx(x, y, z, u, v, rgba);
}

SOKOL_API_IMPL void sgl_matrix_mode_modelview(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.cur_matrix_mode = SGL_MATRIXMODE_MODELVIEW;
}

SOKOL_API_IMPL void sgl_matrix_mode_projection(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.cur_matrix_mode = SGL_MATRIXMODE_PROJECTION;
}

SOKOL_API_IMPL void sgl_matrix_mode_texture(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.cur_matrix_mode = SGL_MATRIXMODE_TEXTURE;
}

SOKOL_API_IMPL void sgl_load_identity(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_identity(_sgl_matrix());
}

SOKOL_API_IMPL void sgl_load_matrix(const float m[16]) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    memcpy(&_sgl_matrix()->v[0][0], &m[0], 64);
}

SOKOL_API_IMPL void sgl_load_transpose_matrix(const float m[16]) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_transpose(_sgl_matrix(), (const _sgl_matrix_t*) &m[0]);
}

SOKOL_API_IMPL void sgl_mult_matrix(const float m[16]) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    const _sgl_matrix_t* m0  = (const _sgl_matrix_t*) &m[0];
    _sgl_mul(_sgl_matrix(), m0);
}

SOKOL_API_IMPL void sgl_mult_transpose_matrix(const float m[16]) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_matrix_t m0;
    _sgl_transpose(&m0, (const _sgl_matrix_t*) &m[0]);
    _sgl_mul(_sgl_matrix(), &m0);
}

SOKOL_API_IMPL void sgl_rotate(float angle_rad, float x, float y, float z) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_rotate(_sgl_matrix(), angle_rad, x, y, z);
}

SOKOL_API_IMPL void sgl_scale(float x, float y, float z) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_scale(_sgl_matrix(), x, y, z);
}

SOKOL_API_IMPL void sgl_translate(float x, float y, float z) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_translate(_sgl_matrix(), x, y, z);
}

SOKOL_API_IMPL void sgl_frustum(float l, float r, float b, float t, float n, float f) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_frustum(_sgl_matrix(), l, r, b, t, n, f);
}

SOKOL_API_IMPL void sgl_ortho(float l, float r, float b, float t, float n, float f) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_ortho(_sgl_matrix(), l, r, b, t, n, f);
}

SOKOL_API_IMPL void sgl_perspective(float fov_y, float aspect, float z_near, float z_far) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_perspective(_sgl_matrix(), fov_y, aspect, z_near, z_far);
}

SOKOL_API_IMPL void sgl_lookat(float eye_x, float eye_y, float eye_z, float center_x, float center_y, float center_z, float up_x, float up_y, float up_z) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    _sgl.matrix_dirty = true;
    _sgl_lookat(_sgl_matrix(), eye_x, eye_y, eye_z, center_x, center_y, center_z, up_x, up_y, up_z);
}

SOKOL_API_DECL void sgl_push_matrix(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT((_sgl.cur_matrix_mode >= 0) && (_sgl.cur_matrix_mode < SGL_NUM_MATRIXMODES));
    _sgl.matrix_dirty = true;
    if (_sgl.matrix_tos[_sgl.cur_matrix_mode] < (_SGL_MAX_STACK_DEPTH - 1)) {
        const _sgl_matrix_t* src = _sgl_matrix();
        _sgl.matrix_tos[_sgl.cur_matrix_mode]++;
        _sgl_matrix_t* dst = _sgl_matrix();
        *dst = *src;
    }
    else {
        _sgl.error = SGL_ERROR_STACK_OVERFLOW;
    }
}

SOKOL_API_DECL void sgl_pop_matrix(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    SOKOL_ASSERT((_sgl.cur_matrix_mode >= 0) && (_sgl.cur_matrix_mode < SGL_NUM_MATRIXMODES));
    _sgl.matrix_dirty = true;
    if (_sgl.matrix_tos[_sgl.cur_matrix_mode] > 0) {
        _sgl.matrix_tos[_sgl.cur_matrix_mode]--;
    }
    else {
        _sgl.error = SGL_ERROR_STACK_UNDERFLOW;
    }
}

/* this renders the accumulated draw commands via sokol-gfx */
SOKOL_API_IMPL void sgl_draw(void) {
    SOKOL_ASSERT(_SGL_INIT_COOKIE == _sgl.init_cookie);
    if ((_sgl.error == SGL_NO_ERROR) && (_sgl.cur_vertex > 0) && (_sgl.cur_command > 0)) {
        uint32_t cur_pip_id = SG_INVALID_ID;
        uint32_t cur_img_id = SG_INVALID_ID;
        int cur_uniform_index = -1;
        sg_push_debug_group("sokol-gl");
        sg_update_buffer(_sgl.vbuf, _sgl.vertices, _sgl.cur_vertex * sizeof(_sgl_vertex_t));
        _sgl.bind.vertex_buffers[0] = _sgl.vbuf;
        for (int i = 0; i < _sgl.cur_command; i++) {
            const _sgl_command_t* cmd = &_sgl.commands[i];
            switch (cmd->cmd) {
                case SGL_COMMAND_VIEWPORT:
                    {
                        const _sgl_viewport_args_t* args = &cmd->args.viewport;
                        sg_apply_viewport(args->x, args->y, args->w, args->h, args->origin_top_left);
                    }
                    break;
                case SGL_COMMAND_SCISSOR_RECT:
                    {
                        const _sgl_scissor_rect_args_t* args = &cmd->args.scissor_rect;
                        sg_apply_scissor_rect(args->x, args->y, args->w, args->h, args->origin_top_left);
                    }
                    break;
                case SGL_COMMAND_DRAW:
                    {
                        const _sgl_draw_args_t* args = &cmd->args.draw;
                        if (args->pip.id != cur_pip_id) {
                            sg_apply_pipeline(args->pip);
                            cur_pip_id = args->pip.id;
                            /* when pipeline changes, also need to re-apply uniforms and bindings */
                            cur_img_id = SG_INVALID_ID;
                            cur_uniform_index = -1;
                        }
                        if (cur_img_id != args->img.id) {
                            _sgl.bind.fs_images[0] = args->img;
                            sg_apply_bindings(&_sgl.bind);
                            cur_img_id = args->img.id;
                        }
                        if (cur_uniform_index != args->uniform_index) {
                            sg_apply_uniforms(SG_SHADERSTAGE_VS, 0, &_sgl.uniforms[args->uniform_index], sizeof(_sgl_uniform_t));
                            cur_uniform_index = args->uniform_index;
                        }
                        /* FIXME: what if number of vertices doesn't match the primitive type? */
                        sg_draw(args->base_vertex, args->num_vertices, 1);
                    }
                    break;
            }
        }
        sg_pop_debug_group();
    }
    _sgl_rewind();
}
#endif /* SOKOL_GL_IMPL */

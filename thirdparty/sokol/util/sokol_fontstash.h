#ifndef SOKOL_FONTSTASH_INCLUDED
/*
    sokol_fontstash.h -- renderer for https://github.com/memononen/fontstash
                         on top of sokol_gl.h

    Project URL: https://github.com/floooh/sokol

    Do this:

        #define SOKOL_FONTSTASH_IMPL

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

    Include the following headers before including sokol_fontstash.h:

        sokol_gfx.h

    Additionally include the following headers for including the sokol_fontstash.h
    implementation:

        sokol_gl.h

    HOW TO
    ======
    --- First initialize sokol-gfx and sokol-gl as usual:

            sg_setup(&(sg_desc){...});
            sgl_setup(&(sgl_desc){...});

    --- Create at least one fontstash context with sfons_create() (this replaces
        glfonsCreate() from fontstash.h's example GL renderer:

            FONScontext* ctx = sfons_create(atlas_width, atlas_height, FONS_ZERO_TOPLEFT);

        Each FONScontext manages one font atlas texture which can hold rasterized
        glyphs for multiple fonts.

    --- From here on, use fontstash.h's functions "as usual" to add TTF
        font data and draw text. Note that (just like with sokol-gl), text
        rendering can happen anywhere in the frame, not only inside
        a sokol-gfx rendering pass.

    --- You can use the helper function

            uint32_t sfons_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a)

        To convert a 0..255 RGBA color into a packed uint32_t color value
        expected by fontstash.h.

    --- Once per frame before calling sgl_draw(), call:

            sfons_flush(FONScontext* ctx)

        ...this will update the dynamic sokol-gfx texture with the latest font
        atlas content.

    --- To actually render the text (and any other sokol-gl draw commands),
        call sgl_draw() inside a sokol-gfx frame.

    --- NOTE that you can mix fontstash.h calls with sokol-gl calls to mix
        text rendering with sokol-gl rendering. You can also use
        sokol-gl's matrix stack to position fontstash.h text in 3D.

    --- finally on application shutdown, call:

            sfons_shutdown()

        before sgl_shutdown() and sg_shutdown()


    WHAT HAPPENS UNDER THE HOOD:
    ============================

    sfons_create():
        - creates a sokol-gfx shader compatible with sokol-gl
        - creates an sgl_pipeline object with alpha-blending using
          this shader
        - creates a 1-byte-per-pixel font atlas texture via sokol-gfx
          (pixel format SG_PIXELFORMAT_R8)

    fonsDrawText():
        - this will call the following sequence of sokol-gl functions:

            sgl_enable_texture();
            sgl_texture(...);
            sgl_push_pipeline();
            sgl_load_pipeline(...);
            sgl_begin_triangles();
            for each vertex:
                sg_v2f_t2f_c1i(...);
            sgl_end();
            sgl_pop_pipeline();
            sgl_disable_texture();

        - note that sokol-gl will merge several sgl_*_begin/sgl_end pairs
          into a single draw call if no relevant state has changed, typically
          all calls to fonsDrawText() will be merged into a single draw call
          as long as all calls use the same FONScontext

    sfons_flush():
        - this will call sg_update_image() on the font atlas texture
          if fontstash.h has added any rasterized glyphs since the last
          frame

    sfons_shutdown():
        - destroy the font atlas texture, sgl_pipeline and sg_shader objects

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
#define SOKOL_FONTSTASH_INCLUDED (1)
#include <stdint.h>
#include <stdlib.h>

#if !defined(SOKOL_GFX_INCLUDED)
#error "Please include sokol_gfx.h before sokol_fontstash.h"
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

SOKOL_API_DECL FONScontext* sfons_create(int width, int height, int flags);
SOKOL_API_DECL void sfons_destroy(FONScontext* ctx);
SOKOL_API_DECL void sfons_flush(FONScontext* ctx);
SOKOL_API_DECL uint32_t sfons_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

#ifdef __cplusplus
} /* extern "C" */
#endif
#endif /* SOKOL_FONTSTASH_INCLUDED */

/*-- IMPLEMENTATION ----------------------------------------------------------*/
#ifdef SOKOL_FONTSTASH_IMPL
#define SOKOL_FONTSTASH_IMPL_INCLUDED (1)
#include <string.h>     /* memset, memcpy */

#if !defined(SOKOL_GL_INCLUDED)
#error "Please include sokol_gl.h before sokol_fontstash.h"
#endif
#if !defined(FONS_H)
#error "Please include fontstash.h before sokol_fontstash.h"
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

#if defined(SOKOL_GLCORE33)
static const char* _sfons_vs_src =
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
static const char* _sfons_fs_src =
    "#version 330\n"
    "uniform sampler2D tex;\n"
    "in vec4 uv;\n"
    "in vec4 color;\n"
    "out vec4 frag_color;\n"
    "void main() {\n"
    "    frag_color = vec4(1.0, 1.0, 1.0, texture(tex, uv.xy).r) * color;\n"
    "}\n";
#elif defined(SOKOL_GLES2) || defined(SOKOL_GLES3)
static const char* _sfons_vs_src =
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
static const char* _sfons_fs_src =
    "precision mediump float;\n"
    "uniform sampler2D tex;\n"
    "varying vec4 uv;\n"
    "varying vec4 color;\n"
    "void main() {\n"
    "    gl_FragColor = vec4(1.0, 1.0, 1.0, texture2D(tex, uv.xy).r) * color;\n"
    "}\n";
#elif defined(SOKOL_METAL)
static const char* _sfons_vs_src =
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
static const char* _sfons_fs_src =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "struct fs_in {\n"
    "  float4 uv;\n"
    "  float4 color;\n"
    "};\n"
    "fragment float4 _main(fs_in in [[stage_in]], texture2d<float> tex [[texture(0)]], sampler smp [[sampler(0)]]) {\n"
    "  return float4(1.0, 1.0, 1.0, tex.sample(smp, in.uv.xy).r) * in.color;\n"
    "}\n";
#elif defined(SOKOL_D3D11)
/*
    Shader blobs for D3D11, compiled with:

    fxc.exe /T vs_4_0 /Fh vs.h /Gec /O3 vs.hlsl
    fxc.exe /T ps_4_0 /Fh fs.h /Gec /O3 fs.hlsl

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
            return float4(1.0, 1.0, 1.0, tex.Sample(smp, uv.xy).r) * color;
        }
*/
static const uint8_t _sfons_vs_bin[] = {
     68,  88,  66,  67,  89, 173,
    124, 225,  74, 102, 159,  55,
     47,  64, 241,  32,  31, 107,
    240, 204,   1,   0,   0,   0,
    244,   3,   0,   0,   5,   0,
      0,   0,  52,   0,   0,   0,
      8,   1,   0,   0, 120,   1,
      0,   0, 236,   1,   0,   0,
    120,   3,   0,   0,  82,  68,
     69,  70, 204,   0,   0,   0,
      1,   0,   0,   0,  68,   0,
      0,   0,   1,   0,   0,   0,
     28,   0,   0,   0,   0,   4,
    254, 255,   0,  17,   0,   0,
    163,   0,   0,   0,  60,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   1,   0,   0,   0,
    112,  97, 114,  97, 109, 115,
      0, 171,  60,   0,   0,   0,
      2,   0,   0,   0,  92,   0,
      0,   0, 128,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0, 140,   0,   0,   0,
      0,   0,   0,   0,  64,   0,
      0,   0,   2,   0,   0,   0,
    144,   0,   0,   0,   0,   0,
      0,   0, 160,   0,   0,   0,
     64,   0,   0,   0,  64,   0,
      0,   0,   2,   0,   0,   0,
    144,   0,   0,   0,   0,   0,
      0,   0, 109, 118, 112,   0,
      3,   0,   3,   0,   4,   0,
      4,   0,   0,   0,   0,   0,
      0,   0,   0,   0, 116, 109,
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
     83,  72,  68,  82, 132,   1,
      0,   0,  64,   0,   1,   0,
     97,   0,   0,   0,  89,   0,
      0,   4,  70, 142,  32,   0,
      0,   0,   0,   0,   8,   0,
      0,   0,  95,   0,   0,   3,
    242,  16,  16,   0,   0,   0,
      0,   0,  95,   0,   0,   3,
     50,  16,  16,   0,   1,   0,
      0,   0,  95,   0,   0,   3,
    242,  16,  16,   0,   2,   0,
      0,   0, 101,   0,   0,   3,
    242,  32,  16,   0,   0,   0,
      0,   0, 101,   0,   0,   3,
    242,  32,  16,   0,   1,   0,
      0,   0, 103,   0,   0,   4,
    242,  32,  16,   0,   2,   0,
      0,   0,   1,   0,   0,   0,
    104,   0,   0,   2,   1,   0,
      0,   0,  56,   0,   0,   8,
    242,   0,  16,   0,   0,   0,
      0,   0,  86,  21,  16,   0,
      1,   0,   0,   0,  70, 142,
     32,   0,   0,   0,   0,   0,
      5,   0,   0,   0,  50,   0,
      0,  10, 242,   0,  16,   0,
      0,   0,   0,   0,  70, 142,
     32,   0,   0,   0,   0,   0,
      4,   0,   0,   0,   6,  16,
     16,   0,   1,   0,   0,   0,
     70,  14,  16,   0,   0,   0,
      0,   0,   0,   0,   0,   8,
    242,  32,  16,   0,   0,   0,
      0,   0,  70,  14,  16,   0,
      0,   0,   0,   0,  70, 142,
     32,   0,   0,   0,   0,   0,
      7,   0,   0,   0,  54,   0,
      0,   5, 242,  32,  16,   0,
      1,   0,   0,   0,  70,  30,
     16,   0,   2,   0,   0,   0,
     56,   0,   0,   8, 242,   0,
     16,   0,   0,   0,   0,   0,
     86,  21,  16,   0,   0,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,  50,   0,   0,  10,
    242,   0,  16,   0,   0,   0,
      0,   0,  70, 142,  32,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   6,  16,  16,   0,
      0,   0,   0,   0,  70,  14,
     16,   0,   0,   0,   0,   0,
     50,   0,   0,  10, 242,   0,
     16,   0,   0,   0,   0,   0,
     70, 142,  32,   0,   0,   0,
      0,   0,   2,   0,   0,   0,
    166,  26,  16,   0,   0,   0,
      0,   0,  70,  14,  16,   0,
      0,   0,   0,   0,  50,   0,
      0,  10, 242,  32,  16,   0,
      2,   0,   0,   0,  70, 142,
     32,   0,   0,   0,   0,   0,
      3,   0,   0,   0, 246,  31,
     16,   0,   0,   0,   0,   0,
     70,  14,  16,   0,   0,   0,
      0,   0,  62,   0,   0,   1,
     83,  84,  65,  84, 116,   0,
      0,   0,   9,   0,   0,   0,
      1,   0,   0,   0,   0,   0,
      0,   0,   6,   0,   0,   0,
      7,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      1,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      1,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0
};
static uint8_t _sfons_fs_bin[] = {
     68,  88,  66,  67, 180,  53,
    115, 174, 239,  17, 254, 112,
     63, 104, 217, 123, 150, 145,
    179,  27,   1,   0,   0,   0,
    120,   2,   0,   0,   5,   0,
      0,   0,  52,   0,   0,   0,
    200,   0,   0,   0,  24,   1,
      0,   0,  76,   1,   0,   0,
    252,   1,   0,   0,  82,  68,
     69,  70, 140,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   2,   0,   0,   0,
     28,   0,   0,   0,   0,   4,
    255, 255,   0,  17,   0,   0,
    100,   0,   0,   0,  92,   0,
      0,   0,   3,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   1,   0,
      0,   0,   1,   0,   0,   0,
     96,   0,   0,   0,   2,   0,
      0,   0,   5,   0,   0,   0,
      4,   0,   0,   0, 255, 255,
    255, 255,   0,   0,   0,   0,
      1,   0,   0,   0,  13,   0,
      0,   0, 115, 109, 112,   0,
    116, 101, 120,   0,  77, 105,
     99, 114, 111, 115, 111, 102,
    116,  32,  40,  82,  41,  32,
     72,  76,  83,  76,  32,  83,
    104,  97, 100, 101, 114,  32,
     67, 111, 109, 112, 105, 108,
    101, 114,  32,  49,  48,  46,
     49,   0,  73,  83,  71,  78,
     72,   0,   0,   0,   2,   0,
      0,   0,   8,   0,   0,   0,
     56,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      3,   0,   0,   0,   0,   0,
      0,   0,  15,   3,   0,   0,
     65,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      3,   0,   0,   0,   1,   0,
      0,   0,  15,  15,   0,   0,
     84,  69,  88,  67,  79,  79,
     82,  68,   0,  67,  79,  76,
     79,  82,   0, 171,  79,  83,
     71,  78,  44,   0,   0,   0,
      1,   0,   0,   0,   8,   0,
      0,   0,  32,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   3,   0,   0,   0,
      0,   0,   0,   0,  15,   0,
      0,   0,  83,  86,  95,  84,
     97, 114, 103, 101, 116,   0,
    171, 171,  83,  72,  68,  82,
    168,   0,   0,   0,  64,   0,
      0,   0,  42,   0,   0,   0,
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
     69,   0,   0,   9, 242,   0,
     16,   0,   0,   0,   0,   0,
     70,  16,  16,   0,   0,   0,
      0,   0, 150, 115,  16,   0,
      0,   0,   0,   0,   0,  96,
     16,   0,   0,   0,   0,   0,
     54,   0,   0,   5,  18,   0,
     16,   0,   0,   0,   0,   0,
      1,  64,   0,   0,   0,   0,
    128,  63,  56,   0,   0,   7,
    242,  32,  16,   0,   0,   0,
      0,   0,   6,  12,  16,   0,
      0,   0,   0,   0,  70,  30,
     16,   0,   1,   0,   0,   0,
     62,   0,   0,   1,  83,  84,
     65,  84, 116,   0,   0,   0,
      4,   0,   0,   0,   1,   0,
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
      0,   0,   0,   0,   1,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,
      0,   0
};
#elif defined(SOKOL_DUMMY_BACKEND)
static const char* _sfons_vs_src = "";
static const char* _sfons_fs_src = "";
#endif

typedef struct _sfons_t {
    sg_shader shd;
    sgl_pipeline pip;
    sg_image img;
    int width, height;
    bool img_dirty;
} _sfons_t;

static int _sfons_render_create(void* user_ptr, int width, int height) {
    SOKOL_ASSERT(user_ptr && (width > 8) && (height > 8));
    _sfons_t* sfons = (_sfons_t*) user_ptr;

    /* sokol-gl compatible shader which treats RED channel as alpha */
    if (sfons->shd.id == SG_INVALID_ID) {
        sg_shader_desc shd_desc;
        memset(&shd_desc, 0, sizeof(shd_desc));
        shd_desc.attrs[0].name = "position";
        shd_desc.attrs[1].name = "texcoord0";
        shd_desc.attrs[2].name = "color0";
        shd_desc.attrs[0].sem_name = "POSITION";
        shd_desc.attrs[1].sem_name = "TEXCOORD";
        shd_desc.attrs[2].sem_name = "COLOR";
        sg_shader_uniform_block_desc* ub = &shd_desc.vs.uniform_blocks[0];
        ub->size = 128;
        ub->uniforms[0].name = "mvp";
        ub->uniforms[0].type = SG_UNIFORMTYPE_MAT4;
        ub->uniforms[1].name = "tm";
        ub->uniforms[1].type = SG_UNIFORMTYPE_MAT4;
        shd_desc.fs.images[0].name = "tex";
        shd_desc.fs.images[0].type = SG_IMAGETYPE_2D;
        #if defined(SOKOL_D3D11)
            shd_desc.vs.byte_code = _sfons_vs_bin;
            shd_desc.vs.byte_code_size = sizeof(_sfons_vs_bin);
            shd_desc.fs.byte_code = _sfons_fs_bin;
            shd_desc.fs.byte_code_size = sizeof(_sfons_fs_bin);
        #else
            shd_desc.vs.source = _sfons_vs_src;
            shd_desc.fs.source = _sfons_fs_src;
        #endif
        shd_desc.label = "sfons-shader";
        sfons->shd = sg_make_shader(&shd_desc);
    }

    /* sokol-gl pipeline object */
    if (sfons->pip.id == SG_INVALID_ID) {
        sg_pipeline_desc pip_desc;
        memset(&pip_desc, 0, sizeof(pip_desc));
        pip_desc.shader = sfons->shd;
        pip_desc.blend.enabled = true;
        pip_desc.blend.src_factor_rgb = SG_BLENDFACTOR_SRC_ALPHA;
        pip_desc.blend.dst_factor_rgb = SG_BLENDFACTOR_ONE_MINUS_SRC_ALPHA;
        sfons->pip = sgl_make_pipeline(&pip_desc);
    }

    /* create or re-create font atlas texture */
    if (sfons->img.id != SG_INVALID_ID) {
        sg_destroy_image(sfons->img);
        sfons->img.id = SG_INVALID_ID;
    }
    sfons->width = width;
    sfons->height = height;

    SOKOL_ASSERT(sfons->img.id == SG_INVALID_ID);
    sg_image_desc img_desc;
    memset(&img_desc, 0, sizeof(img_desc));
    img_desc.width = sfons->width;
    img_desc.height = sfons->height;
    img_desc.min_filter = SG_FILTER_LINEAR;
    img_desc.mag_filter = SG_FILTER_LINEAR;
    img_desc.usage = SG_USAGE_DYNAMIC;
    img_desc.pixel_format = SG_PIXELFORMAT_R8;
    sfons->img = sg_make_image(&img_desc);
    return 1;
}

static int _sfons_render_resize(void* user_ptr, int width, int height) {
    return _sfons_render_create(user_ptr, width, height);
}

static void _sfons_render_update(void* user_ptr, int* rect, const unsigned char* data) {
    SOKOL_ASSERT(user_ptr && rect && data);
    _sfons_t* sfons = (_sfons_t*) user_ptr;
    sfons->img_dirty = true;
}

static void _sfons_render_draw(void* user_ptr, const float* verts, const float* tcoords, const unsigned int* colors, int nverts) {
    SOKOL_ASSERT(user_ptr && verts && tcoords && colors && (nverts > 0));
    _sfons_t* sfons = (_sfons_t*) user_ptr;
    sgl_enable_texture();
    sgl_texture(sfons->img);
    sgl_push_pipeline();
    sgl_load_pipeline(sfons->pip);
    sgl_begin_triangles();
    for (int i = 0; i < nverts; i++) {
        sgl_v2f_t2f_c1i(verts[2*i+0], verts[2*i+1], tcoords[2*i+0], tcoords[2*i+1], colors[i]);
    }
    sgl_end();
    sgl_pop_pipeline();
    sgl_disable_texture();
}

static void _sfons_render_delete(void* user_ptr) {
    SOKOL_ASSERT(user_ptr);
    _sfons_t* sfons = (_sfons_t*) user_ptr;
    if (sfons->img.id != SG_INVALID_ID) {
        sg_destroy_image(sfons->img);
        sfons->img.id = SG_INVALID_ID;
    }
    if (sfons->pip.id != SG_INVALID_ID) {
        sgl_destroy_pipeline(sfons->pip);
        sfons->pip.id = SG_INVALID_ID;
    }
    if (sfons->shd.id != SG_INVALID_ID) {
        sg_destroy_shader(sfons->shd);
        sfons->shd.id = SG_INVALID_ID;
    }
    SOKOL_FREE(sfons);
}

SOKOL_API_IMPL FONScontext* sfons_create(int width, int height, int flags) {
    SOKOL_ASSERT((width > 0) && (height > 0));
    FONSparams params;
    _sfons_t* sfons = (_sfons_t*) SOKOL_MALLOC(sizeof(_sfons_t));
    memset(sfons, 0, sizeof(_sfons_t));
    memset(&params, 0, sizeof(params));
    params.width = width;
    params.height = height;
    params.flags = (unsigned char) flags;
    params.renderCreate = _sfons_render_create;
    params.renderResize = _sfons_render_resize;
    params.renderUpdate = _sfons_render_update;
    params.renderDraw = _sfons_render_draw;
    params.renderDelete = _sfons_render_delete;
    params.userPtr = sfons;
    return fonsCreateInternal(&params);
}

SOKOL_API_IMPL void sfons_destroy(FONScontext* ctx) {
    SOKOL_ASSERT(ctx);
    fonsDeleteInternal(ctx);
}

SOKOL_API_IMPL void sfons_flush(FONScontext* ctx) {
    SOKOL_ASSERT(ctx && ctx->params.userPtr);
    _sfons_t* sfons = (_sfons_t*) ctx->params.userPtr;
    if (sfons->img_dirty) {
        sfons->img_dirty = false;
        sg_image_content content;
        memset(&content, 0, sizeof(content));
        content.subimage[0][0].ptr = ctx->texData;
        content.subimage[0][0].size = sfons->width * sfons->height;
        sg_update_image(sfons->img, &content);
    }
}

SOKOL_API_IMPL uint32_t sfons_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a) {
    return (r) | (g<<8) | (b<<16) | (a<<24);
}

#endif /* SOKOL_FONTSTASH_IMPL */

# gg.canvas

`gg.canvas` draws 2D shapes using Skia Graphite's rendering algorithms through
`gg`/sokol, without linking Skia. It supports Metal on macOS and OpenGL 4.3 or
newer on Linux. The Metal backend targets bit-for-bit agreement with Graphite
on the same GPU, for example when comparing a browser engine against Chromium
screenshots. The Linux backend uses the same geometry, paint records and shader
formulas through OpenGL; pixel identity across graphics backends or GPUs is
not guaranteed.

The implementation follows Skia's rendering steps:

- On macOS, the GPU backend (`GpuContext`, `GpuSurface`) issues the same Metal work as
  Skia Graphite: the Graphite-generated MSL shaders are embedded unchanged
  (`shaders/`), compiled with Graphite's MSL version and fast-math mode, and
  fed byte-identical instance and uniform data through sokol.gfx.
- On Linux, the assembled shaders are translated to GLSL 4.30, with paint
  records in shader storage buffers. OpenGL render targets use RGBA8 internally;
  pixel uploads and readback retain the public premultiplied BGRA8 format and
  top-to-bottom row order.
- Draws are ordered, sorted (with a port of libc++'s `std::sort`) and batched
  into instanced draw calls exactly like Graphite's `Device`, `DrawList` and
  `DrawBufferManager`. Splitting or reordering a batch changes antialiased
  and MSAA pixels on Apple GPUs.
- Geometry is normalized with the same floating-point operations as Skia
  (`SkRRect`, Graphite's `Rect` and `Transform`). Skia is compiled with
  `-ffp-contract=off`, so the code here avoids expressions that the C compiler
  could fuse into FMAs: products feeding a sum are separate statements.

Supported so far: filled, stroked and hairline rects, rounded rects and ovals
(the analytic rrect and non-AA bounds renderers, including Graphite's inner
fills), single-sampled and 4x MSAA surfaces, uploaded backdrops (loaded into
MSAA passes with Graphite's LoadMSAAFromResolve draw), BGRA8 premultiplied
readback, filled and stroked double rounded rects (`draw_drrect`), and clips: `save`/`restore`,
intersect and difference `clip_rect`, `clip_rrect` and `clip_path` under scales, flips,
90-degree rotations and other affine transforms (a port of Graphite's
ClipStack: element simplification, geometric intersection of draws with
clips, scissors, the analytic clip shaders, and depth-only draws in a 4x MSAA
pass for further elements: convex wedges for convex difference elements,
stencil wedges with an inverse cover of the scissor for intersect elements,
and stencil wedges with a regular cover for non-convex difference paths).
Filled paths (`Path`, built with `PathBuilder` like `SkPathBuilder`, including
its rect, oval and rrect contours) are drawn
like Graphite's `drawPath`: rect, oval and nested-rect paths use those
renderers, convex paths the shaded convex wedges and other paths the MSAA
stencil-and-cover wedges (winding and even-odd), including Skia's path
convexity, rect detection and Wang's formula for lines, quads, conics and
cubics.
Strokes of paths, and of rrects the analytic renderer cannot stroke
(elliptical corners), use Graphite's tessellated stroke renderer: deferred
first patches joined to closing patches, miter, round and bevel joins, butt,
round and square caps, cubic, quad and conic cusps, hairlines and cubic
convex-180 chops. Convex
path fills and strokes apply the analytic clip in their fragment shaders.
Curves that need more than 32 parametric segments are chopped like Graphite's
PatchWriter, in fills and strokes. Inverse fills use the stencil wedges with
an inverse cover of the clip. Draws that need other renderers (curved path
clips under rotations, ...) return an error instead of rendering differently.
Lines (`draw_line`, and paths that are a single line) are stroked by the
analytic renderer with caps, like Graphite's `drawPoints`.
Shapes, path fills and strokes can also be painted with dithered linear and
radial gradients (`Paint.gradient`, like `SkShaders::LinearGradient` and
`RadialGradient` with up to 8 stops, every tile mode, premultiplied or
unpremultiplied interpolation, a local matrix and the paint's alpha), with or
without analytic clips. Their shaders combine each renderer's solid-color shaders with pieces
of Graphite's gradient MSL (`shaders/paint_snippets.metal`), and the uniforms
follow `SkGradientBaseShader`'s stop normalization and Graphite's gradient,
local matrix, color space and dither blocks. Gradient pipelines are compiled
on first use.
`save_layer` mirrors SkCanvas::saveLayer() without a paint: later draws go to
an approximately sized offscreen texture covering the clip bounds, which the
matching `restore` renders in its own pass and draws back through the clip
with Graphite's image shader (on the bounds, analytic rrect or per-edge AA quad renderer, with
the analytic clip or stencil clip elements). `save`/`restore` also restore the
matrix, like SkCanvas. Fractional rectangular layer bounds use PerEdgeAAQuad's
16 template vertices, 29 indices, 80-byte instances and coverage shader.

`new_gpu_context()` borrows an initialized sokol.gfx context. Otherwise it
creates a headless Metal context on macOS or an offscreen EGL/OpenGL context on
Linux. Linux requires an OpenGL 4.3 context with vertex and fragment shader
storage buffers and at least 16 storage-buffer bindings. Mesa's surfaceless EGL
implementation can render without X11 or Wayland. An existing
application context must be current on the calling thread. Create, draw and
free GPU resources on that thread, free surfaces before their context, and
keep the owner of a borrowed sokol.gfx context alive until canvas cleanup.

Geometry and recording code can be imported on other platforms; GPU rendering,
typeface creation and raster text return unsupported-platform errors there.
CoreText and EGL/FreeType implementation files are compiled only on their
respective platforms.

`RasterSurface` reproduces Skia's CPU raster text on macOS (`draw_glyphs`,
like `SkCanvas::drawTextBlob` with one positioned run): the scaler record,
CoreGraphics glyph masks with Skia's linearization and `SkMaskGamma`
pre-blend, subpixel glyph positioning, and the lowp `SkRasterPipeline` mask
blit into BGRA8 premultiplied pixels. On Linux it uses FreeType glyph masks and
the same positioning, mask gamma, bounded caches and BGRA compositor. Load font
files or bytes with `Typeface.from_file()` or `Typeface.from_data()` on either
platform. Linux requires scalable font faces. `Typeface.from_ct_font()` is
specific to macOS. Linux tests use a
font bundled in the repository, so they do not depend on installed system fonts.

Synthetic bold, color glyphs and glyphs that need path rendering (over 256px)
return an error. macOS also rejects aliased text; Linux supports monochrome and
grayscale masks. LCD masks are not supported: `subpixel_antialias` uses grayscale
coverage for the surface's unknown pixel geometry. FreeType and CoreText can
produce different masks for the same font, so CoreText image hashes are tested
only on macOS.

Text drawing and cleanup run serially on the drawing thread. The process-wide
strike cache retains at most 32 scaler contexts in least-recently-used order.
Each strike caches at most 256 glyphs and 256 KiB of masks; larger individual
glyph masks (over 512 pixels in either dimension) return an error. Eviction
releases masks and native resources owned by the strike. Call
`canvas.clear_text_cache()` to release all strikes while keeping typefaces
usable, and `typeface.free()` when the caller finishes with a face. The latter
also evicts that face's strikes and releases its retained native font.

The native text tests render the available glyph-run cases (five with all fixture fonts) and
check nonempty premultiplied pixels, cache reuse/regeneration, and integer translation with
clipping. A missing optional system font skips its case with a diagnostic; at least one case must
run. Fixed A8 mask vectors separately check exact compositor pixels for opaque and translucent
sources and destinations. These checks do not depend on another machine's CoreGraphics
rasterization.

The original Skia m144 whole-image hashes remain available as a reference-host check:

```sh
V_CANVAS_TEXT_SKIA_GOLDENS=1 ./v vlib/gg/canvas/text_test.v
```

Use this only with the fonts, CoreGraphics version and font-smoothing configuration that produced
the captures. The original capture environment was not recorded, so the hashes are not a portable
macOS baseline. This opt-in check supplies the whole-image Skia comparison; the default tests
cover native text integration and exact mask compositing without claiming cross-host pixel parity.

```v
import gg.canvas

mut ctx := canvas.new_gpu_context()!
defer { ctx.free() }
mut surface := ctx.new_surface(width: 64, height: 48)!
defer { surface.free() }
surface.clear(canvas.Color4f{})
r := canvas.Point{6, 6}
surface.draw_rrect(canvas.RRect.rect_radii(canvas.Rect.ltrb(4.5, 4, 60, 44), [r, r, r, r]!),
	canvas.Paint.rgba8(35, 167, 79, 255))!
mut pixels := []u8{len: 64 * 48 * 4}
surface.read_pixels(pixels.data, 64 * 4)! // BGRA, premultiplied
```

## Linux dependencies and tests

On Debian or Ubuntu, install the FreeType, EGL/OpenGL and sokol development
dependencies:

```sh
sudo apt-get install libfreetype-dev libegl-dev libgl-dev libgl1-mesa-dri \
  libx11-dev libxi-dev libxcursor-dev
```

Run the canvas tests with Mesa software rendering and no display server:

```sh
env -u DISPLAY -u WAYLAND_DISPLAY LIBGL_ALWAYS_SOFTWARE=true EGL_PLATFORM=surfaceless \
  ./v -cc gcc test vlib/gg/canvas
```

Offscreen-only programs can also use `-d no_sokol_app` to omit sokol's windowing
implementation. The Linux CI workflow tests both the default import with GCC
and this offscreen configuration with TCC. GPU tests require context creation
and rendering to succeed; they check pixel transfers, shapes, clips, paths,
gradients, layers and MSAA. Text tests exercise native rasterization with a
pinned font, positioning, compositing, cache bounds and font cleanup.

The shaders in `shaders/` are generated by Skia Graphite and distributed under
Skia's BSD license (`shaders/LICENSE-skia`); the ported algorithms follow the
same Skia sources.

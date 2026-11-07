# gg.canvas

`gg.canvas` draws 2D shapes with exactly the same pixels as Skia. It is meant
for programs that must match a Skia-based renderer bit for bit (for example a
browser engine compared against Chromium screenshots), while using `gg`/sokol
instead of linking Skia.

Exactness is achieved by reproducing what Skia does, not by approximating its
output:

- The GPU backend (`GpuContext`, `GpuSurface`) issues the same Metal work as
  Skia Graphite: the Graphite-generated MSL shaders are embedded unchanged
  (`shaders/`), compiled with Graphite's MSL version and fast-math mode, and
  fed byte-identical instance and uniform data through sokol.gfx.
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

Only Metal is supported: the reproduced output is Graphite's Metal output on
the same GPU. Geometry and recording code can be imported on other platforms;
typeface creation and raster text return unsupported-platform errors there.
CoreText implementation files are compiled only on macOS.

`RasterSurface` reproduces Skia's CPU raster text on macOS (`draw_glyphs`,
like `SkCanvas::drawTextBlob` with one positioned run): the scaler record,
CoreGraphics glyph masks with Skia's linearization and `SkMaskGamma`
pre-blend, subpixel glyph positioning, and the lowp `SkRasterPipeline` mask
blit into BGRA8 premultiplied pixels. Synthetic bold, color glyphs, aliased
text and glyphs that Skia draws as paths (over 256px) return an error.

Text drawing and cleanup run serially on the drawing thread. The process-wide
strike cache retains at most 32 scaler contexts in least-recently-used order.
Each strike caches at most 256 glyphs and 256 KiB of masks; larger individual
glyph masks (over 512 pixels in either dimension) return an error. Eviction
releases masks, the copied font, and the native bitmap/context. Call
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
mut surface := ctx.new_surface(width: 64, height: 48)!
surface.clear(canvas.Color4f{})
r := canvas.Point{6, 6}
surface.draw_rrect(canvas.RRect.rect_radii(canvas.Rect.ltrb(4.5, 4, 60, 44), [r, r, r, r]!),
	canvas.Paint.rgba8(35, 167, 79, 255))!
mut pixels := []u8{len: 64 * 48 * 4}
surface.read_pixels(pixels.data, 64 * 4)! // BGRA, premultiplied
```

The shaders in `shaders/` are generated by Skia Graphite and distributed under
Skia's BSD license (`shaders/LICENSE-skia`); the ported algorithms follow the
same Skia sources.

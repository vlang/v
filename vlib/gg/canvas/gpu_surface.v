module canvas

import math
import sokol.gfx

// GpuSurfaceConfig selects the size and sample count of a surface.
@[params]
pub struct GpuSurfaceConfig {
pub:
	width  int
	height int
	// 4 renders with 4x MSAA like a Graphite pass that contains an MSAA draw;
	// 1 renders single-sampled.
	sample_count int = 1
}

struct GpuDraw {
	step     RenderStep
	instance []u8
	// Premultiplied solid color and analytic clip.
	uniforms PaintUniforms
	// Disabled for draws without coverage whose paint is opaque: Graphite then
	// prefers a fixed "src" blend, which rounds differently than src-over.
	blend bool = true
	order DrawOrder
	// Insertion indices into the draw list's pipeline and uniform caches.
	pipeline_index u32
	uniform_index  u32
	// The render step's index within its renderer (the sort key's step field).
	step_index u32
	// Phantom draws are simulated for ordering and buffer use but not rendered:
	// the number of instances they append (0 for real draws).
	phantom_instances u32
	// The instances in `instance` of a real draw.
	instance_count u32 = 1
	clip           DrawClip
}

// DrawClip is the part of Graphite's Clip that the draw pass uses to choose
// scissors: the draw's integer scissor, its bounds and its unclipped,
// transformed shape bounds.
struct DrawClip {
	scissor      Rect
	draw_bounds  Rect
	shape_bounds Rect
}

struct PipelineKey {
	step    RenderStep
	blend   bool
	clipped bool
}

// GpuSurface records draws into a BGRA8 premultiplied render target and
// renders them like Skia Graphite does, so the pixels read back match a
// Graphite SkSurface with the same draws exactly.
@[heap]
pub struct GpuSurface {
pub:
	width        int
	height       int
	sample_count int
mut:
	ctx         &GpuContext = unsafe { nil }
	color       gfx.Image
	msaa        gfx.Image
	depth       gfx.Image
	attachments gfx.Attachments
	draws       []GpuDraw
	// The pass clears to this color unless the contents were written.
	clear_color Color4f
	load        bool
	depth_index u16
	transform   Transform
	bounds      BoundsManager
	pipelines   []PipelineKey
	uniforms    []PaintUniforms
	clip        ClipStackState
	// The MSAA attachments of a single-sampled surface whose pass contains a
	// draw that requires MSAA, like Graphite's discardable MSAA texture.
	dynamic_msaa        gfx.Image
	dynamic_depth       gfx.Image
	dynamic_attachments gfx.Attachments
}

// new_surface creates an offscreen surface, initially transparent black.
pub fn (mut ctx GpuContext) new_surface(cfg GpuSurfaceConfig) !&GpuSurface {
	if cfg.width <= 0 || cfg.height <= 0 {
		return error('gg.canvas: invalid surface size ${cfg.width}x${cfg.height}')
	}
	if cfg.sample_count != 1 && cfg.sample_count != 4 {
		return error('gg.canvas: unsupported sample count ${cfg.sample_count}')
	}
	mut s := &GpuSurface{
		ctx:          unsafe { ctx }
		width:        cfg.width
		height:       cfg.height
		sample_count: cfg.sample_count
		transform:    Transform.new(Matrix.identity())
		bounds:       new_bounds_manager(cfg.width, cfg.height)
		clip:         new_clip_stack(cfg.width, cfg.height)
	}
	s.color = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         cfg.width
		height:        cfg.height
		pixel_format:  .bgra8
		sample_count:  1
	})
	if cfg.sample_count > 1 {
		s.msaa = gfx.make_image(&gfx.ImageDesc{
			render_target: true
			width:         cfg.width
			height:        cfg.height
			pixel_format:  .bgra8
			sample_count:  cfg.sample_count
		})
	}
	s.depth = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         cfg.width
		height:        cfg.height
		// Graphite's multisampled passes have a combined depth/stencil
		// attachment (Depth32Float_Stencil8).
		pixel_format:  if cfg.sample_count > 1 {
			gfx.PixelFormat.depth_stencil
		} else {
			gfx.PixelFormat.depth
		}
		sample_count:  cfg.sample_count
	})
	mut desc := gfx.AttachmentsDesc{}
	if cfg.sample_count > 1 {
		desc.colors[0].image = s.msaa
		desc.resolves[0].image = s.color
	} else {
		desc.colors[0].image = s.color
	}
	desc.depth_stencil.image = s.depth
	s.attachments = gfx.make_attachments(&desc)
	if gfx.query_attachments_state(s.attachments) != .valid {
		s.free()
		return error('gg.canvas: could not create the render target')
	}
	return s
}

pub fn (mut s GpuSurface) free() {
	if s.dynamic_msaa.id != 0 {
		gfx.destroy_attachments(s.dynamic_attachments)
		gfx.destroy_image(s.dynamic_depth)
		gfx.destroy_image(s.dynamic_msaa)
	}
	gfx.destroy_attachments(s.attachments)
	gfx.destroy_image(s.depth)
	if s.msaa.id != 0 {
		gfx.destroy_image(s.msaa)
	}
	gfx.destroy_image(s.color)
}

// clear discards pending draws and fills the surface with `color`.
pub fn (mut s GpuSurface) clear(color Color4f) {
	s.reset_draws()
	s.clear_color = color.premul()
	s.load = false
}

fn (mut s GpuSurface) reset_draws() {
	s.draws.clear()
	s.pipelines.clear()
	s.uniforms.clear()
	s.bounds = new_bounds_manager(s.width, s.height)
	s.depth_index = 0
}

// write_pixels replaces the surface contents with BGRA8 premultiplied pixels,
// discarding pending draws.
pub fn (mut s GpuSurface) write_pixels(pixels voidptr, pitch int) ! {
	s.reset_draws()
	if !gfx.write_image(s.color, 0, 0, s.width, s.height, pixels, pitch) {
		return error('gg.canvas: could not upload pixels')
	}
	s.load = true
}

// set_matrix replaces the local to device transform of later draws.
pub fn (mut s GpuSurface) set_matrix(m Matrix) {
	s.transform = Transform.new(m)
}

// draw_rect draws a rectangle like SkCanvas::drawRect() on a Graphite device.
// It returns an error for draws that need renderers which are not ported yet.
pub fn (mut s GpuSurface) draw_rect(r Rect, paint Paint) ! {
	if !s.transform.valid() {
		return
	}
	if paint.style == .stroke_and_fill {
		return error('gg.canvas: stroke-and-fill needs the tessellating path renderer')
	}
	mut rect := r.sorted()
	if s.quick_reject(rect, paint) {
		return
	}
	mut style := paint
	if !paint.anti_alias {
		// Graphite emulates non-AA rectangles by snapping them to pixels.
		rect, style.stroke_width = s.snap_rect_to_pixels(rect, paint)
	}
	s.draw_shape(ShapeGeometry{
		kind: .rect
		rect: rect
	}, style)!
}

// draw_oval draws an ellipse inscribed in `r`, like SkCanvas::drawOval().
pub fn (mut s GpuSurface) draw_oval(r Rect, paint Paint) ! {
	if s.quick_reject(r.sorted(), paint) {
		return
	}
	oval := RRect.oval(r)
	if oval.is_rect() {
		s.draw_rect(oval.rect, paint)!
	} else if !oval.is_empty() {
		s.draw_rrect_unchecked(oval, paint)!
	}
}

// draw_rrect draws a rounded rectangle like SkCanvas::drawRRect() on a
// Graphite device.
pub fn (mut s GpuSurface) draw_rrect(rr RRect, paint Paint) ! {
	if rr.is_rect() {
		s.draw_rect(rr.rect, paint)!
		return
	}
	if rr.typ == .oval {
		s.draw_oval(rr.rect, paint)!
		return
	}
	if rr.is_empty() || !s.transform.valid() || s.quick_reject(rr.rect, paint) {
		return
	}
	s.draw_rrect_unchecked(rr, paint)!
}

// draw_rrect_unchecked mirrors Device::drawRRect(), after SkCanvas routed
// rects and ovals and quick-rejected the draw.
fn (mut s GpuSurface) draw_rrect_unchecked(rr RRect, paint Paint) ! {
	if paint.style == .stroke_and_fill {
		return error('gg.canvas: stroke-and-fill needs the tessellating path renderer')
	}
	mut shape := rr
	mut style := paint
	if !paint.anti_alias {
		// Snap the straight edges like drawRect(); the corners keep their AA.
		snapped, width := s.snap_rect_to_pixels(rr.rect, paint)
		style.stroke_width = width
		shape = RRect.rect_radii(snapped, rr.radii)
	}
	s.draw_shape(ShapeGeometry{
		kind:  .rrect
		rrect: shape
	}, style)!
}

// quick_reject mirrors SkCanvas::internalQuickReject(): transparent paints
// and draws whose fast bounds miss the device (outset by 1px for AA) are
// dropped before they reach the device and take a depth value.
fn (s &GpuSurface) quick_reject(bounds Rect, paint Paint) bool {
	if !bounds.is_finite() || paint.color.a == 0 {
		return true
	}
	radius := inflation_radius(paint)
	// SkRect::makeOutset
	fast := Rect.ltrb(bounds.left() - radius, bounds.top() - radius, bounds.right() + radius,
		bounds.bottom() + radius)
	device := map_rect_points(s.transform.matrix, fast)
	if !device.is_finite() {
		return true
	}
	// SkRect::intersects with the device clip bounds outset by one pixel
	// (SkCanvas::computeDeviceClipBounds()).
	clip := s.conservative_clip_bounds() or { return true }
	l := max_f32(device.left(), clip.left() - 1)
	r := min_f32(device.right(), clip.right() + 1)
	t := max_f32(device.top(), clip.top() - 1)
	b := min_f32(device.bottom(), clip.bottom() + 1)
	return !(l < r && t < b)
}

// inflation_radius mirrors SkStrokeRec::GetInflationRadius() for a paint.
fn inflation_radius(paint Paint) f32 {
	if paint.style == .fill {
		return 0
	}
	if paint.stroke_width == 0 {
		return 1
	}
	mut multiplier := f32(1)
	if paint.join == .miter {
		multiplier = max_f32(multiplier, paint.miter_limit)
	}
	if paint.cap == .square {
		multiplier = max_f32(multiplier, f32(1.41421356))
	}
	return paint.stroke_width / f32(2) * multiplier
}

// snap_rect_to_pixels mirrors snap_rect_to_pixels() in Graphite's Device.cpp.
// It returns the snapped local rectangle and the adjusted stroke width.
fn (s &GpuSurface) snap_rect_to_pixels(rect Rect, paint Paint) (Rect, f32) {
	t := s.transform
	mut stroke_width := paint.stroke_width
	if int(t.typ) > int(TransformType.rect_stays_rect) {
		return rect, stroke_width
	}
	mut device := Rect{}
	if paint.style == .fill {
		device = t.map_rect(rect).round()
	} else if stroke_width == 0 {
		// Hairlines are outset by half a pixel before rounding, then inset.
		device = t.map_rect(rect).outset(0.5).round().inset(0.5)
	} else {
		device = t.map_rect(rect.outset(f32(0.5) * stroke_width)).round()
		m := t.matrix.m
		scale_x := max_f32(abs_f32(m[0]), abs_f32(m[1]))
		scale_y := max_f32(abs_f32(m[3]), abs_f32(m[4]))
		device_x := max_f32(C.roundf(stroke_width * scale_x), 1)
		device_y := max_f32(C.roundf(stroke_width * scale_y), 1)
		stroke_width = if device_x > device_y { device_x / scale_x } else { device_y / scale_y }
		// inset(0.5f * devScales * strokeWidth) on skvx vectors: the products
		// are rounded before the addition.
		half_x := f32(0.5) * scale_x
		half_y := f32(0.5) * scale_y
		inset_x := half_x * stroke_width
		inset_y := half_y * stroke_width
		device = Rect{
			vals: [device.vals[0] + inset_x, device.vals[1] + inset_y, device.vals[2] + inset_x,
				device.vals[3] + inset_y]!
		}
	}
	local := t.inverse_map_rect(device)
	if device.nearly_equals(t.map_rect(local), scalar_nearly_zero) {
		return local, stroke_width
	}
	return rect, paint.stroke_width
}

// draw_msaa_trigger records the effects of the draw that makes Graphite render
// a pass with 4x MSAA: a tiny transparent path at the origin, drawn with the
// stencil-and-cover path renderer, like `force_msaa_render_pass()` in Vox's
// native Graphite backend. It covers no samples, so it is not rendered, but
// it takes the next depth, an order, cache entries and bounds that change how
// the later draws are sorted and batched. Use it on 4x MSAA surfaces where
// the reproduced renderer drew that path.
pub fn (mut s GpuSurface) draw_msaa_trigger() {
	s.depth_index++
	// A transparent, dst-dependent paint after the empty clip, in the first
	// disjoint stencil set.
	order := DrawOrder{
		paint_order:   1
		stencil_index: 0
		depth:         s.depth_index
	}
	// Step 0 tessellates three wedges with the 64-byte localToDevice matrix
	// uniform (never equal to a paint color); step 1 covers them with the
	// transparent paint.
	nan := f32(math.nan())
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	trigger_bounds := Rect.ltrb(0, 0, 0.01, 0.01)
	trigger_clip := DrawClip{
		scissor:      device
		draw_bounds:  trigger_bounds
		shape_bounds: trigger_bounds
	}
	s.record_step(.tessellate_wedges, false, []u8{}, identity_local_to_device, order, 0, 3,
		trigger_clip, 0)
	s.record_step(.cover_fills, true, []u8{}, PaintUniforms{}, order, 1, 1, trigger_clip, 0)
	// The path bounds (0, 0, 0.01, 0.01) are subpixel, so the clip stack
	// outsets them by the local AA radius before clipping to the device.
	path := Rect.ltrb(0, 0, f32(0.01), f32(0.01)).outset(1)
	s.bounds.record_draw(path.intersect(Rect.ltrb(0, 0, f32(s.width), f32(s.height))), 1)
}

// draw_shape mirrors Device::drawGeometry() for the analytic renderers.
fn (mut s GpuSurface) draw_shape(geometry ShapeGeometry, paint Paint) ! {
	bounds := geometry.bounds()
	if int(s.transform.typ) > int(TransformType.affine) {
		return error('gg.canvas: perspective transforms are not supported')
	}
	stroked := paint.style == .stroke
	if stroked && geometry.kind == .rrect && paint.stroke_width > 0 {
		tolerance := s.transform.local_aa_radius() * shape_pixel_tolerance
		if !all_corners_relatively_circular(geometry.rrect, tolerance) {
			return error('gg.canvas: elliptical strokes need the tessellating path renderer')
		}
	}
	// The draw bounds clipped to the device; empty bounds clip the draw out
	// before it consumes a depth value.
	clipped_bounds, shape_bounds := s.draw_bounds(paint, bounds) or { return }
	influence, clip_rect, depth_elements := s.clip_for_draw(clipped_bounds)!
	if influence == .clipped_out {
		return
	}
	clipped := influence == .analytic
	color := paint.color.premul()
	uniforms := PaintUniforms{
		color:   color
		clipped: clipped
		clip:    if clipped { analytic_clip_uniforms(clip_rect) } else { AnalyticClipUniforms{} }
	}
	pixel_aligned := !stroked && geometry.kind == .rect && is_pixel_aligned(geometry.rect, s.transform)
	step := if pixel_aligned { RenderStep.cover_bounds } else { RenderStep.analytic_rrect }
	// Renderers with analytic coverage include their 1px AA ramp in the
	// bounds used for ordering (Clip::outsetBoundsForAA()).
	device_bounds := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	draw_bounds := if step == .analytic_rrect {
		clipped_bounds.outset(1).intersect(device_bounds)
	} else {
		clipped_bounds
	}
	// Difference clips scissor to the device.
	draw_clip := DrawClip{
		scissor:      device_bounds
		draw_bounds:  draw_bounds
		shape_bounds: if step == .analytic_rrect { shape_bounds.outset(1) } else { shape_bounds }
	}
	// DstUsage: src-over with analytic coverage (from the renderer or an
	// analytic clip) always reads the destination; without coverage only a
	// translucent paint does.
	depends_on_dst := step == .analytic_rrect || clipped || !color.is_opaque()
	s.depth_index++
	mut order := DrawOrder{
		depth: s.depth_index
	}
	// The draw follows the depth-only draws of the clip elements it uses.
	clip_order := s.update_clip_state_for_draw(depth_elements, draw_bounds, order.depth)
	order.depends_on_paint_order(clip_order)
	if depends_on_dst {
		order.depends_on_paint_order(s.bounds.most_recent_draw(draw_bounds))
	} else if !stroked && geometry.kind == .rect {
		// Opaque rectangles are sorted front to back.
		order.reverse_depth_as_stencil()
	}
	// With an analytic clip the destination is not only read by the renderer,
	// so there is no inner fill.
	if !stroked && step == .analytic_rrect && color.is_opaque() && !clipped {
		// The destination is only read for the coverage ramp, so Graphite
		// fills the interior with an extra non-AA draw at the same depth.
		inner := inner_fill_bounds(geometry, s.transform)
		if !inner.is_empty_negative_or_nan() {
			mut inner_order := DrawOrder{
				depth: order.depth
			}
			inner_order.depends_on_paint_order(clip_order)
			inner_order.reverse_depth_as_stencil()
			mut instance := []u8{cap: cover_bounds_instance_stride}
			write_cover_bounds_instance(mut instance, inner, s.transform, order.depth, 0)
			s.record(.cover_bounds, false, instance, uniforms, inner_order, draw_clip)
			order.depends_on_paint_order(inner_order.paint_order)
		}
	}
	mut instance := []u8{cap: analytic_rrect_instance_stride}
	if step == .cover_bounds {
		write_cover_bounds_instance(mut instance, geometry.rect, s.transform, order.depth, 0)
	} else {
		write_analytic_rrect_instance(mut instance, geometry, s.transform, paint, order.depth, 0)
	}
	s.record(step, step == .analytic_rrect || depends_on_dst, instance, uniforms, order,
		draw_clip)
	s.bounds.record_draw(draw_bounds, order.paint_order)
	$if gg_canvas_debug ? {
		eprintln('bounds depth ${order.depth}: ${draw_bounds.left()} ${draw_bounds.top()} ${draw_bounds.right()} ${draw_bounds.bottom()} local ${bounds.left()} ${bounds.top()} ${bounds.right()} ${bounds.bottom()} kind ${geometry.kind} style ${paint.style} width ${paint.stroke_width} aa ${paint.anti_alias} m ${s.transform.matrix.m}')
	}
}

// record mirrors DrawList::recordDraw() for a single-step renderer.
fn (mut s GpuSurface) record(step RenderStep, blend bool, instance []u8, uniforms PaintUniforms, order DrawOrder, clip DrawClip) {
	s.record_step(step, blend, instance, uniforms, order, 0, 0, clip, 1)
}

// record_step assigns the render step's pipeline and uniform cache indices
// in insertion order, like DrawList::recordDraw().
fn (mut s GpuSurface) record_step(step RenderStep, blend bool, instance []u8, uniforms PaintUniforms, order DrawOrder, step_index u32, phantom_instances u32, clip DrawClip, instance_count u32) {
	key := PipelineKey{step, blend, uniforms.clipped}
	mut pipeline_index := s.pipelines.index(key)
	if pipeline_index < 0 {
		pipeline_index = s.pipelines.len
		s.pipelines << key
	}
	mut uniform_index := -1
	for i, u in s.uniforms {
		if same_paint_uniforms(u, uniforms) {
			uniform_index = i
			break
		}
	}
	if uniform_index < 0 {
		uniform_index = s.uniforms.len
		s.uniforms << uniforms
	}
	s.draws << GpuDraw{
		step:              step
		instance:          instance
		uniforms:          uniforms
		blend:             blend
		order:             order
		pipeline_index:    u32(pipeline_index)
		uniform_index:     u32(uniform_index)
		step_index:        step_index
		phantom_instances: phantom_instances
		instance_count:    instance_count
		clip:              clip
	}
}

// draw_bounds mirrors DrawShape::applyStyle() in
// ClipStack::visitClipStackForDraw(): the styled shape bounds mapped to
// device space and clipped to the surface (difference clips scissor to it).
// It also returns the unclipped transformed shape bounds.
fn (s &GpuSurface) draw_bounds(paint Paint, bounds Rect) ?(Rect, Rect) {
	w := bounds.width()
	h := bounds.height()
	if !is_finite_f32(w) || !is_finite_f32(h) {
		return none
	}
	stroked := paint.style == .stroke
	if w == 0 || h == 0 {
		// An empty fill, or a zero-length butt-capped stroke, draws nothing.
		if !stroked || (paint.cap == .butt && w == 0 && h == 0) {
			return none
		}
	}
	local_aa := s.transform.local_aa_radius()
	mut local_outset := f32(0)
	hairline := stroked && paint.stroke_width == 0
	if stroked && !hairline {
		// Rects and rrects have neither miters nor caps.
		local_outset = paint.stroke_width / f32(2)
	}
	if hairline || (stroked && paint.stroke_width < local_aa) || (!stroked && (w < local_aa
		|| h < local_aa)) {
		local_outset += local_aa
	}
	mut shape_bounds := bounds
	if local_outset > 0 {
		shape_bounds = shape_bounds.outset(local_outset)
	}
	device_bounds := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	transformed := s.transform.map_rect(shape_bounds)
	outer := transformed.intersect(device_bounds)
	if outer.is_empty_negative_or_nan() {
		return none
	}
	return outer, transformed
}

fn patch_u32(mut bytes []u8, offset int, value u32) {
	bytes[offset] = u8(value)
	bytes[offset + 1] = u8(value >> 8)
	bytes[offset + 2] = u8(value >> 16)
	bytes[offset + 3] = u8(value >> 24)
}

fn instance_stride(step RenderStep) int {
	return match step {
		.analytic_rrect { analytic_rrect_instance_stride }
		.tessellate_wedges, .convex_wedges { wedge_instance_stride }
		else { cover_bounds_instance_stride }
	}
}

// TextureDataCache::kInvalidIndex, the texture field of draws without textures.
const texture_index_invalid = u64(4096)

// shape_pixel_tolerance is Shape::kDefaultPixelTolerance.
const shape_pixel_tolerance = f32(0.0039)

fn is_pixel_aligned(r Rect, t Transform) bool {
	if int(t.typ) <= int(TransformType.rect_stays_rect) {
		device := t.map_rect(r)
		return device.nearly_equals(device.round(), shape_pixel_tolerance)
	}
	return false
}

// inner_fill_bounds mirrors get_inner_bounds() in Graphite's Device.cpp.
fn inner_fill_bounds(geometry ShapeGeometry, t Transform) Rect {
	base := if geometry.kind == .rect {
		geometry.rect
	} else if geometry.rrect.is_rect() {
		geometry.rrect.rect
	} else {
		rrect_inner_bounds(geometry.rrect)
	}
	aa_inset := t.local_aa_radius()
	rect := base.inset(aa_inset)
	scale_factor := f32(1) / aa_inset
	// Only worth a second draw with a reasonable number of covered pixels.
	return if scale_factor * rect.area() >= f32(64 * 64) { rect } else { Rect.infinite_inverted() }
}

fn all_corners_relatively_circular(rr RRect, tolerance f32) bool {
	for r in rr.radii {
		if !is_relatively_circular(r.x, r.y, tolerance) {
			return false
		}
	}
	return true
}

fn is_relatively_circular(rx f32, ry f32, tolerance f32) bool {
	return abs_f32(rx - ry) <= tolerance || (rx <= tolerance && ry <= tolerance)
}

// read_pixels renders the pending draws and copies the surface into BGRA8
// premultiplied `pixels` with a row stride of `pitch` bytes.
pub fn (mut s GpuSurface) read_pixels(pixels voidptr, pitch int) ! {
	s.flush()!
	if !gfx.read_image(s.color, 0, 0, s.width, s.height, pixels, pitch) {
		return error('gg.canvas: could not read back the surface')
	}
	if s.ctx.owns_gfx {
		gfx.commit()
	}
}

// flush renders the pending draws.
pub fn (mut s GpuSurface) flush() ! {
	s.record_deferred_clip_draws()
	if s.draws.len == 0 && s.load {
		return
	}
	mut ctx := s.ctx
	// A pass with a draw that requires MSAA (depth-only clip draws, path steps)
	// is multisampled, even on a single-sampled surface.
	requires_msaa := s.draws.any(it.step in [.convex_wedges, .tessellate_wedges, .cover_fills])
	samples := if s.sample_count == 1 && requires_msaa { 4 } else { s.sample_count }
	attachments := if samples == s.sample_count {
		s.attachments
	} else {
		s.dynamic_msaa_attachments()!
	}
	// Sort like DrawList::snapDrawPass(), then batch consecutive draws with the
	// same pipeline into one instanced draw call, like its DrawWriter.
	mut keys := []SortKey{cap: s.draws.len}
	for i, draw in s.draws {
		keys << SortKey{
			pipeline_key: (u64(draw.order.paint_order) << 48) | (u64(draw.order.stencil_index) << 32) | (u64(draw.step_index) << 30) | u64(draw.pipeline_index)
			uniform_key:  (u64(draw.uniform_index) << 30) | texture_index_invalid
			draw:         i
		}
	}
	$if gg_canvas_debug ? {
		for i, draw in s.draws {
			eprintln('draw ${i} depth ${draw.order.depth} paint ${draw.order.paint_order} stencil ${draw.order.stencil_index} pipeline ${draw.pipeline_index} uniform ${draw.uniform_index} step ${draw.step}')
		}
	}
	libcpp_sort(mut keys)
	// Simulate DrawList::snapDrawPass() to find the draw call boundaries:
	// pipeline changes, uniform storage rebinds, scissor changes and full
	// instance buffers.
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	mut buffers := new_buffer_manager_sim()
	mut writer := DrawWriterSim{}
	mut tracker := UniformTrackerSim{}
	mut call_of := []int{len: keys.len} // the simulated draw call of each sorted key
	mut scissor_of := []Rect{len: keys.len}
	mut scissor := device
	mut last_pipeline := u32(0xffffffff)
	for i, key in keys {
		draw := s.draws[key.draw]
		uniform_size := if draw.uniforms.local_to_device || draw.uniforms.clipped {
			u32(clip_uniform_size)
		} else {
			u32(solid_uniform_size)
		}
		rebind := tracker.write_uniforms(int(draw.uniform_index), uniform_size, mut buffers)
		new_scissor, scissor_changes := draw_scissor(draw.clip, scissor, device)
		if draw.pipeline_index != last_pipeline {
			writer.new_pipeline_state(u32(instance_stride(draw.step)), mut buffers)
			last_pipeline = draw.pipeline_index
		} else if rebind || scissor_changes {
			writer.flush()
		}
		if scissor_changes {
			scissor = new_scissor
		}
		count := if draw.phantom_instances > 0 {
			int(draw.phantom_instances)
		} else {
			int(draw.instance_count)
		}
		for _ in 0 .. count {
			writer.append_instance(mut buffers)
		}
		call_of[i] = writer.calls.len
		scissor_of[i] = scissor
	}
	writer.flush()
	mut batches := []int{} // start index into `keys` of each real draw call, plus keys.len
	mut real_keys := []SortKey{cap: keys.len}
	mut real_scissors := []Rect{cap: keys.len}
	for i, key in keys {
		if s.draws[key.draw].phantom_instances > 0 {
			continue
		}
		if real_keys.len == 0 || call_of[i] != call_of[i - 1] || s.draws[keys[i - 1].draw].phantom_instances > 0 {
			batches << real_keys.len
		}
		real_keys << key
		real_scissors << scissor_of[i]
	}
	batches << real_keys.len
	keys = real_keys.clone()
	mut instances := []u8{}
	mut offsets := []int{cap: keys.len}
	mut batch_uniforms := [][]PaintUniforms{len: batches.len - 1}
	for b in 0 .. batches.len - 1 {
		for i in batches[b] .. batches[b + 1] {
			draw := s.draws[keys[i].draw]
			offsets << instances.len
			start := instances.len
			instances << draw.instance
			mut slot := -1
			for j, u in batch_uniforms[b] {
				if same_paint_uniforms(u, draw.uniforms) {
					slot = j
					break
				}
			}
			if slot < 0 {
				slot = batch_uniforms[b].len
				batch_uniforms[b] << draw.uniforms
			}
			stride := instance_stride(draw.step)
			ssbo_offset := match draw.step {
				.analytic_rrect { 68 }
				.convex_wedges { wedge_ssbo_offset }
				else { 20 }
			}
			for k in 0 .. int(draw.instance_count) {
				patch_u32(mut instances, start + k * stride + ssbo_offset, u32(slot))
			}
		}
	}
	mut base := 0
	if instances.len > 0 {
		ctx.ensure_instance_capacity(instances.len)
		if gfx.query_buffer_will_overflow(ctx.instances, u32(instances.len)) {
			ctx.instance_capacity = 0
			gfx.destroy_buffer(ctx.instances)
			ctx.instances = gfx.Buffer{}
			ctx.ensure_instance_capacity(instances.len)
		}
		base = gfx.append_buffer(ctx.instances, &gfx.Range{
			ptr:  instances.data
			size: u32(instances.len)
		})
	}
	mut pass := gfx.Pass{
		attachments: attachments
	}
	// Like Graphite on Metal, a multisampled pass discards its samples and
	// loads existing pixels with a draw from the resolve texture.
	load_msaa := s.load && samples > 1
	if load_msaa {
		pass.action.colors[0].load_action = .dontcare
	} else if s.load {
		pass.action.colors[0].load_action = .load
	} else {
		pass.action.colors[0].load_action = .clear
		pass.action.colors[0].clear_value = gfx.Color{s.clear_color.r, s.clear_color.g, s.clear_color.b, s.clear_color.a}
	}
	// Graphite resolves multisampled passes without storing the samples.
	pass.action.colors[0].store_action = if samples > 1 {
		gfx.StoreAction.dontcare
	} else {
		gfx.StoreAction.store
	}
	pass.action.depth.load_action = .clear
	pass.action.depth.clear_value = 1.0
	pass.action.depth.store_action = .dontcare
	gfx.begin_pass(&pass)
	if load_msaa {
		gfx.apply_pipeline(ctx.load_msaa_pipeline())
		mut bindings := gfx.Bindings{}
		bindings.fs.images[0] = s.color
		bindings.fs.samplers[0] = ctx.msaa_load_sampler
		gfx.apply_bindings(&bindings)
		gfx.draw(0, 4, 1)
	}
	intrinsics := [f32(0), 0, f32(2) / f32(s.width), -(f32(2) / f32(s.height)), 0, 0, 0, 0]!
	intrinsic_range := gfx.Range{
		ptr:  unsafe { &intrinsics[0] }
		size: intrinsic_uniform_size
	}
	for b in 0 .. batches.len - 1 {
		first := batches[b]
		mut count := 0
		for i in first .. batches[b + 1] {
			count += int(s.draws[keys[i].draw].instance_count)
		}
		draw := s.draws[keys[first].draw]
		gfx.apply_pipeline(ctx.pipeline(draw.step, samples, draw.blend, draw.uniforms.clipped))
		sc := real_scissors[first]
		gfx.apply_scissor_rect(int(sc.left()), int(sc.top()), int(sc.width()), int(sc.height()),
			true)
		mut bindings := gfx.Bindings{}
		match draw.step {
			.analytic_rrect {
				bindings.vertex_buffers[0] = ctx.rrect_vertices
				bindings.vertex_buffers[1] = ctx.instances
				bindings.vertex_buffer_offsets[1] = base + offsets[first]
				bindings.index_buffer = ctx.rrect_indices
			}
			.convex_wedges {
				bindings.vertex_buffers[0] = ctx.wedge_vertices
				bindings.vertex_buffers[1] = ctx.instances
				bindings.vertex_buffer_offsets[1] = base + offsets[first]
				bindings.index_buffer = ctx.wedge_indices
			}
			else {
				bindings.vertex_buffers[0] = ctx.instances
				bindings.vertex_buffer_offsets[0] = base + offsets[first]
			}
		}
		gfx.apply_bindings(&bindings)
		paint_uniforms := serialize_uniforms(batch_uniforms[b])
		paint_range := gfx.Range{
			ptr:  paint_uniforms.data
			size: u32(paint_uniforms.len * 4)
		}
		gfx.apply_uniforms(.vs, 0, &intrinsic_range)
		gfx.apply_uniforms(.vs, 1, &paint_range)
		if draw.step != .convex_wedges {
			gfx.apply_uniforms(.fs, 0, &intrinsic_range)
			gfx.apply_uniforms(.fs, 1, &paint_range)
		}
		match draw.step {
			.analytic_rrect { gfx.draw(0, analytic_rrect_index_count, count) }
			.convex_wedges { gfx.draw(0, wedge_index_count, count) }
			else { gfx.draw(0, 4, count) }
		}
	}
	gfx.end_pass()
	s.reset_draws()
	// Later flushes draw on top of the rendered pixels.
	s.load = true
}

// serialize_uniforms writes a batch's uniform blocks like Graphite's
// storage-buffer uniforms: the localToDevice matrix of wedge steps, or the
// paint color followed by the analytic clip data.
fn serialize_uniforms(blocks []PaintUniforms) []f32 {
	mut out := []f32{cap: blocks.len * clip_uniform_size / 4}
	for u in blocks {
		if u.local_to_device {
			for v in u.matrix {
				out << v
			}
			continue
		}
		out << u.color.r
		out << u.color.g
		out << u.color.b
		out << u.color.a
		if u.clipped {
			// CombinedUniformData: rect_5, radiusPlusHalf_5 (padded), edgeSelect_5.
			for v in u.clip.rect {
				out << v
			}
			out << u.clip.radius_plus_half[0]
			out << u.clip.radius_plus_half[1]
			out << 0
			out << 0
			for v in u.clip.edge_select {
				out << v
			}
		}
	}
	return out
}

// draw_scissor mirrors RenderStep::getScissor() for regular fills: the
// scissor to set before a draw, and whether it is set (a state change, even
// when the scissor stays the same).
fn draw_scissor(clip DrawClip, current Rect, device Rect) (Rect, bool) {
	if rect_equal(current, clip.scissor) {
		return current, false
	}
	if rect_equal(clip.shape_bounds.intersect(current), clip.draw_bounds) {
		return current, false
	}
	if rect_equal(clip.draw_bounds, clip.shape_bounds) {
		return device, true
	}
	return clip.scissor, true
}

fn rect_equal(a Rect, b Rect) bool {
	return a.vals[0] == b.vals[0] && a.vals[1] == b.vals[1] && a.vals[2] == b.vals[2]
		&& a.vals[3] == b.vals[3]
}

// dynamic_msaa_attachments returns the 4x MSAA attachments that resolve into
// a single-sampled surface, creating them on first use.
fn (mut s GpuSurface) dynamic_msaa_attachments() !gfx.Attachments {
	if s.dynamic_msaa.id == 0 {
		s.dynamic_msaa = gfx.make_image(&gfx.ImageDesc{
			render_target: true
			width:         s.width
			height:        s.height
			pixel_format:  .bgra8
			sample_count:  4
		})
		s.dynamic_depth = gfx.make_image(&gfx.ImageDesc{
			render_target: true
			width:         s.width
			height:        s.height
			pixel_format:  .depth_stencil
			sample_count:  4
		})
		mut desc := gfx.AttachmentsDesc{}
		desc.colors[0].image = s.dynamic_msaa
		desc.resolves[0].image = s.color
		desc.depth_stencil.image = s.dynamic_depth
		s.dynamic_attachments = gfx.make_attachments(&desc)
	}
	if gfx.query_attachments_state(s.dynamic_attachments) != .valid {
		return error('gg.canvas: could not create the MSAA render target')
	}
	return s.dynamic_attachments
}

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
	// The draw's texture and its index in the texture binding cache.
	texture       TextureBinding
	texture_index u64 = texture_index_invalid
mut:
	// The index count of a wedge draw's fixed-count template.
	template_count u32 = wedge_base_index_count
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
	shading Shading
}

// One depth value beyond the final shaded draw is reserved for deferred clips.
const max_draw_depth = u16(0xfffe)

// GpuSurface records draws using Skia Graphite's render steps. Pixel transfers
// use premultiplied BGRA8 on both backends; Metal rendering matches Graphite's
// Metal output, while OpenGL uses the same geometry and paint algorithms.
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
	clip        ClipStack
	// Device's IntersectionTreeSet: per paint order, the bounds of the
	// stencil draws in each disjoint stencil set.
	disjoint map[u16][][]Rect
	// The MSAA attachments of a single-sampled surface whose pass contains a
	// draw that requires MSAA, like Graphite's discardable MSAA texture.
	dynamic_msaa        gfx.Image
	dynamic_depth       gfx.Image
	dynamic_attachments gfx.Attachments
	// The render target's size: the device size, except for layers, whose
	// textures are approximately sized (SkBackingFit::kApprox).
	texture_width  int
	texture_height int
	// The draw list's texture bindings, in insertion order.
	textures []TextureBinding
	// The canvas matrix (the global CTM) and this device's mapping from the
	// global coordinates (a translation for layers).
	ctm              Matrix = Matrix.identity()
	global_to_device Matrix = Matrix.identity()
	// The active layer, which receives the draws, clips and saves until the
	// save that created it is restored, and the restored layers this device
	// samples until it flushes.
	layer    &GpuSurface = unsafe { nil }
	retired  []&GpuSurface
	is_layer bool
	origin_x int
	origin_y int
	// The saves made on this device, and the canvas matrix of each.
	save_count     int
	saved_matrices []Matrix
	// An error of a layer's restore(), reported by the next flush().
	deferred_error string
}

// new_surface creates an offscreen surface, initially transparent black.
pub fn (mut ctx GpuContext) new_surface(cfg GpuSurfaceConfig) !&GpuSurface {
	if cfg.width <= 0 || cfg.height <= 0 {
		return error('gg.canvas: invalid surface size ${cfg.width}x${cfg.height}')
	}
	if cfg.sample_count != 1 && cfg.sample_count != 4 {
		return error('gg.canvas: unsupported sample count ${cfg.sample_count}')
	}
	return ctx.new_sized_surface(cfg.width, cfg.height, cfg.width, cfg.height, cfg.sample_count)!
}

// new_sized_surface creates a device of `width` x `height` pixels whose
// render target is `texture_width` x `texture_height`.
fn (mut ctx GpuContext) new_sized_surface(width int, height int, texture_width int, texture_height int, sample_count int) !&GpuSurface {
	mut s := &GpuSurface{
		ctx:            unsafe { ctx }
		width:          width
		height:         height
		texture_width:  texture_width
		texture_height: texture_height
		sample_count:   sample_count
		transform:      Transform.new(Matrix.identity())
		bounds:         new_bounds_manager(width, height)
		clip:           new_clip_stack(width, height)
	}
	s.color = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         texture_width
		height:        texture_height
		pixel_format:  ctx.color_format()
		sample_count:  1
	})
	if sample_count > 1 {
		s.msaa = gfx.make_image(&gfx.ImageDesc{
			render_target: true
			width:         texture_width
			height:        texture_height
			pixel_format:  ctx.color_format()
			sample_count:  sample_count
		})
	}
	s.depth = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         texture_width
		height:        texture_height
		// Graphite's multisampled passes have a combined depth/stencil
		// attachment (Depth32Float_Stencil8).
		pixel_format:  if sample_count > 1 {
			gfx.PixelFormat.depth_stencil
		} else {
			gfx.PixelFormat.depth
		}
		sample_count:  sample_count
	})
	mut desc := gfx.AttachmentsDesc{}
	if sample_count > 1 {
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
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.free()
		s.layer = unsafe { nil }
	}
	s.free_retired_layers()
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
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.clear(color)
		return
	}
	s.reset_draws()
	s.clear_color = color.premul()
	s.load = false
}

fn (mut s GpuSurface) reset_draws() {
	s.draws.clear()
	s.pipelines.clear()
	s.uniforms.clear()
	s.bounds = new_bounds_manager(s.width, s.height)
	s.textures.clear()
	s.disjoint = map[u16][][]Rect{}
	s.depth_index = 0
	// Clip geometry and saves survive a new draw list, but their pending
	// usage/order/depth refer only to the draws that were just discarded.
	for mut element in s.clip.elements {
		element.usage = Rect.infinite_inverted()
		element.order = no_intersection
		element.max_z = 0
	}
}

fn (mut s GpuSurface) next_draw_depth() !u16 {
	if s.deferred_error != '' {
		return error(s.deferred_error)
	}
	if s.depth_index >= max_draw_depth {
		return error('gg.canvas: draw-list depth limit reached; flush before drawing again')
	}
	s.depth_index++
	return s.depth_index
}

// Reserve every uniform block of a draw before recording any of its steps,
// including the matrices of clips that will be recorded later at restore/flush.
fn (s &GpuSurface) check_uniform_capacity(uniforms []PaintUniforms, clip_elements []int) ! {
	if s.uniforms.len + uniforms.len + s.clip.elements.len <= max_batch_colors {
		return
	}
	mut candidates := uniforms.clone()
	for i, element in s.clip.elements {
		if element.has_pending_draw() || i in clip_elements {
			candidates << local_to_device_uniforms(element.l2d.matrix)
		}
	}
	mut additional := []PaintUniforms{}
	for candidate in candidates {
		if s.uniforms.any(same_paint_uniforms(it, candidate))
			|| additional.any(same_paint_uniforms(it, candidate)) {
			continue
		}
		if s.uniforms.len + additional.len >= max_batch_colors {
			return error('gg.canvas: draw-list uniform limit reached; flush before drawing again')
		}
		additional << candidate
	}
}

// write_pixels replaces the surface contents with BGRA8 premultiplied pixels,
// discarding pending draws.
pub fn (mut s GpuSurface) write_pixels(pixels voidptr, pitch int) ! {
	if s.layer != unsafe { nil } {
		return error('gg.canvas: cannot write pixels under a layer')
	}
	s.reset_draws()
	if s.ctx.is_gl {
		if pixels == unsafe { nil } || pitch < s.texture_width * 4 {
			return error('gg.canvas: invalid pixel buffer or pitch')
		}
		mut rgba := []u8{len: s.texture_width * s.texture_height * 4}
		swizzle_pixel_channels(pixels, pitch, rgba.data, s.texture_width * 4, s.texture_width,
			s.texture_height)
		if !gfx.write_image(s.color, 0, 0, s.texture_width, s.texture_height, rgba.data,
			s.texture_width * 4) {
			return error('gg.canvas: could not upload pixels')
		}
	} else {
		if !gfx.write_image(s.color, 0, 0, s.texture_width, s.texture_height, pixels, pitch) {
			return error('gg.canvas: could not upload pixels')
		}
	}
	s.load = true
}

// set_matrix replaces the local to device transform of later draws.
pub fn (mut s GpuSurface) set_matrix(m Matrix) {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.set_matrix(m)
		return
	}
	s.ctm = m
	s.transform = Transform.new(s.device_matrix())
}

// device_matrix maps the canvas matrix to this device, like
// SkDevice::setGlobalCTM(): layers translate it to their origin with
// SkM44's arithmetic.
fn (s &GpuSurface) device_matrix() Matrix {
	return if s.is_layer { concat_affine(s.global_to_device, s.ctm) } else { s.ctm }
}

// concat multiplies the current matrix by the affine matrix `m`, like
// SkCanvas::concat(SkM44): the product is rounded like SkM44::setConcat().
pub fn (mut s GpuSurface) concat(m Matrix) {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.concat(m)
		return
	}
	s.ctm = concat_affine(s.ctm, m)
	s.transform = Transform.new(s.device_matrix())
}

// draw_rect draws a rectangle like SkCanvas::drawRect() on a Graphite device.
// It returns an error for draws that need renderers which are not ported yet.
pub fn (mut s GpuSurface) draw_rect(r Rect, paint Paint) ! {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_rect(r, paint)!
		return
	}
	if !s.transform.valid() {
		return
	}
	if paint.style == .stroke_and_fill {
		return error('gg.canvas: stroke-and-fill needs the tessellating path renderer')
	}
	rect := r.sorted()
	if s.quick_reject(rect, paint) {
		return
	}
	s.device_draw_rect(rect, paint)!
}

// draw_line strokes a line like SkCanvas::drawLine(): drawPoints() in lines
// mode with the paint as a stroke (or hairline) style.
pub fn (mut s GpuSurface) draw_line(x0 f32, y0 f32, x1 f32, y1 f32, paint Paint) ! {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_line(x0, y0, x1, y1, paint)!
		return
	}
	if !s.transform.valid() {
		return
	}
	mut stroke := paint
	if stroke.style == .fill {
		stroke.style = .stroke
	}
	// SkCanvas::onDrawPoints() quick-rejects the sorted end points.
	if s.quick_reject(Rect.ltrb(x0, y0, x1, y1).sorted(), stroke) {
		return
	}
	s.device_draw_line(Point{x0, y0}, Point{x1, y1}, paint)!
}

// device_draw_line mirrors Device::drawPoints() for one line: the line
// Shape with SkStrokeRec(paint, kStroke_Style), which the analytic rrect
// renderer strokes with caps.
fn (mut s GpuSurface) device_draw_line(p0 Point, p1 Point, paint Paint) ! {
	mut stroke := paint
	stroke.style = .stroke
	s.draw_shape(ShapeGeometry{
		kind: .line
		rect: Rect.ltrb(p0.x, p0.y, p1.x, p1.y)
	}, stroke)!
}

// device_draw_rect mirrors Device::drawRect(), after SkCanvas sorted and
// quick-rejected the rectangle.
fn (mut s GpuSurface) device_draw_rect(r Rect, paint Paint) ! {
	mut rect := r
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
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_oval(r, paint)!
		return
	}
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
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_rrect(rr, paint)!
		return
	}
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

// draw_drrect fills the area between two rounded rectangles, like
// SkCanvas::drawDRRect() on a Graphite device.
pub fn (mut s GpuSurface) draw_drrect(outer RRect, inner RRect, paint Paint) ! {
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_drrect(outer, inner, paint)!
		return
	}
	if outer.is_empty() {
		return
	}
	if inner.is_empty() {
		s.draw_rrect(outer, paint)!
		return
	}
	// SkRect::contains() on the bounds.
	o := outer.rect
	i := inner.rect
	if !(i.left() < i.right() && i.top() < i.bottom()) || !(o.left() <= i.left()
		&& o.top() <= i.top() && o.right() >= i.right() && o.bottom() >= i.bottom()) {
		return
	}
	if !s.transform.valid() || s.quick_reject(o, paint) {
		return
	}
	s.device_draw_drrect(outer, inner, paint)!
}

// device_draw_drrect mirrors Device::drawDRRect().
fn (mut s GpuSurface) device_draw_drrect(outer RRect, inner RRect, paint Paint) ! {
	o := outer.rect
	i := inner.rect
	if paint.style != .fill {
		// SkDevice::drawDRRect(): both contours in an even-odd path.
		mut b := PathBuilder{}
		b.add_rrect(outer, .cw, 6)
		b.add_rrect(inner, .cw, 6)
		b.set_fill_type(.even_odd)
		s.draw_path(b.detach(), paint)!
		return
	}
	// Device::drawDRRect(): equal insets on all sides may be a stroked rrect.
	gap := [i.vals[0] - o.vals[0], i.vals[1] - o.vals[1], i.vals[2] - o.vals[2], i.vals[3] - o.vals[3]]!
	tolerance := shape_pixel_tolerance * s.transform.local_aa_radius()
	stroke_width := gap[0]
	mut equal_insets := true
	for g in gap {
		if !(g > tolerance && abs_f32(g - stroke_width) <= tolerance) {
			equal_insets = false
		}
	}
	if !equal_insets {
		if outer.is_rect() && inner.is_rect() {
			if diff := rect_subtract_exact(o, i) {
				s.device_draw_rect(diff, paint)!
				return
			}
		}
	} else {
		stroke_radius := f32(0.5) * stroke_width
		stroke_rect := o.inset(stroke_radius)
		mut stroke := Paint{
			...paint
			style:        .stroke
			stroke_width: stroke_width
			cap:          .butt
			miter_limit:  4
		}
		mut valid_corners := 0
		mut rect_corners := 0
		mut required_join := ?StrokeJoin(none)
		mut corners := [4]Point{}
		for c in 0 .. 4 {
			outer_radii := outer.radii[c]
			inner_radii := inner.radii[c]
			mut corner := f32(0)
			if !is_relatively_circular(outer_radii.x, outer_radii.y, tolerance) {
				break
			} else if abs_f32(outer_radii.x) <= tolerance {
				if join := required_join {
					if join != .miter {
						break
					}
				}
				required_join = .miter
				corner = 0
				rect_corners++
			} else {
				corner = outer_radii.x - stroke_radius
				if corner < -tolerance {
					break
				} else if corner <= tolerance {
					if join := required_join {
						if join != .round {
							break
						}
					}
					required_join = .round
					corner = 0
					rect_corners++
				}
			}
			expected := max_f32(0, corner - stroke_radius)
			if !is_relatively_circular(inner_radii.x, expected, tolerance)
				|| !is_relatively_circular(inner_radii.y, expected, tolerance) {
				break
			}
			corners[c] = Point{corner, corner}
			valid_corners++
		}
		if valid_corners == 4 {
			stroke.join = required_join or { StrokeJoin.round }
			if rect_corners == 4 {
				s.device_draw_rect(stroke_rect, stroke)!
			} else {
				s.draw_rrect_unchecked(RRect.rect_radii(stroke_rect, corners), stroke)!
			}
			return
		}
	}
	// Otherwise the outer shape is drawn with the inner one clipped out.
	s.save()
	defer {
		s.restore()
	}
	if inner.is_rect() {
		s.clip_shape(rect_shape(inner.rect), .difference, !paint.anti_alias)!
	} else {
		s.clip_shape(rrect_shape(inner), .difference, !paint.anti_alias)!
	}
	if outer.is_rect() {
		s.device_draw_rect(o, paint)!
	} else {
		s.draw_rrect_unchecked(outer, paint)!
	}
}

// rect_subtract_exact mirrors SkRectPriv::Subtract(): `a` minus `b`, or none
// when the difference is not a rectangle.
fn rect_subtract_exact(a Rect, b Rect) ?Rect {
	exact := subtract_rect(a, b, true)
	if rect_equal(exact, a) && sk_rects_intersect(a, b) && !rect_equal(subtract_rect(a, b, false), a) {
		return none
	}
	return exact
}

// quick_reject mirrors SkCanvas::internalQuickReject(): transparent paints
// and draws whose fast bounds miss the device (outset by 1px for AA) are
// dropped before they reach the device and take a depth value.
fn (s &GpuSurface) quick_reject(bounds Rect, paint Paint) bool {
	if !bounds.is_finite() || nothing_to_draw(paint) {
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
// nothing_to_draw mirrors SkPaint::nothingToDraw() for src-over: the alpha
// rounds to zero in SkPaint::getAlpha(), `floor(double(a * 255) + 0.5)`.
fn nothing_to_draw(paint Paint) bool {
	scaled := paint.color.a * f32(255)
	return u8(i64(math.floor(f64(scaled) + 0.5))) == 0
}

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
	if s.layer != unsafe { nil } {
		mut top := s.layer
		top.draw_msaa_trigger()
		return
	}
	s.check_uniform_capacity([identity_local_to_device, PaintUniforms{}], []) or {
		s.deferred_error = err.msg()
		return
	}
	depth := s.next_draw_depth() or {
		s.deferred_error = err.msg()
		return
	}
	// A transparent, dst-dependent paint after the empty clip, in the first
	// disjoint stencil set.
	order := DrawOrder{
		paint_order:   1
		stencil_index: 0
		depth:         depth
	}
	// Step 0 tessellates three wedges with the 64-byte localToDevice matrix
	// uniform (never equal to a paint color); step 1 covers them with the
	// transparent paint.
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	trigger_bounds := Rect.ltrb(0, 0, 0.01, 0.01)
	trigger_clip := DrawClip{
		scissor:      device
		draw_bounds:  trigger_bounds
		shape_bounds: trigger_bounds
	}
	s.record_step(.stencil_wedges_winding, false, []u8{}, identity_local_to_device, order, 0, 3,
		trigger_clip, 0)
	s.record_step(.cover_fill, true, []u8{}, PaintUniforms{}, order, 1, 1, trigger_clip, 0)
	// The path bounds (0, 0, 0.01, 0.01) are subpixel, so the clip stack
	// outsets them by the local AA radius before clipping to the device.
	path := Rect.ltrb(0, 0, f32(0.01), f32(0.01)).outset(1)
	draw_bounds := path.intersect(Rect.ltrb(0, 0, f32(s.width), f32(s.height)))
	s.add_disjoint_stencil(1, draw_bounds)
	s.bounds.record_draw(draw_bounds, 1)
}

// draw_shape mirrors Device::drawGeometry() for the analytic renderers.
fn (mut s GpuSurface) draw_shape(geometry_in ShapeGeometry, paint Paint) ! {
	if int(s.transform.typ) > int(TransformType.affine) {
		return error('gg.canvas: perspective transforms are not supported')
	}
	stroked := paint.style == .stroke
	if stroked && geometry_in.kind == .rrect && paint.stroke_width > 0 {
		tolerance := s.transform.local_aa_radius() * shape_pixel_tolerance
		if !all_corners_relatively_circular(geometry_in.rrect, tolerance) {
			// Not a simple shape: Graphite strokes it as a path.
			shape := rrect_shape(geometry_in.rrect)
			s.draw_stroke(element_path(shape), shape, paint)!
			return
		}
	}
	clip_shape := match geometry_in.kind {
		.rect {
			rect_shape(geometry_in.rect)
		}
		.rrect {
			rrect_shape(geometry_in.rrect)
		}
		.line {
			ClipShape{
				kind: .line
				rect: geometry_in.bounds()
			}
		}
	}
	clip := s.visit_clip_stack_for_draw(clip_shape, paint)!
	if clip.clipped_out {
		return
	}
	if clip.modified && clip.shape.is_flood_fill() {
		s.draw_flood_fill(clip, paint)!
		return
	}
	// The clip stack may have intersected the geometry with clip elements.
	geometry := if !clip.modified {
		geometry_in
	} else {
		match clip.shape.kind {
			.rect {
				ShapeGeometry{
					kind: .rect
					rect: clip.shape.rect
				}
			}
			.rrect {
				ShapeGeometry{
					kind:  .rrect
					rrect: clip.shape.rrect
				}
			}
			.empty {
				return error('gg.canvas: draws replaced by a flood fill are not supported')
			}
			.path, .line {
				// Only filled rects and rrects can be modified.
				geometry_in
			}
		}
	}
	clipped := !clip.analytic.is_empty()
	// Graphite draws a layer as an edge-AA quad: a pixel-aligned rect uses the
	// bounds renderer, another rect the per-edge AA quad renderer.
	uniforms, opaque := shaded_uniforms(paint, clip.analytic)!
	pixel_aligned := !stroked && geometry.kind == .rect && is_pixel_aligned(geometry.rect, s.transform)
	step := if pixel_aligned {
		RenderStep.cover_bounds
	} else if paint.layer_image != none && geometry.kind == .rect {
		RenderStep.per_edge_aa_quad
	} else {
		RenderStep.analytic_rrect
	}
	coverage := step in [.analytic_rrect, .per_edge_aa_quad]
	// Renderers with analytic coverage include their 1px AA ramp in the
	// bounds used for ordering (Clip::outsetBoundsForAA()).
	draw_bounds := if coverage {
		clip.draw_bounds.outset(1).intersect(clip.scissor)
	} else {
		clip.draw_bounds
	}
	draw_clip := DrawClip{
		scissor:      clip.scissor
		draw_bounds:  draw_bounds
		shape_bounds: if coverage {
			clip.shape_bounds.outset(1)
		} else {
			clip.shape_bounds
		}
	}
	// DstUsage: src-over with analytic coverage (from the renderer or an
	// analytic clip) always reads the destination; without coverage only a
	// translucent paint does.
	depends_on_dst := coverage || clipped || !opaque
	s.check_uniform_capacity([uniforms], clip.elements)!
	depth := s.next_draw_depth()!
	mut order := DrawOrder{
		depth: depth
	}
	// The draw follows the depth-only draws of the clip elements it uses.
	clip_order := s.update_clip_state_for_draw(clip.elements, draw_bounds, order.depth)
	order.depends_on_paint_order(clip_order)
	if depends_on_dst {
		order.depends_on_paint_order(s.bounds.most_recent_draw(draw_bounds))
	} else if !stroked && geometry.kind == .rect {
		// Opaque rectangles are sorted front to back.
		order.reverse_depth_as_stencil()
	}
	// With an analytic clip the destination is not only read by the renderer,
	// so there is no inner fill.
	if !stroked && step == .analytic_rrect && opaque && !clipped {
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
	} else if step == .per_edge_aa_quad {
		write_per_edge_quad_instance(mut instance, geometry.rect, s.transform, order.depth, 0)
	} else {
		write_analytic_rrect_instance(mut instance, geometry, s.transform, paint, order.depth, 0)
	}
	s.record(step, coverage || depends_on_dst, instance, uniforms, order,
		draw_clip)
	s.bounds.record_draw(draw_bounds, order.paint_order)
	$if gg_canvas_debug ? {
		bounds := geometry.bounds()
		eprintln('bounds depth ${order.depth}: ${draw_bounds.left()} ${draw_bounds.top()} ${draw_bounds.right()} ${draw_bounds.bottom()} local ${bounds.left()} ${bounds.top()} ${bounds.right()} ${bounds.bottom()} kind ${geometry.kind} style ${paint.style} width ${paint.stroke_width} aa ${paint.anti_alias} m ${s.transform.matrix.m}')
	}
}

// shaded_uniforms returns a draw's uniforms for its paint and analytic clip,
// and whether the paint is opaque (a gradient's colors are, like
// SkShader::isOpaque(); a layer's image is not).
fn shaded_uniforms(paint Paint, analytic AnalyticClip) !(PaintUniforms, bool) {
	clipped := !analytic.is_empty()
	color := paint.color.premul()
	mut shading, gradient := paint.shading()!
	mut image := ImageUniforms{}
	mut texture := TextureBinding{}
	if layer := paint.layer_image {
		shading = .image
		image = ImageUniforms{
			// The identity local matrix of drawSpecial()'s Rect2Rect(src, dst).
			upper2x2:       [f32(1), 0, 0, 1]!
			inv_dimensions: [f32(1) / f32(layer.texture_width), f32(1) / f32(layer.texture_height)]!
		}
		texture = TextureBinding{
			kind:  .image
			image: layer.image
		}
	}
	opaque := color.is_opaque() && shading != .image
		&& (!shading.is_gradient() || gradient.opaque)
	return PaintUniforms{
		color:    color
		shading:  shading
		gradient: gradient
		image:    image
		texture:  texture
		clipped:  clipped
		clip:     if clipped {
			analytic_clip_uniforms(analytic)
		} else {
			AnalyticClipUniforms{}
		}
	}, opaque
}

// draw_flood_fill mirrors Device::drawGeometry() for a draw the clip stack
// replaced with a flood fill (an inverse-filled empty shape): the
// coverage-less bounds renderer fills the scissor, or an opaque fill of the
// whole surface clears the pending draws.
fn (mut s GpuSurface) draw_flood_fill(clip ClipResult, paint Paint) ! {
	clipped := !clip.analytic.is_empty()
	uniforms, opaque := shaded_uniforms(paint, clip.analytic)!
	color := uniforms.color
	if !clipped && opaque && uniforms.shading == .solid && clip.elements.len == 0
		&& clip.scissor.contains(s.device_bounds()) {
		if s.sample_count != 1 {
			return error('gg.canvas: clearing a multisampled pass is not supported')
		}
		// DrawContext::clear(): the pass starts from this color; the depth and
		// bounds of the discarded draws remain in use.
		s.draws.clear()
		s.pipelines.clear()
		s.uniforms.clear()
		s.textures.clear()
		s.clear_color = color
		s.load = false
		return
	}
	depends_on_dst := clipped || !opaque
	s.check_uniform_capacity([uniforms], clip.elements)!
	depth := s.next_draw_depth()!
	mut order := DrawOrder{
		depth: depth
	}
	clip_order := s.update_clip_state_for_draw(clip.elements, clip.draw_bounds, order.depth)
	order.depends_on_paint_order(clip_order)
	if depends_on_dst {
		order.depends_on_paint_order(s.bounds.most_recent_draw(clip.draw_bounds))
	}
	// Inverse fills encode device-space bounds as [right, bottom, left, top].
	sc := clip.scissor
	mut instance := []u8{cap: cover_bounds_instance_stride}
	write_cover_bounds_instance(mut instance, Rect{
		vals: [sc.right(), sc.bottom(), -sc.left(), -sc.top()]!
	}, s.transform, order.depth, 0)
	s.record(.cover_bounds, depends_on_dst, instance, uniforms, order, DrawClip{
		scissor:      clip.scissor
		draw_bounds:  clip.draw_bounds
		shape_bounds: clip.shape_bounds
	})
	s.bounds.record_draw(clip.draw_bounds, order.paint_order)
}

// record mirrors DrawList::recordDraw() for a single-step renderer.
fn (mut s GpuSurface) record(step RenderStep, blend bool, instance []u8, uniforms PaintUniforms, order DrawOrder, clip DrawClip) {
	s.record_step(step, blend, instance, uniforms, order, 0, 0, clip, 1)
}

// record_step assigns the render step's pipeline and uniform cache indices
// in insertion order, like DrawList::recordDraw().
fn (mut s GpuSurface) record_step(step RenderStep, blend bool, instance []u8, uniforms PaintUniforms, order DrawOrder, step_index u32, phantom_instances u32, clip DrawClip, instance_count u32) {
	key := PipelineKey{step, blend, uniforms.clipped, uniforms.shading}
	mut pipeline_index := s.pipelines.index(key)
	if pipeline_index < 0 {
		pipeline_index = s.pipelines.len
		s.pipelines << key
	}
	mut uniform_index := if has_uniforms(step) { -1 } else { uniform_index_invalid }
	for i, u in s.uniforms {
		if uniform_index >= 0 {
			break
		}
		if same_paint_uniforms(u, uniforms) {
			uniform_index = i
		}
	}
	if uniform_index < 0 {
		uniform_index = s.uniforms.len
		s.uniforms << uniforms
	}
	// TextureDataCache: gradients share the dither LUT binding.
	texture := if uniforms.shading == .image {
		uniforms.texture
	} else if uniforms.shading.is_gradient() {
		TextureBinding{
			kind: .dither
		}
	} else {
		TextureBinding{}
	}
	mut texture_index := texture_index_invalid
	if texture.kind != .none {
		mut found := -1
		for i, t in s.textures {
			if same_texture_binding(t, texture) {
				found = i
				break
			}
		}
		if found < 0 {
			found = s.textures.len
			s.textures << texture
		}
		texture_index = u64(found)
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
		texture:           texture
		texture_index:     texture_index
	}
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
		.per_edge_aa_quad { per_edge_quad_instance_stride }
		.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges, .wedges_shaded {
			wedge_instance_stride
		}
		.tessellate_strokes {
			stroke_instance_stride
		}
		else { cover_bounds_instance_stride }
	}
}

// TextureDataCache::kInvalidIndex, the texture field of draws without textures.
const texture_index_invalid = u64(4096)

// UniformDataCache::kInvalidIndex, the uniform field of draws without uniforms.
const uniform_index_invalid = 4096

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

// is_relatively_circular mirrors SkRRectPriv::IsRelativelyCircular() as built
// with SK_GRAPHITE_USE_LEGACY_RRECT_CLIP_SHADER.
fn is_relatively_circular(rx f32, ry f32, tolerance f32) bool {
	return abs_f32(rx - ry) <= tolerance * max_f32(rx, ry)
}

// read_pixels renders the pending draws and copies the surface into BGRA8
// premultiplied `pixels` with a row stride of `pitch` bytes.
pub fn (mut s GpuSurface) read_pixels(pixels voidptr, pitch int) ! {
	s.flush()!
	if s.ctx.is_gl {
		if pixels == unsafe { nil } || pitch < s.width * 4 {
			return error('gg.canvas: invalid pixel buffer or pitch')
		}
		mut rgba := []u8{len: s.width * s.height * 4}
		if !gfx.read_image(s.color, 0, 0, s.width, s.height, rgba.data, s.width * 4) {
			return error('gg.canvas: could not read back the surface')
		}
		swizzle_pixel_channels(rgba.data, s.width * 4, pixels, pitch, s.width, s.height)
	} else {
		if !gfx.read_image(s.color, 0, 0, s.width, s.height, pixels, pitch) {
			return error('gg.canvas: could not read back the surface')
		}
	}
	if s.ctx.owns_gfx {
		gfx.commit()
	}
}

// OpenGL render targets use RGBA8; the public pixel transfer API uses BGRA8.
// The caller has checked both row pitches and the pixel buffer pointers.
fn swizzle_pixel_channels(source voidptr, source_pitch int, destination voidptr, destination_pitch int, width int, height int) {
	src := unsafe { &u8(source) }
	dst := unsafe { &u8(destination) }
	for y in 0 .. height {
		for x in 0 .. width {
			i := y * source_pitch + x * 4
			j := y * destination_pitch + x * 4
			unsafe {
				dst[j] = src[i + 2]
				dst[j + 1] = src[i + 1]
				dst[j + 2] = src[i]
				dst[j + 3] = src[i + 3]
			}
		}
	}
}

// flush renders the pending draws.
pub fn (mut s GpuSurface) flush() ! {
	if s.layer != unsafe { nil } {
		return error('gg.canvas: a layer is still active')
	}
	if s.deferred_error != '' {
		message := s.deferred_error
		s.deferred_error = ''
		s.reset_draws()
		s.free_retired_layers()
		return error(message)
	}
	s.record_deferred_clip_draws()
	if s.draws.len == 0 && s.load {
		return
	}
	mut ctx := s.ctx
	// A pass with a draw that requires MSAA (depth-only clip draws, path steps)
	// is multisampled, even on a single-sampled surface.
	requires_msaa := s.draws.any(is_wedge_step(it.step) || it.step in [.cover_fill,
		.cover_inverse_fill, .tessellate_strokes, .cover_inverse_clip, .cover_clip])
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
			uniform_key:  (u64(draw.uniform_index) << 30) | draw.texture_index
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
	// TextureTracker: the last valid binding.
	mut last_texture := texture_index_invalid
	for i, key in keys {
		draw := s.draws[key.draw]
		uniform_size := u32(step_uniform_size(draw.step, draw.uniforms.clipped, draw.uniforms.shading))
		rebind := if has_uniforms(draw.step) {
			tracker.write_uniforms(int(draw.uniform_index), uniform_size, mut buffers)
		} else {
			false
		}
		new_scissor, scissor_changes := draw_scissor(draw.clip, scissor, device)
		texture_changes := draw.texture_index != texture_index_invalid
			&& draw.texture_index != last_texture
		if texture_changes {
			last_texture = draw.texture_index
		}
		if draw.pipeline_index != last_pipeline {
			writer.new_pipeline_state(u32(instance_stride(draw.step)), mut buffers)
			last_pipeline = draw.pipeline_index
		} else if rebind || scissor_changes || texture_changes {
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
	mut storage_uniforms := []f32{}
	for b in 0 .. batches.len - 1 {
		first_draw := s.draws[keys[batches[b]].draw]
		mut uniform_slot_offset := 0
		if ctx.is_gl && has_uniforms(first_draw.step) {
			// Each shader views the same storage buffer as an array of its
			// own paint struct. Align a batch to that struct's stride and
			// add its base index to the instances instead of rebinding.
			stride := step_uniform_size(first_draw.step, first_draw.uniforms.clipped,
				first_draw.uniforms.shading) / 4
			for storage_uniforms.len % stride != 0 {
				storage_uniforms << f32(0)
			}
			uniform_slot_offset = storage_uniforms.len / stride
		}
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
				if slot >= max_batch_colors {
					s.reset_draws()
					s.free_retired_layers()
					return error('gg.canvas: batch exceeds the shader uniform block')
				}
				batch_uniforms[b] << draw.uniforms
			}
			stride := instance_stride(draw.step)
			ssbo_offset := match draw.step {
				.analytic_rrect { 68 }
				.per_edge_aa_quad { 40 }
				.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges, .wedges_shaded {
					wedge_ssbo_offset
				}
				.tessellate_strokes {
					stroke_ssbo_offset
				}
				else { 20 }
			}
			for k in 0 .. int(draw.instance_count) {
				patch_u32(mut instances, start + k * stride + ssbo_offset, u32(slot + uniform_slot_offset))
			}
		}
		if ctx.is_gl && has_uniforms(first_draw.step) {
			storage_uniforms << serialize_uniforms(batch_uniforms[b])
		}
	}
	mut paint_buffer := gfx.Buffer{}
	if storage_uniforms.len > 0 {
		paint_buffer = gfx.make_buffer(&gfx.BufferDesc{
			@type: .storagebuffer
			data:  gfx.Range{
				ptr:  storage_uniforms.data
				size: u32(storage_uniforms.len * 4)
			}
		})
		if gfx.query_buffer_state(paint_buffer) != .valid {
			gfx.destroy_buffer(paint_buffer)
			s.reset_draws()
			s.free_retired_layers()
			return error('gg.canvas: could not create the paint storage buffer')
		}
	}
	defer {
		if paint_buffer.id != 0 {
			gfx.destroy_buffer(paint_buffer)
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
	// Create the pipelines before the pass, so a failure leaves no pass open.
	for b in 0 .. batches.len - 1 {
		draw := s.draws[keys[batches[b]].draw]
		pipeline := ctx.pipeline(draw.step, samples, draw.blend, draw.uniforms.clipped,
			draw.uniforms.shading)
		if pipeline.id == 0 || gfx.query_pipeline_state(pipeline) != .valid {
			s.reset_draws()
			return error('gg.canvas: could not create a pipeline (sokol.gfx pools exhausted?)')
		}
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
	// GL row zero is the logical canvas top. Positive viewport Y keeps
	// fragment coordinates, layer sampling and pixel transfers in the same
	// coordinate system without flipping textures or readback rows.
	y_scale := (if ctx.is_gl { f32(2) } else { f32(-2) }) / f32(s.texture_height)
	intrinsics := [f32(0), 0, f32(2) / f32(s.texture_width), y_scale, 0, 0, 0, 0]!
	intrinsic_range := gfx.Range{
		ptr:  unsafe { &intrinsics[0] }
		size: intrinsic_uniform_size
	}
	for b in 0 .. batches.len - 1 {
		first := batches[b]
		mut count := 0
		// A batch of wedge draws uses the largest template of its draws
		// (DrawWriter::DynamicInstances).
		mut template_count := u32(0)
		for i in first .. batches[b + 1] {
			count += int(s.draws[keys[i].draw].instance_count)
			template_count = math.max(template_count, s.draws[keys[i].draw].template_count)
		}
		draw := s.draws[keys[first].draw]
		gfx.apply_pipeline(ctx.pipeline(draw.step, samples, draw.blend, draw.uniforms.clipped,
			draw.uniforms.shading))
		sc := real_scissors[first]
		gfx.apply_scissor_rect(int(sc.left()), int(sc.top()), int(sc.width()), int(sc.height()),
			!ctx.is_gl)
		mut bindings := gfx.Bindings{}
		match draw.step {
			.per_edge_aa_quad {
				bindings.vertex_buffers[0] = ctx.quad_vertices
				bindings.vertex_buffers[1] = ctx.instances
				bindings.vertex_buffer_offsets[1] = base + offsets[first]
				bindings.index_buffer = ctx.quad_indices
			}
			.analytic_rrect {
				bindings.vertex_buffers[0] = ctx.rrect_vertices
				bindings.vertex_buffers[1] = ctx.instances
				bindings.vertex_buffer_offsets[1] = base + offsets[first]
				bindings.index_buffer = ctx.rrect_indices
			}
			.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges, .wedges_shaded {
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
		match draw.texture.kind {
			.dither {
				bindings.fs.images[0], bindings.fs.samplers[0] = ctx.dither_bindings()
			}
			.image {
				bindings.fs.images[0] = draw.texture.image
				bindings.fs.samplers[0] = ctx.layer_sampler()
			}
			.none {}
		}
		if ctx.is_gl && has_uniforms(draw.step) {
			bindings.vs.storage_buffers[0] = paint_buffer
			bindings.fs.storage_buffers[0] = paint_buffer
		}
		gfx.apply_bindings(&bindings)
		gfx.apply_uniforms(.vs, 0, &intrinsic_range)
		if writes_color(draw.step) {
			gfx.apply_uniforms(.fs, 0, &intrinsic_range)
		}
		if !ctx.is_gl {
			paint_uniforms := serialize_uniforms(batch_uniforms[b])
			paint_range := gfx.Range{
				ptr:  paint_uniforms.data
				size: u32(paint_uniforms.len * 4)
			}
			gfx.apply_uniforms(.vs, 1, &paint_range)
			if writes_color(draw.step) {
				gfx.apply_uniforms(.fs, 1, &paint_range)
			}
		}
		match draw.step {
			.analytic_rrect { gfx.draw(0, analytic_rrect_index_count, count) }
			.per_edge_aa_quad { gfx.draw(0, per_edge_quad_index_count, count) }
			.stencil_wedges_winding, .stencil_wedges_even_odd, .convex_wedges, .wedges_shaded,
			.tessellate_strokes {
				gfx.draw(0, int(template_count), count)
			}
			else { gfx.draw(0, 4, count) }
		}
	}
	gfx.end_pass()
	s.reset_draws()
	// The pass sampled the restored layers.
	s.free_retired_layers()
	// Later flushes draw on top of the rendered pixels.
	s.load = true
}

fn (mut s GpuSurface) free_retired_layers() {
	for mut layer in s.retired {
		layer.free()
	}
	s.retired.clear()
}

// serialize_uniforms writes a batch's uniform blocks like Graphite's
// storage-buffer uniforms: the localToDevice matrix of wedge steps, or the
// paint color followed by the analytic clip data.
fn serialize_uniforms(blocks []PaintUniforms) []f32 {
	mut out := []f32{cap: blocks.len * clip_uniform_size / 4}
	for u in blocks {
		if u.shading.is_gradient() {
			u.gradient.serialize(u.shading, mut out)
		} else if u.shading == .image {
			u.image.serialize(mut out)
		} else if !u.local_to_device || u.with_color {
			out << u.color.r
			out << u.color.g
			out << u.color.b
			out << u.color.a
		}
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
		if u.local_to_device {
			// Strokes use the 2x2, translation and max scale (padded).
			for v in u.matrix[..if u.stroke_xform { 8 } else { 16 }] {
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
			width:         s.texture_width
			height:        s.texture_height
			pixel_format:  s.ctx.color_format()
			sample_count:  4
		})
		s.dynamic_depth = gfx.make_image(&gfx.ImageDesc{
			render_target: true
			width:         s.texture_width
			height:        s.texture_height
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

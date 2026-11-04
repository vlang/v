module canvas

// ClipOp combines a clip shape with the current clip, like SkClipOp.
pub enum ClipOp {
	intersect
	difference
}

enum ClipState {
	wide_open
	empty
	complex
}

// ClipElement is a clip rectangle in device space. Rect elements have equal
// inner and outer bounds.
struct ClipElement {
	rect Rect
mut:
	valid bool = true
	// The draws clipped by the element since its depth-only draw was last
	// recorded: RawElement's fOrder, fMaxZ and fUsageBounds.
	pending bool
	order   u16
	max_z   u16
	usage   Rect
}

// ClipStackState mirrors the single save record of Graphite's ClipStack for
// difference rectangles: its aggregate bounds and its elements.
struct ClipStackState {
mut:
	state ClipState
	outer Rect
	inner Rect
	// Elements before oldest_valid can no longer affect draws.
	elements     []ClipElement
	oldest_valid int
}

fn new_clip_stack(width int, height int) ClipStackState {
	device := Rect.ltrb(0, 0, f32(width), f32(height))
	return ClipStackState{
		outer: device
		inner: device
	}
}

enum ClipInfluence {
	none
	clipped_out
	analytic
}

// AnalyticClipUniforms is Graphite's NonMSAAClip uniform data for a rectangle:
// the clip bounds outset by half a pixel, (±(radius + 0.5), 1 / (radius + 0.5))
// with the sign selecting difference, and the edges adjacent to round corners.
struct AnalyticClipUniforms {
	rect             [4]f32
	radius_plus_half [2]f32
	edge_select      [4]f32
}

// PaintUniforms is a draw's uniform block: its premultiplied color and, for
// draws with an analytic clip, the clip data; or for wedge steps without a
// paint, their localToDevice matrix.
struct PaintUniforms {
	color           Color4f
	clipped         bool
	clip            AnalyticClipUniforms
	local_to_device bool
	matrix          [16]f32
}

// identity_local_to_device is the uniform block of draws in device space.
const identity_local_to_device = PaintUniforms{
	local_to_device: true
	matrix:          [f32(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]!
}

// The analytic-clip paint uniforms: float4 color, float4 rect, float2
// radiusPlusHalf (padded to 16 bytes) and float4 edgeSelect.
const clip_uniform_size = 64

// clip_rect clips later draws to `r` in local coordinates, like
// SkCanvas::clipRect() on a Graphite device. Only difference clips are
// supported so far, under transforms that keep rectangles axis-aligned.
pub fn (mut s GpuSurface) clip_rect(r Rect, op ClipOp, anti_alias bool) ! {
	if op != .difference {
		return error('gg.canvas: intersect clips are not supported yet')
	}
	if s.clip.state == .empty || !s.transform.valid() {
		return
	}
	if int(s.transform.typ) > int(TransformType.rect_stays_rect) {
		return error('gg.canvas: clips under transforms that rotate or skew are not supported')
	}
	// RawElement(): map to device space, clip to the device and snap non-AA
	// rectangles to pixels. An empty difference clip has no effect.
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	mut element := s.transform.map_rect(r.sorted()).intersect(device)
	if !element.is_empty_negative_or_nan() && !anti_alias {
		element = element.round()
	}
	if element.is_empty_negative_or_nan() {
		return
	}
	for removed in s.clip.add_difference(element, device) {
		s.draw_clip(removed)
	}
}

// add_difference mirrors SaveRecord::addElement() for a difference rectangle.
// It returns the elements it removes, in the order Graphite draws their
// pending depth-only draws.
fn (mut c ClipStackState) add_difference(e Rect, device Rect) []ClipElement {
	if c.state == .wide_open {
		// Intersect (device) + Difference (e)
		if e.contains(device) {
			c.state = .empty
			mut removed := c.elements.reverse()
			c.elements.clear()
			c.oldest_valid = 0
			return removed
		}
		return c.replace_with(e)
	}
	// Difference (stack) + Difference (e); the stack only checks its bounds.
	if c.inner.contains(e) {
		return []ClipElement{}
	}
	if e.contains(c.outer) {
		return c.replace_with(e)
	}
	c.outer = c.outer.join(e)
	if e.area() > c.inner.area() {
		c.inner = e
	}
	return c.append(e)
}

fn (mut c ClipStackState) replace_with(e Rect) []ClipElement {
	c.state = .complex
	c.inner = e
	c.outer = e
	removed := c.elements.reverse()
	c.elements = [ClipElement{
		rect: e
	}]
	c.oldest_valid = 0
	return removed
}

// append mirrors SaveRecord::appendElement() for a stack with one save record.
fn (mut c ClipStackState) append(e Rect) []ClipElement {
	mut youngest_valid := -1
	mut oldest_valid := c.elements.len
	mut oldest_active_invalid := -1
	for i := c.elements.len - 1; i >= c.oldest_valid; i-- {
		mut existing := &c.elements[i]
		if existing.valid {
			// RawElement::updateForElement(): Difference (A) + Difference (B)
			if existing.rect.contains(e) {
				// The new element changes nothing.
				return []ClipElement{}
			} else if e.contains(existing.rect) {
				existing.valid = false
			}
		}
		if !existing.valid {
			oldest_active_invalid = i
		} else {
			oldest_valid = i
			if i > youngest_valid {
				youngest_valid = i
			}
		}
	}
	invalid_index := if oldest_active_invalid >= 0 { oldest_active_invalid } else { c.elements.len }
	c.oldest_valid = if oldest_valid < invalid_index { oldest_valid } else { invalid_index }
	c.state = .complex
	mut target := youngest_valid + 1
	mut reuse := oldest_active_invalid
	if reuse < 0 || reuse >= target {
		target++
		reuse = -1
	}
	mut removed := []ClipElement{}
	for c.elements.len > target {
		removed << c.elements.pop()
	}
	added := ClipElement{
		rect: e
	}
	if reuse >= 0 {
		removed << c.elements[reuse]
		c.elements[reuse] = added
	} else if c.elements.len < target {
		c.elements << added
	} else {
		removed << c.elements[c.elements.len - 1]
		c.elements[c.elements.len - 1] = added
	}
	return removed
}

// clip_for_draw mirrors ClipStack::visitClipStackForDraw() for a draw whose
// styled device bounds are `outer`. It returns the analytic clip element's
// rectangle and the indices of the elements that need depth-only clip draws.
fn (s &GpuSurface) clip_for_draw(outer Rect) !(ClipInfluence, Rect, []int) {
	c := s.clip
	if c.state == .empty {
		return ClipInfluence.clipped_out, Rect{}, []int{}
	}
	if c.state == .wide_open {
		return ClipInfluence.none, Rect{}, []int{}
	}
	// Difference (stack) + Intersect (draw)
	if !outer.intersects(c.outer) {
		return ClipInfluence.none, Rect{}, []int{}
	}
	if c.inner.contains(outer) {
		return ClipInfluence.clipped_out, Rect{}, []int{}
	}
	rotated := int(s.transform.typ) > int(TransformType.rect_stays_rect)
	mut influence := ClipInfluence.none
	mut analytic := Rect{}
	mut depth_elements := []int{}
	mut unsupported := ''
	// Newest elements first; the first one that interacts becomes the
	// analytic clip, and any element can still clip the draw out entirely.
	for i := c.elements.len - 1; i >= c.oldest_valid; i-- {
		element := c.elements[i]
		if !element.valid || !outer.intersects(element.rect) {
			continue
		}
		if rotated {
			unsupported = 'gg.canvas: clipping rotated or skewed draws is not supported'
			continue
		}
		if element.rect.contains(outer) {
			return ClipInfluence.clipped_out, Rect{}, []int{}
		}
		if influence == .analytic {
			// Later elements are drawn into the depth attachment.
			depth_elements << i
			continue
		}
		influence = .analytic
		analytic = element.rect
	}
	if unsupported != '' {
		return error(unsupported)
	}
	return influence, analytic, depth_elements
}

// analytic_clip_uniforms mirrors PaintParams' NonMSAAClip data for a
// difference rectangle (kRadiusMin, no rounded edges, not inverted).
fn analytic_clip_uniforms(rect Rect) AnalyticClipUniforms {
	radius := f32(0.5) + f32(0.5)
	bounds := rect.outset(0.5)
	return AnalyticClipUniforms{
		rect:             [bounds.left(), bounds.top(), bounds.right(), bounds.bottom()]!
		radius_plus_half: [-radius, f32(1) / radius]!
	}
}

// conservative_clip_bounds mirrors Device::devClipBounds(): the clip's
// conservative bounds rounded out to pixels, or none for an empty clip.
fn (s &GpuSurface) conservative_clip_bounds() ?Rect {
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	return match s.clip.state {
		.empty { none }
		.wide_open { device }
		.complex { subtract_rect(device, s.clip.inner, true).round_out() }
	}
}

// subtract_rect mirrors ClipStack's subtract(): `a` minus `b` when that is a
// rectangle, otherwise `a` when `exact` or else the largest part of `a`
// outside `b` (SkRectPriv::Subtract()).
fn subtract_rect(a Rect, b Rect, exact bool) Rect {
	if a.is_empty_negative_or_nan() || b.is_empty_negative_or_nan() || !sk_rects_intersect(a, b) {
		return a
	}
	a_height := a.height()
	a_width := a.width()
	mut left_area, mut right_area, mut top_area, mut bottom_area := f32(0), f32(0), f32(0), f32(0)
	mut positive := 0
	if b.left() > a.left() {
		left_area = (b.left() - a.left()) / a_width
		positive++
	}
	if a.right() > b.right() {
		right_area = (a.right() - b.right()) / a_width
		positive++
	}
	if b.top() > a.top() {
		top_area = (b.top() - a.top()) / a_height
		positive++
	}
	if a.bottom() > b.bottom() {
		bottom_area = (a.bottom() - b.bottom()) / a_height
		positive++
	}
	if positive == 0 {
		return Rect.ltrb(0, 0, 0, 0)
	}
	if positive > 1 && exact {
		return a
	}
	mut l, mut t, mut r, mut bt := a.left(), a.top(), a.right(), a.bottom()
	if left_area > right_area && left_area > top_area && left_area > bottom_area {
		r = b.left()
	} else if right_area > top_area && right_area > bottom_area {
		l = b.right()
	} else if top_area > bottom_area {
		bt = b.top()
	} else {
		t = b.bottom()
	}
	return Rect.ltrb(l, t, r, bt)
}

// sk_rects_intersect mirrors SkRect::Intersects(): a non-empty overlap.
fn sk_rects_intersect(a Rect, b Rect) bool {
	l := max_f32(a.left(), b.left())
	r := min_f32(a.right(), b.right())
	t := max_f32(a.top(), b.top())
	bt := min_f32(a.bottom(), b.bottom())
	return l < r && t < bt
}

// same_paint_uniforms compares uniform blocks with float equality, like the
// byte-keyed cache for these values (a NaN block is never shared).
fn same_paint_uniforms(a PaintUniforms, b PaintUniforms) bool {
	if a.local_to_device || b.local_to_device {
		return a.local_to_device == b.local_to_device && a.matrix == b.matrix
	}
	if a.clipped != b.clipped || a.color.r != b.color.r || a.color.g != b.color.g
		|| a.color.b != b.color.b || a.color.a != b.color.a {
		return false
	}
	if !a.clipped {
		return true
	}
	for i in 0 .. 4 {
		if a.clip.rect[i] != b.clip.rect[i] || a.clip.edge_select[i] != b.clip.edge_select[i] {
			return false
		}
	}
	return a.clip.radius_plus_half[0] == b.clip.radius_plus_half[0]
		&& a.clip.radius_plus_half[1] == b.clip.radius_plus_half[1]
}

// snap_scissor mirrors ClipStack's snap_scissor(): `a` rounded out to the
// next multiple of 4 pixels and clipped to the device.
fn snap_scissor(a Rect, device Rect) Rect {
	o := a.outset(3)
	quarter := Rect{
		vals: [o.vals[0] * f32(0.25), o.vals[1] * f32(0.25), o.vals[2] * f32(0.25),
			o.vals[3] * f32(0.25)]!
	}
	r := quarter.round_out()
	snapped := Rect{
		vals: [r.vals[0] * 4, r.vals[1] * 4, r.vals[2] * 4, r.vals[3] * 4]!
	}
	return snapped.intersect(device)
}

// update_clip_state_for_draw mirrors ClipStack::updateClipStateForDraw(): the
// elements that clip a draw at depth `z` with bounds `draw_bounds` record
// their usage, and the draw is ordered after the latest of their depth-only
// draws.
fn (mut s GpuSurface) update_clip_state_for_draw(elements []int, draw_bounds Rect, z u16) u16 {
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	snapped := snap_scissor(draw_bounds, device)
	mut max_order := no_intersection
	for i in elements {
		mut e := &s.clip.elements[i]
		if !e.pending {
			// Ordered after the most recent draw under the element's bounds.
			e.usage = snapped
			e.max_z = z
			e.order = s.bounds.most_recent_draw(snap_scissor(e.rect, device)) + 1
			e.pending = true
		} else {
			e.usage = e.usage.join(snapped)
			if z > e.max_z {
				e.max_z = z
			}
		}
		if e.order > max_order {
			max_order = e.order
		}
	}
	return max_order
}

// draw_clip mirrors RawElement::drawClip(): an element that clipped draws is
// drawn into the depth attachment at one past their largest depth, with its
// pending usage as the scissor.
fn (mut s GpuSurface) draw_clip(e ClipElement) {
	if !e.pending {
		return
	}
	device := Rect.ltrb(0, 0, f32(s.width), f32(s.height))
	snapped_outer := snap_scissor(e.rect, device)
	mut scissor := e.usage.intersect(snapped_outer)
	if f32(0.5) * snapped_outer.area() < scissor.area() {
		scissor = snapped_outer
	}
	draw_bounds := e.rect.intersect(scissor)
	if draw_bounds.is_empty_negative_or_nan() {
		return
	}
	order := DrawOrder{
		depth:       e.max_z + 1
		paint_order: e.order
	}
	mut instance := []u8{cap: 4 * wedge_instance_stride}
	write_rect_wedge_patches(mut instance, e.rect, order.depth)
	s.record_step(.convex_wedges, false, instance, identity_local_to_device, order, 0, 0,
		DrawClip{
			scissor:      scissor
			draw_bounds:  draw_bounds
			shape_bounds: e.rect
		}, 4)
	if order.depth > s.depth_index {
		s.depth_index = order.depth
	}
}

// record_deferred_clip_draws mirrors ClipStack::recordDeferredClipDraws()
// before a flush; elements then collect their usage again.
fn (mut s GpuSurface) record_deferred_clip_draws() {
	for i in 0 .. s.clip.elements.len {
		s.draw_clip(s.clip.elements[i])
		s.clip.elements[i].pending = false
	}
}

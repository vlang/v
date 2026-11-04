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

// PaintUniforms is a draw's paint uniform block: its premultiplied color and,
// for draws with an analytic clip, the clip data.
struct PaintUniforms {
	color   Color4f
	clipped bool
	clip    AnalyticClipUniforms
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
	s.clip.add_difference(element, device)
}

// add_difference mirrors SaveRecord::addElement() for a difference rectangle.
fn (mut c ClipStackState) add_difference(e Rect, device Rect) {
	if c.state == .wide_open {
		// Intersect (device) + Difference (e)
		if e.contains(device) {
			c.state = .empty
			c.elements.clear()
			c.oldest_valid = 0
			return
		}
		c.replace_with(e)
		return
	}
	// Difference (stack) + Difference (e); the stack only checks its bounds.
	if c.inner.contains(e) {
		return
	}
	if e.contains(c.outer) {
		c.replace_with(e)
		return
	}
	c.outer = c.outer.join(e)
	if e.area() > c.inner.area() {
		c.inner = e
	}
	c.append(e)
}

fn (mut c ClipStackState) replace_with(e Rect) {
	c.state = .complex
	c.inner = e
	c.outer = e
	c.elements = [ClipElement{
		rect: e
	}]
	c.oldest_valid = 0
}

// append mirrors SaveRecord::appendElement() for a stack with one save record.
fn (mut c ClipStackState) append(e Rect) {
	mut youngest_valid := -1
	mut oldest_valid := c.elements.len
	mut oldest_active_invalid := -1
	for i := c.elements.len - 1; i >= c.oldest_valid; i-- {
		mut existing := &c.elements[i]
		if existing.valid {
			// RawElement::updateForElement(): Difference (A) + Difference (B)
			if existing.rect.contains(e) {
				// The new element changes nothing.
				return
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
	for c.elements.len > target {
		c.elements.delete_last()
	}
	added := ClipElement{
		rect: e
	}
	if reuse >= 0 {
		c.elements[reuse] = added
	} else if c.elements.len < target {
		c.elements << added
	} else {
		c.elements[c.elements.len - 1] = added
	}
}

// clip_for_draw mirrors ClipStack::visitClipStackForDraw() for a draw whose
// styled device bounds are `outer`. It returns the analytic clip element's
// rectangle for draws that need one.
fn (s &GpuSurface) clip_for_draw(outer Rect) !(ClipInfluence, Rect) {
	c := s.clip
	if c.state == .empty {
		return ClipInfluence.clipped_out, Rect{}
	}
	if c.state == .wide_open {
		return ClipInfluence.none, Rect{}
	}
	// Difference (stack) + Intersect (draw)
	if !outer.intersects(c.outer) {
		return ClipInfluence.none, Rect{}
	}
	if c.inner.contains(outer) {
		return ClipInfluence.clipped_out, Rect{}
	}
	rotated := int(s.transform.typ) > int(TransformType.rect_stays_rect)
	mut influence := ClipInfluence.none
	mut analytic := Rect{}
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
			return ClipInfluence.clipped_out, Rect{}
		}
		if influence == .analytic {
			unsupported = 'gg.canvas: more than one clip element needs depth-only clip draws'
			continue
		}
		influence = .analytic
		analytic = element.rect
	}
	if unsupported != '' {
		return error(unsupported)
	}
	return influence, analytic
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

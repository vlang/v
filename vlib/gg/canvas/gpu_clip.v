module canvas

import math

// This file ports Skia Graphite's ClipStack: save records, clip elements and
// their simplification, the clip state of each draw (geometric intersection,
// scissors, the analytic clip and depth-only element draws) and the
// depth-only draws of the elements.

// ClipOp combines a clip shape with the current clip, like SkClipOp.
pub enum ClipOp {
	intersect
	difference
}

enum ClipState {
	empty
	wide_open
	device_rect
	device_rrect
	complex
}

enum ClipShapeKind {
	empty
	rect
	rrect
}

// ClipShape is the part of Graphite's Shape that clips and the draws of
// gg.canvas use: an empty shape, a rectangle or a rounded rectangle, any of
// them possibly inverse-filled. An inverted empty shape is a flood fill.
struct ClipShape {
mut:
	kind     ClipShapeKind
	rect     Rect
	rrect    RRect
	inverted bool
}

fn rect_shape(r Rect) ClipShape {
	return ClipShape{
		kind: .rect
		rect: r
	}
}

fn rrect_shape(rr RRect) ClipShape {
	return ClipShape{
		kind:  .rrect
		rrect: rr
	}
}

fn (s ClipShape) bounds() Rect {
	return match s.kind {
		.empty { Rect.ltrb(0, 0, 0, 0) }
		.rect { s.rect }
		.rrect { s.rrect.rect }
	}
}

fn (s ClipShape) is_flood_fill() bool {
	return s.kind == .empty && s.inverted
}

fn (mut s ClipShape) set_rect(r Rect) {
	s.kind = .rect
	s.rect = r
	s.inverted = false
}

fn (mut s ClipShape) set_rrect(rr RRect) {
	s.kind = .rrect
	s.rrect = rr
	s.inverted = false
}

fn (mut s ClipShape) reset() {
	s.kind = .empty
	s.inverted = false
}

// conservative_contains mirrors Shape::conservativeContains(Rect).
fn (s ClipShape) conservative_contains(r Rect) bool {
	return match s.kind {
		.empty { false }
		.rect { s.rect.contains(r) }
		.rrect { s.rrect.contains_rect(r) }
	}
}

// conservative_contains_point mirrors Shape::conservativeContains(float2).
fn (s ClipShape) conservative_contains_point(x f32, y f32) bool {
	return match s.kind {
		.empty { false }
		.rect { s.rect.contains(Rect.ltrb(x, y, x, y)) }
		.rrect { s.rrect.contains_point(x, y) }
	}
}

fn transforms_equal(a Transform, b Transform) bool {
	if a.valid() != b.valid() {
		return false
	}
	return !a.valid() || a.matrix.m == b.matrix.m
}

// TransformedShape mirrors ClipStack::TransformedShape: a shape with its
// transform, device-space bounds and clip op, compared with others.
struct TransformedShape {
	l2d   Transform
	shape ClipShape
	outer Rect
	inner Rect
	op    ClipOp
	// Skips the detailed containment checks.
	contains_checks_only_bounds bool
}

fn (a TransformedShape) intersects(o TransformedShape) bool {
	if !a.outer.intersects(o.outer) {
		return false
	}
	rect_stays_rect := int(TransformType.rect_stays_rect)
	if int(a.l2d.typ) <= rect_stays_rect && int(o.l2d.typ) <= rect_stays_rect {
		// Both outer bounds are tight to their shapes.
		return true
	}
	if transforms_equal(a.l2d, o.l2d) {
		return a.shape.bounds().intersects(o.shape.bounds())
	}
	if a.l2d.typ != .perspective && o.l2d.typ != .perspective {
		return oriented_bbox_intersection(a.shape.bounds(), a.l2d, o.shape.bounds(), o.l2d)
	}
	return true
}

fn (a TransformedShape) contains(o TransformedShape) bool {
	if a.inner.contains(o.outer) {
		return true
	}
	if a.contains_checks_only_bounds || !a.outer.contains(o.outer) {
		return false
	}
	rect_stays_rect := int(TransformType.rect_stays_rect)
	if transforms_equal(a.l2d, o.l2d) {
		if a.shape.kind == .rrect && o.shape.kind == .rrect {
			return same_rrect(conservative_intersect(a.shape.rrect, o.shape.rrect), o.shape.rrect)
		}
		return a.shape.conservative_contains(o.shape.bounds())
	} else if int(a.l2d.typ) <= rect_stays_rect && int(o.l2d.typ) <= rect_stays_rect {
		local := a.l2d.inverse_map_rect(o.l2d.map_rect(o.shape.bounds()))
		return a.shape.conservative_contains(local)
	}
	// Rects and rrects are convex: `o` is inside when all four corners of its
	// bounds are.
	device := map_rect_corners(o.l2d.matrix, o.shape.bounds())
	for p in device {
		local := map_point_affine(a.l2d.inverse, p)
		if !a.shape.conservative_contains_point(local.x, local.y) {
			return false
		}
	}
	return true
}

// map_rect_corners maps the corners of `r` like Transform::mapPoints() for
// an affine transform: c0 * x + c1 * y + c3, one operation at a time.
fn map_rect_corners(m Matrix, r Rect) [4]Point {
	return [map_point_affine(m, Point{r.left(), r.top()}),
		map_point_affine(m, Point{r.right(), r.top()}),
		map_point_affine(m, Point{r.right(), r.bottom()}),
		map_point_affine(m, Point{r.left(), r.bottom()})]!
}

fn map_point_affine(m Matrix, p Point) Point {
	ax := m.m[0] * p.x
	ay := m.m[3] * p.x
	bx := m.m[1] * p.y
	by := m.m[4] * p.y
	sx := ax + bx
	sy := ay + by
	return Point{sx + m.m[2], sy + m.m[5]}
}

// oriented_bbox_intersection mirrors the separating axis test in
// ClipStack.cpp for two transformed rectangles.
fn oriented_bbox_intersection(a Rect, a_xform Transform, b Rect, b_xform Transform) bool {
	quad_a := map_rect_corners(a_xform.matrix, a)
	quad_b := map_rect_corners(b_xform.matrix, b)
	am := a_xform.matrix.m
	bm := b_xform.matrix.m
	axes_x := [-am[3], -am[4], -bm[3], -bm[4]]!
	axes_y := [am[0], am[1], bm[0], bm[1]]!
	for i in 0 .. 4 {
		mut min_a := f32(0)
		mut max_a := f32(0)
		mut min_b := f32(0)
		mut max_b := f32(0)
		for j in 0 .. 4 {
			pa := project(quad_a[j], axes_x[i], axes_y[i])
			pb := project(quad_b[j], axes_x[i], axes_y[i])
			if j == 0 {
				min_a, max_a, min_b, max_b = pa, pa, pb, pb
			} else {
				min_a = min_f32(min_a, pa)
				max_a = max_f32(max_a, pa)
				min_b = min_f32(min_b, pb)
				max_b = max_f32(max_b, pb)
			}
		}
		if !(min_b <= max_a && min_a <= max_b) {
			return false
		}
	}
	return true
}

fn project(p Point, ax f32, ay f32) f32 {
	x := p.x * ax
	y := p.y * ay
	return x + y
}

enum SimplifyResult {
	empty
	a_only
	b_only
	both
}

// simplify mirrors ClipStack::Simplify(). Applied to a clip and a draw, empty
// clips the draw out, a_only replaces the draw with the clip, b_only leaves
// the draw unclipped and both is a complex interaction.
fn simplify(a TransformedShape, b TransformedShape) SimplifyResult {
	if a.op == .intersect && b.op == .intersect {
		if !a.intersects(b) {
			return .empty
		} else if b.contains(a) {
			return .a_only
		} else if a.contains(b) {
			return .b_only
		}
		return .both
	} else if a.op == .intersect {
		if !a.intersects(b) {
			return .a_only
		} else if b.contains(a) {
			return .empty
		}
		return .both
	} else if b.op == .intersect {
		if !b.intersects(a) {
			return .b_only
		} else if a.contains(b) {
			return .empty
		}
		return .both
	}
	if a.contains(b) {
		return .a_only
	} else if b.contains(a) {
		return .b_only
	}
	return .both
}

// RawElement mirrors ClipStack::RawElement: a clip shape in device space when
// its transform keeps rectangles axis-aligned, with the draws it clipped
// since its depth-only draw was last recorded.
struct RawElement {
mut:
	shape ClipShape
	l2d   Transform
	op    ClipOp
	outer Rect
	inner Rect
	usage Rect = Rect.infinite_inverted()
	order u16  = no_intersection
	max_z u16
	// The first element index of the save record that invalidated it.
	invalidated_by int = -1
}

fn (e RawElement) has_pending_draw() bool {
	return e.order != no_intersection
}

fn (e RawElement) is_invalid() bool {
	return e.invalidated_by >= 0
}

fn (e RawElement) transformed_shape() TransformedShape {
	return TransformedShape{
		l2d:   e.l2d
		shape: e.shape
		outer: e.outer
		inner: e.inner
		op:    e.op
	}
}

fn (e RawElement) clip_type() ClipState {
	return match e.shape.kind {
		.empty { ClipState.empty }
		.rect {
			if e.op == .intersect && e.l2d.typ == .identity { .device_rect } else { .complex }
		}
		.rrect {
			if e.op == .intersect && e.l2d.typ == .identity { .device_rrect } else { .complex }
		}
	}
}

// new_raw_element mirrors the RawElement constructor: rects and rrects under
// rect-stays-rect transforms move to device space, clipped to the device and
// optionally snapped to pixels.
fn new_raw_element(device Rect, l2d Transform, shape ClipShape, op ClipOp, snap bool) RawElement {
	mut e := RawElement{
		shape: shape
		l2d:   l2d
		op:    op
	}
	if !l2d.valid() {
		e.shape.reset()
	}
	if e.shape.inverted {
		e.op = if e.op == .intersect { ClipOp.difference } else { ClipOp.intersect }
	}
	e.outer = e.l2d.map_rect(e.shape.bounds()).intersect(device)
	e.inner = Rect.infinite_inverted()
	if !e.outer.is_empty_negative_or_nan() && int(e.l2d.typ) <= int(TransformType.rect_stays_rect) {
		if e.shape.kind == .rect {
			if snap {
				e.outer = e.outer.round()
			}
			e.shape.set_rect(e.outer)
			e.l2d = Transform.new(Matrix.identity())
			e.inner = e.outer
		} else if e.shape.kind == .rrect {
			if xformed := e.shape.rrect.transform_legacy(e.l2d) {
				mut rr := xformed
				if snap {
					// The corners stay anti-aliased; the straight edges snap.
					rr = RRect.rect_radii(rr.rect.round(), rr.radii)
				}
				e.shape.set_rrect(rr)
				e.l2d = Transform.new(Matrix.identity())
				e.outer = e.shape.bounds().intersect(device)
				e.inner = rrect_inner_bounds(rr).intersect(e.outer)
			}
		}
	}
	if e.outer.is_empty_negative_or_nan() {
		e.shape.reset()
		e.inner = Rect.infinite_inverted()
	}
	// Intersect elements are drawn inverse-filled into the depth attachment.
	e.shape.inverted = e.op == .intersect
	return e
}

// SaveRecord mirrors ClipStack::SaveRecord: the aggregate bounds and state of
// the elements added since the matching save.
struct SaveRecord {
mut:
	inner          Rect
	outer          Rect
	starting_index int
	oldest_valid   int
	deferred_saves int
	stack_op       ClipOp    = .intersect
	state          ClipState = .wide_open
}

fn (r SaveRecord) can_be_updated() bool {
	return r.deferred_saves == 0
}

fn (r SaveRecord) transformed_shape() TransformedShape {
	return TransformedShape{
		l2d:                         Transform.new(Matrix.identity())
		shape:                       rect_shape(r.outer)
		outer:                       r.outer
		inner:                       r.inner
		op:                          r.stack_op
		contains_checks_only_bounds: true
	}
}

// ClipStack holds the save records and the elements of a surface.
struct ClipStack {
mut:
	elements []RawElement
	saves    []SaveRecord
}

fn new_clip_stack(width int, height int) ClipStack {
	device := Rect.ltrb(0, 0, f32(width), f32(height))
	return ClipStack{
		saves: [SaveRecord{
			inner: device
			outer: device
		}]
	}
}

fn (c &ClipStack) current() SaveRecord {
	return c.saves[c.saves.len - 1]
}

fn (s &GpuSurface) device_bounds() Rect {
	return Rect.ltrb(0, 0, f32(s.width), f32(s.height))
}

// save mirrors SkCanvas::save(): the clip can be restored later.
pub fn (mut s GpuSurface) save() {
	s.clip.saves[s.clip.saves.len - 1].deferred_saves++
}

// restore mirrors SkCanvas::restore(): it restores the clip of the matching
// save, recording the depth-only draws of the removed elements.
pub fn (mut s GpuSurface) restore() {
	if s.clip.saves.len == 1 && s.clip.saves[0].deferred_saves == 0 {
		return
	}
	last := s.clip.saves.len - 1
	s.clip.saves[last].deferred_saves--
	if s.clip.saves[last].deferred_saves >= 0 {
		return
	}
	start := s.clip.saves[last].starting_index
	for s.clip.elements.len > start {
		e := s.clip.elements.pop()
		s.draw_clip(e)
	}
	s.clip.saves.delete_last()
	// Elements invalidated only by the removed record become valid again.
	top := s.clip.current()
	for i := s.clip.elements.len - 1; i >= top.oldest_valid; i-- {
		if top.starting_index < s.clip.elements[i].invalidated_by {
			s.clip.elements[i].invalidated_by = -1
		}
	}
}

// clip_rect clips later draws to `r` in local coordinates, like
// SkCanvas::clipRect() on a Graphite device.
pub fn (mut s GpuSurface) clip_rect(r Rect, op ClipOp, anti_alias bool) ! {
	if !r.is_finite() {
		return
	}
	s.clip_shape(rect_shape(r.sorted()), op, !anti_alias)!
}

// clip_rrect clips later draws to a rounded rectangle, like
// SkCanvas::clipRRect() on a Graphite device.
pub fn (mut s GpuSurface) clip_rrect(rr RRect, op ClipOp, anti_alias bool) ! {
	if rr.is_rect() {
		s.clip_rect(rr.rect, op, anti_alias)!
		return
	}
	s.clip_shape(rrect_shape(rr), op, !anti_alias)!
}

// clip_shape mirrors ClipStack::clipShape().
fn (mut s GpuSurface) clip_shape(shape ClipShape, op ClipOp, snap bool) ! {
	if s.clip.current().state == .empty {
		return
	}
	if int(s.transform.typ) > int(TransformType.affine) {
		return error('gg.canvas: clips under perspective transforms are not supported')
	}
	if shape.kind == .rrect && int(s.transform.typ) <= int(TransformType.rect_stays_rect)
		&& s.transform.typ != .identity && s.transform.matrix.m[0] == 0 {
		return error('gg.canvas: rounded rectangle clips under 90 degree rotations are not supported')
	}
	element := new_raw_element(s.device_bounds(), s.transform, shape, op, snap)
	if element.shape.kind == .empty && element.op == .difference {
		return
	}
	if element.shape.kind != .empty {
		// Depth-only draws of curved elements must not need chopping.
		mut probe := []u8{}
		write_wedge_patches(mut probe, element_path(element.shape), element.l2d.matrix, 0)!
	}
	mut was_deferred := false
	last := s.clip.saves.len - 1
	if !s.clip.saves[last].can_be_updated() {
		s.clip.saves[last].deferred_saves--
		prior := s.clip.saves[last]
		s.clip.saves << SaveRecord{
			inner:          prior.inner
			outer:          prior.outer
			starting_index: s.clip.elements.len
			oldest_valid:   prior.oldest_valid
			stack_op:       prior.stack_op
			state:          prior.state
		}
		was_deferred = true
	}
	added, removed := s.clip.add_element(element)!
	for e in removed {
		s.draw_clip(e)
	}
	if !added && was_deferred {
		s.clip.saves.delete_last()
		s.clip.saves[s.clip.saves.len - 1].deferred_saves++
	}
}

// add_element mirrors SaveRecord::addElement() on the current record. It
// returns whether the element changed the clip and the removed elements in
// the order Graphite draws their pending depth-only draws.
fn (mut c ClipStack) add_element(to_add RawElement) !(bool, []RawElement) {
	last := c.saves.len - 1
	mut rec := c.saves[last]
	defer {
		c.saves[last] = rec
	}
	if rec.state == .empty {
		return false, []RawElement{}
	} else if to_add.shape.kind == .empty {
		rec.state = .empty
		return true, c.remove_elements(rec.starting_index)
	}
	match simplify(rec.transformed_shape(), to_add.transformed_shape()) {
		.empty {
			rec.state = .empty
			return true, c.remove_elements(rec.starting_index)
		}
		.a_only {
			return false, []RawElement{}
		}
		.b_only {
			return true, c.replace_with_element(mut rec, to_add)
		}
		.both {}
	}
	if rec.state == .wide_open {
		return true, c.replace_with_element(mut rec, to_add)
	}
	if rec.stack_op == .intersect {
		if to_add.op == .intersect {
			rec.outer = rec.outer.intersect(to_add.outer)
			rec.inner = rec.inner.intersect(to_add.inner)
		} else {
			rec.outer = subtract_rect(rec.outer, to_add.inner, true)
			rec.inner = subtract_rect(rec.inner, to_add.outer, false)
		}
	} else {
		if to_add.op == .intersect {
			old_outer := rec.outer
			rec.outer = subtract_rect(to_add.outer, rec.inner, true)
			rec.inner = subtract_rect(to_add.inner, old_outer, false)
		} else {
			rec.outer = rec.outer.join(to_add.outer)
			if to_add.inner.area() > rec.inner.area() {
				rec.inner = to_add.inner
			}
		}
	}
	return c.append_element(mut rec, to_add)!
}

fn (mut c ClipStack) remove_elements(start int) []RawElement {
	mut removed := []RawElement{}
	for c.elements.len > start {
		removed << c.elements.pop()
	}
	return removed
}

// replace_with_element mirrors SaveRecord::replaceWithElement().
fn (mut c ClipStack) replace_with_element(mut rec SaveRecord, to_add RawElement) []RawElement {
	rec.inner = to_add.inner
	rec.outer = to_add.outer
	rec.stack_op = to_add.op
	rec.state = to_add.clip_type()
	target := rec.starting_index + 1
	mut removed := []RawElement{}
	for c.elements.len > target {
		removed << c.elements.pop()
	}
	if c.elements.len < target {
		c.elements << to_add
	} else {
		removed << c.elements[c.elements.len - 1]
		c.elements[c.elements.len - 1] = to_add
	}
	rec.oldest_valid = rec.starting_index
	return removed
}

// append_element mirrors SaveRecord::appendElement().
fn (mut c ClipStack) append_element(mut rec SaveRecord, element RawElement) !(bool, []RawElement) {
	mut to_add := element
	mut youngest_valid := rec.starting_index - 1
	mut oldest_valid := c.elements.len
	mut oldest_active_invalid := -1
	for i := c.elements.len - 1; i >= rec.oldest_valid; i-- {
		mut existing := c.elements[i]
		update_for_element(mut existing, mut to_add, rec)!
		c.elements[i] = existing
		if to_add.is_invalid() {
			if existing.is_invalid() {
				// Both invalid: the clip is empty.
				rec.state = .empty
				return true, []RawElement{}
			}
			// The new element does not change the clip.
			return false, []RawElement{}
		} else if existing.is_invalid() {
			if i >= rec.starting_index {
				oldest_active_invalid = i
			}
		} else {
			oldest_valid = i
			if i > youngest_valid {
				youngest_valid = i
			}
		}
	}
	invalid_index := if oldest_active_invalid >= 0 { oldest_active_invalid } else { c.elements.len }
	rec.oldest_valid = if oldest_valid < invalid_index { oldest_valid } else { invalid_index }
	rec.state = if oldest_valid == c.elements.len { to_add.clip_type() } else { ClipState.complex }
	if rec.stack_op == .difference && to_add.op == .intersect {
		rec.stack_op = .intersect
	}
	mut target := youngest_valid + 1
	mut reuse := oldest_active_invalid
	if reuse < 0 || reuse >= target {
		target++
		reuse = -1
	}
	mut removed := []RawElement{}
	for c.elements.len > target {
		removed << c.elements.pop()
	}
	if reuse >= 0 {
		removed << c.elements[reuse]
		c.elements[reuse] = to_add
	} else if c.elements.len < target {
		c.elements << to_add
	} else {
		removed << c.elements[c.elements.len - 1]
		c.elements[c.elements.len - 1] = to_add
	}
	return true, removed
}

// update_for_element mirrors RawElement::updateForElement(): `existing` is
// the older element, `added` the new one.
fn update_for_element(mut existing RawElement, mut added RawElement, rec SaveRecord) ! {
	if existing.is_invalid() {
		return
	}
	match simplify(existing.transformed_shape(), added.transformed_shape()) {
		.empty {
			existing.invalidated_by = rec.starting_index
			added.invalidated_by = rec.starting_index
		}
		.a_only {
			added.invalidated_by = rec.starting_index
		}
		.b_only {
			existing.invalidated_by = rec.starting_index
		}
		.both {
			if combine_elements(mut added, existing)! {
				existing.invalidated_by = rec.starting_index
			}
		}
	}
}

// combine_elements mirrors RawElement::combine(): two intersect elements
// without pending draws merge when their intersection is a rect or rrect.
fn combine_elements(mut e RawElement, other RawElement) !bool {
	if e.has_pending_draw() || other.has_pending_draw() {
		return false
	}
	if other.op != .intersect || e.op != .intersect {
		return false
	}
	if intersect_shape(other.l2d, other.shape, e.l2d, mut e.shape)! {
		e.outer = e.outer.intersect(other.outer)
		e.inner = e.inner.intersect(other.inner)
		e.shape.inverted = true
		return true
	}
	return false
}

// intersect_shape mirrors intersect_shape() in ClipStack.cpp: it replaces
// `shape` (drawn with `l2d`) with its intersection with `other` (drawn with
// `other_l2d`) when that is a rect or rrect in the same coordinate space.
fn intersect_shape(other_l2d Transform, other ClipShape, l2d Transform, mut shape ClipShape) !bool {
	shape_ok := shape.kind == .rect || shape.kind == .rrect || shape.is_flood_fill()
	other_ok := other.kind == .rect || other.kind == .rrect
	if !shape_ok || !other_ok {
		return false
	}
	rect_stays_rect := int(TransformType.rect_stays_rect)
	mut local_to_other := Transform{}
	mut relative := true
	if transforms_equal(other_l2d, l2d) {
		relative = false
	} else if other_l2d.typ == .identity && int(l2d.typ) <= rect_stays_rect {
		local_to_other = l2d
	} else if int(other_l2d.typ) <= rect_stays_rect && l2d.typ == .identity {
		local_to_other = Transform{
			...other_l2d
			matrix:  other_l2d.inverse
			inverse: other_l2d.matrix
		}
	} else {
		return error('gg.canvas: clips and draws in unrelated coordinate spaces are not supported')
	}
	mut local_other_rect := other.bounds()
	if relative {
		mapped := local_to_other.inverse_map_rect(local_other_rect)
		other_tol := shape_pixel_tolerance * other_l2d.local_aa_radius()
		if local_other_rect.is_empty_negative_or_nan()
			|| !local_to_other.map_rect(mapped).nearly_equals(local_other_rect, other_tol) {
			return false
		}
		local_other_rect = mapped
	}
	if !shape.is_flood_fill() {
		local_other_rect = local_other_rect.intersect(shape.bounds())
	}
	// A subpixel intersection renders differently from a clipped draw.
	local_aa := l2d.local_aa_radius()
	if !is_finite_f32(local_aa) || local_other_rect.width() <= local_aa
		|| local_other_rect.height() <= local_aa {
		return false
	}
	mut local_other_rrect := RRect{}
	if other.kind == .rect {
		if shape.kind == .rect || shape.is_flood_fill() {
			shape.set_rect(local_other_rect)
			return true
		}
		local_other_rrect = RRect.from_rect(local_other_rect)
	} else {
		if relative {
			inverse := Transform.new(local_to_other.inverse)
			local_other_rrect = other.rrect.transform_legacy(inverse) or { return false }
		} else {
			local_other_rrect = other.rrect
		}
		if shape.is_flood_fill() {
			shape.set_rrect(local_other_rrect)
			return true
		}
	}
	local := conservative_intersect(local_other_rrect, if shape.kind == .rect {
		RRect.from_rect(shape.rect)
	} else {
		shape.rrect
	})
	if local.is_rect() {
		shape.set_rect(local.rect)
		return true
	} else if !local.is_empty() {
		shape.set_rrect(local)
		return true
	}
	return false
}

// conservative_clip_bounds mirrors Device::devClipBounds(): the clip's
// conservative bounds rounded out to pixels, or none for an empty clip.
fn (s &GpuSurface) conservative_clip_bounds() ?Rect {
	current := s.clip.current()
	device := s.device_bounds()
	return match current.state {
		.empty { none }
		.wide_open { device }
		else {
			if current.stack_op == .difference {
				subtract_rect(device, current.inner, true).round_out()
			} else {
				current.outer.round_out()
			}
		}
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

// AnalyticClip mirrors the legacy AnalyticClip: one rect or circular rrect
// element that the paint's shader clips against.
struct AnalyticClip {
	bounds     Rect = Rect.ltrb(0, 0, 0, 0)
	radius     f32
	edge_flags u32
	inverted   bool = true
}

const analytic_left_edge = u32(1)
const analytic_top_edge = u32(2)
const analytic_right_edge = u32(4)
const analytic_bottom_edge = u32(8)
const analytic_all_edges = u32(15)

fn (c AnalyticClip) is_empty() bool {
	return c.bounds.is_empty_negative_or_nan()
}

// can_apply_analytic_clip mirrors can_apply_analytic_clip() with
// SK_GRAPHITE_USE_LEGACY_RRECT_CLIP_SHADER: device-space rects and rrects
// whose rounded corners share one circular radius.
fn can_apply_analytic_clip(shape ClipShape, l2d Transform) AnalyticClip {
	if l2d.typ != .identity {
		return AnalyticClip{}
	}
	radius_min := f32(0.5)
	if shape.kind == .rect {
		return AnalyticClip{
			bounds:   shape.rect
			radius:   radius_min
			inverted: shape.inverted
		}
	}
	if shape.kind != .rrect {
		return AnalyticClip{}
	}
	rr := shape.rrect
	if rr.typ == .oval || rr.typ == .simple {
		radii := rr.radii[0]
		if radii.x < radius_min || radii.y < radius_min {
			return AnalyticClip{
				bounds:   rr.rect
				radius:   radius_min
				inverted: shape.inverted
			}
		}
		if is_relatively_circular(radii.x, radii.y, shape_pixel_tolerance) {
			return AnalyticClip{
				bounds:     rr.rect
				radius:     radii.x
				edge_flags: analytic_all_edges
				inverted:   shape.inverted
			}
		}
		return AnalyticClip{}
	}
	corner_flags := [analytic_top_edge | analytic_left_edge, analytic_top_edge | analytic_right_edge,
		analytic_bottom_edge | analytic_right_edge, analytic_bottom_edge | analytic_left_edge]!
	mut circular_radius := f32(0)
	mut edge_flags := u32(0)
	mut square_count := 0
	for corner in 0 .. 4 {
		radii := rr.radii[corner]
		if !is_relatively_circular(radii.x, radii.y, shape_pixel_tolerance) {
			return AnalyticClip{}
		}
		if radii.x < radius_min || radii.y < radius_min {
			square_count++
			continue
		}
		if edge_flags == 0 {
			circular_radius = radii.x
		} else if !is_relatively_circular(radii.x, circular_radius, shape_pixel_tolerance) {
			return AnalyticClip{}
		}
		edge_flags |= corner_flags[corner]
	}
	if edge_flags == 0 {
		return AnalyticClip{
			bounds:   rr.rect
			radius:   radius_min
			inverted: shape.inverted
		}
	} else if edge_flags == analytic_all_edges && square_count != 0 {
		return AnalyticClip{}
	}
	return AnalyticClip{
		bounds:     rr.rect
		radius:     circular_radius
		edge_flags: edge_flags
		inverted:   shape.inverted
	}
}

// AnalyticClipUniforms is Graphite's NonMSAAClip uniform data: the clip
// bounds outset by half a pixel, (±(radius + 0.5), 1 / (radius + 0.5)) with
// the sign selecting intersect or difference, and the edges adjacent to round
// corners.
struct AnalyticClipUniforms {
	rect             [4]f32
	radius_plus_half [2]f32
	edge_select      [4]f32
}

// analytic_clip_uniforms mirrors ShadingParams::handleClipping().
fn analytic_clip_uniforms(c AnalyticClip) AnalyticClipUniforms {
	radius := c.radius + f32(0.5)
	bounds := c.bounds.outset(0.5)
	flag := fn [c] (f u32) f32 {
		return if c.edge_flags & f != 0 { f32(1) } else { f32(0) }
	}
	return AnalyticClipUniforms{
		rect:             [bounds.left(), bounds.top(), bounds.right(), bounds.bottom()]!
		radius_plus_half: [if c.inverted { radius } else { -radius }, f32(1) / radius]!
		edge_select:      [flag(analytic_left_edge), flag(analytic_top_edge),
			flag(analytic_right_edge), flag(analytic_bottom_edge)]!
	}
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
const identity_local_to_device = local_to_device_uniforms(Matrix.identity())

// local_to_device_uniforms is the column-major SkM44 of a wedge step's
// transform.
fn local_to_device_uniforms(m Matrix) PaintUniforms {
	v := m.m
	return PaintUniforms{
		local_to_device: true
		matrix:          [v[0], v[3], 0, v[6], v[1], v[4], 0, v[7], 0, 0, 1, 0, v[2], v[5], 0,
			v[8]]!
	}
}

// The analytic-clip paint uniforms: float4 color, float4 rect, float2
// radiusPlusHalf (padded to 16 bytes) and float4 edgeSelect.
const clip_uniform_size = 64

// DrawShape mirrors ClipStack::DrawShape: the styled shape of a draw, which
// the clip stack may intersect with clip elements.
struct DrawShape {
mut:
	l2d                Transform
	shape              ClipShape
	transformed_bounds Rect
	outer              Rect
	inner              Rect
	scissor            Rect = Rect{
		vals: [f32(math.inf(-1)), f32(math.inf(-1)), f32(math.inf(-1)), f32(math.inf(-1))]!
	}
	matches_geometry   bool = true
	compatible         bool
	modified           bool
}

fn (d DrawShape) can_be_modified() bool {
	return d.compatible && d.matches_geometry
}

fn (d DrawShape) transformed_shape() TransformedShape {
	return TransformedShape{
		l2d:                         d.l2d
		shape:                       d.shape
		outer:                       d.outer
		inner:                       d.inner
		op:                          if d.shape.inverted {
			ClipOp.difference
		} else {
			ClipOp.intersect
		}
		contains_checks_only_bounds: !d.can_be_modified()
	}
}

// apply_style mirrors DrawShape::applyStyle(). It returns false for draws
// that cannot produce coverage.
fn (mut d DrawShape) apply_style(paint Paint, device Rect) bool {
	d.transformed_bounds = d.shape.bounds()
	w := d.transformed_bounds.width()
	h := d.transformed_bounds.height()
	if !is_finite_f32(w) || !is_finite_f32(h) {
		return false
	}
	stroked := paint.style == .stroke
	hairline := stroked && paint.stroke_width == 0
	if !d.shape.inverted && (w == 0 || h == 0) {
		if !stroked || (paint.cap == .butt && w == 0 && h == 0) {
			return false
		}
	}
	local_aa := d.l2d.local_aa_radius()
	if !is_finite_f32(local_aa) {
		d.transformed_bounds = device
		d.shape.set_rect(device)
		d.l2d = Transform.new(Matrix.identity())
		d.matches_geometry = false
	} else {
		mut local_outset := f32(0)
		if stroked && !hairline {
			// Rects and rrects have neither miters nor caps.
			local_outset = paint.stroke_width / f32(2)
		}
		if hairline || (stroked && paint.stroke_width < local_aa)
			|| (!stroked && !d.shape.inverted && (w < local_aa || h < local_aa)) {
			local_outset += local_aa
		}
		if local_outset > 0 {
			d.transformed_bounds = d.transformed_bounds.outset(local_outset)
			inverted := d.shape.inverted
			if d.shape.kind == .rrect {
				d.shape.rrect = d.shape.rrect.outset(local_outset, local_outset)
			} else {
				d.shape.set_rect(d.transformed_bounds)
			}
			d.shape.inverted = inverted
			d.matches_geometry = false
		}
		d.transformed_bounds = d.l2d.map_rect(d.transformed_bounds)
	}
	d.outer = d.transformed_bounds
	d.inner = Rect.infinite_inverted()
	if d.can_be_modified() && int(d.l2d.typ) <= int(TransformType.rect_stays_rect) {
		if d.shape.kind == .rect {
			d.inner = d.outer
		} else if d.shape.kind == .rrect {
			inner := rrect_inner_bounds(d.shape.rrect)
			if inner.left() < inner.right() && inner.top() < inner.bottom() {
				d.inner = d.l2d.map_rect(inner)
			}
		}
	}
	return true
}

fn (mut d DrawShape) apply_scissor(scissor Rect) {
	d.scissor = d.scissor.intersect(scissor)
	d.outer = d.outer.intersect(scissor)
	d.inner = d.inner.intersect(scissor)
}

fn (mut d DrawShape) reset_to_flood_fill() {
	if d.can_be_modified() && !d.shape.is_flood_fill() {
		d.shape.reset()
		d.shape.inverted = true
		d.outer = Rect.infinite_inverted()
		d.inner = Rect.infinite_inverted()
		d.modified = true
	}
}

fn (mut d DrawShape) intersect_clip_element(e RawElement) !bool {
	if d.can_be_modified() && intersect_shape(e.l2d, e.shape, d.l2d, mut d.shape)! {
		if d.outer.is_empty_negative_or_nan() {
			d.outer = e.outer
			d.inner = e.inner
		} else {
			d.outer = d.outer.intersect(e.outer)
			d.inner = d.inner.intersect(e.inner)
		}
		d.modified = true
		return true
	}
	return false
}

// ClipResult is the Clip of a draw from visit_clip_stack_for_draw(), with
// the draw's geometry after the clip stack possibly intersected it.
struct ClipResult {
mut:
	clipped_out bool
	// The intersected geometry, when the clip stack modified the draw's.
	modified     bool
	shape        ClipShape
	draw_bounds  Rect
	shape_bounds Rect
	scissor      Rect
	analytic     AnalyticClip
	// The indices of the elements that need depth-only draws.
	elements []int
}

const clipped_out_draw = ClipResult{
	clipped_out: true
}

// visit_clip_stack_for_draw mirrors ClipStack::visitClipStackForDraw() for a
// rect or rrect `shape` drawn with the current transform.
fn (s &GpuSurface) visit_clip_stack_for_draw(shape ClipShape, paint Paint) !ClipResult {
	cs := s.clip.current()
	if cs.state == .empty {
		return clipped_out_draw
	}
	device := s.device_bounds()
	mut draw := DrawShape{
		l2d:        s.transform
		shape:      shape
		compatible: shape.is_flood_fill() || (!shape.inverted && (shape.kind == .rect
			|| shape.kind == .rrect))
	}
	if !draw.apply_style(paint, device) {
		return clipped_out_draw
	}
	draw.apply_scissor(if cs.stack_op == .intersect {
		snap_scissor(cs.outer, device)
	} else {
		device
	})
	match simplify(cs.transformed_shape(), draw.transformed_shape()) {
		.empty {
			return clipped_out_draw
		}
		.b_only {
			return draw.to_clip(AnalyticClip{}, []int{})
		}
		.a_only {
			draw.reset_to_flood_fill()
		}
		.both {}
	}
	mut analytic := AnalyticClip{}
	mut elements := []int{}
	for i := s.clip.elements.len - 1; i >= cs.oldest_valid; i-- {
		e := s.clip.elements[i]
		if e.is_invalid() {
			continue
		}
		influence := simplify(e.transformed_shape(), draw.transformed_shape())
		match influence {
			.empty {
				return clipped_out_draw
			}
			.b_only {
				continue
			}
			.a_only, .both {
				if influence == .a_only {
					draw.reset_to_flood_fill()
				}
				if e.op == .intersect && draw.intersect_clip_element(e)! {
					continue
				}
				if e.clip_type() == .device_rect {
					scissor := e.shape.rect.round()
					if e.shape.rect.nearly_equals(scissor, shape_pixel_tolerance) {
						draw.apply_scissor(scissor)
						continue
					}
				}
				if analytic.is_empty() {
					analytic = can_apply_analytic_clip(e.shape, e.l2d)
					if !analytic.is_empty() {
						continue
					}
				}
				if e.op == .intersect {
					return error('gg.canvas: intersect clip elements drawn into the depth attachment need the stencil renderer')
				}
				elements << i
			}
		}
	}
	return draw.to_clip(analytic, elements)
}

// to_clip mirrors DrawShape::toClip().
fn (d DrawShape) to_clip(analytic AnalyticClip, elements []int) ClipResult {
	mut transformed := d.transformed_bounds
	mut outer := d.outer
	if d.modified {
		transformed = d.l2d.map_rect(d.shape.bounds())
		outer = transformed.intersect(d.scissor)
	}
	draw_bounds := if d.shape.inverted { d.scissor } else { outer }
	return ClipResult{
		clipped_out:  draw_bounds.is_empty_negative_or_nan()
		modified:     d.modified
		shape:        d.shape
		draw_bounds:  draw_bounds
		shape_bounds: transformed
		scissor:      d.scissor
		analytic:     analytic
		elements:     elements
	}
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
	device := s.device_bounds()
	snapped := snap_scissor(draw_bounds, device)
	mut max_order := no_intersection
	for i in elements {
		mut e := &s.clip.elements[i]
		if !e.has_pending_draw() {
			// Ordered after the most recent draw under the element's bounds.
			e.usage = snapped
			e.max_z = z
			e.order = s.bounds.most_recent_draw(snap_scissor(e.outer, device)) + 1
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
fn (mut s GpuSurface) draw_clip(e RawElement) {
	if !e.has_pending_draw() {
		return
	}
	device := s.device_bounds()
	snapped_outer := snap_scissor(e.outer, device)
	mut scissor := e.usage.intersect(snapped_outer)
	if f32(0.5) * snapped_outer.area() < scissor.area() {
		scissor = snapped_outer
	}
	draw_bounds := if e.op == .intersect { scissor } else { e.outer.intersect(scissor) }
	if draw_bounds.is_empty_negative_or_nan() {
		return
	}
	order := DrawOrder{
		depth:       e.max_z + 1
		paint_order: e.order
	}
	mut instance := []u8{}
	count, n4 := write_wedge_patches(mut instance, element_path(e.shape), e.l2d.matrix,
		order.depth) or { panic(err) }
	s.record_step(.convex_wedges, false, instance, local_to_device_uniforms(e.l2d.matrix),
		order, 0, 0, DrawClip{
			scissor:      scissor
			draw_bounds:  draw_bounds
			shape_bounds: e.outer
		}, u32(count))
	s.draws[s.draws.len - 1].template_count = wedge_vertex_count(n4)
	if order.depth > s.depth_index {
		s.depth_index = order.depth
	}
}

// record_deferred_clip_draws mirrors ClipStack::recordDeferredClipDraws()
// before a flush; elements then collect their usage again.
fn (mut s GpuSurface) record_deferred_clip_draws() {
	for i in 0 .. s.clip.elements.len {
		s.draw_clip(s.clip.elements[i])
		s.clip.elements[i].usage = Rect.infinite_inverted()
		s.clip.elements[i].order = no_intersection
		s.clip.elements[i].max_z = 0
	}
}

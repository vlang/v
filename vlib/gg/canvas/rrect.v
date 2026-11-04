module canvas

// RRectType classifies a rounded rectangle exactly like SkRRect::Type.
pub enum RRectType {
	empty
	rect
	oval
	simple
	nine_patch
	complex
}

// Corner indexes radii in clockwise order, starting at the top-left corner.
pub enum Corner {
	upper_left
	upper_right
	lower_right
	lower_left
}

// RRect is a rectangle with elliptical corners. Construction normalizes the
// radii exactly like SkRRect (clamping, proportional scaling and type
// classification), because renderers depend on the resulting values.
pub struct RRect {
pub:
	rect  Rect
	radii [4]Point
	typ   RRectType
}

// RRect.from_rect returns a rectangle without rounded corners.
pub fn RRect.from_rect(rect Rect) RRect {
	if !rect.is_finite() {
		return RRect{
			rect: Rect.ltrb(0, 0, 0, 0)
		}
	}
	sorted := rect.sorted()
	if sorted.is_empty_negative_or_nan() {
		return RRect{
			rect: sorted
			typ:  .empty
		}
	}
	return RRect{
		rect: sorted
		typ:  .rect
	}
}

// RRect.oval returns the ellipse inscribed in `rect`, like SkRRect::setOval().
pub fn RRect.oval(rect Rect) RRect {
	if !rect.is_finite() {
		return RRect{
			rect: Rect.ltrb(0, 0, 0, 0)
		}
	}
	sorted := rect.sorted()
	if sorted.is_empty_negative_or_nan() {
		return RRect{
			rect: sorted
			typ:  .empty
		}
	}
	half_w := f32(0.5 * (f64(-sorted.left()) + f64(sorted.right())))
	half_h := f32(0.5 * (f64(-sorted.top()) + f64(sorted.bottom())))
	if half_w == 0 || half_h == 0 {
		return RRect{
			rect: sorted
			typ:  .rect
		}
	}
	radius := Point{half_w, half_h}
	return RRect{
		rect:  sorted
		radii: [radius, radius, radius, radius]!
		typ:   .oval
	}
}

// RRect.rect_radii mirrors SkRRect::setRectRadii(). `radii` lists the x/y
// radius pairs of the four corners in clockwise order from the top left.
pub fn RRect.rect_radii(rect Rect, radii [4]Point) RRect {
	if !rect.is_finite() {
		return RRect{
			rect: Rect.ltrb(0, 0, 0, 0)
		}
	}
	sorted := rect.sorted()
	if sorted.is_empty_negative_or_nan() {
		return RRect{
			rect: sorted
			typ:  .empty
		}
	}
	for corner in radii {
		if !is_finite_f32(corner.x) || !is_finite_f32(corner.y) {
			return RRect.from_rect(sorted)
		}
	}
	mut r := [4]Point{}
	for i in 0 .. 4 {
		r[i] = radii[i]
	}
	if clamp_radii_to_zero(mut r) {
		return RRect.from_rect(sorted)
	}
	_, typ := scale_radii(sorted, mut r)
	if !rrect_is_valid(sorted, r) {
		return RRect.from_rect(sorted)
	}
	return RRect{
		rect:  sorted
		radii: r
		typ:   typ
	}
}

// scale_radii mirrors SkRRect::scaleRadii(): it scales the radii down
// proportionally so adjacent corners fit, then classifies the result. It
// reports whether the radii had to be scaled.
fn scale_radii(rect Rect, mut r [4]Point) (bool, RRectType) {
	// The sides may be larger than a float, so the scale is a double.
	width := f64(rect.right()) - f64(rect.left())
	height := f64(rect.bottom()) - f64(rect.top())
	mut scale := 1.0
	scale = compute_min_scale(r[0].x, r[1].x, width, scale)
	scale = compute_min_scale(r[1].y, r[2].y, height, scale)
	scale = compute_min_scale(r[2].x, r[3].x, width, scale)
	scale = compute_min_scale(r[3].y, r[0].y, height, scale)
	r[0].x, r[1].x = flush_to_zero(r[0].x, r[1].x)
	r[1].y, r[2].y = flush_to_zero(r[1].y, r[2].y)
	r[2].x, r[3].x = flush_to_zero(r[2].x, r[3].x)
	r[3].y, r[0].y = flush_to_zero(r[3].y, r[0].y)
	if scale < 1.0 {
		r[0].x, r[1].x = adjust_radii(width, scale, r[0].x, r[1].x)
		r[1].y, r[2].y = adjust_radii(height, scale, r[1].y, r[2].y)
		r[2].x, r[3].x = adjust_radii(width, scale, r[2].x, r[3].x)
		r[3].y, r[0].y = adjust_radii(height, scale, r[3].y, r[0].y)
	}
	clamp_radii_to_zero(mut r)
	return scale < 1.0, compute_rrect_type(rect, mut r)
}

pub fn (rr RRect) is_rect() bool {
	return rr.typ == .rect
}

pub fn (rr RRect) is_empty() bool {
	return rr.typ == .empty
}

pub fn (rr RRect) bounds() Rect {
	return rr.rect
}

pub fn (rr RRect) radius(corner Corner) Point {
	return rr.radii[int(corner)]
}

fn clamp_radii_to_zero(mut radii [4]Point) bool {
	mut all_square := true
	for i in 0 .. 4 {
		if radii[i].x <= 0 || radii[i].y <= 0 {
			radii[i].x = 0
			radii[i].y = 0
		} else {
			all_square = false
		}
	}
	return all_square
}

fn compute_min_scale(rad1 f32, rad2 f32, limit f64, current f64) f64 {
	sum := f64(rad1) + f64(rad2)
	if sum > limit {
		ratio := limit / sum
		return if ratio < current { ratio } else { current }
	}
	return current
}

fn flush_to_zero(a f32, b f32) (f32, f32) {
	if a + b == a {
		return a, 0
	} else if a + b == b {
		return 0, b
	}
	return a, b
}

// adjust_radii mirrors SkScaleToSides::AdjustRadii.
fn adjust_radii(limit f64, scale f64, a_in f32, b_in f32) (f32, f32) {
	mut a := f32(f64(a_in) * scale)
	mut b := f32(f64(b_in) * scale)
	if f64(a + b) > limit {
		a_is_min := !(a > b)
		new_min := if a_is_min { a } else { b }
		mut new_max := f32(limit - f64(new_min))
		for f64(new_max + new_min) > limit {
			new_max = C.nextafterf(new_max, 0)
		}
		if a_is_min {
			b = new_max
		} else {
			a = new_max
		}
	}
	return a, b
}

fn compute_rrect_type(rect Rect, mut radii [4]Point) RRectType {
	if rect.is_empty_negative_or_nan() {
		return .empty
	}
	mut all_equal := true
	mut all_square := radii[0].x == 0 || radii[0].y == 0
	for i in 1 .. 4 {
		if radii[i].x != 0 && radii[i].y != 0 {
			all_square = false
		}
		if radii[i].x != radii[i - 1].x || radii[i].y != radii[i - 1].y {
			all_equal = false
		}
	}
	if all_square {
		return .rect
	}
	if all_equal {
		if radii[0].x >= rect.width() / f32(2) && radii[0].y >= rect.height() / f32(2) {
			// SkRectPriv::HalfWidth/HalfHeight compute the midpoint, then the
			// distance to it, to avoid overflowing for huge rectangles.
			half_w := f32(0.5 * (f64(-rect.left()) + f64(rect.right())))
			half_h := f32(0.5 * (f64(-rect.top()) + f64(rect.bottom())))
			for i in 0 .. 4 {
				radii[i] = Point{half_w, half_h}
			}
			return .oval
		}
		return .simple
	}
	if radii[0].x == radii[3].x && radii[0].y == radii[1].y && radii[1].x == radii[2].x
		&& radii[3].y == radii[2].y {
		return .nine_patch
	}
	return .complex
}

// rrect_is_valid mirrors SkRRectPriv::AreRectAndRadiiValid(); the type
// computed by compute_rrect_type is consistent by construction.
fn rrect_is_valid(rect Rect, radii [4]Point) bool {
	if !rect.is_finite() || rect.left() > rect.right() || rect.top() > rect.bottom() {
		return false
	}
	return radii_predicates_valid(radii[0].x, radii[1].x, rect.left(), rect.right())
		&& radii_predicates_valid(radii[3].x, radii[2].x, rect.left(), rect.right())
		&& radii_predicates_valid(radii[0].y, radii[3].y, rect.top(), rect.bottom())
		&& radii_predicates_valid(radii[1].y, radii[2].y, rect.top(), rect.bottom())
}

fn radii_predicates_valid(radius_from_min f32, radius_from_max f32, min_coord f32, max_coord f32) bool {
	if min_coord > max_coord || radius_from_min < 0 || radius_from_max < 0 {
		return false
	}
	limit := max_coord - min_coord
	sum := radius_from_min + radius_from_max
	if sum <= limit && min_coord + radius_from_min <= max_coord - radius_from_max {
		return true
	}
	return abs_f32(sum - limit) <= scalar_nearly_zero || almost_equal_ulps(sum, limit, 4)
}

// scalar_nearly_zero is SK_ScalarNearlyZero, 1 / 2^12.
const scalar_nearly_zero = f32(1.0) / f32(4096.0)

// almost_equal_ulps mirrors SkFloatingPoint<float, max_ulps>::AlmostEquals().
fn almost_equal_ulps(a f32, b f32, max_ulps u32) bool {
	if a != a || b != b {
		return false
	}
	return u32_distance(biased_bits(a), biased_bits(b)) <= max_ulps
}

// biased_bits maps IEEE sign-magnitude bits to a monotonic unsigned order.
fn biased_bits(value f32) u32 {
	bits := unsafe { *(&u32(&value)) }
	sign_bit := u32(1) << 31
	return if bits & sign_bit != 0 { ~bits + 1 } else { sign_bit | bits }
}

fn u32_distance(a u32, b u32) u32 {
	return if a >= b { a - b } else { b - a }
}

// rrect_inner_bounds mirrors SkRRectPriv::InnerBounds(): an approximately
// largest rectangle inscribed in the rounded rectangle.
fn rrect_inner_bounds(rr RRect) Rect {
	if rr.is_empty() || rr.is_rect() {
		return rr.rect
	}
	mut left := rr.rect.left()
	mut top := rr.rect.top()
	mut right := rr.rect.right()
	mut bottom := rr.rect.bottom()
	tl := rr.radii[int(Corner.upper_left)]
	tr := rr.radii[int(Corner.upper_right)]
	br := rr.radii[int(Corner.lower_right)]
	bl := rr.radii[int(Corner.lower_left)]
	left_shift := max_f32(tl.x, bl.x)
	top_shift := max_f32(tl.y, tr.y)
	right_shift := max_f32(tr.x, br.x)
	bottom_shift := max_f32(bl.y, br.y)
	dw := left_shift + right_shift
	dh := top_shift + bottom_shift
	width := right - left
	height := bottom - top
	horiz_area := (width - dw) * height
	vert_area := (height - dh) * width
	// (1 - SK_ScalarRoot2Over2) + 1e-5f
	k_scale := (f32(1) - f32(0.707106781)) + f32(1e-5)
	// Products are separate statements: Skia is compiled with -ffp-contract=off.
	scaled_dw := k_scale * dw
	scaled_dh := k_scale * dh
	inner_area := (width - scaled_dw) * (height - scaled_dh)
	if horiz_area > vert_area && horiz_area > inner_area {
		left += left_shift
		right -= right_shift
	} else if vert_area > inner_area {
		top += top_shift
		bottom -= bottom_shift
	} else if inner_area > 0 {
		scaled_left := k_scale * left_shift
		scaled_right := k_scale * right_shift
		scaled_top := k_scale * top_shift
		scaled_bottom := k_scale * bottom_shift
		left += scaled_left
		right -= scaled_right
		top += scaled_top
		bottom -= scaled_bottom
	} else {
		return Rect.ltrb(0, 0, 0, 0)
	}
	return Rect.ltrb(left, top, right, bottom)
}

// inset mirrors SkRRect::inset(): the rectangle is inset by (dx, dy) and
// every non-zero radius shrinks by the same amount. Negative amounts outset.
pub fn (rr RRect) inset(dx f32, dy f32) RRect {
	mut l := rr.rect.left() + dx
	mut t := rr.rect.top() + dy
	mut r := rr.rect.right() - dx
	mut b := rr.rect.bottom() - dy
	mut degenerate := false
	if r <= l {
		degenerate = true
		l = float_midpoint(l, r)
		r = l
	}
	if b <= t {
		degenerate = true
		t = float_midpoint(t, b)
		b = t
	}
	if degenerate {
		return RRect{
			rect: Rect.ltrb(l, t, r, b)
			typ:  .empty
		}
	}
	rect := Rect.ltrb(l, t, r, b)
	if !rect.is_finite() {
		return RRect{
			rect: Rect.ltrb(0, 0, 0, 0)
		}
	}
	mut radii := rr.radii
	for i in 0 .. 4 {
		if radii[i].x != 0 {
			radii[i].x -= dx
		}
		if radii[i].y != 0 {
			radii[i].y -= dy
		}
	}
	return RRect.rect_radii(rect, radii)
}

// outset mirrors SkRRect::outset().
pub fn (rr RRect) outset(dx f32, dy f32) RRect {
	return rr.inset(-dx, -dy)
}

// float_midpoint mirrors sk_float_midpoint(), which averages in double
// precision.
fn float_midpoint(a f32, b f32) f32 {
	return f32(0.5 * (f64(a) + f64(b)))
}

// is_simple reports SkRRect::isSimple(): all corners share one radius pair.
pub fn (rr RRect) is_simple() bool {
	return rr.typ == .simple
}

// same_rrect compares rectangles and radii with float equality, like
// SkRRect::operator==.
fn same_rrect(a RRect, b RRect) bool {
	if !rect_equal(a.rect, b.rect) {
		return false
	}
	for i in 0 .. 4 {
		if a.radii[i].x != b.radii[i].x || a.radii[i].y != b.radii[i].y {
			return false
		}
	}
	return true
}

// check_corner_containment mirrors SkRRect::checkCornerContainment() for a
// point inside the bounds.
fn (rr RRect) check_corner_containment(x f32, y f32) bool {
	mut cx := f32(0)
	mut cy := f32(0)
	mut index := 0
	l := rr.rect.left()
	t := rr.rect.top()
	r := rr.rect.right()
	b := rr.rect.bottom()
	ul := rr.radii[int(Corner.upper_left)]
	ur := rr.radii[int(Corner.upper_right)]
	lr := rr.radii[int(Corner.lower_right)]
	ll := rr.radii[int(Corner.lower_left)]
	if rr.typ == .oval {
		cx = x - float_midpoint(l, r)
		cy = y - float_midpoint(t, b)
	} else if x < l + ul.x && y < t + ul.y {
		index = int(Corner.upper_left)
		cx = x - (l + ul.x)
		cy = y - (t + ul.y)
	} else if x < l + ll.x && y > b - ll.y {
		index = int(Corner.lower_left)
		cx = x - (l + ll.x)
		cy = y - (b - ll.y)
	} else if x > r - ur.x && y < t + ur.y {
		index = int(Corner.upper_right)
		cx = x - (r - ur.x)
		cy = y - (t + ur.y)
	} else if x > r - lr.x && y > b - lr.y {
		index = int(Corner.lower_right)
		cx = x - (r - lr.x)
		cy = y - (b - lr.y)
	} else {
		return true
	}
	radius := rr.radii[index]
	// Each product is a separate statement: Skia is compiled with
	// -ffp-contract=off.
	cx2 := cx * cx
	cy2 := cy * cy
	ry2 := radius.y * radius.y
	rx2 := radius.x * radius.x
	a := cx2 * ry2
	c := cy2 * rx2
	dist := a + c
	rxy := radius.x * radius.y
	return dist <= rxy * rxy
}

// contains_point mirrors SkRRect::contains(SkPoint).
fn (rr RRect) contains_point(x f32, y f32) bool {
	if !(x >= rr.rect.left() && x < rr.rect.right() && y >= rr.rect.top() && y < rr.rect.bottom()) {
		return false
	}
	return rr.is_rect() || rr.check_corner_containment(x, y)
}

// contains_rect mirrors SkRRect::contains(SkRect).
fn (rr RRect) contains_rect(o Rect) bool {
	// SkRect::contains(SkRect) rejects empty rectangles.
	if !(o.left() < o.right() && o.top() < o.bottom()) || rr.rect.is_empty_negative_or_nan()
		|| !(rr.rect.left() <= o.left() && rr.rect.top() <= o.top()
			&& rr.rect.right() >= o.right() && rr.rect.bottom() >= o.bottom()) {
		return false
	}
	if rr.is_rect() {
		return true
	}
	return rr.check_corner_containment(o.left(), o.top())
		&& rr.check_corner_containment(o.right(), o.top())
		&& rr.check_corner_containment(o.right(), o.bottom())
		&& rr.check_corner_containment(o.left(), o.bottom())
}

// transform_legacy mirrors SkRRect::transform() as built with
// SK_SUPPORT_LEGACY_RRECT_TRANSFORM, for scale and translate transforms. It
// returns none when the result is not a valid rounded rectangle.
fn (rr RRect) transform_legacy(t Transform) ?RRect {
	if t.typ == .identity {
		return rr
	}
	m := t.matrix.m
	if int(t.typ) > int(TransformType.rect_stays_rect) || m[0] == 0 {
		// Rotations by 90 degrees swap radii; they are not ported.
		return none
	}
	new_rect := t.map_rect(rr.rect)
	if !new_rect.is_finite() || !(new_rect.left() < new_rect.right()
		&& new_rect.top() < new_rect.bottom()) {
		return none
	}
	if rr.typ == .rect {
		return RRect{
			rect: new_rect
			typ:  .rect
		}
	}
	if rr.typ == .oval {
		half := Point{new_rect.width() / f32(2), new_rect.height() / f32(2)}
		return RRect{
			rect:  new_rect
			radii: [half, half, half, half]!
			typ:   .oval
		}
	}
	mut x_scale := m[0]
	mut y_scale := m[4]
	mut radii := rr.radii
	flip_x := x_scale < 0
	if flip_x {
		x_scale = -x_scale
	}
	flip_y := y_scale < 0
	if flip_y {
		y_scale = -y_scale
	}
	for i in 0 .. 4 {
		radii[i].x *= x_scale
		radii[i].y *= y_scale
	}
	ul, ur, lr, ll := int(Corner.upper_left), int(Corner.upper_right), int(Corner.lower_right), int(Corner.lower_left)
	if flip_x && flip_y {
		radii[ul], radii[lr] = radii[lr], radii[ul]
		radii[ur], radii[ll] = radii[ll], radii[ur]
	} else if flip_x {
		radii[ur], radii[ul] = radii[ul], radii[ur]
		radii[lr], radii[ll] = radii[ll], radii[lr]
	} else if flip_y {
		radii[ul], radii[ll] = radii[ll], radii[ul]
		radii[ur], radii[lr] = radii[lr], radii[ur]
	}
	_, typ := scale_radii(new_rect, mut radii)
	if !rrect_is_valid(new_rect, radii) {
		return none
	}
	return RRect{
		rect:  new_rect
		radii: radii
		typ:   typ
	}
}

fn rect_corner(r Rect, corner Corner) Point {
	return match corner {
		.upper_left { Point{r.left(), r.top()} }
		.upper_right { Point{r.right(), r.top()} }
		.lower_right { Point{r.right(), r.bottom()} }
		.lower_left { Point{r.left(), r.bottom()} }
	}
}

fn inside_corner(corner Corner, a Point, b Point) bool {
	return match corner {
		.upper_left { a.x >= b.x && a.y >= b.y }
		.upper_right { a.x <= b.x && a.y >= b.y }
		.lower_right { a.x <= b.x && a.y <= b.y }
		.lower_left { a.x >= b.x && a.y <= b.y }
	}
}

// conservative_intersect mirrors SkRRectPriv::ConservativeIntersect(): the
// intersection of two rounded rectangles when it is itself a rounded
// rectangle, otherwise an empty one.
fn conservative_intersect(a RRect, b RRect) RRect {
	empty := RRect{
		rect: Rect.ltrb(0, 0, 0, 0)
	}
	l := max_f32(a.rect.left(), b.rect.left())
	r := min_f32(a.rect.right(), b.rect.right())
	t := max_f32(a.rect.top(), b.rect.top())
	bt := min_f32(a.rect.bottom(), b.rect.bottom())
	if !(l < r && t < bt) {
		return empty
	}
	rect := Rect.ltrb(l, t, r, bt)
	mut radii := [4]Point{}
	for corner in [Corner.upper_left, .upper_right, .lower_right, .lower_left] {
		test := rect_corner(rect, corner)
		a_corner := rect_corner(a.rect, corner)
		b_corner := rect_corner(b.rect, corner)
		a_radii := a.radii[int(corner)]
		b_radii := b.radii[int(corner)]
		at_a := test.x == a_corner.x && test.y == a_corner.y
		at_b := test.x == b_corner.x && test.y == b_corner.y
		same_radii := a_radii.x == b_radii.x && a_radii.y == b_radii.y
		mut ok := false
		if at_a && at_b {
			if a_radii.x >= b_radii.x && a_radii.y >= b_radii.y {
				radii[int(corner)] = a_radii
				ok = true
			} else if b_radii.x >= a_radii.x && b_radii.y >= a_radii.y {
				radii[int(corner)] = b_radii
				ok = true
			}
		} else if at_a {
			radii[int(corner)] = a_radii
			ok = if same_radii {
				inside_corner(corner, a_corner, b_corner)
			} else {
				b.check_corner_containment(a_corner.x, a_corner.y)
			}
		} else if at_b {
			radii[int(corner)] = b_radii
			ok = if same_radii {
				inside_corner(corner, b_corner, a_corner)
			} else {
				a.check_corner_containment(b_corner.x, b_corner.y)
			}
		} else {
			ok = a.check_corner_containment(test.x, test.y)
				&& b.check_corner_containment(test.x, test.y)
		}
		if !ok {
			return empty
		}
	}
	if !rrect_is_valid(rect, radii) {
		return empty
	}
	scaled, typ := scale_radii(rect, mut radii)
	if scaled {
		return empty
	}
	return RRect{
		rect:  rect
		radii: radii
		typ:   typ
	}
}

module canvas

// Paths mirror SkPath as built by SkPathBuilder in the pinned Chromium Skia:
// the same verbs, points and conic weights, bounds, convexity and the shape
// queries (isLine, isRect, isOval, nested rects) that decide how Graphite
// draws them.

pub enum PathFillType {
	winding
	even_odd
	inverse_winding
	inverse_even_odd
}

pub enum PathDirection {
	cw
	ccw
}

pub enum PathVerb {
	move
	line
	quad
	conic
	cubic
	close
}

// PathIsA records SkPathIsAType: a path made by addOval/addRRect alone.
enum PathIsA {
	general
	oval
	rrect
}

pub enum PathConvexity {
	unknown
	convex_cw
	convex_ccw
	convex_degenerate
	concave
}

// Path is an immutable SkPath.
pub struct Path {
pub:
	verbs     []PathVerb
	points    []Point
	weights   []f32
	fill_type PathFillType
mut:
	is_a PathIsA
}

// PathBuilder mirrors SkPathBuilder for the verbs gg.canvas supports.
pub struct PathBuilder {
mut:
	verbs     []PathVerb
	points    []Point
	weights   []f32
	fill_type PathFillType
	last_move int = -1
	is_a      PathIsA
}

pub fn (mut b PathBuilder) set_fill_type(ft PathFillType) {
	b.fill_type = ft
}

pub fn (mut b PathBuilder) move_to(x f32, y f32) {
	if b.verbs.len > 0 && b.verbs[b.verbs.len - 1] == .move {
		b.points[b.points.len - 1] = Point{x, y}
		return
	}
	b.last_move = b.points.len
	b.points << Point{x, y}
	b.verbs << .move
	if b.is_a != .general {
		b.is_a = .general
	}
}

// ensure_move mirrors SkPathBuilder::ensureMove().
fn (mut b PathBuilder) ensure_move() {
	b.is_a = .general
	if b.verbs.len == 0 {
		b.move_to(0, 0)
	} else if b.verbs[b.verbs.len - 1] == .close {
		p := b.points[b.last_move]
		b.move_to(p.x, p.y)
	}
}

pub fn (mut b PathBuilder) line_to(x f32, y f32) {
	b.ensure_move()
	b.points << Point{x, y}
	b.verbs << .line
}

pub fn (mut b PathBuilder) quad_to(x1 f32, y1 f32, x2 f32, y2 f32) {
	b.ensure_move()
	b.points << Point{x1, y1}
	b.points << Point{x2, y2}
	b.verbs << .quad
}

pub fn (mut b PathBuilder) conic_to(x1 f32, y1 f32, x2 f32, y2 f32, w f32) {
	b.ensure_move()
	if w <= 0 {
		b.line_to(x2, y2)
		return
	}
	b.points << Point{x1, y1}
	b.points << Point{x2, y2}
	if w == 1 {
		b.verbs << .quad
	} else if is_finite_f32(w) {
		b.verbs << .conic
		b.weights << w
	} else {
		b.verbs << .line
		b.verbs << .line
	}
}

pub fn (mut b PathBuilder) cubic_to(x1 f32, y1 f32, x2 f32, y2 f32, x3 f32, y3 f32) {
	b.ensure_move()
	b.points << Point{x1, y1}
	b.points << Point{x2, y2}
	b.points << Point{x3, y3}
	b.verbs << .cubic
}

pub fn (mut b PathBuilder) close() {
	if b.verbs.len > 0 && b.verbs[b.verbs.len - 1] != .close {
		b.ensure_move()
		b.verbs << .close
	}
}

// add_oval mirrors SkPathBuilder::addOval(oval, dir, index).
pub fn (mut b PathBuilder) add_oval(r Rect, dir PathDirection, start int) {
	was_empty := b.verbs.len == 0 || (b.verbs.len == 1 && b.verbs[0] == .move)
	raw := oval_path_dir(r, dir, start)
	b.add_raw(raw)
	if was_empty {
		b.is_a = .oval
	}
}

// add_rect mirrors SkPathBuilder::addRect(rect, dir, index); SkPathBuilder's
// default start index is 0.
pub fn (mut b PathBuilder) add_rect(r Rect, dir PathDirection, start int) {
	b.add_raw(rect_path_dir(r, dir, start))
}

// add_rrect mirrors SkPathBuilder::addRRect(rrect, dir, index): rects and
// ovals become those contours. SkPathBuilder's default start index is 6 for
// clockwise and 7 for counter-clockwise contours.
pub fn (mut b PathBuilder) add_rrect(rr RRect, dir PathDirection, start int) {
	if rr.is_rect() || rr.is_empty() {
		b.add_rect(rr.rect, dir, (start + 1) / 2)
		return
	}
	if rr.is_oval() {
		b.add_oval(rr.rect, dir, start / 2)
		return
	}
	was_empty := b.verbs.len == 0 || (b.verbs.len == 1 && b.verbs[0] == .move)
	b.add_raw(rrect_path_dir(rr, dir, start))
	if was_empty {
		b.is_a = .rrect
	}
}

// add_raw mirrors SkPathBuilder::addRaw(): each verb is appended through the
// builder, then a trailing move the iterator dropped is restored.
fn (mut b PathBuilder) add_raw(raw Path) {
	mut pi := 0
	mut wi := 0
	for verb in raw.verbs {
		match verb {
			.move {
				b.move_to(raw.points[pi].x, raw.points[pi].y)
				pi++
			}
			.line {
				b.line_to(raw.points[pi].x, raw.points[pi].y)
				pi++
			}
			.quad {
				b.quad_to(raw.points[pi].x, raw.points[pi].y, raw.points[pi + 1].x, raw.points[pi +
					1].y)
				pi += 2
			}
			.conic {
				b.conic_to(raw.points[pi].x, raw.points[pi].y, raw.points[pi + 1].x, raw.points[pi +
					1].y, raw.weights[wi])
				pi += 2
				wi++
			}
			.cubic {
				b.cubic_to(raw.points[pi].x, raw.points[pi].y, raw.points[pi + 1].x, raw.points[pi +
					1].y, raw.points[pi + 2].x, raw.points[pi + 2].y)
				pi += 3
			}
			.close {
				b.close()
			}
		}
	}
}

// detach returns the path and resets the builder, like SkPathBuilder::detach().
pub fn (mut b PathBuilder) detach() Path {
	p := Path{
		verbs:     b.verbs.clone()
		points:    b.points.clone()
		weights:   b.weights.clone()
		fill_type: b.fill_type
		is_a:      if b.verbs.len <= 1 { PathIsA.general } else { b.is_a }
	}
	b.verbs.clear()
	b.points.clear()
	b.weights.clear()
	b.fill_type = .winding
	b.last_move = -1
	b.is_a = .general
	return p
}

// oval_path_dir mirrors set_as_oval() in SkPathRawShapes.cpp.
// rect_path_dir mirrors set_as_rect() in SkPathRawShapes.cpp.
fn rect_path_dir(r Rect, dir PathDirection, start int) Path {
	pts := rect_points(r)
	advance := if dir == .cw { 1 } else { 3 }
	mut i := start % 4
	mut points := [pts[i]]
	for _ in 0 .. 3 {
		i = (i + advance) % 4
		points << pts[i]
	}
	return Path{
		verbs:  [PathVerb.move, .line, .line, .line, .close]
		points: points
	}
}

// rrect_path_dir mirrors set_as_rrect() in SkPathRawShapes.cpp: the contour
// starts with a line at even clockwise or odd counter-clockwise indices and
// with a conic otherwise, whose closing line is left to the close verb.
fn rrect_path_dir(rr RRect, dir PathDirection, start int) Path {
	b := rr.rect
	ul := rr.radii[int(Corner.upper_left)]
	ur := rr.radii[int(Corner.upper_right)]
	lr := rr.radii[int(Corner.lower_right)]
	ll := rr.radii[int(Corner.lower_left)]
	l, t, r, bt := b.left(), b.top(), b.right(), b.bottom()
	// SkPath_RRectPointIterator's points, in clockwise order.
	rrect_pts := [Point{l + ul.x, t}, Point{r - ur.x, t}, Point{r, t + ur.y}, Point{r, bt - lr.y},
		Point{r - lr.x, bt}, Point{l + ll.x, bt}, Point{l, bt - ll.y}, Point{l, t + ul.y}]!
	rect := rect_points(b)
	cw := dir == .cw
	rrect_advance := if cw { 1 } else { 7 }
	rect_advance := if cw { 1 } else { 3 }
	mut ri := start % 8
	mut qi := (start / 2 + if cw { 0 } else { 1 }) % 4
	mut points := [rrect_pts[ri]]
	weights := [sk_root2_over_2, sk_root2_over_2, sk_root2_over_2, sk_root2_over_2]
	if ((start & 1) == 1) == cw {
		for i in 0 .. 4 {
			qi = (qi + rect_advance) % 4
			points << rect[qi]
			ri = (ri + rrect_advance) % 8
			points << rrect_pts[ri]
			if i < 3 {
				ri = (ri + rrect_advance) % 8
				points << rrect_pts[ri]
			}
		}
		return Path{
			verbs:   [PathVerb.move, .conic, .line, .conic, .line, .conic, .line, .conic, .close]
			points:  points
			weights: weights
		}
	}
	for _ in 0 .. 4 {
		ri = (ri + rrect_advance) % 8
		points << rrect_pts[ri]
		qi = (qi + rect_advance) % 4
		points << rect[qi]
		ri = (ri + rrect_advance) % 8
		points << rrect_pts[ri]
	}
	return Path{
		verbs:   [PathVerb.move, .line, .conic, .line, .conic, .line, .conic, .line, .conic, .close]
		points:  points
		weights: weights
	}
}

fn oval_path_dir(r Rect, dir PathDirection, start int) Path {
	cx := float_midpoint(r.left(), r.right())
	cy := float_midpoint(r.top(), r.bottom())
	oval := [Point{cx, r.top()}, Point{r.right(), cy}, Point{cx, r.bottom()}, Point{r.left(), cy}]!
	rect := rect_points(r)
	advance := if dir == .cw { 1 } else { 3 }
	mut oi := start % 4
	mut ri := (start + if dir == .cw { 0 } else { 1 }) % 4
	mut points := [oval[oi]]
	for _ in 0 .. 4 {
		ri = (ri + advance) % 4
		points << rect[ri]
		oi = (oi + advance) % 4
		points << oval[oi]
	}
	return Path{
		verbs:   [PathVerb.move, .conic, .conic, .conic, .conic, .close]
		points:  points
		weights: [sk_root2_over_2, sk_root2_over_2, sk_root2_over_2, sk_root2_over_2]
	}
}

pub fn (p Path) is_inverse_fill() bool {
	return p.fill_type == .inverse_winding || p.fill_type == .inverse_even_odd
}

pub fn (p Path) is_empty() bool {
	return p.verbs.len == 0
}

// bounds mirrors SkPath::getBounds(): the bounds of the points, ignoring a
// trailing move unless it is the only verb.
pub fn (p Path) bounds() Rect {
	mut n := p.points.len
	if p.verbs.len > 1 && p.verbs[p.verbs.len - 1] == .move {
		n--
	}
	if n <= 0 {
		return Rect.ltrb(0, 0, 0, 0)
	}
	mut l, mut t, mut r, mut b := p.points[0].x, p.points[0].y, p.points[0].x, p.points[0].y
	for i in 1 .. n {
		pt := p.points[i]
		l = min_f32(l, pt.x)
		t = min_f32(t, pt.y)
		r = max_f32(r, pt.x)
		b = max_f32(b, pt.y)
	}
	return Rect.ltrb(l, t, r, b)
}

pub fn (p Path) is_finite() bool {
	for pt in p.points {
		if !is_finite_f32(pt.x) || !is_finite_f32(pt.y) {
			return false
		}
	}
	return true
}

fn (p Path) only_lines() bool {
	mut has_line := false
	for v in p.verbs {
		match v {
			.line { has_line = true }
			.quad, .conic, .cubic { return false }
			else {}
		}
	}
	return has_line
}

// is_line mirrors SkPath::isLine().
pub fn (p Path) is_line() ?(Point, Point) {
	if p.verbs.len == 2 && p.verbs[1] == .line {
		return p.points[0], p.points[1]
	}
	return none
}

// is_oval mirrors SkPath::isOval(): only paths made by addOval alone.
pub fn (p Path) is_oval() ?Rect {
	if p.is_a == .oval {
		return p.bounds()
	}
	return none
}

pub fn (p Path) is_last_contour_closed() bool {
	return p.verbs.len > 0 && p.verbs[p.verbs.len - 1] == .close
}

// RectContour is SkPathPriv::RectContour.
struct RectContour {
	rect            Rect
	closed          bool
	dir             PathDirection
	points_consumed int
	verbs_consumed  int
}

// is_rect mirrors SkPath::isRect(): the rectangle and whether it is closed.
pub fn (p Path) is_rect() ?(Rect, bool) {
	rc := is_rect_contour(p.points, p.verbs, p.only_lines(), false) or { return none }
	return rc.rect, rc.closed
}

fn rect_make_dir(dx f32, dy f32) int {
	return (if dx != 0 { 1 } else { 0 }) | (if dx > 0 || dy > 0 { 2 } else { 0 })
}

fn point_rect(a Point, b Point) Rect {
	return Rect.ltrb(min_f32(a.x, b.x), min_f32(a.y, b.y), max_f32(a.x, b.x), max_f32(a.y,
		b.y))
}

// trivial_rect mirrors trivial_rect() in SkPathPriv.cpp.
fn trivial_rect(pts []Point, vbs []PathVerb) ?RectContour {
	if pts.len != 4 || vbs != [PathVerb.move, .line, .line, .line, .close] {
		return none
	}
	v0 := Point{pts[1].x - pts[0].x, pts[1].y - pts[0].y}
	v1 := Point{pts[2].x - pts[1].x, pts[2].y - pts[1].y}
	v2 := Point{pts[3].x - pts[2].x, pts[3].y - pts[2].y}
	v3 := Point{pts[0].x - pts[3].x, pts[0].y - pts[3].y}
	orthogonal := fn (a Point, b Point) bool {
		return ((a.x == 0) != (b.x == 0)) && ((a.y == 0) != (b.y == 0))
	}
	if !(((v0.x == 0) != (v0.y == 0)) && orthogonal(v0, v1) && orthogonal(v1, v2)
		&& orthogonal(v2, v3)) {
		return none
	}
	rect := Rect.ltrb(pts[0].x, pts[0].y, pts[2].x, pts[2].y).sorted()
	a := v0.x * v1.y
	b := v0.y * v1.x
	return RectContour{
		rect:            rect
		closed:          true
		dir:             if a - b > 0 { PathDirection.cw } else { PathDirection.ccw }
		points_consumed: 4
		verbs_consumed:  5
	}
}

// is_rect_contour mirrors SkPathPriv::IsRectContour(). `lines_only` is the
// segment mask test (only line segments).
fn is_rect_contour(pts []Point, vbs []PathVerb, lines_only bool, allow_partial bool) ?RectContour {
	if !lines_only || pts.len < 4 || vbs.len < 4 {
		return none
	}
	if rc := trivial_rect(pts, vbs) {
		return rc
	}
	mut curr_verb := 0
	mut corners := 0
	mut line_start := Point{0, 0}
	mut first_pt := -1
	mut last_pt := -1
	mut first_corner := Point{}
	mut third_corner := Point{}
	mut pi := 0
	mut save_pts := -1
	mut directions := [-1, -1, -1, -1, -1]
	mut closed_or_moved := false
	mut auto_close := false
	mut insert_close := false
	for curr_verb < vbs.len && (!allow_partial || !auto_close) {
		verb := if insert_close { PathVerb.close } else { vbs[curr_verb] }
		mut advance := true
		match verb {
			.close, .line {
				if verb == .close {
					save_pts = pi
					auto_close = true
					insert_close = false
				} else {
					last_pt = pi
				}
				line_end := if verb == .close { pts[first_pt] } else { pts[pi] }
				if verb != .close {
					pi++
				}
				dx := line_end.x - line_start.x
				dy := line_end.y - line_start.y
				if dx != 0 && dy != 0 {
					return none
				}
				if !is_finite_f32(dx) || !is_finite_f32(dy) {
					return none
				}
				if line_start.x == line_end.x && line_start.y == line_end.y {
					// A single point on a side.
				} else {
					next_direction := rect_make_dir(dx, dy)
					if corners == 0 {
						directions[0] = next_direction
						corners = 1
						closed_or_moved = false
						line_start = line_end
					} else {
						if closed_or_moved {
							return none
						}
						if auto_close && next_direction == directions[0] {
							// Colinear with the first side.
						} else {
							closed_or_moved = auto_close
							if directions[corners - 1] == next_direction {
								if corners == 3 && verb == .line {
									third_corner = line_end
								}
								line_start = line_end
							} else {
								directions[corners] = next_direction
								corners++
								match corners {
									2 {
										first_corner = line_start
									}
									3 {
										if directions[0] ^ directions[2] != 2 {
											return none
										}
										third_corner = line_end
									}
									4 {
										if directions[1] ^ directions[3] != 2 {
											return none
										}
									}
									else {
										return none
									}
								}
								line_start = line_end
							}
						}
					}
				}
			}
			.quad, .conic, .cubic {
				return none
			}
			.move {
				if allow_partial && !auto_close && directions[0] >= 0 {
					// Close the contour first, then retry the move (which ends
					// the loop): the consumed verbs exclude the move.
					insert_close = true
					curr_verb--
					advance = false
				} else {
					if corners == 0 {
						first_pt = pi
					} else {
						cx := pts[first_pt].x - pts[last_pt].x
						cy := pts[first_pt].y - pts[last_pt].y
						if cx != 0 && cy != 0 {
							return none
						}
					}
					line_start = pts[pi]
					pi++
					closed_or_moved = true
				}
			}
		}
		if advance {
			curr_verb++
		}
	}
	if corners < 3 || corners > 4 {
		return none
	}
	cx := pts[first_pt].x - pts[last_pt].x
	cy := pts[first_pt].y - pts[last_pt].y
	if cx != 0 && cy != 0 {
		return none
	}
	return RectContour{
		rect:            point_rect(first_corner, third_corner)
		closed:          auto_close
		dir:             if directions[0] == ((directions[1] + 1) & 3) {
			PathDirection.cw
		} else {
			PathDirection.ccw
		}
		points_consumed: if save_pts >= 0 { save_pts } else { 0 }
		verbs_consumed:  curr_verb
	}
}

// nested_fill_rects mirrors SkPathPriv::IsNestedFillRects(): two rectangle
// contours, the first containing the second.
fn (p Path) nested_fill_rects() ?([2]Rect, [2]PathDirection) {
	lines_only := p.only_lines()
	first := is_rect_contour(p.points, p.verbs, lines_only, true) or { return none }
	second := is_rect_contour(p.points[first.points_consumed..], p.verbs[first.verbs_consumed..],
		lines_only, false) or { return none }
	if sk_rect_contains(first.rect, second.rect) {
		return [first.rect, second.rect]!, [first.dir, second.dir]!
	}
	if sk_rect_contains(second.rect, first.rect) {
		return [second.rect, first.rect]!, [second.dir, first.dir]!
	}
	return none
}

// sk_rect_contains mirrors SkRect::contains(SkRect), which rejects empty
// rectangles.
fn sk_rect_contains(a Rect, b Rect) bool {
	return b.left() < b.right() && b.top() < b.bottom() && a.left() < a.right()
		&& a.top() < a.bottom() && a.left() <= b.left() && a.top() <= b.top()
		&& a.right() >= b.right() && a.bottom() >= b.bottom()
}

fn verb_point_count(v PathVerb) int {
	return match v {
		.move, .line { 1 }
		.quad, .conic { 2 }
		.cubic { 3 }
		.close { 0 }
	}
}

// Convexicator mirrors the single-contour state of SkPathPriv::ComputeConvexity().
struct Convexicator {
mut:
	first_pt     Point
	first_vec    Point
	last_pt      Point
	last_vec     Point
	expected_dir int           = dir_invalid
	first_dir    PathConvexity = .unknown
	reversals    int
	finite       bool = true
}

const dir_unknown = 0
const dir_left = 1
const dir_right = 2
const dir_straight = 3
const dir_backwards = 4
const dir_invalid = 5

fn (mut c Convexicator) set_move_pt(p Point) {
	c.first_pt = p
	c.last_pt = p
	c.expected_dir = dir_invalid
}

fn (mut c Convexicator) add_pt(p Point) bool {
	if c.last_pt.x == p.x && c.last_pt.y == p.y {
		return true
	}
	v := Point{p.x - c.last_pt.x, p.y - c.last_pt.y}
	if c.first_pt.x == c.last_pt.x && c.first_pt.y == c.last_pt.y && c.expected_dir == dir_invalid
		&& c.last_vec.x == 0 && c.last_vec.y == 0 {
		c.last_vec = v
		c.first_vec = v
	} else if !c.add_vec(v) {
		return false
	}
	c.last_pt = p
	return true
}

fn (c Convexicator) direction_change(v Point) int {
	a := c.last_vec.x * v.y
	b := c.last_vec.y * v.x
	cross := a - b
	if !is_finite_f32(cross) {
		return dir_unknown
	}
	if cross == 0 {
		da := c.last_vec.x * v.x
		db := c.last_vec.y * v.y
		return if da + db < 0 { dir_backwards } else { dir_straight }
	}
	return if cross > 0 { dir_right } else { dir_left }
}

fn (mut c Convexicator) add_vec(v Point) bool {
	dir := c.direction_change(v)
	match dir {
		dir_left, dir_right {
			if c.expected_dir == dir_invalid {
				c.expected_dir = dir
				c.first_dir = if dir == dir_right { PathConvexity.convex_cw } else { .convex_ccw }
			} else if dir != c.expected_dir {
				c.first_dir = .unknown
				return false
			}
			c.last_vec = v
		}
		dir_straight {}
		dir_backwards {
			c.last_vec = v
			c.reversals++
			return c.reversals < 3
		}
		else {
			c.finite = false
			return false
		}
	}
	return true
}

fn (mut c Convexicator) close() bool {
	return c.add_pt(c.first_pt) && c.add_vec(c.first_vec)
}

// is_concave_by_sign mirrors Convexicator::IsConcaveBySign().
fn is_concave_by_sign(points []Point) bool {
	if points.len <= 3 {
		return false
	}
	mut curr := points[0]
	first := curr
	mut idx := 1
	mut dxes := 0
	mut dyes := 0
	mut last_sx := 2
	mut last_sy := 2
	for outer in 0 .. 2 {
		for {
			if outer == 0 && idx >= points.len {
				break
			}
			next := if outer == 0 { points[idx] } else { first }
			vx := next.x - curr.x
			vy := next.y - curr.y
			if vx != 0 || vy != 0 {
				if !is_finite_f32(vx) || !is_finite_f32(vy) {
					return true
				}
				sx := if vx < 0 { 1 } else { 0 }
				sy := if vy < 0 { 1 } else { 0 }
				if sx != last_sx {
					dxes++
				}
				if sy != last_sy {
					dyes++
				}
				if dxes > 3 || dyes > 3 {
					return true
				}
				last_sx = sx
				last_sy = sy
			}
			curr = next
			idx++
			if outer == 1 {
				break
			}
		}
	}
	return false
}

// convexity mirrors SkPathPriv::ComputeConvexity().
pub fn (p Path) convexity() PathConvexity {
	mut verbs := p.verbs.clone()
	mut points := p.points.clone()
	for verbs.len > 0 && verbs[verbs.len - 1] == .move {
		verbs.delete_last()
		points.delete_last()
	}
	if verbs.len == 0 {
		return .convex_degenerate
	}
	if is_concave_by_sign(points) {
		return .concave
	}
	mut contours := 0
	mut needs_close := false
	mut state := Convexicator{}
	mut pi := 0
	for verb in verbs {
		// The verb's points start at the previous end point.
		start := if verb == .move { pi } else { pi - 1 }
		count := verb_point_count(verb)
		if contours == 0 {
			if verb == .move {
				state.set_move_pt(points[start])
			} else {
				contours++
				needs_close = true
			}
		}
		if contours == 1 {
			if verb == .close || verb == .move {
				if !state.close() {
					return .concave
				}
				needs_close = false
				contours++
			} else {
				for i in 1 .. count + 1 {
					if !state.add_pt(points[start + i]) {
						return .concave
					}
				}
			}
		} else if verb != .move {
			return .concave
		}
		if verb != .close {
			pi += count
		}
	}
	if needs_close && !state.close() {
		return .concave
	}
	if state.first_dir == .unknown && state.reversals >= 3 {
		return .concave
	}
	return if state.first_dir == .unknown {
		PathConvexity.convex_degenerate
	} else {
		state.first_dir
	}
}

pub fn (p Path) is_convex() bool {
	return p.convexity() in [.convex_cw, .convex_ccw, .convex_degenerate]
}

#include "@DIR/path_math.h"

fn C.gg_canvas_sincosf(x f32, s &f32, c &f32)
fn C.cosf(x f32) f32
fn C.tanf(x f32) f32
fn C.atan2f(y f32, x f32) f32
fn C.ceilf(x f32) f32

// sk_scalar_pi is SK_ScalarPI.
const sk_scalar_pi = f32(3.14159265)

// sin_cos_snap_to_zero mirrors SkScalarSinSnapToZero() and
// SkScalarCosSnapToZero() of one angle, which the compiled Skia evaluates
// with a single sincos call.
fn sin_cos_snap_to_zero(radians f32) (f32, f32) {
	mut s := f32(0)
	mut c := f32(0)
	C.gg_canvas_sincosf(radians, &s, &c)
	tolerance := f32(1) / f32(1 << 16)
	return if abs_f32(s) <= tolerance { f32(0) } else { s }, if abs_f32(c) <= tolerance {
		f32(0)
	} else {
		c
	}
}

// AffinePoints is the 2x2 of an SkMatrix without translation, applied like
// SkMatrix::mapPoints(): (x * sx + y * kx, y * sy + x * ky).
struct AffinePoints {
	sx f32
	kx f32
	ky f32
	sy f32
}

fn (m AffinePoints) map(p Point) Point {
	a := p.x * m.sx
	b := p.y * m.kx
	c := p.y * m.sy
	d := p.x * m.ky
	return Point{a + b, c + d}
}

// arc_to_svg mirrors SkPathBuilder::arcTo(SkPoint rad, angle, arcLarge,
// arcSweep, endPt): an SVG elliptical arc from the last point, added as
// conics.
pub fn (mut b PathBuilder) arc_to_svg(rad_x f32, rad_y f32, angle f32, large bool, sweep PathDirection, x f32, y f32) {
	b.ensure_move()
	src0 := b.points[b.points.len - 1]
	src1 := Point{x, y}
	if rad_x == 0 || rad_y == 0 {
		b.line_to(x, y)
		return
	}
	if src0.x == src1.x && src0.y == src1.y {
		b.line_to(x, y)
		return
	}
	mut rx := abs_f32(rad_x)
	mut ry := abs_f32(rad_y)
	mid_dx := src0.x - src1.x
	mid_dy := src0.y - src1.y
	mid := Point{mid_dx * f32(0.5), mid_dy * f32(0.5)}
	// setRotate(-angle), then mapPoint().
	deg_to_rad := sk_scalar_pi / f32(180)
	neg_rad := -angle * deg_to_rad
	s0, c0 := sin_cos_snap_to_zero(neg_rad)
	rotate := AffinePoints{c0, -s0, s0, c0}
	tm := rotate.map(mid)
	square_rx := rx * rx
	square_ry := ry * ry
	square_x := tm.x * tm.x
	square_y := tm.y * tm.y
	qx := square_x / square_rx
	qy := square_y / square_ry
	mut radii_scale := qx + qy
	if radii_scale > 1 {
		radii_scale = C.sqrtf(radii_scale)
		rx *= radii_scale
		ry *= radii_scale
	}
	// setScale(1 / rx, 1 / ry).preRotate(-angle)
	isx := f32(1) / rx
	isy := f32(1) / ry
	unit_xform := AffinePoints{isx * c0, isx * -s0, isy * s0, isy * c0}
	mut u0 := unit_xform.map(src0)
	mut u1 := unit_xform.map(src1)
	mut delta := Point{u1.x - u0.x, u1.y - u0.y}
	dxx := delta.x * delta.x
	dyy := delta.y * delta.y
	d := dxx + dyy
	inv_d := f32(1) / d
	scale_factor_squared := max_f32(inv_d - f32(0.25), 0)
	mut scale_factor := C.sqrtf(scale_factor_squared)
	if (sweep == .ccw) != large {
		scale_factor = -scale_factor
	}
	delta = Point{delta.x * scale_factor, delta.y * scale_factor}
	sum := Point{u0.x + u1.x, u0.y + u1.y}
	mut center := Point{sum.x * f32(0.5), sum.y * f32(0.5)}
	center = Point{center.x + -delta.y, center.y + delta.x}
	u0 = Point{u0.x - center.x, u0.y - center.y}
	u1 = Point{u1.x - center.x, u1.y - center.y}
	theta1 := C.atan2f(u0.y, u0.x)
	theta2 := C.atan2f(u1.y, u1.x)
	mut theta_arc := theta2 - theta1
	two_pi := sk_scalar_pi * f32(2)
	if theta_arc < 0 && sweep == .cw {
		theta_arc += two_pi
	} else if theta_arc > 0 && sweep != .cw {
		theta_arc -= two_pi
	}
	if abs_f32(theta_arc) < sk_scalar_pi / f32(1000 * 1000) {
		b.line_to(x, y)
		return
	}
	// setRotate(angle).preScale(rx, ry)
	rad := angle * deg_to_rad
	s1, c1 := sin_cos_snap_to_zero(rad)
	xform := AffinePoints{c1 * rx, -s1 * ry, s1 * rx, c1 * ry}
	third := (f32(2) * sk_scalar_pi) / f32(3)
	segments := int(C.ceilf(abs_f32(theta_arc / third)))
	theta_width := theta_arc / f32(segments)
	t := C.tanf(f32(0.5) * theta_width)
	if !is_finite_f32(t) {
		return
	}
	mut start_theta := theta1
	half_cos := C.cosf(theta_width) * f32(0.5)
	w := C.sqrtf(f32(0.5) + half_cos)
	is_integer := fn (v f32) bool {
		return v == floor_f32(v)
	}
	half_pi := sk_scalar_pi / f32(2)
	expect_integers := abs_f32(half_pi - abs_f32(theta_width)) <= scalar_nearly_zero
		&& is_integer(rx) && is_integer(ry) && is_integer(x) && is_integer(y)
	for _ in 0 .. segments {
		end_theta := start_theta + theta_width
		sin_end, cos_end := sin_cos_snap_to_zero(end_theta)
		p1 := Point{cos_end + center.x, sin_end + center.y}
		ts := t * sin_end
		tc := -t * cos_end
		p0 := Point{p1.x + ts, p1.y + tc}
		mut m0 := xform.map(p0)
		mut m1 := xform.map(p1)
		if expect_integers {
			m0 = Point{floor_f32(m0.x + f32(0.5)), floor_f32(m0.y + f32(0.5))}
			m1 = Point{floor_f32(m1.x + f32(0.5)), floor_f32(m1.y + f32(0.5))}
		}
		b.conic_to(m0.x, m0.y, m1.x, m1.y, w)
		start_theta = end_theta
	}
	// The final point is exactly the requested end point.
	b.points[b.points.len - 1] = Point{x, y}
}

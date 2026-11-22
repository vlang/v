module svg

import math
import strings

// PathCmdKind identifies an SVG path command.
pub enum PathCmdKind {
	move_to
	line_to
	h_line_to
	v_line_to
	c_curve
	s_curve
	q_curve
	t_curve
	a_arc
	close
}

// PathCmd is a compact path command. vals holds up to 7 raw arguments;
// relative records whether the coordinates are relative to the current point.
pub struct PathCmd {
pub:
	kind     PathCmdKind
	relative bool
	vals     [7]f64 = [f64(0), 0, 0, 0, 0, 0, 0]
}

// Path is an SVG path (the contents of a `d` attribute).
pub struct Path {
pub mut:
	cmds []PathCmd
}

// Cubic is a cubic Bezier segment with absolute control points.
pub struct Cubic {
pub:
	p0 Vec2
	p1 Vec2
	p2 Vec2
	p3 Vec2
}

// AbsSegment is a path segment with absolute coordinates.
pub struct AbsSegment {
pub:
	kind  PathCmdKind
	p0    Vec2
	p1    Vec2
	p2    Vec2
	p3    Vec2
	rx    f64
	ry    f64
	rot   f64
	large bool
	sweep bool
}

fn is_cmd_letter(c u8) bool {
	return (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`)
}

fn upper_cmd(c u8) u8 {
	if c >= `a` && c <= `z` {
		return c - 32
	}
	return c
}

fn args_for(cmd u8) !int {
	c := upper_cmd(cmd)
	if c == `M` || c == `L` || c == `T` {
		return 2
	}
	if c == `H` || c == `V` {
		return 1
	}
	if c == `C` {
		return 6
	}
	if c == `S` || c == `Q` {
		return 4
	}
	if c == `A` {
		return 7
	}
	if c == `Z` {
		return 0
	}
	return error('invalid path command: ${cmd}')
}

// parse_path parses SVG path data. Implicit repetitions and compact number
// syntax (e.g. M10-20A5.5.3-4 110-.1) are supported and normalized into
// explicit commands.
pub fn parse_path(input string) !Path {
	mut s := input.trim_space()
	mut p := Path{}
	if s.len == 0 {
		return p
	}
	mut i := 0
	mut cmd := u8(0)
	for {
		i = skip_ws(s, i)
		if i >= s.len {
			break
		}
		if is_cmd_letter(s[i]) {
			cmd = s[i]
			i++
		} else if cmd == 0 {
			return error('path data must start with a command')
		}
		n := args_for(cmd) or { return err }
		if n == 0 {
			p.cmds << PathCmd{ kind: .close, relative: cmd == `z` }
			continue
		}
		vals := read_args(s, i, cmd, n) or { return err }
		i = vals.next
		p.cmds << make_cmd(cmd, vals.values)
		// Implicit repetition: keep reading the same command while the next
		// token is a number rather than a new command letter.
		mut rep_cmd := cmd
		for {
			saved := i
			i = skip_ws(s, i)
			if i >= s.len || is_cmd_letter(s[i]) {
				i = saved
				break
			}
			more := read_args(s, i, rep_cmd, n) or {
				i = saved
				break
			}
			i = more.next
			p.cmds << make_cmd(rep_cmd, more.values)
			// After the first moveto, implicit repeats are lineto.
			if cmd == `M` || cmd == `m` {
				rep_cmd = if cmd == `M` { `L` } else { `l` }
			}
		}
	}
	return p
}

struct ArgRead {
mut:
	values [7]f64 = [f64(0), 0, 0, 0, 0, 0, 0]
	next   int
}

fn read_args(s string, start int, cmd u8, n int) !ArgRead {
	mut out := ArgRead{ next: start }
	mut i := start
	for k in 0 .. n {
		i = skip_ws(s, i)
		if i < s.len && s[i] == `,` {
			i = skip_ws(s, i + 1)
		}
		if i >= s.len {
			return error('unexpected end of path data')
		}
		// Arc flags are single digits and may abut the next number.
		if upper_cmd(cmd) == `A` && (k == 3 || k == 4) {
			if s[i] != `0` && s[i] != `1` {
				return error('arc flags must be 0 or 1')
			}
			out.values[k] = if s[i] == `1` { 1 } else { 0 }
			i++
			continue
		}
		num := scan_number(s, i) or { return error('bad number in path data') }
		out.values[k] = num.value
		i += num.consumed
	}
	out.next = i
	return out
}

fn make_cmd(cmd u8, vals [7]f64) PathCmd {
	kind := match upper_cmd(cmd) {
		`M` { PathCmdKind.move_to }
		`L` { PathCmdKind.line_to }
		`H` { PathCmdKind.h_line_to }
		`V` { PathCmdKind.v_line_to }
		`C` { PathCmdKind.c_curve }
		`S` { PathCmdKind.s_curve }
		`Q` { PathCmdKind.q_curve }
		`T` { PathCmdKind.t_curve }
		`A` { PathCmdKind.a_arc }
		else { PathCmdKind.close }
	}
	return PathCmd{ kind: kind, relative: cmd >= `a` && cmd <= `z`, vals: vals }
}

fn cmd_letter(k PathCmdKind, relative bool) string {
	base := match k {
		.move_to { 'M' }
		.line_to { 'L' }
		.h_line_to { 'H' }
		.v_line_to { 'V' }
		.c_curve { 'C' }
		.s_curve { 'S' }
		.q_curve { 'Q' }
		.t_curve { 'T' }
		.a_arc { 'A' }
		.close { 'Z' }
	}
	if !relative {
		return base
	}
	return base.to_lower()
}

fn arg_count(k PathCmdKind) int {
	return match k {
		.move_to, .line_to, .t_curve { 2 }
		.h_line_to, .v_line_to { 1 }
		.c_curve { 6 }
		.s_curve, .q_curve { 4 }
		.a_arc { 7 }
		.close { 0 }
	}
}

// str serializes the path back into compact SVG path data.
pub fn (p Path) str() string {
	mut sb := strings.new_builder(p.cmds.len * 12 + 16)
	for c in p.cmds {
		sb.write_string(cmd_letter(c.kind, c.relative))
		n := arg_count(c.kind)
		for k in 0 .. n {
			if k > 0 {
				sb.write_string(' ')
			}
			sb.write_string(fmt_num(c.vals[k]))
		}
	}
	return sb.str()
}

// segments converts raw commands into absolute segments.
pub fn (p Path) segments() []AbsSegment {
	mut out := []AbsSegment{cap: p.cmds.len}
	p.segments_into(mut out)
	return out
}

// segments_into fills a reusable buffer with absolute segments.
pub fn (p Path) segments_into(mut out []AbsSegment) {
	out.clear()
	mut cur := Vec2{}
	mut start := Vec2{}
	mut prev_ctrl := Vec2{}
	mut prev_kind := PathCmdKind.close
	for cm in p.cmds {
		rel := cm.relative
		add := fn (base f32, v f64, relative bool) f32 {
			return if relative { base + f32(v) } else { f32(v) }
		}
		match cm.kind {
			.move_to {
				to := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				out << AbsSegment{ kind: .move_to, p0: cur, p1: to }
				cur = to
				start = to
				prev_ctrl = Vec2{}
			}
			.line_to {
				to := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				out << AbsSegment{ kind: .line_to, p0: cur, p1: to }
				cur = to
				prev_ctrl = Vec2{}
			}
			.h_line_to {
				to := Vec2{ x: add(cur.x, cm.vals[0], rel), y: cur.y }
				out << AbsSegment{ kind: .line_to, p0: cur, p1: to }
				cur = to
				prev_ctrl = Vec2{}
			}
			.v_line_to {
				to := Vec2{ x: cur.x, y: add(cur.y, cm.vals[0], rel) }
				out << AbsSegment{ kind: .line_to, p0: cur, p1: to }
				cur = to
				prev_ctrl = Vec2{}
			}
			.c_curve {
				c1 := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				c2 := Vec2{ x: add(cur.x, cm.vals[2], rel), y: add(cur.y, cm.vals[3], rel) }
				to := Vec2{ x: add(cur.x, cm.vals[4], rel), y: add(cur.y, cm.vals[5], rel) }
				out << AbsSegment{ kind: .c_curve, p0: cur, p1: c1, p2: c2, p3: to }
				prev_ctrl = c2
				cur = to
			}
			.s_curve {
				c2 := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				to := Vec2{ x: add(cur.x, cm.vals[2], rel), y: add(cur.y, cm.vals[3], rel) }
				mut c1 := cur
				if prev_kind == .c_curve || prev_kind == .s_curve {
					c1 = Vec2{ x: 2 * cur.x - prev_ctrl.x, y: 2 * cur.y - prev_ctrl.y }
				}
				out << AbsSegment{ kind: .c_curve, p0: cur, p1: c1, p2: c2, p3: to }
				prev_ctrl = c2
				cur = to
			}
			.q_curve {
				c := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				to := Vec2{ x: add(cur.x, cm.vals[2], rel), y: add(cur.y, cm.vals[3], rel) }
				out << AbsSegment{ kind: .q_curve, p0: cur, p1: c, p3: to }
				prev_ctrl = c
				cur = to
			}
			.t_curve {
				to := Vec2{ x: add(cur.x, cm.vals[0], rel), y: add(cur.y, cm.vals[1], rel) }
				mut c := cur
				if prev_kind == .q_curve || prev_kind == .t_curve {
					c = Vec2{ x: 2 * cur.x - prev_ctrl.x, y: 2 * cur.y - prev_ctrl.y }
				}
				out << AbsSegment{ kind: .q_curve, p0: cur, p1: c, p3: to }
				prev_ctrl = c
				cur = to
			}
			.a_arc {
				to := Vec2{ x: add(cur.x, cm.vals[5], rel), y: add(cur.y, cm.vals[6], rel) }
				out << AbsSegment{
					kind:  .a_arc
					p0:    cur
					p1:    to
					rx:    cm.vals[0]
					ry:    cm.vals[1]
					rot:   cm.vals[2]
					large: cm.vals[3] != 0
					sweep: cm.vals[4] != 0
				}
				cur = to
				prev_ctrl = Vec2{}
			}
			.close {
				out << AbsSegment{ kind: .close, p0: cur, p1: start }
				cur = start
				prev_ctrl = Vec2{}
			}
		}
		prev_kind = cm.kind
	}
}

// quad_to_cubic converts a quadratic Bezier to a cubic one.
pub fn quad_to_cubic(s AbsSegment) Cubic {
	return Cubic{
		p0: s.p0
		p1: Vec2{
			x: s.p0.x + (s.p1.x - s.p0.x) * 2 / 3
			y: s.p0.y + (s.p1.y - s.p0.y) * 2 / 3
		}
		p2: Vec2{
			x: s.p3.x + (s.p1.x - s.p3.x) * 2 / 3
			y: s.p3.y + (s.p1.y - s.p3.y) * 2 / 3
		}
		p3: s.p3
	}
}

fn angle_between(ux f64, uy f64, vx f64, vy f64) f64 {
	dot := ux * vx + uy * vy
	len := math.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy))
	if len < 1e-12 {
		return 0
	}
	mut a := math.acos(clamp_limits(dot / len, -1, 1))
	if ux * vy - uy * vx < 0 {
		a = -a
	}
	return a
}

// arc_to_cubics converts an SVG elliptical arc into one or more cubics.
pub fn arc_to_cubics(p0 Vec2, p1 Vec2, rx_in f64, ry_in f64, rot_in f64, large bool, sweep bool) []Cubic {
	mut out := []Cubic{}
	x1 := f64(p0.x)
	y1 := f64(p0.y)
	x2 := f64(p1.x)
	y2 := f64(p1.y)
	if x1 == x2 && y1 == y2 {
		return out
	}
	mut rx := math.abs(rx_in)
	mut ry := math.abs(ry_in)
	if rx < 1e-9 || ry < 1e-9 {
		out << Cubic{ p0: p0, p1: p0, p2: p1, p3: p1 }
		return out
	}
	phi := rot_in * math.pi / 180
	cos_phi := math.cos(phi)
	sin_phi := math.sin(phi)
	dx := (x1 - x2) / 2
	dy := (y1 - y2) / 2
	x1p := cos_phi * dx + sin_phi * dy
	y1p := -sin_phi * dx + cos_phi * dy
	lambda := x1p * x1p / (rx * rx) + y1p * y1p / (ry * ry)
	if lambda > 1 {
		scale := math.sqrt(lambda)
		rx *= scale
		ry *= scale
	}
	sign := if large != sweep { 1.0 } else { -1.0 }
	numer := rx * rx * ry * ry - rx * rx * y1p * y1p - ry * ry * x1p * x1p
	denom := rx * rx * y1p * y1p + ry * ry * x1p * x1p
	mut coef := 0.0
	if denom > 1e-12 {
		coef = sign * math.sqrt(math.max(f64(0), numer / denom))
	}
	cxp := coef * rx * y1p / ry
	cyp := coef * -ry * x1p / rx
	cx := cos_phi * cxp - sin_phi * cyp + (x1 + x2) / 2
	cy := sin_phi * cxp + cos_phi * cyp + (y1 + y2) / 2
	ux := (x1p - cxp) / rx
	uy := (y1p - cyp) / ry
	vx := (-x1p - cxp) / rx
	vy := (-y1p - cyp) / ry
	theta1 := math.atan2(uy, ux)
	mut delta := angle_between(ux, uy, vx, vy)
	if !sweep && delta > 0 {
		delta -= 2 * math.pi
	}
	if sweep && delta < 0 {
		delta += 2 * math.pi
	}
	mut count := int(math.ceil(math.abs(delta) / (math.pi / 2)))
	if count < 1 {
		count = 1
	}
	step := delta / f64(count)
	mut t := theta1
	for _ in 0 .. count {
		t2 := t + step
		c1 := vec2(f32(cx + rx * math.cos(t) * cos_phi - ry * math.sin(t) * sin_phi),
			f32(cy + rx * math.cos(t) * sin_phi + ry * math.sin(t) * cos_phi))
		c2 := vec2(f32(cx + rx * math.cos(t2) * cos_phi - ry * math.sin(t2) * sin_phi),
			f32(cy + rx * math.cos(t2) * sin_phi + ry * math.sin(t2) * cos_phi))
		alpha := 4.0 / 3 * math.tan(step / 4)
		ctrl1 := Vec2{
			x: c1.x + f32(-alpha * rx * math.sin(t) * cos_phi - alpha * ry * math.cos(t) * sin_phi)
			y: c1.y + f32(-alpha * rx * math.sin(t) * sin_phi + alpha * ry * math.cos(t) * cos_phi)
		}
		ctrl2 := Vec2{
			x: c2.x + f32(alpha * rx * math.sin(t2) * cos_phi + alpha * ry * math.cos(t2) * sin_phi)
			y: c2.y + f32(alpha * rx * math.sin(t2) * sin_phi - alpha * ry * math.cos(t2) * cos_phi)
		}
		out << Cubic{ p0: c1, p1: ctrl1, p2: ctrl2, p3: c2 }
		t = t2
	}
	return out
}

fn segment_cubics(s AbsSegment) []Cubic {
	match s.kind {
		.c_curve {
			return [Cubic{ p0: s.p0, p1: s.p1, p2: s.p2, p3: s.p3 }]
		}
		.q_curve {
			return [quad_to_cubic(s)]
		}
		.line_to {
			return [Cubic{ p0: s.p0, p1: s.p0, p2: s.p1, p3: s.p1 }]
		}
		.a_arc {
			return arc_to_cubics(s.p0, s.p1, s.rx, s.ry, s.rot, s.large, s.sweep)
		}
		else {
			return []
		}
	}
}

// to_cubics converts all drawable segments into cubic Beziers.
pub fn (p Path) to_cubics() []Cubic {
	mut out := []Cubic{}
	for s in p.segments() {
		out << segment_cubics(s)
	}
	return out
}

fn cubic_point(c Cubic, t f32) Vec2 {
	u := 1 - t
	w0 := u * u * u
	w1 := 3 * u * u * t
	w2 := 3 * u * t * t
	w3 := t * t * t
	return Vec2{
		x: c.p0.x * w0 + c.p1.x * w1 + c.p2.x * w2 + c.p3.x * w3
		y: c.p0.y * w0 + c.p1.y * w1 + c.p2.y * w2 + c.p3.y * w3
	}
}

fn is_flat(c Cubic, tol f32) bool {
	d1 := c.p1.sub(c.p0)
	d2 := c.p2.sub(c.p0)
	d3 := c.p3.sub(c.p0)
	mut max_len := math.max(d1.length(), math.max(d2.length(), d3.length()))
	if max_len < 1e-6 {
		return true
	}
	// Distance of control points from the chord p0->p3.
	chord := c.p3.sub(c.p0)
	cl := chord.length()
	if cl < 1e-6 {
		return math.max(d1.length(), d2.length()) <= tol
	}
	nx := -chord.y / cl
	ny := chord.x / cl
	for p in [c.p1, c.p2] {
		d := math.abs((p.x - c.p0.x) * nx + (p.y - c.p0.y) * ny)
		max_len = math.max(max_len, d)
	}
	return max_len <= tol
}

fn split_cubic(c Cubic) (Cubic, Cubic) {
	p01 := c.p0.lerp(c.p1, 0.5)
	p12 := c.p1.lerp(c.p2, 0.5)
	p23 := c.p2.lerp(c.p3, 0.5)
	p012 := p01.lerp(p12, 0.5)
	p123 := p12.lerp(p23, 0.5)
	mid := p012.lerp(p123, 0.5)
	return Cubic{ p0: c.p0, p1: p01, p2: p012, p3: mid }, Cubic{ p0: mid, p1: p123, p2: p23, p3: c.p3 }
}

fn flatten_cubic(c Cubic, tol f32, mut out []Vec2, depth int) {
	if is_flat(c, tol) || depth > 16 {
		out << c.p3
		return
	}
	a, b := split_cubic(c)
	flatten_cubic(a, tol, mut out, depth + 1)
	flatten_cubic(b, tol, mut out, depth + 1)
}

// flatten converts the path into subpaths of points. Closed subpaths repeat
// their first point at the end.
pub fn (p Path) flatten(tol f32) [][]Vec2 {
	mut scratch := [][]Vec2{}
	mut segs := []AbsSegment{cap: p.cmds.len}
	n := p.flatten_into(tol, mut scratch, mut segs)
	scratch.trim(n)
	return scratch
}

// flatten_into appends flattened subpaths into reusable buffers and returns
// the number of valid subpaths in scratch.
pub fn (p Path) flatten_into(tol f32, mut scratch [][]Vec2, mut segs []AbsSegment) int {
	p.segments_into(mut segs)
	mut count := 0
	mut have := false
	for s in segs {
		match s.kind {
			.move_to {
				if have {
					count++
				}
				if count >= scratch.len {
					scratch << []Vec2{}
				}
				scratch[count].clear()
				scratch[count] << s.p1
				have = true
			}
			.close {
				if count < scratch.len && scratch[count].len > 0 {
					scratch[count] << scratch[count][0]
				}
			}
			else {
				if !have {
					count = 0
					if scratch.len == 0 {
						scratch << []Vec2{}
					}
					scratch[0].clear()
					scratch[0] << s.p0
					have = true
				}
				for c in segment_cubics(s) {
					flatten_cubic(c, tol, mut scratch[count], 0)
				}
			}
		}
	}
	if have {
		count++
	}
	return count
}

// bounds returns the axis-aligned bounds of the path (including control points).
pub fn (p Path) bounds() Rect {
	mut r := Rect{}
	mut set := false
	for s in p.segments() {
		for pt in [s.p0, s.p1, s.p2, s.p3] {
			if !set {
				r = Rect{ x: pt.x, y: pt.y, w: 0, h: 0 }
				set = true
			} else {
				x0 := math.min(r.x, pt.x)
				y0 := math.min(r.y, pt.y)
				x1 := math.max(r.max_x(), pt.x)
				y1 := math.max(r.max_y(), pt.y)
				r = Rect{ x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
			}
		}
	}
	return r
}

// transform applies an affine transform and returns a new path made of cubics.
pub fn (p Path) transform(t Transform) Path {
	mut out := Path{}
	mut open := false
	for s in p.segments() {
		match s.kind {
			.move_to {
				to := t.apply(s.p1)
				out.cmds << PathCmd{
					kind: .move_to
					vals: [f64(to.x), f64(to.y), 0, 0, 0, 0, 0]
				}
				open = true
			}
			.close {
				out.cmds << PathCmd{ kind: .close }
				open = false
			}
			else {
				if !open {
					start := t.apply(s.p0)
					out.cmds << PathCmd{
						kind: .move_to
						vals: [f64(start.x), f64(start.y), 0, 0, 0, 0, 0]
					}
					open = true
				}
				for c in segment_cubics(s) {
					a := t.apply(c.p1)
					b := t.apply(c.p2)
					d := t.apply(c.p3)
					out.cmds << PathCmd{
						kind: .c_curve
						vals: [f64(a.x), f64(a.y), f64(b.x), f64(b.y), f64(d.x), f64(d.y), 0]
					}
				}
			}
		}
	}
	return out
}

// length approximates the total length of the path.
pub fn (p Path) length() f64 {
	mut total := 0.0
	for sub in p.flatten(0.1) {
		for i in 1 .. sub.len {
			total += f64(sub[i].distance(sub[i - 1]))
		}
	}
	return total
}

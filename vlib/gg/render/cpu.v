module render

import math
import os

pub struct Frame {
pub mut:
	width  int
	height int
	pixels []u32
	// depth holds one float per pixel when rendered with render_cpu_depth().
	// A value of 1.0 is the far plane; smaller values are nearer.
	depth []f32
}

struct Viewport {
mut:
	x               int
	y               int
	w               int
	h               int
	origin_top_left bool
}

struct Scissor {
mut:
	x int
	y int
	w int
	h int
}

// render_cpu rasterizes the recorded commands into a pixel buffer without a
// GPU. It uses painter-order drawing, matching the 2D GPU pipeline.
pub fn (ctx &Context) render_cpu(width int, height int, clear u32) Frame {
	return ctx.render_cpu_impl(width, height, clear, false, 1.0, mat4_identity(), false)
}

// render_cpu_aa renders with supersampling anti-aliasing. The scene is
// rasterized at width*factor by height*factor and box-filtered down, giving
// smoother edges than render_cpu at the cost of factor^2 times the work.
// factor must be 1, 2, 3 or 4; other values fall back to 1.
pub fn (ctx &Context) render_cpu_aa(width int, height int, clear u32, factor int) Frame {
	f := if factor >= 2 && factor <= 4 { factor } else { 1 }
	if f == 1 {
		return ctx.render_cpu(width, height, clear)
	}
	big := ctx.render_cpu_impl(width * f, height * f, clear, false, 1.0, mat4_identity(), false)
	return downsample(big, width, height, f)
}

// render_cpu_depth_aa is render_cpu_depth with supersampling anti-aliasing.
pub fn (ctx &Context) render_cpu_depth_aa(width int, height int, clear u32, clear_depth f32, factor int) Frame {
	f := if factor >= 2 && factor <= 4 { factor } else { 1 }
	if f == 1 {
		return ctx.render_cpu_depth(width, height, clear, clear_depth)
	}
	big := ctx.render_cpu_impl(width * f, height * f, clear, true, clear_depth, mat4_identity(), false)
	return downsample(big, width, height, f)
}

// render_cpu_depth_with_mvp_aa is render_cpu_depth_with_mvp with
// supersampling anti-aliasing.
pub fn (ctx &Context) render_cpu_depth_with_mvp_aa(width int, height int, clear u32, clear_depth f32, mvp Mat4, factor int) Frame {
	f := if factor >= 2 && factor <= 4 { factor } else { 1 }
	if f == 1 {
		return ctx.render_cpu_depth_with_mvp(width, height, clear, clear_depth, mvp)
	}
	big := ctx.render_cpu_impl(width * f, height * f, clear, true, clear_depth, mvp, true)
	return downsample(big, width, height, f)
}

// downsample box-filters a supersampled frame into a frame of the given size.
fn downsample(big Frame, width int, height int, f int) Frame {
	mut out := Frame{
		width:  width
		height: height
		pixels: []u32{len: width * height}
	}
	if big.depth.len > 0 {
		out.depth = []f32{len: width * height, init: 1.0}
	}
	n := f * f
	for y in 0 .. height {
		for x in 0 .. width {
			mut r := f32(0)
			mut g := f32(0)
			mut b := f32(0)
			mut a := f32(0)
			mut nearest := f32(1)
			for sy in 0 .. f {
				for sx in 0 .. f {
					p := big.pixels[(y * f + sy) * big.width + (x * f + sx)]
					r += f32(p & 0xff)
					g += f32((p >> 8) & 0xff)
					b += f32((p >> 16) & 0xff)
					a += f32((p >> 24) & 0xff)
					if big.depth.len > 0 {
						d := big.depth[(y * f + sy) * big.width + (x * f + sx)]
						if d < nearest {
							nearest = d
						}
					}
				}
			}
			idx := y * width + x
			out.pixels[idx] = pack_color(u8(r / n), u8(g / n), u8(b / n), u8(a / n))
			if out.depth.len > 0 {
				out.depth[idx] = nearest
			}
		}
	}
	return out
}

// render_cpu_depth rasterizes with a depth buffer, so depth-tested 3D works
// without a window or GPU. Triangles behind the camera are skipped.
pub fn (ctx &Context) render_cpu_depth(width int, height int, clear u32, clear_depth f32) Frame {
	return ctx.render_cpu_impl(width, height, clear, true, clear_depth, mat4_identity(), false)
}

// render_cpu_depth_with_mvp is like render_cpu_depth, but overrides every
// command's model-view-projection matrix. This lets callers pass a 3D camera
// that was not necessarily active while the commands were recorded.
pub fn (ctx &Context) render_cpu_depth_with_mvp(width int, height int, clear u32, clear_depth f32, mvp Mat4) Frame {
	return ctx.render_cpu_impl(width, height, clear, true, clear_depth, mvp, true)
}

// rasterize_depth_cmd dispatches one draw command's triangles into a depth
// buffer. Backfaces are culled to match the GPU's depth pipeline and to skip
// roughly half the triangles of closed meshes.
fn rasterize_depth_cmd(mut frame Frame, verts []Vertex, uniforms []Uniform, cmd Command, mvp Mat4, vp Viewport, sc Scissor, ccw bool) {
	mut uni := uniforms[cmd.uniform_index]
	uni.mvp = mvp
	// With a top-left origin the y-axis is flipped on screen, so a front face
	// (CCW in NDC) has negative screen-space area. A bottom-left origin keeps
	// the sign; CW winding flips it in both cases.
	keep_positive := if ccw { !vp.origin_top_left } else { vp.origin_top_left }
	if cmd.primitive == .triangles {
		mut i := 0
		for i + 2 < cmd.num_vertices {
			rasterize_tri_depth(mut frame, verts, uni, vp, sc, keep_positive, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
			i += 3
		}
	} else if cmd.primitive == .triangle_strip {
		mut i := 0
		for i + 2 < cmd.num_vertices {
			if i % 2 == 0 {
				rasterize_tri_depth(mut frame, verts, uni, vp, sc, keep_positive, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
			} else {
				rasterize_tri_depth(mut frame, verts, uni, vp, sc, keep_positive, cmd.base_vertex + i + 1, cmd.base_vertex + i, cmd.base_vertex + i + 2)
			}
			i++
		}
	} else if cmd.primitive == .quads {
		mut i := 0
		for i + 2 < cmd.num_vertices {
			rasterize_tri_depth(mut frame, verts, uni, vp, sc, keep_positive, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
			i += 3
		}
	}
}

// render_depth_only rasterizes only depth from the given MVP, skipping all
// colour, texture and blending work. Used by shadow-map capture where only
// the depth buffer is needed. Returns a Frame with pixels.len == 0 and a
// depth buffer of width*height. Lines and points are intentionally ignored:
// a single sample carries no surface depth useful for a shadow map.
pub fn (ctx &Context) render_depth_only(width int, height int, clear_depth f32, mvp Mat4) Frame {
	if width <= 0 || height <= 0 {
		return Frame{}
	}
	mut st := ctx.st
	mut frame := Frame{
		width:  width
		height: height
		pixels: []u32{}
	}
	frame.depth = []f32{len: width * height, init: clear_depth}
	mut vp := Viewport{ x: 0, y: 0, w: width, h: height, origin_top_left: true }
	mut sc := Scissor{ x: 0, y: 0, w: width, h: height }

	for ci in 0 .. st.commands_count {
		cmd := st.commands[ci]
		if cmd.kind == .viewport {
			vp.x = cmd.x
			vp.y = cmd.y
			vp.w = cmd.w
			vp.h = cmd.h
			vp.origin_top_left = cmd.origin_top_left
		} else if cmd.kind == .scissor {
			sc.x = cmd.x
			sc.y = cmd.y
			sc.w = cmd.w
			sc.h = cmd.h
		} else if cmd.kind == .draw {
			if cmd.num_vertices <= 0 { continue }
			if cmd.uniform_index < 0 || cmd.uniform_index >= st.uniforms_count { continue }
			rasterize_depth_cmd(mut frame, st.verts, st.uniforms, cmd, mvp, vp, sc, st.face_winding == .ccw)
			// .lines, .line_strip and .points are skipped (see doc comment).
		}
	}
	return frame
}

fn (ctx &Context) render_cpu_impl(width int, height int, clear u32, with_depth bool, clear_depth f32, mvp_override Mat4, use_override bool) Frame {
	mut st := ctx.st
	mut frame := Frame{
		width:  width
		height: height
		pixels: []u32{len: width * height, init: clear}
	}
	if with_depth {
		frame.depth = []f32{len: width * height, init: clear_depth}
	}
	mut vp := Viewport{ x: 0, y: 0, w: width, h: height, origin_top_left: true }
	mut sc := Scissor{ x: 0, y: 0, w: width, h: height }

	for ci in 0 .. st.commands_count {
		cmd := st.commands[ci]
		if cmd.kind == .viewport {
			vp.x = cmd.x
			vp.y = cmd.y
			vp.w = cmd.w
			vp.h = cmd.h
			vp.origin_top_left = cmd.origin_top_left
		} else if cmd.kind == .scissor {
			sc.x = cmd.x
			sc.y = cmd.y
			sc.w = cmd.w
			sc.h = cmd.h
		} else if cmd.kind == .draw {
			if cmd.num_vertices <= 0 { continue }
			if cmd.uniform_index < 0 || cmd.uniform_index >= st.uniforms_count { continue }
			mut uni := st.uniforms[cmd.uniform_index]
			if use_override {
				uni.mvp = mvp_override
			}
			if cmd.primitive == .triangles {
				mut i := 0
				for i + 2 < cmd.num_vertices {
					rasterize_tri(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
					i += 3
				}
			} else if cmd.primitive == .triangle_strip {
				// strip alternation keeps the fragment winding consistent
				mut i := 0
				for i + 2 < cmd.num_vertices {
					if i % 2 == 0 {
						rasterize_tri(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
					} else {
						rasterize_tri(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i + 1, cmd.base_vertex + i, cmd.base_vertex + i + 2)
					}
					i++
				}
			} else if cmd.primitive == .quads {
				// vertices are pre-expanded into a 6-vertex triangle soup
				// (v0,v1,v2, v0,v2,v3 per quad); draw them as plain triangles.
				mut i := 0
				for i + 2 < cmd.num_vertices {
					rasterize_tri(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i, cmd.base_vertex + i + 1, cmd.base_vertex + i + 2)
					i += 3
				}
			} else if cmd.primitive == .line_strip {
				mut i := 0
				for i + 1 < cmd.num_vertices {
					draw_line(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i, cmd.base_vertex + i + 1)
					i++
				}
			} else if cmd.primitive == .lines {
				mut i := 0
				for i + 1 < cmd.num_vertices {
					draw_line(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i, cmd.base_vertex + i + 1)
					i += 2
				}
			} else if cmd.primitive == .points {
				for i in 0 .. cmd.num_vertices {
					draw_point(mut frame, st, cmd, uni, vp, sc, cmd.base_vertex + i)
				}
			}
		}
	}
	return frame
}

struct Proj {
mut:
	x     f32
	y     f32
	z     f32 // NDC depth, -1 near, 1 far
	w     f32 // clip-space w, used for perspective-correct interpolation
	valid bool
}

fn project_vertex(v Vertex, mvp Mat4, vp Viewport) Proj {
	cx := mvp.v[0][0] * v.x + mvp.v[1][0] * v.y + mvp.v[2][0] * v.z + mvp.v[3][0]
	cy := mvp.v[0][1] * v.x + mvp.v[1][1] * v.y + mvp.v[2][1] * v.z + mvp.v[3][1]
	cz := mvp.v[0][2] * v.x + mvp.v[1][2] * v.y + mvp.v[2][2] * v.z + mvp.v[3][2]
	cw := mvp.v[0][3] * v.x + mvp.v[1][3] * v.y + mvp.v[2][3] * v.z + mvp.v[3][3]
	if cw <= 1.0e-6 {
		return Proj{}
	}
	ndc_x := cx / cw
	ndc_y := cy / cw
	px := (ndc_x + 1.0) * 0.5 * f32(vp.w) + f32(vp.x)
	py := if vp.origin_top_left {
		(1.0 - ndc_y) * 0.5 * f32(vp.h) + f32(vp.y)
	} else {
		(ndc_y + 1.0) * 0.5 * f32(vp.h) + f32(vp.y)
	}
	return Proj{ x: px, y: py, z: cz / cw, w: cw, valid: true }
}

fn transform_vertex(v Vertex, mvp Mat4, vp Viewport) (f32, f32) {
	p := project_vertex(v, mvp, vp)
	return p.x, p.y
}

@[inline]
fn in_scissor(sc Scissor, x int, y int) bool {
	return x >= sc.x && y >= sc.y && x < sc.x + sc.w && y < sc.y + sc.h
}

@[inline]
fn pack_color(r u8, g u8, b u8, a u8) u32 {
	return u32(r) | (u32(g) << 8) | (u32(b) << 16) | (u32(a) << 24)
}

@[inline]
fn unpack_r(c u32) f32 { return f32(c & 0xff) / 255.0 }

@[inline]
fn unpack_g(c u32) f32 { return f32((c >> 8) & 0xff) / 255.0 }

@[inline]
fn unpack_b(c u32) f32 { return f32((c >> 16) & 0xff) / 255.0 }

@[inline]
fn unpack_a(c u32) f32 { return f32((c >> 24) & 0xff) / 255.0 }

fn sample_image(pixels []u8, w int, h int, ch_in int, filter int, u f32, v f32) u32 {
	if pixels.len == 0 || w <= 0 || h <= 0 {
		return 0xffffffff
	}
	mut uu := u
	mut vv := v
	if uu < 0.0 { uu = 0.0 }
	if uu > 1.0 { uu = 1.0 }
	if vv < 0.0 { vv = 0.0 }
	if vv > 1.0 { vv = 1.0 }
	ch := if ch_in > 0 { ch_in } else { 4 }
	if filter == 0 {
		ix := int(uu * f32(w - 1) + 0.5)
		iy := int(vv * f32(h - 1) + 0.5)
		idx := (iy * w + ix) * ch
		r := pixels[idx]
		g := if ch > 1 { pixels[idx + 1] } else { r }
		b := if ch > 2 { pixels[idx + 2] } else { r }
		a := if ch > 3 { pixels[idx + 3] } else { u8(255) }
		return pack_color(r, g, b, a)
	}
	fx := uu * f32(w - 1)
	fy := vv * f32(h - 1)
	x0 := int(math.floor(fx))
	y0 := int(math.floor(fy))
	x1 := if x0 + 1 < w { x0 + 1 } else { x0 }
	y1 := if y0 + 1 < h { y0 + 1 } else { y0 }
	wx := fx - f32(x0)
	wy := fy - f32(y0)
	idx00 := (y0 * w + x0) * ch
	r00 := pixels[idx00]
	g00 := if ch > 1 { pixels[idx00 + 1] } else { r00 }
	b00 := if ch > 2 { pixels[idx00 + 2] } else { r00 }
	a00 := if ch > 3 { pixels[idx00 + 3] } else { u8(255) }
	idx10 := (y0 * w + x1) * ch
	r10 := pixels[idx10]
	g10 := if ch > 1 { pixels[idx10 + 1] } else { r10 }
	b10 := if ch > 2 { pixels[idx10 + 2] } else { r10 }
	a10 := if ch > 3 { pixels[idx10 + 3] } else { u8(255) }
	idx01 := (y1 * w + x0) * ch
	r01 := pixels[idx01]
	g01 := if ch > 1 { pixels[idx01 + 1] } else { r01 }
	b01 := if ch > 2 { pixels[idx01 + 2] } else { r01 }
	a01 := if ch > 3 { pixels[idx01 + 3] } else { u8(255) }
	idx11 := (y1 * w + x1) * ch
	r11 := pixels[idx11]
	g11 := if ch > 1 { pixels[idx11 + 1] } else { r11 }
	b11 := if ch > 2 { pixels[idx11 + 2] } else { r11 }
	a11 := if ch > 3 { pixels[idx11 + 3] } else { u8(255) }
	r := u8(f32(r00) * (1 - wx) * (1 - wy) + f32(r10) * wx * (1 - wy) + f32(r01) * (1 - wx) * wy + f32(r11) * wx * wy)
	g := u8(f32(g00) * (1 - wx) * (1 - wy) + f32(g10) * wx * (1 - wy) + f32(g01) * (1 - wx) * wy + f32(g11) * wx * wy)
	b := u8(f32(b00) * (1 - wx) * (1 - wy) + f32(b10) * wx * (1 - wy) + f32(b01) * (1 - wx) * wy + f32(b11) * wx * wy)
	a := u8(f32(a00) * (1 - wx) * (1 - wy) + f32(a10) * wx * (1 - wy) + f32(a01) * (1 - wx) * wy + f32(a11) * wx * wy)
	return pack_color(r, g, b, a)
}

fn rasterize_tri(mut frame Frame, st &State, cmd Command, uni Uniform, vp Viewport, sc Scissor,
	i0 int, i1 int, i2 int) {
	v0 := st.verts[i0]
	v1 := st.verts[i1]
	v2 := st.verts[i2]
	q0 := project_vertex(v0, uni.mvp, vp)
	q1 := project_vertex(v1, uni.mvp, vp)
	q2 := project_vertex(v2, uni.mvp, vp)
	if !q0.valid || !q1.valid || !q2.valid {
		return
	}
	w := frame.width
	h := frame.height
	use_depth := frame.depth.len == w * h
	// Every fragment depth is a convex combination of the three vertex depths,
	// so if all vertices lie beyond the same clip plane no fragment can pass
	// the z-range test below. The non-depth path deliberately does not clip on
	// z, hence the use_depth guard to preserve its behaviour.
	if use_depth && ((q0.z < -1.0 && q1.z < -1.0 && q2.z < -1.0)
		|| (q0.z > 1.0 && q1.z > 1.0 && q2.z > 1.0)) {
		return
	}
	iw0 := 1.0 / q0.w
	iw1 := 1.0 / q1.w
	iw2 := 1.0 / q2.w
	p0x, p0y := q0.x, q0.y
	p1x, p1y := q1.x, q1.y
	p2x, p2y := q2.x, q2.y

	area := (p1x - p0x) * (p2y - p0y) - (p2x - p0x) * (p1y - p0y)
	if area == 0.0 {
		return
	}

	// Inline min/max: avoid math.min/math.max calls in the bounding-box path.
	mut min_xf := if p0x < p1x { p0x } else { p1x }
	if p2x < min_xf { min_xf = p2x }
	mut max_xf := if p0x > p1x { p0x } else { p1x }
	if p2x > max_xf { max_xf = p2x }
	mut min_yf := if p0y < p1y { p0y } else { p1y }
	if p2y < min_yf { min_yf = p2y }
	mut max_yf := if p0y > p1y { p0y } else { p1y }
	if p2y > max_yf { max_yf = p2y }

	min_x := int(math.floor(min_xf))
	max_x := int(math.ceil(max_xf))
	min_y := int(math.floor(min_yf))
	max_y := int(math.ceil(max_yf))

	// Fold the frame-bounds and scissor tests into the loop bounds, so the
	// per-pixel `continue` checks and the in_scissor call disappear entirely.
	mut lo_x := if min_x > sc.x { min_x } else { sc.x }
	if lo_x < 0 { lo_x = 0 }
	mut hi_x := if max_x < sc.x + sc.w - 1 { max_x } else { sc.x + sc.w - 1 }
	if hi_x >= w { hi_x = w - 1 }
	mut lo_y := if min_y > sc.y { min_y } else { sc.y }
	if lo_y < 0 { lo_y = 0 }
	mut hi_y := if max_y < sc.y + sc.h - 1 { max_y } else { sc.y + sc.h - 1 }
	if hi_y >= h { hi_y = h - 1 }
	if lo_x > hi_x || lo_y > hi_y {
		return
	}

	// Vertex colours are constant across the triangle: unpack them once here
	// instead of doing twelve shifts/masks/divides per fragment.
	ur0, ug0, ub0, ua0 := unpack_r(v0.rgba), unpack_g(v0.rgba), unpack_b(v0.rgba), unpack_a(v0.rgba)
	ur1, ug1, ub1, ua1 := unpack_r(v1.rgba), unpack_g(v1.rgba), unpack_b(v1.rgba), unpack_a(v1.rgba)
	ur2, ug2, ub2, ua2 := unpack_r(v2.rgba), unpack_g(v2.rgba), unpack_b(v2.rgba), unpack_a(v2.rgba)

	// Incremental edge functions advance by three additions per pixel rather
	// than recomputing six multiplies and six subtractions each time.
	a0 := p2y - p1y
	b0 := p1x - p2x
	a1 := p0y - p2y
	b1 := p2x - p0x
	a2 := p1y - p0y
	b2 := p0x - p1x
	sx := f32(lo_x) + 0.5
	sy := f32(lo_y) + 0.5
	w0_start := (sx - p1x) * a0 + (sy - p1y) * b0
	w1_start := (sx - p2x) * a1 + (sy - p2y) * b1
	w2_start := (sx - p0x) * a2 + (sy - p0y) * b2
	// The edge sum is constant across the triangle (x/y coefficients cancel),
	// so one reciprocal normalizes every fragment.
	wsum := w0_start + w1_start + w2_start
	if wsum == 0.0 {
		return
	}
	inv_wsum := 1.0 / wsum

	mut w0_row := w0_start
	mut w1_row := w1_start
	mut w2_row := w2_start
	for y in lo_y .. hi_y + 1 {
		mut w0 := w0_row
		mut w1 := w1_row
		mut w2 := w2_row
		row := y * w
		for x in lo_x .. hi_x + 1 {
			// Copy this pixel's edges and advance for the next one up front, so
			// the early-exit `continue`s below cannot skip the increments.
			lw0, lw1, lw2 := w0, w1, w2
			w0 += a0
			w1 += a1
			w2 += a2
			inside := if area > 0 {
				lw0 <= 0 && lw1 <= 0 && lw2 <= 0
			} else {
				lw0 >= 0 && lw1 >= 0 && lw2 >= 0
			}
			if !inside {
				continue
			}

			bw0 := lw0 * inv_wsum
			bw1 := lw1 * inv_wsum
			bw2 := lw2 * inv_wsum
			idx := row + x

			mut u := v0.u * bw0 + v1.u * bw1 + v2.u * bw2
			mut vv := v0.v * bw0 + v1.v * bw1 + v2.v * bw2
			if use_depth {
				// Precomputed reciprocal w avoids six divisions per fragment.
				iw0w := bw0 * iw0
				iw1w := bw1 * iw1
				iw2w := bw2 * iw2
				inv_w := iw0w + iw1w + iw2w
				if inv_w <= 0 {
					continue
				}
				// q*.z is already NDC depth (cz / cw). Interpolate it with
				// screen-space barycentric weights; weighting it by 1/w here
				// would perspective-correct the same value a second time.
				z := q0.z * bw0 + q1.z * bw1 + q2.z * bw2
				if z < -1.0 || z > 1.0 {
					continue
				}
				if z > frame.depth[idx] {
					continue
				}
				frame.depth[idx] = z
				// Attributes still in clip space do need perspective correction.
				u = (iw0w * v0.u + iw1w * v1.u + iw2w * v2.u) / inv_w
				vv = (iw0w * v0.v + iw1w * v1.v + iw2w * v2.v) / inv_w
			}

			r := ur0 * bw0 + ur1 * bw1 + ur2 * bw2
			g := ug0 * bw0 + ug1 * bw1 + ug2 * bw2
			b := ub0 * bw0 + ub1 * bw1 + ub2 * bw2
			a := ua0 * bw0 + ua1 * bw1 + ua2 * bw2

			mut tex_color := u32(0xffffffff)
			if cmd.image.id != 0 && cmd.image.id != st.def_img.id {
				if cmd.image.id in st.image_data {
					img_data := st.image_data[cmd.image.id] or { ImageData{} }
					filter := if img_data.filter == 1 { 1 } else { 0 }
					tex_color = sample_image(img_data.pixels, img_data.width, img_data.height, img_data.channels, filter, u, vv)
				}
			}

			tr := unpack_r(tex_color) * r
			tg := unpack_g(tex_color) * g
			tb := unpack_b(tex_color) * b
			ta := unpack_a(tex_color) * a

			dst := frame.pixels[idx]
			dr := unpack_r(dst)
			dg := unpack_g(dst)
			db := unpack_b(dst)
			da := unpack_a(dst)
			mut nr := f32(0)
			mut ng := f32(0)
			mut nb := f32(0)
			mut na := f32(0)
			if cmd.blend_mode == .add {
				nr = tr + dr
				ng = tg + dg
				nb = tb + db
				na = ta + da
			} else if cmd.blend_mode == .multiply {
				nr = tr * dr
				ng = tg * dg
				nb = tb * db
				na = ta * da
			} else if cmd.blend_mode == .screen {
				nr = 1.0 - (1.0 - tr) * (1.0 - dr)
				ng = 1.0 - (1.0 - tg) * (1.0 - dg)
				nb = 1.0 - (1.0 - tb) * (1.0 - db)
				na = ta + da * (1.0 - ta)
			} else if cmd.blend_mode == .none {
				nr = tr
				ng = tg
				nb = tb
				na = ta
			} else {
				nr = tr + dr * (1.0 - ta)
				ng = tg + dg * (1.0 - ta)
				nb = tb + db * (1.0 - ta)
				na = ta + da * (1.0 - ta)
			}
			if nr > 1.0 { nr = 1.0 }
			if ng > 1.0 { ng = 1.0 }
			if nb > 1.0 { nb = 1.0 }
			if na > 1.0 { na = 1.0 }
			frame.pixels[idx] = pack_color(u8(nr * 255), u8(ng * 255), u8(nb * 255), u8(na * 255))
		}
		w0_row += b0
		w1_row += b1
		w2_row += b2
	}
}

// rasterize_tri_depth is the depth-only counterpart of rasterize_tri. It uses
// the same projection, rejection and barycentric edge-function loop, but only
// interpolates NDC z and updates frame.depth; no colour, texture or blending
// work is done and frame.pixels is never touched.
fn rasterize_tri_depth(mut frame Frame, verts []Vertex, uni Uniform, vp Viewport, sc Scissor,
	keep_positive bool, i0 int, i1 int, i2 int) {
	v0 := verts[i0]
	v1 := verts[i1]
	v2 := verts[i2]
	q0 := project_vertex(v0, uni.mvp, vp)
	q1 := project_vertex(v1, uni.mvp, vp)
	q2 := project_vertex(v2, uni.mvp, vp)
	if !q0.valid || !q1.valid || !q2.valid {
		return
	}
	// Every fragment depth is a convex combination of the three vertex depths,
	// so if all vertices lie beyond the same clip plane, no fragment can pass
	// the z-range test below.
	if (q0.z < -1.0 && q1.z < -1.0 && q2.z < -1.0)
		|| (q0.z > 1.0 && q1.z > 1.0 && q2.z > 1.0) {
		return
	}
	w := frame.width
	h := frame.height
	p0x, p0y := q0.x, q0.y
	p1x, p1y := q1.x, q1.y
	p2x, p2y := q2.x, q2.y

	area := (p1x - p0x) * (p2y - p0y) - (p2x - p0x) * (p1y - p0y)
	if area == 0.0 {
		return
	}
	// Backface culling: backfaces of closed meshes are always farther from the
	// light and would lose the depth test anyway, so skipping them is a pure
	// work saving that also matches the GPU depth pipeline.
	if keep_positive {
		if area < 0.0 { return }
	} else {
		if area > 0.0 { return }
	}

	mut min_xf := if p0x < p1x { p0x } else { p1x }
	if p2x < min_xf { min_xf = p2x }
	mut max_xf := if p0x > p1x { p0x } else { p1x }
	if p2x > max_xf { max_xf = p2x }
	mut min_yf := if p0y < p1y { p0y } else { p1y }
	if p2y < min_yf { min_yf = p2y }
	mut max_yf := if p0y > p1y { p0y } else { p1y }
	if p2y > max_yf { max_yf = p2y }

	min_x := int(math.floor(min_xf))
	max_x := int(math.ceil(max_xf))
	min_y := int(math.floor(min_yf))
	max_y := int(math.ceil(max_yf))

	// Fold the frame-bounds and scissor tests into the loop bounds.
	mut lo_x := if min_x > sc.x { min_x } else { sc.x }
	if lo_x < 0 { lo_x = 0 }
	mut hi_x := if max_x < sc.x + sc.w - 1 { max_x } else { sc.x + sc.w - 1 }
	if hi_x >= w { hi_x = w - 1 }
	mut lo_y := if min_y > sc.y { min_y } else { sc.y }
	if lo_y < 0 { lo_y = 0 }
	mut hi_y := if max_y < sc.y + sc.h - 1 { max_y } else { sc.y + sc.h - 1 }
	if hi_y >= h { hi_y = h - 1 }
	if lo_x > hi_x || lo_y > hi_y {
		return
	}

	// Edge-function slopes: E(x+1,y) = E + a, E(x,y+1) = E + b. Advancing
	// three additions per pixel replaces six multiplies and six subtractions.
	a0 := p2y - p1y
	b0 := p1x - p2x
	a1 := p0y - p2y
	b1 := p2x - p0x
	a2 := p1y - p0y
	b2 := p0x - p1x

	sx := f32(lo_x) + 0.5
	sy := f32(lo_y) + 0.5
	w0_start := (sx - p1x) * a0 + (sy - p1y) * b0
	w1_start := (sx - p2x) * a1 + (sy - p2y) * b1
	w2_start := (sx - p0x) * a2 + (sy - p0y) * b2
	// The three edge sums to a constant across the triangle (the x and y
	// coefficients cancel), so a single reciprocal normalizes every pixel.
	wsum := w0_start + w1_start + w2_start
	if wsum == 0.0 {
		return
	}
	inv_wsum := 1.0 / wsum

	mut w0_row := w0_start
	mut w1_row := w1_start
	mut w2_row := w2_start
	for y in lo_y .. hi_y + 1 {
		mut w0 := w0_row
		mut w1 := w1_row
		mut w2 := w2_row
		row := y * w
		for x in lo_x .. hi_x + 1 {
			inside := if area > 0 {
				w0 <= 0 && w1 <= 0 && w2 <= 0
			} else {
				w0 >= 0 && w1 >= 0 && w2 >= 0
			}
			if inside {
				// q*.z is already NDC depth; interpolate with screen-space
				// barycentric weights, as in rasterize_tri's depth path.
				z := q0.z * (w0 * inv_wsum) + q1.z * (w1 * inv_wsum) + q2.z * (w2 * inv_wsum)
				if z >= -1.0 && z <= 1.0 && z <= frame.depth[row + x] {
					frame.depth[row + x] = z
				}
			}
			w0 += a0
			w1 += a1
			w2 += a2
		}
		w0_row += b0
		w1_row += b1
		w2_row += b2
	}
}

fn draw_line(mut frame Frame, st &State, _cmd Command, uni Uniform, vp Viewport, sc Scissor,
	i0 int, i1 int) {
	v0 := st.verts[i0]
	v1 := st.verts[i1]
	q0 := project_vertex(v0, uni.mvp, vp)
	q1 := project_vertex(v1, uni.mvp, vp)
	if !q0.valid || !q1.valid {
		return
	}
	x0, y0 := q0.x, q0.y
	x1p, y1p := q1.x, q1.y
	dx := x1p - x0
	dy := y1p - y0
	steps := int(math.max(math.abs(dx), math.abs(dy)))
	if steps == 0 {
		if int(x0) >= 0 && int(y0) >= 0 && int(x0) < frame.width && int(y0) < frame.height {
			if in_scissor(sc, int(x0), int(y0)) {
				ix, iy := int(x0), int(y0)
				mut depth_ok := true
				if frame.depth.len == frame.width * frame.height {
					di := iy * frame.width + ix
					if q0.z < -1.0 || q0.z > 1.0 || q0.z > frame.depth[di] {
						depth_ok = false
					} else {
						frame.depth[di] = q0.z
					}
				}
				if depth_ok {
					frame.pixels[iy * frame.width + ix] = v0.rgba
				}
			}
		}
		return
	}
	for i in 0 .. steps + 1 {
		t := f32(i) / f32(steps)
		x := int(x0 + dx * t + 0.5)
		y := int(y0 + dy * t + 0.5)
		if x >= 0 && y >= 0 && x < frame.width && y < frame.height {
			if in_scissor(sc, x, y) {
				mut depth_ok := true
				if frame.depth.len == frame.width * frame.height {
					z := q0.z + (q1.z - q0.z) * t
					di := y * frame.width + x
					if z < -1.0 || z > 1.0 || z > frame.depth[di] {
						depth_ok = false
					} else {
						frame.depth[di] = z
					}
				}
				if depth_ok {
					frame.pixels[y * frame.width + x] = v0.rgba
				}
			}
		}
	}
}

fn draw_point(mut frame Frame, st &State, _cmd Command, uni Uniform, vp Viewport, sc Scissor, i int) {
	v := st.verts[i]
	q := project_vertex(v, uni.mvp, vp)
	if !q.valid {
		return
	}
	ix := int(q.x + 0.5)
	iy := int(q.y + 0.5)
	if ix >= 0 && iy >= 0 && ix < frame.width && iy < frame.height {
		if in_scissor(sc, ix, iy) {
			mut depth_ok := true
			if frame.depth.len == frame.width * frame.height {
				di := iy * frame.width + ix
				if q.z < -1.0 || q.z > 1.0 || q.z > frame.depth[di] {
					depth_ok = false
				} else {
					frame.depth[di] = q.z
				}
			}
			if depth_ok {
				frame.pixels[iy * frame.width + ix] = v.rgba
			}
		}
	}
}

// save_ppm writes the frame as a binary P6 PPM file, returning an error when
// the file cannot be written.
pub fn (f Frame) save_ppm(path string) ! {
	mut data := []u8{cap: 64 + f.width * f.height * 3}
	header := 'P6\n${f.width} ${f.height}\n255\n'
	for c in header {
		data << u8(c)
	}
	for p in f.pixels {
		data << u8(p & 0xff)
		data << u8((p >> 8) & 0xff)
		data << u8((p >> 16) & 0xff)
	}
	os.write_file(path, data.bytestr())!
}

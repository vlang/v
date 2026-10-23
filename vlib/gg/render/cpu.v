module render

import math
import os

pub struct Frame {
pub mut:
	width  int
	height int
	pixels []u32
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

pub fn (ctx &Context) render_cpu(width int, height int, clear u32) Frame {
	mut st := ctx.st
	mut frame := Frame{
		width:  width
		height: height
		pixels: []u32{len: width * height, init: clear}
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
			uni := st.uniforms[cmd.uniform_index]
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

fn transform_vertex(v Vertex, mvp Mat4, vp Viewport) (f32, f32) {
	cx := mvp.v[0][0] * v.x + mvp.v[1][0] * v.y + mvp.v[2][0] * v.z + mvp.v[3][0]
	cy := mvp.v[0][1] * v.x + mvp.v[1][1] * v.y + mvp.v[2][1] * v.z + mvp.v[3][1]
	cw := mvp.v[0][3] * v.x + mvp.v[1][3] * v.y + mvp.v[2][3] * v.z + mvp.v[3][3]
	if cw == 0.0 {
		return 0.0, 0.0
	}
	ndc_x := cx / cw
	ndc_y := cy / cw
	px := (ndc_x + 1.0) * 0.5 * f32(vp.w) + f32(vp.x)
	py := if vp.origin_top_left {
		(1.0 - ndc_y) * 0.5 * f32(vp.h) + f32(vp.y)
	} else {
		(ndc_y + 1.0) * 0.5 * f32(vp.h) + f32(vp.y)
	}
	return px, py
}

fn in_scissor(sc Scissor, x int, y int) bool {
	return x >= sc.x && y >= sc.y && x < sc.x + sc.w && y < sc.y + sc.h
}

fn pack_color(r u8, g u8, b u8, a u8) u32 {
	return u32(r) | (u32(g) << 8) | (u32(b) << 16) | (u32(a) << 24)
}

fn unpack_r(c u32) f32 { return f32(c & 0xff) / 255.0 }

fn unpack_g(c u32) f32 { return f32((c >> 8) & 0xff) / 255.0 }

fn unpack_b(c u32) f32 { return f32((c >> 16) & 0xff) / 255.0 }

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
	p0x, p0y := transform_vertex(v0, uni.mvp, vp)
	p1x, p1y := transform_vertex(v1, uni.mvp, vp)
	p2x, p2y := transform_vertex(v2, uni.mvp, vp)

	area := (p1x - p0x) * (p2y - p0y) - (p2x - p0x) * (p1y - p0y)
	if area == 0.0 {
		return
	}

	min_x := int(math.floor(math.min(math.min(p0x, p1x), p2x)))
	max_x := int(math.ceil(math.max(math.max(p0x, p1x), p2x)))
	min_y := int(math.floor(math.min(math.min(p0y, p1y), p2y)))
	max_y := int(math.ceil(math.max(math.max(p0y, p1y), p2y)))

	for y in min_y .. max_y + 1 {
		for x in min_x .. max_x + 1 {
			if x < 0 || y < 0 || x >= frame.width || y >= frame.height {
				continue
			}
			if !in_scissor(sc, x, y) {
				continue
			}
			px := f32(x) + 0.5
			py := f32(y) + 0.5
			w0 := (px - p1x) * (p2y - p1y) - (py - p1y) * (p2x - p1x)
			w1 := (px - p2x) * (p0y - p2y) - (py - p2y) * (p0x - p2x)
			w2 := (px - p0x) * (p1y - p0y) - (py - p0y) * (p1x - p0x)
			inside := if area > 0 {
				w0 <= 0 && w1 <= 0 && w2 <= 0
			} else {
				w0 >= 0 && w1 >= 0 && w2 >= 0
			}
			if !inside {
				continue
			}

			wsum := w0 + w1 + w2
			if wsum == 0.0 {
				continue
			}
			bw0 := w0 / area
			bw1 := w1 / area
			bw2 := w2 / area

			u := v0.u * bw0 + v1.u * bw1 + v2.u * bw2
			vv := v0.v * bw0 + v1.v * bw1 + v2.v * bw2

			c0 := v0.rgba
			c1 := v1.rgba
			c2 := v2.rgba
			r := unpack_r(c0) * bw0 + unpack_r(c1) * bw1 + unpack_r(c2) * bw2
			g := unpack_g(c0) * bw0 + unpack_g(c1) * bw1 + unpack_g(c2) * bw2
			b := unpack_b(c0) * bw0 + unpack_b(c1) * bw1 + unpack_b(c2) * bw2
			a := unpack_a(c0) * bw0 + unpack_a(c1) * bw1 + unpack_a(c2) * bw2

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

			dst := frame.pixels[y * frame.width + x]
			dr := unpack_r(dst)
			dg := unpack_g(dst)
			db := unpack_b(dst)
			da := unpack_a(dst)
			mut nr := f32(0)
			mut ng := f32(0)
			mut nb := f32(0)
			mut na := f32(0)
			if cmd.blend_additive {
				nr = tr + dr
				ng = tg + dg
				nb = tb + db
				na = ta + da
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
			frame.pixels[y * frame.width + x] = pack_color(u8(nr * 255), u8(ng * 255), u8(nb * 255), u8(na * 255))
		}
	}
}

fn draw_line(mut frame Frame, st &State, _cmd Command, uni Uniform, vp Viewport, sc Scissor,
	i0 int, i1 int) {
	v0 := st.verts[i0]
	v1 := st.verts[i1]
	x0, y0 := transform_vertex(v0, uni.mvp, vp)
	x1p, y1p := transform_vertex(v1, uni.mvp, vp)
	dx := x1p - x0
	dy := y1p - y0
	steps := int(math.max(math.abs(dx), math.abs(dy)))
	if steps == 0 {
		if int(x0) >= 0 && int(y0) >= 0 && int(x0) < frame.width && int(y0) < frame.height {
			if in_scissor(sc, int(x0), int(y0)) {
				frame.pixels[int(y0) * frame.width + int(x0)] = v0.rgba
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
				frame.pixels[y * frame.width + x] = v0.rgba
			}
		}
	}
}

fn draw_point(mut frame Frame, st &State, _cmd Command, uni Uniform, vp Viewport, sc Scissor, i int) {
	v := st.verts[i]
	x, y := transform_vertex(v, uni.mvp, vp)
	ix := int(x + 0.5)
	iy := int(y + 0.5)
	if ix >= 0 && iy >= 0 && ix < frame.width && iy < frame.height {
		if in_scissor(sc, ix, iy) {
			frame.pixels[iy * frame.width + ix] = v.rgba
		}
	}
}

pub fn (f Frame) save_ppm(path string) {
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
	os.write_file(path, data.bytestr()) or {}
}

module svg

import image
import math

// RasterOptions controls rasterization.
pub struct RasterOptions {
pub:
	width         int   = 300
	height        int   = 150
	dpi           f64   = 96
	background    Color = Color{ a: 0 }
	samples       int   = 2
	text_renderer ?TextDrawFn
}

// TextDrawFn is a pluggable text renderer. It receives the target image,
// the text item, its position, font size and resolved color.
pub type TextDrawFn = fn (mut image.RGBA, TextItem, Vec2, f32, Color)

// rasterize flattens and renders a document into an RGBA image.
pub fn (doc Document) rasterize(opts RasterOptions) !image.RGBA {
	scene := flatten_document(&doc, FlattenOptions{ dpi: opts.dpi }) or { return err }
	return scene.rasterize(opts)
}

// rasterize renders a resolved scene into an RGBA image.
pub fn (scene Scene) rasterize(opts RasterOptions) !image.RGBA {
	w := math.max(1, opts.width)
	h := math.max(1, opts.height)
	samples := math.max(1, math.min(4, opts.samples))
	mut img := image.new_rgba(image.rect(0, 0, w, h))
	fill_image(mut img, opts.background)

	mut clip_masks := map[string][]f32{}
	for id, paths in scene.resources.clips {
		mut cov := new_coverage(w, h)
		for p in paths {
			add_path_coverage(p, Transform.identity(), .nonzero, w, h, samples, mut cov)
		}
		clip_masks[id] = cov
	}

	for item in scene.items {
		abs_path := item.path.transform(item.transform)
		mut coverage := new_coverage(w, h)
		add_path_coverage(abs_path, Transform.identity(), item.fill_rule, w, h, samples, mut coverage)
		apply_clips(mut coverage, item.clip_ids, clip_masks)

		if item.fill.kind != .none {
			composite_paint(mut img, scene, item, abs_path, coverage, w, h, item.opacity * item.fill_opacity, false)
		}
		if item.stroke.kind != .none && item.stroke_width > 0 {
			mut stroke_cov := new_coverage(w, h)
			add_stroke_coverage(abs_path, item, w, h, samples, mut stroke_cov)
			apply_clips(mut stroke_cov, item.clip_ids, clip_masks)
			composite_paint(mut img, scene, item, abs_path, stroke_cov, w, h, item.opacity * item.stroke_opacity, true)
		}
		if item.filter_id != '' {
			if fe := scene.resources.filters[item.filter_id] {
				f := parse_filter(&fe) or { Filter{} }
				if filter_is_supported(f) {
					apply_filter(mut img, f, abs_path.bounds())
				}
			}
		}
	}

	if scene.texts.len > 0 {
		renderer := opts.text_renderer or { default_text_renderer }
		for t in scene.texts {
			size := f32(t.style.length('font-size', 16, 300, 16))
			col := t.style.color('fill', rgb(0, 0, 0))
			renderer(mut img, t, t.pos, size, col)
		}
	}
	return img
}

fn apply_clips(mut coverage []f32, ids []string, masks map[string][]f32) {
	for id in ids {
		if mask := masks[id] {
			for i in 0 .. coverage.len {
				coverage[i] *= mask[i]
			}
		}
	}
}

fn fill_image(mut img image.RGBA, c Color) {
	for y in 0 .. img.rect.max.y {
		for x in 0 .. img.rect.max.x {
			set_pixel(mut img, x, y, c)
		}
	}
}

fn set_pixel(mut img image.RGBA, x int, y int, c Color) {
	if x < 0 || y < 0 || x >= img.rect.max.x || y >= img.rect.max.y {
		return
	}
	i := y * img.stride + x * 4
	img.pix[i] = c.r
	img.pix[i + 1] = c.g
	img.pix[i + 2] = c.b
	img.pix[i + 3] = c.a
}

fn blend_pixel(mut img image.RGBA, x int, y int, c Color, coverage f32) {
	if coverage <= 0 || x < 0 || y < 0 || x >= img.rect.max.x || y >= img.rect.max.y {
		return
	}
	i := y * img.stride + x * 4
	src_a := f64(c.a) / 255 * f64(math.min(1, coverage))
	if src_a <= 0 {
		return
	}
	dst_a := f64(img.pix[i + 3]) / 255
	out_a := src_a + dst_a * (1 - src_a)
	if out_a <= 0 {
		return
	}
	channels := [c.r, c.g, c.b]
	for k in 0 .. 3 {
		src := f64(channels[k]) / 255
		dst := f64(img.pix[i + k]) / 255
		img.pix[i + k] = u8(math.round(255 * (src * src_a + dst * dst_a * (1 - src_a)) / out_a))
	}
	img.pix[i + 3] = u8(math.round(255 * out_a))
}

fn new_coverage(w int, h int) []f32 {
	return []f32{len: w * h, init: 0}
}

struct Edge {
	x0  f32
	y0  f32
	x1  f32
	y1  f32
	dir int
}

struct Crossing {
	x   f64
	dir int
}

struct Span {
	x0 f64
	x1 f64
}

fn cross_compare(a &Crossing, b &Crossing) int {
	if a.x < b.x {
		return -1
	}
	if a.x > b.x {
		return 1
	}
	return 0
}

fn add_path_coverage(p Path, t Transform, rule FillRule, w int, h int, samples int, mut cov []f32) {
	for poly in p.flatten(0.25) {
		if poly.len < 3 {
			continue
		}
		mut edges := []Edge{}
		for i in 0 .. poly.len {
			a0 := t.apply(poly[i])
			a1 := t.apply(poly[(i + 1) % poly.len])
			if a0.y == a1.y {
				continue
			}
			edges << Edge{
				x0:  a0.x
				y0:  a0.y
				x1:  a1.x
				y1:  a1.y
				dir: if a1.y > a0.y { 1 } else { -1 }
			}
		}
		fill_polygon_edges(edges, rule, w, h, samples, mut cov)
	}
}

fn fill_polygon_edges(edges []Edge, rule FillRule, w int, h int, samples int, mut cov []f32) {
	for sy in 0 .. h * samples {
		y := (f32(sy) + 0.5) / f32(samples)
		mut crossings := []Crossing{}
		for e in edges {
			if (e.y0 <= y && y < e.y1) || (e.y1 <= y && y < e.y0) {
				x := f64(e.x0) + (f64(y) - f64(e.y0)) * f64(e.x1 - e.x0) / f64(e.y1 - e.y0)
				crossings << Crossing{ x: x, dir: e.dir }
			}
		}
		if crossings.len < 2 {
			continue
		}
		crossings.sort_with_compare(cross_compare)
		mut spans := []Span{}
		if rule == .even_odd {
			mut i := 0
			for i + 1 < crossings.len {
				spans << Span{ x0: crossings[i].x, x1: crossings[i + 1].x }
				i += 2
			}
		} else {
			mut winding := 0
			mut start := 0.0
			for i, c in crossings {
				before := winding
				winding += c.dir
				if before == 0 && winding != 0 {
					start = c.x
				}
				if before != 0 && winding == 0 {
					spans << Span{ x0: start, x1: c.x }
				}
				_ = i
			}
		}
		py := sy / samples
		for span in spans {
			accumulate_span(span.x0, span.x1, py, w, samples, mut cov)
		}
	}
}

fn accumulate_span(xa f64, xb f64, py int, w int, samples int, mut cov []f32) {
	if xb <= xa {
		return
	}
	lo := math.max(0, int(xa))
	hi := math.min(w - 1, int(xb))
	for px in lo .. hi + 1 {
		overlap := math.min(f64(px + 1), xb) - math.max(f64(px), xa)
		if overlap > 0 {
			cov[py * w + px] += f32(overlap / f64(samples))
		}
	}
}

fn add_stroke_coverage(p Path, item DrawItem, w int, h int, samples int, mut cov []f32) {
	half := f32(item.stroke_width / 2)
	for sub in p.flatten(0.25) {
		for i in 1 .. sub.len {
			a := item.transform.apply(sub[i - 1])
			b := item.transform.apply(sub[i])
			add_quad_coverage(a, b, half, w, h, samples, mut cov)
		}
	}
}

fn add_quad_coverage(a Vec2, b Vec2, half f32, w int, h int, samples int, mut cov []f32) {
	dx := b.x - a.x
	dy := b.y - a.y
	len := math.sqrtf(dx * dx + dy * dy)
	if len < 1e-6 {
		return
	}
	nx := -dy / len * half
	ny := dx / len * half
	quad := [
		vec2(a.x + nx, a.y + ny)
		vec2(b.x + nx, b.y + ny)
		vec2(b.x - nx, b.y - ny)
		vec2(a.x - nx, a.y - ny),
	]
	mut edges := []Edge{}
	for i in 0 .. 4 {
		p0 := quad[i]
		p1 := quad[(i + 1) % 4]
		if p0.y == p1.y {
			continue
		}
		edges << Edge{
			x0:  p0.x
			y0:  p0.y
			x1:  p1.x
			y1:  p1.y
			dir: if p1.y > p0.y { 1 } else { -1 }
		}
	}
	fill_polygon_edges(edges, .nonzero, w, h, samples, mut cov)
}

fn composite_paint(mut img image.RGBA, scene Scene, item DrawItem, path Path, coverage []f32, w int, h int, alpha f64, is_stroke bool) {
	paint := if is_stroke { item.stroke } else { item.fill }
	bounds := path.bounds()
	for y in 0 .. h {
		for x in 0 .. w {
			c := coverage[y * w + x]
			if c <= 0 {
				continue
			}
			col := paint_color(scene, paint, f64(x), f64(y), bounds)
			blend_pixel(mut img, x, y, col.multiplied_alpha(alpha), c)
		}
	}
}

fn paint_color(scene Scene, paint Paint, x f64, y f64, bounds Rect) Color {
	match paint.kind {
		.color {
			return paint.color
		}
		.gradient {
			if g := scene.resources.gradients[paint.ref_id] {
				return sample_gradient(g, x, y, bounds)
			}
			return rgb(0, 0, 0)
		}
		.pattern {
			if p := scene.resources.patterns[paint.ref_id] {
				return sample_pattern(p, x, y)
			}
			return Color{ a: 0 }
		}
		.none {
			return Color{ a: 0 }
		}
	}
}

fn sample_gradient(g Gradient, x f64, y f64, bounds Rect) Color {
	inv := g.transform.inverse() or { Transform.identity() }
	mut p := inv.apply_f64(x, y)
	mut t := 0.0
	if g.kind == .linear {
		dx := g.x2 - g.x1
		dy := g.y2 - g.y1
		len2 := dx * dx + dy * dy
		if g.units == .object_bbox {
			p = Vec2{
				x: f32((x - f64(bounds.x)) / f64(math.max(f32(1e-6), bounds.w)))
				y: f32((y - f64(bounds.y)) / f64(math.max(f32(1e-6), bounds.h)))
			}
		}
		if len2 > 1e-9 {
			t = ((f64(p.x) - g.x1) * dx + (f64(p.y) - g.y1) * dy) / len2
		}
	} else {
		mut cx := g.cx
		mut cy := g.cy
		mut rr := math.max(g.r, 1e-6)
		if g.units == .object_bbox {
			cx = f64(bounds.x) + g.cx * f64(bounds.w)
			cy = f64(bounds.y) + g.cy * f64(bounds.h)
			rr = g.r * f64(math.max(bounds.w, bounds.h))
		}
		dx := x - cx
		dy := y - cy
		t = math.sqrt(dx * dx + dy * dy) / rr
	}
	return gradient_color(g, t)
}

fn sample_pattern(p Pattern, x f64, y f64) Color {
	if p.w <= 0 || p.h <= 0 {
		return Color{ a: 0 }
	}
	tx := x - math.floor(x / p.w) * p.w
	ty := y - math.floor(y / p.h) * p.h
	for child in p.children {
		if child.is_shape() {
			st := computed_style(&child, [])
			c := st.color('fill', Color{ a: 0 })
			if c.a > 0 && tx >= p.x && tx <= p.x + p.w && ty >= p.y && ty <= p.y + p.h {
				return c
			}
		}
	}
	return Color{ a: 0 }
}

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
pub type TextDrawFn = fn (mut image.RGBA, TextItem, Vec2, f32, Color, Transform)

// rasterize flattens and renders a document into an RGBA image.
pub fn (doc Document) rasterize(opts RasterOptions) !image.RGBA {
	// The requested raster size is the viewport: the document viewBox (if any)
	// must be mapped into it, not into the SVG's intrinsic width/height.
	mut sized := doc
	sized.width = opts.width.str()
	sized.height = opts.height.str()
	scene := flatten_document(&sized, FlattenOptions{ dpi: opts.dpi }) or { return err }
	return scene.rasterize(opts)
}

// rasterize renders a resolved scene into an RGBA image.
pub fn (scene Scene) rasterize(opts RasterOptions) !image.RGBA {
	w := math.max(1, opts.width)
	h := math.max(1, opts.height)
	samples := math.max(1, math.min(4, opts.samples))
	mut img := image.new_rgba(image.rect(0, 0, w, h))
	fill_image(mut img, opts.background)

	mut scan := ScanBuffers{}
	mut poly_scratch := [][]Vec2{}
	mut seg_scratch := []AbsSegment{}
	mut fill_prev := CoverBounds{}
	mut stroke_prev := CoverBounds{}
	mut clip_masks := map[string][]f32{}
	for id, paths in scene.resources.clips {
		mut cov := new_coverage(w, h)
		for p in paths {
			polys := p.flatten(0.25)
			add_path_polys(polys, .nonzero, w, h, samples, mut cov, mut scan)
		}
		clip_masks[id] = cov
	}

	mut coverage := new_coverage(w, h)
	mut stroke_cov := new_coverage(w, h)
	for item in scene.items {
		// Flatten once per item into reusable buffers and transform the points
		// in place, instead of building a transformed Path and flattening twice.
		npolys := item.path.flatten_into(0.25, mut poly_scratch, mut seg_scratch)
		poly_scratch.trim(npolys)
		mut polys := poly_scratch
		for i in 0 .. polys.len {
			for j in 0 .. polys[i].len {
				polys[i][j] = item.transform.apply(polys[i][j])
			}
		}
		fill_prev = coverage_bounds(polys, w, h)
		if item.fill.kind != .none {
			clear_region(mut coverage, w, fill_prev)
			add_path_polys(polys, item.fill_rule, w, h, samples, mut coverage, mut scan)
			apply_clips(mut coverage, item.clip_ids, clip_masks)
			composite_paint(mut img, scene, item, polys, coverage, w, h, fill_prev, item.opacity * item.fill_opacity, false)
		}
		if item.stroke.kind != .none && item.stroke_width > 0 {
			clear_region(mut stroke_cov, w, stroke_prev)
			stroke_prev = expand_bounds(fill_prev, int(item.stroke_width / 2) + 2, w, h)
			add_stroke_polys(polys, f32(item.stroke_width / 2), item.dash_array, w, h, samples, mut stroke_cov, mut scan)
			apply_clips(mut stroke_cov, item.clip_ids, clip_masks)
			composite_paint(mut img, scene, item, polys, stroke_cov, w, h, stroke_prev, item.opacity * item.stroke_opacity, true)
		}
		if item.filter_id != '' {
			if fe := scene.resources.filters[item.filter_id] {
				f := parse_filter(&fe) or { Filter{} }
				if filter_is_supported(f) {
					apply_filter(mut img, f, bounds_of_polys(polys))
				}
			}
		}
	}

	if scene.texts.len > 0 {
		renderer := opts.text_renderer or { default_text_renderer }
		for t in scene.texts {
			user_size := f32(t.style.length('font-size', 16, 300, 16))
			scale := f32(math.sqrt(math.max(t.transform.a * t.transform.a + t.transform.b * t.transform.b, 1e-12)))
			size := user_size * scale
			col := t.style.color('fill', rgb(0, 0, 0))
			renderer(mut img, t, t.pos, size, col, t.transform)
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
	stride := img.stride
	mut i := 0
	for _ in 0 .. img.rect.max.y {
		for _ in 0 .. img.rect.max.x {
			img.pix[i] = c.r
			img.pix[i + 1] = c.g
			img.pix[i + 2] = c.b
			img.pix[i + 3] = c.a
			i += 4
		}
		i += stride - img.rect.max.x * 4
	}
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
	inv := 1 / out_a
	weight := dst_a * (1 - src_a)
	r := f64(c.r) * src_a + f64(img.pix[i]) * weight
	g := f64(c.g) * src_a + f64(img.pix[i + 1]) * weight
	b := f64(c.b) * src_a + f64(img.pix[i + 2]) * weight
	img.pix[i] = u8(math.min(255, r * inv + 0.5))
	img.pix[i + 1] = u8(math.min(255, g * inv + 0.5))
	img.pix[i + 2] = u8(math.min(255, b * inv + 0.5))
	img.pix[i + 3] = u8(math.min(255, 255 * out_a + 0.5))
}

fn new_coverage(w int, h int) []f32 {
	return []f32{len: w * h, init: 0}
}

struct CoverBounds {
mut:
	x0 int = -1
	y0 int = -1
	x1 int = -1
	y1 int = -1
}

fn coverage_bounds(polys [][]Vec2, w int, h int) CoverBounds {
	mut b := CoverBounds{}
	mut set := false
	for sub in polys {
		for p in sub {
			ix := int(p.x)
			iy := int(p.y)
			if !set {
				b = CoverBounds{ x0: ix, y0: iy, x1: ix, y1: iy }
				set = true
			} else {
				b.x0 = math.min(b.x0, ix)
				b.y0 = math.min(b.y0, iy)
				b.x1 = math.max(b.x1, ix)
				b.y1 = math.max(b.y1, iy)
			}
		}
	}
	if !set {
		return b
	}
	b.x0 = math.max(0, b.x0)
	b.y0 = math.max(0, b.y0)
	b.x1 = math.min(w - 1, b.x1)
	b.y1 = math.min(h - 1, b.y1)
	return b
}

fn expand_bounds(b CoverBounds, pad int, w int, h int) CoverBounds {
	if b.x0 < 0 {
		return b
	}
	return CoverBounds{
		x0: math.max(0, b.x0 - pad)
		y0: math.max(0, b.y0 - pad)
		x1: math.min(w - 1, b.x1 + pad)
		y1: math.min(h - 1, b.y1 + pad)
	}
}

fn clear_region(mut cov []f32, w int, b CoverBounds) {
	if b.x0 < 0 || b.y0 < 0 || b.x1 < b.x0 || b.y1 < b.y0 {
		return
	}
	for y in b.y0 .. b.y1 + 1 {
		mut i := y * w + b.x0
		for _ in b.x0 .. b.x1 + 1 {
			cov[i] = 0
			i++
		}
	}
}

fn bounds_of_polys(polys [][]Vec2) Rect {
	mut r := Rect{}
	mut set := false
	for sub in polys {
		for p in sub {
			if !set {
				r = Rect{ x: p.x, y: p.y, w: 0, h: 0 }
				set = true
			} else {
				x0 := math.min(r.x, p.x)
				y0 := math.min(r.y, p.y)
				x1 := math.max(r.max_x(), p.x)
				y1 := math.max(r.max_y(), p.y)
				r = Rect{ x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
			}
		}
	}
	return r
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

// ScanBuffers are reused across every polygon and subrow to avoid allocating
// scratch slices inside the rasterization hot loop.
struct ScanBuffers {
mut:
	edges     []Edge
	crossings []Crossing
	spans     []Span
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

fn add_path_polys(polys [][]Vec2, rule FillRule, w int, h int, samples int, mut cov []f32, mut scan ScanBuffers) {
	// Build one edge set for all subpaths: fill-rule (even-odd / nonzero) is
	// defined over the whole path, not per subpath.
	scan.edges.clear()
	for poly in polys {
		if poly.len < 3 {
			continue
		}
		for i in 0 .. poly.len {
			a0 := poly[i]
			a1 := poly[(i + 1) % poly.len]
			if a0.y == a1.y {
				continue
			}
			scan.edges << Edge{
				x0:  a0.x
				y0:  a0.y
				x1:  a1.x
				y1:  a1.y
				dir: if a1.y > a0.y { 1 } else { -1 }
			}
		}
	}
	fill_polygon_edges(scan.edges, rule, w, h, samples, mut cov, mut scan)
}

fn fill_polygon_edges(edges []Edge, rule FillRule, w int, h int, samples int, mut cov []f32, mut scan ScanBuffers) {
	if edges.len == 0 {
		return
	}
	// Only visit subrows the polygon can actually cover. Most SVG shapes are
	// much smaller than the viewport, so this removes the bulk of the work.
	mut ymin := math.min(edges[0].y0, edges[0].y1)
	mut ymax := math.max(edges[0].y0, edges[0].y1)
	for e in edges {
		ymin = math.min(ymin, math.min(e.y0, e.y1))
		ymax = math.max(ymax, math.max(e.y0, e.y1))
	}
	start_sy := math.max(0, int(ymin * f32(samples)))
	end_sy := math.min(h * samples - 1, int(ymax * f32(samples)))
	if end_sy < start_sy {
		return
	}
	step := 1.0 / f32(samples)
	for sy in start_sy .. end_sy + 1 {
		y := (f32(sy) + 0.5) * step
		scan.crossings.clear()
		for e in edges {
			if (e.y0 <= y && y < e.y1) || (e.y1 <= y && y < e.y0) {
				x := f64(e.x0) + (f64(y) - f64(e.y0)) * f64(e.x1 - e.x0) / f64(e.y1 - e.y0)
				scan.crossings << Crossing{ x: x, dir: e.dir }
			}
		}
		if scan.crossings.len < 2 {
			continue
		}
		scan.crossings.sort_with_compare(cross_compare)
		scan.spans.clear()
		if rule == .even_odd {
			mut i := 0
			for i + 1 < scan.crossings.len {
				scan.spans << Span{ x0: scan.crossings[i].x, x1: scan.crossings[i + 1].x }
				i += 2
			}
		} else {
			mut winding := 0
			mut start := 0.0
			for c in scan.crossings {
				before := winding
				winding += c.dir
				if before == 0 && winding != 0 {
					start = c.x
				}
				if before != 0 && winding == 0 {
					scan.spans << Span{ x0: start, x1: c.x }
				}
			}
		}
		py := sy / samples
		for span in scan.spans {
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

fn add_stroke_polys(polys [][]Vec2, half f32, dashes []f64, w int, h int, samples int, mut cov []f32, mut scan ScanBuffers) {
	mut pattern := []f64{}
	for d in dashes {
		if d > 0 {
			pattern << d
		}
	}
	if pattern.len % 2 == 1 {
		pattern << pattern[pattern.len - 1]
	}
	mut period := 0.0
	for d in pattern {
		period += d
	}
	for sub in polys {
		mut travelled := 0.0
		for i in 1 .. sub.len {
			a := sub[i - 1]
			b := sub[i]
			dx := f64(b.x - a.x)
			dy := f64(b.y - a.y)
			seg := math.sqrt(dx * dx + dy * dy)
			if seg < 1e-9 {
				continue
			}
			if pattern.len == 0 || period <= 0 {
				add_stroke_segment(a, b, half, w, h, samples, mut cov, mut scan)
				travelled += seg
				continue
			}
			// Dash phase continues across adjacent segments of the subpath.
			mut pos := math.fmod(travelled, period)
			mut done := 0.0
			for done < seg - 1e-9 {
				mut idx := 0
				mut acc := 0.0
				for idx < pattern.len {
					if pos < acc + pattern[idx] {
						break
					}
					acc += pattern[idx]
					idx++
				}
				if idx >= pattern.len {
					idx = 0
					acc = 0
					pos = 0
				}
				available := acc + pattern[idx] - pos
				if available < 1e-9 {
					pos = math.fmod(pos + 1e-6, period)
					continue
				}
				step := math.min(seg - done, available)
				if idx % 2 == 0 {
					t0 := done / seg
					t1 := (done + step) / seg
					pa := vec2(a.x + (b.x - a.x) * f32(t0), a.y + (b.y - a.y) * f32(t0))
					pb := vec2(a.x + (b.x - a.x) * f32(t1), a.y + (b.y - a.y) * f32(t1))
					add_stroke_segment(pa, pb, half, w, h, samples, mut cov, mut scan)
				}
				pos += step
				done += step
			}
			travelled += seg
		}
	}
}

fn add_stroke_segment(a Vec2, b Vec2, half f32, w int, h int, samples int, mut cov []f32, mut scan ScanBuffers) {
	dx := b.x - a.x
	dy := b.y - a.y
	len := math.sqrtf(dx * dx + dy * dy)
	if len < 1e-6 {
		return
	}
	nx := -dy / len * half
	ny := dx / len * half
	q0x := a.x + nx
	q0y := a.y + ny
	q1x := b.x + nx
	q1y := b.y + ny
	q2x := b.x - nx
	q2y := b.y - ny
	q3x := a.x - nx
	q3y := a.y - ny
	scan.edges.clear()
	if q0y != q1y {
		scan.edges << Edge{ x0: q0x, y0: q0y, x1: q1x, y1: q1y, dir: if q1y > q0y { 1 } else { -1 } }
	}
	if q1y != q2y {
		scan.edges << Edge{ x0: q1x, y0: q1y, x1: q2x, y1: q2y, dir: if q2y > q1y { 1 } else { -1 } }
	}
	if q2y != q3y {
		scan.edges << Edge{ x0: q2x, y0: q2y, x1: q3x, y1: q3y, dir: if q3y > q2y { 1 } else { -1 } }
	}
	if q3y != q0y {
		scan.edges << Edge{ x0: q3x, y0: q3y, x1: q0x, y1: q0y, dir: if q0y > q3y { 1 } else { -1 } }
	}
	fill_polygon_edges(scan.edges, .nonzero, w, h, samples, mut cov, mut scan)
}

fn composite_paint(mut img image.RGBA, scene Scene, item DrawItem, polys [][]Vec2, coverage []f32, w int, h int, region CoverBounds, alpha f64, is_stroke bool) {
	if region.x0 < 0 || region.x1 < region.x0 || region.y1 < region.y0 {
		return
	}
	_ = h
	paint := if is_stroke { item.stroke } else { item.fill }
	// Hoist all per-paint setup out of the pixel loop.
	mut solid := Color{ a: 0 }
	mut grad := Gradient{}
	mut grad_inv := Transform.identity()
	mut has_grad := false
	mut bounds := Rect{}
	if paint.kind == .color {
		solid = paint.color.multiplied_alpha(alpha)
	} else if paint.kind == .gradient {
		bounds = bounds_of_polys(polys)
		if g := scene.resources.gradients[paint.ref_id] {
			grad = g
			grad_inv = g.transform.inverse() or { Transform.identity() }
			has_grad = true
		}
	} else if paint.kind == .pattern {
		bounds = bounds_of_polys(polys)
	}
	stride := img.stride
	if paint.kind == .color {
		// Fast path: most covered pixels are fully covered and start from a
		// transparent destination, so they can be written without blending.
		for y in region.y0 .. region.y1 + 1 {
			mut cov_idx := y * w + region.x0
			mut idx := y * stride + region.x0 * 4
			for x in region.x0 .. region.x1 + 1 {
				c := coverage[cov_idx]
				cov_idx++
				if c <= 0 {
					idx += 4
					continue
				}
				if c >= 0.999 && img.pix[idx + 3] == 0 {
					img.pix[idx] = solid.r
					img.pix[idx + 1] = solid.g
					img.pix[idx + 2] = solid.b
					img.pix[idx + 3] = solid.a
					idx += 4
					continue
				}
				blend_pixel(mut img, x, y, solid, c)
				idx += 4
			}
		}
		return
	}
	for y in region.y0 .. region.y1 + 1 {
		mut cov_idx := y * w + region.x0
		for x in region.x0 .. region.x1 + 1 {
			c := coverage[cov_idx]
			cov_idx++
			if c <= 0 {
				continue
			}
			mut col := Color{ a: 0 }
			if paint.kind == .gradient {
				if has_grad {
					col = sample_gradient(grad, grad_inv, f64(x), f64(y), bounds).multiplied_alpha(alpha)
				} else {
					col = rgb(0, 0, 0).multiplied_alpha(alpha)
				}
			} else if paint.kind == .pattern {
				col = paint_color(scene, paint, f64(x), f64(y), bounds).multiplied_alpha(alpha)
			}
			blend_pixel(mut img, x, y, col, c)
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
				inv := g.transform.inverse() or { Transform.identity() }
				return sample_gradient(g, inv, x, y, bounds)
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

fn sample_gradient(g Gradient, inv Transform, x f64, y f64, bounds Rect) Color {
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
		mut rx := math.max(g.r, 1e-6)
		mut ry := rx
		mut cx := g.cx
		mut cy := g.cy
		if g.units == .object_bbox {
			// In objectBoundingBox space the radial gradient is a circle in
			// normalized coordinates, which is an ellipse in user space.
			cx = f64(bounds.x) + g.cx * f64(bounds.w)
			cy = f64(bounds.y) + g.cy * f64(bounds.h)
			rx = math.max(g.r * f64(bounds.w), 1e-6)
			ry = math.max(g.r * f64(bounds.h), 1e-6)
		}
		dx := (x - cx) / rx
		dy := (y - cy) / ry
		t = math.sqrt(dx * dx + dy * dy)
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

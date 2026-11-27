module svg

import math

const kappa = 0.552284749831

// FillRule selects the even-odd or nonzero winding fill rule.
pub enum FillRule {
	nonzero
	even_odd
}

// DrawItem is one flattened drawable with absolute geometry and resolved paint.
pub struct DrawItem {
pub:
	path            Path
	fill            Paint
	stroke          Paint
	stroke_width    f64    = 1
	stroke_linecap  string = 'butt'
	stroke_linejoin string = 'miter'
	miter_limit     f64    = 4
	dash_array      []f64
	opacity         f64      = 1
	fill_opacity    f64      = 1
	stroke_opacity  f64      = 1
	fill_rule       FillRule = .nonzero
	clip_ids        []string
	mask_id         string
	filter_id       string
	transform       Transform
}

// TextItem is a flattened text run (the rasterizer uses a pluggable renderer).
pub struct TextItem {
pub:
	text      string
	pos       Vec2
	style     Style
	transform Transform
	opacity   f64 = 1
}

// Resources holds defs referenced by drawable items.
pub struct Resources {
pub mut:
	gradients map[string]Gradient
	patterns  map[string]Pattern
	clips     map[string][]Path
	masks     map[string]Element
	filters   map[string]Element
}

// Scene is the fully resolved, flattened document ready for rasterization.
pub struct Scene {
pub mut:
	items     []DrawItem
	texts     []TextItem
	resources Resources
	viewport  Rect
}

// FlattenOptions controls flattening.
pub struct FlattenOptions {
pub:
	tolerance f64 = 0.1
	dpi       f64 = 96
	time      f64 = -1
}

// flatten_document resolves CSS, transforms, defs and shapes into a Scene.
pub fn flatten_document(doc &Document, opts FlattenOptions) !Scene {
	mut scene := Scene{ viewport: document_viewport(doc, opts) }
	mut resources := Resources{}
	index_defs(&doc.root, doc.css, mut resources)
	scene.resources = resources
	root_transform := if doc.has_view_box {
		doc.view_box.matrix_for(scene.viewport, doc.preserve)
	} else {
		Transform.identity()
	}
	mut ctx := WalkContext{
		doc:       doc
		opts:      opts
		viewport:  scene.viewport
		transform: root_transform
	}
	walk(&doc.root, &ctx, mut scene, 0)
	return scene
}

struct WalkContext {
	doc       &Document
	opts      FlattenOptions
	viewport  Rect
	transform Transform = Transform.identity()
	opacity   f64       = 1
	// clip-path, mask and filter on a group apply to all its descendants.
	clip_ids  []string
	mask_id   string
	filter_id string
}

fn document_viewport(doc &Document, opts FlattenOptions) Rect {
	w := length_px(doc.width, opts.dpi, 300)
	h := length_px(doc.height, opts.dpi, 150)
	return rect(0, 0, f32(w), f32(h))
}

fn length_px(s string, dpi f64, fallback f64) f64 {
	if s == '' {
		return fallback
	}
	l := parse_length(s) or { return fallback }
	return l.to_px(dpi, 16, fallback)
}

fn index_defs(e &Element, rules []CssRule, mut res Resources) {
	for child in e.children {
		match child.tag {
			'linearGradient', 'radialGradient' {
				g := gradient_from_element(&child) or { Gradient{} }
				if g.id != '' {
					res.gradients[g.id] = g
				}
			}
			'pattern' {
				p := pattern_from_element(&child) or { Pattern{} }
				if p.id != '' {
					res.patterns[p.id] = p
				}
			}
			'clipPath' {
				res.clips[child.id()] = clip_paths(&child, rules)
			}
			'mask' {
				res.masks[child.id()] = child
			}
			'filter' {
				res.filters[child.id()] = child
			}
			else {}
		}
		index_defs(&child, rules, mut res)
	}
}

fn clip_paths(e &Element, rules []CssRule) []Path {
	mut out := []Path{}
	for child in e.children {
		if child.is_shape() {
			p := shape_path(&child, rect(0, 0, 1000, 1000), rules) or { Path{} }
			if p.cmds.len > 0 {
				out << p
			}
		}
	}
	return out
}

// computed_style merges presentation attributes with CSS rules and inline style.
pub fn computed_style(e &Element, rules []CssRule) Style {
	mut s := Style{}
	for a in e.attrs {
		if a.name in ['fill', 'stroke', 'stroke-width', 'opacity', 'fill-opacity', 'stroke-opacity',
			'fill-rule', 'stroke-linecap', 'stroke-linejoin', 'stroke-dasharray', 'font-family',
			'font-size', 'font-weight', 'text-anchor', 'filter', 'clip-path', 'mask', 'display',
			'visibility', 'stop-color', 'stop-opacity'] {
			s.set(a.name, a.value)
		}
	}
	cascade_style_into(e, rules, mut s)
	return s
}

fn style_transform(e &Element, st Style) !Transform {
	mut v := st.get('transform')
	if v == '' {
		v = e.get_attr('transform')
	}
	if v == '' {
		return Transform.identity()
	}
	return parse_transform(v)
}

fn walk(e &Element, ctx &WalkContext, mut scene Scene, depth int) {
	if depth > 32 {
		return
	}
	st := computed_style(e, ctx.doc.css)
	if st.get('display') == 'none' || st.get('visibility') == 'hidden' {
		return
	}
	t := ctx.transform.then(style_transform(e, st) or { Transform.identity() })
	op := ctx.opacity * st.number('opacity', 1)
	mut child_clips := ctx.clip_ids.clone()
	if st.get('clip-path') != '' {
		for id in parse_refs(st.get('clip-path')) {
			child_clips << id
		}
	}
	mut child_mask := ctx.mask_id
	if st.get('mask') != '' {
		child_mask = parse_ref(st.get('mask'))
	}
	mut child_filter := ctx.filter_id
	if st.get('filter') != '' {
		child_filter = parse_ref(st.get('filter'))
	}

	if e.is_shape() {
		p := shape_path(e, ctx.viewport, ctx.doc.css) or { Path{} }
		if p.cmds.len > 0 {
			scene.items << draw_item(p, st, t, op, child_clips, child_mask, child_filter)
		}
		return
	}
	if e.tag == 'text' || e.tag == 'tspan' {
		mut pos := Vec2{}
		pos.x = f32(length_px(e.get_attr('x'), ctx.opts.dpi, 0))
		pos.y = f32(length_px(e.get_attr('y'), ctx.opts.dpi, 0))
		scene.texts << TextItem{
			text:      e.text_content()
			pos:       t.apply(pos)
			style:     st
			transform: t
			opacity:   op
		}
		for child in e.children {
			if child.tag == 'tspan' {
				walk(&child, &WalkContext{
					doc:       ctx.doc
					opts:      ctx.opts
					viewport:  ctx.viewport
					transform: t
					opacity:   op
					clip_ids:  child_clips
					mask_id:   child_mask
					filter_id: child_filter
				}, mut scene, depth + 1)
			}
		}
		return
	}
	if e.tag == 'use' {
		mut href := e.get_attr('href')
		if href == '' {
			href = e.get_attr('xlink:href')
		}
		if href.starts_with('#') {
			if target := ctx.doc.root.find(href[1..]) {
				dx := f32(length_px(e.get_attr('x'), ctx.opts.dpi, 0))
				dy := f32(length_px(e.get_attr('y'), ctx.opts.dpi, 0))
				use_t := t.then(Transform.translate(dx, dy))
				walk(target, &WalkContext{
					doc:       ctx.doc
					opts:      ctx.opts
					viewport:  ctx.viewport
					transform: use_t
					opacity:   op
					clip_ids:  child_clips
					mask_id:   child_mask
					filter_id: child_filter
				}, mut scene, depth + 1)
			}
		}
		return
	}
	if e.tag == 'defs' || e.tag == 'clipPath' || e.tag == 'mask' || e.tag == 'filter'
		|| e.tag == 'linearGradient' || e.tag == 'radialGradient' || e.tag == 'pattern' || e.tag == 'symbol' || e.tag == 'marker' {
		return
	}
	for i in 0 .. e.children.len {
		if e.children[i].tag == 'style' || e.children[i].tag == 'title' || e.children[i].tag == 'desc' {
			continue
		}
		walk(&e.children[i], &WalkContext{
			doc:       ctx.doc
			opts:      ctx.opts
			viewport:  ctx.viewport
			transform: t
			opacity:   op
			clip_ids:  child_clips
			mask_id:   child_mask
			filter_id: child_filter
		}, mut scene, depth + 1)
	}
}

fn draw_item(p Path, st Style, t Transform, op f64, clip_ids []string, mask_id string, filter_id string) DrawItem {
	rule := if st.get('fill-rule') == 'evenodd' { FillRule.even_odd } else { FillRule.nonzero }
	mut dashes := []f64{}
	dash := st.get('stroke-dasharray')
	if dash != '' {
		for part in dash.replace(',', ' ').split(' ') {
			v := parse_number(part.trim_space()) or { continue }
			dashes << v
		}
	}
	return DrawItem{
		path:            p
		fill:            paint_from(st.get('fill'))
		stroke:          paint_from(st.get('stroke'))
		stroke_width:    st.number('stroke-width', 1)
		stroke_linecap:  st.get('stroke-linecap')
		stroke_linejoin: st.get('stroke-linejoin')
		miter_limit:     st.number('stroke-miterlimit', 4)
		dash_array:      dashes
		opacity:         op
		fill_opacity:    st.number('fill-opacity', 1)
		stroke_opacity:  st.number('stroke-opacity', 1)
		fill_rule:       rule
		clip_ids:        clip_ids
		mask_id:         mask_id
		filter_id:       filter_id
		transform:       t
	}
}

fn parse_ref(v string) string {
	s := v.trim_space()
	if s.starts_with('url(') && s.ends_with(')') {
		inner := s[4..s.len - 1].trim_space()
		return if inner.starts_with('#') { inner[1..] } else { inner }
	}
	return ''
}

fn parse_refs(v string) []string {
	ref_id := parse_ref(v)
	return if ref_id == '' { []string{} } else { [ref_id] }
}

// shape_path converts an SVG shape element into a Path.
pub fn shape_path(e &Element, viewport Rect, rules []CssRule) !Path {
	st := computed_style(e, rules)
	mut p := Path{}
	match e.tag {
		'path' {
			return parse_path(e.get_attr('d'))
		}
		'line' {
			x1 := attr_num(e, 'x1', 0)
			y1 := attr_num(e, 'y1', 0)
			x2 := attr_num(e, 'x2', 0)
			y2 := attr_num(e, 'y2', 0)
			p.cmds << move_cmd(x1, y1)
			p.cmds << line_cmd(x2, y2)
		}
		'polyline', 'polygon' {
			pts := parse_points(e.get_attr('points'))
			if pts.len == 0 {
				return p
			}
			p.cmds << move_cmd(f64(pts[0].x), f64(pts[0].y))
			for pt in pts[1..] {
				p.cmds << line_cmd(f64(pt.x), f64(pt.y))
			}
			if e.tag == 'polygon' {
				p.cmds << PathCmd{ kind: .close }
			}
		}
		'rect' {
			x := attr_num(e, 'x', 0)
			y := attr_num(e, 'y', 0)
			w := attr_num(e, 'width', 0)
			h := attr_num(e, 'height', 0)
			if w <= 0 || h <= 0 {
				return p
			}
			mut rx := attr_num(e, 'rx', attr_num(e, 'ry', 0))
			mut ry := attr_num(e, 'ry', rx)
			if rx <= 0 || ry <= 0 {
				p.cmds << move_cmd(x, y)
				p.cmds << line_cmd(x + w, y)
				p.cmds << line_cmd(x + w, y + h)
				p.cmds << line_cmd(x, y + h)
				p.cmds << PathCmd{ kind: .close }
			} else {
				rx = math.min(rx, w / 2)
				ry = math.min(ry, h / 2)
				p.cmds << move_cmd(x + rx, y)
				p.cmds << line_cmd(x + w - rx, y)
				p.cmds << cubic_cmd(x + w - rx + rx * kappa, y, x + w, y + ry - ry * kappa, x + w, y + ry)
				p.cmds << line_cmd(x + w, y + h - ry)
				p.cmds << cubic_cmd(x + w, y + h - ry + ry * kappa, x + w - rx + rx * kappa, y + h, x + w - rx, y + h)
				p.cmds << line_cmd(x + rx, y + h)
				p.cmds << cubic_cmd(x + rx - rx * kappa, y + h, x, y + h - ry + ry * kappa, x, y + h - ry)
				p.cmds << line_cmd(x, y + ry)
				p.cmds << cubic_cmd(x, y + ry - ry * kappa, x + rx - rx * kappa, y, x + rx, y)
				p.cmds << PathCmd{ kind: .close }
			}
		}
		'circle', 'ellipse' {
			cx := attr_num(e, 'cx', 0)
			cy := attr_num(e, 'cy', 0)
			rx := attr_num(e, 'r', attr_num(e, 'rx', 0))
			ry := attr_num(e, 'ry', rx)
			if rx <= 0 || ry <= 0 {
				return p
			}
			k := 0.552284749831 * 1.3333333
			p.cmds << move_cmd(cx + rx, cy)
			p.cmds << arc_cmd(rx, ry, 0, cx - rx, cy)
			p.cmds << arc_cmd(rx, ry, 0, cx + rx, cy)
			p.cmds << PathCmd{ kind: .close }
			_ = k
		}
		else {
			return error('not a shape: ${e.tag}')
		}
	}
	_ = st
	_ = viewport
	return p
}

fn attr_num(e &Element, name string, fallback f64) f64 {
	v := e.get_attr(name)
	if v == '' {
		return fallback
	}
	l := parse_length(v) or { return parse_number(v) or { return fallback } }
	return l.to_px(96, 16, 100)
}

fn move_cmd(x f64, y f64) PathCmd {
	return PathCmd{ kind: .move_to, vals: [x, y, 0, 0, 0, 0, 0] }
}

fn line_cmd(x f64, y f64) PathCmd {
	return PathCmd{ kind: .line_to, vals: [x, y, 0, 0, 0, 0, 0] }
}

fn cubic_cmd(x1 f64, y1 f64, x2 f64, y2 f64, x f64, y f64) PathCmd {
	return PathCmd{ kind: .c_curve, vals: [x1, y1, x2, y2, x, y, 0] }
}

fn arc_cmd(rx f64, ry f64, rot f64, x f64, y f64) PathCmd {
	return PathCmd{ kind: .a_arc, vals: [rx, ry, rot, 1, 1, x, y] }
}

pub fn parse_points(s string) []Vec2 {
	mut out := []Vec2{}
	mut i := 0
	for {
		i = skip_ws(s, i)
		if i >= s.len {
			break
		}
		x := scan_number(s, i) or { break }
		i = skip_ws(s, i + x.consumed)
		if i < s.len && s[i] == `,` {
			i = skip_ws(s, i + 1)
		}
		y := scan_number(s, i) or { break }
		i += y.consumed
		out << vec2(f32(x.value), f32(y.value))
	}
	return out
}

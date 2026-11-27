module svg

import os

// Document is a parsed or generated SVG document.
@[heap]
pub struct Document {
pub mut:
	width        string = '100%'
	height       string = '100%'
	view_box     ViewBox
	has_view_box bool
	preserve     AspectRatio
	root         Element = elem('svg')
	css          []CssRule
	namespace    string = 'http://www.w3.org/2000/svg'
	xmlns_xlink  string = 'http://www.w3.org/1999/xlink'
}

// new creates an empty document with the given pixel dimensions.
pub fn new(width int, height int) &Document {
	mut d := &Document{
		width:  width.str()
		height: height.str()
	}
	d.sync_root()
	return d
}

// sync_root copies document-level attributes onto the root element.
pub fn (mut d Document) sync_root() {
	d.root.set_attr('xmlns', d.namespace)
	if d.xmlns_xlink != '' {
		d.root.set_attr('xmlns:xlink', d.xmlns_xlink)
	}
	d.root.set_attr('width', d.width)
	d.root.set_attr('height', d.height)
	if d.has_view_box {
		d.root.set_attr('viewBox', d.view_box.str())
	} else {
		d.root.remove_attr('viewBox')
	}
	if d.preserve.align != .xmid_ymid || d.preserve.slice {
		ar := if d.preserve.slice { ' slice' } else { '' }
		d.root.set_attr('preserveAspectRatio', '${align_str(d.preserve.align)}${ar}')
	}
}

fn align_str(a Align) string {
	return match a {
		.xmin_ymin { 'xMinYMin' }
		.xmin_ymid { 'xMinYMid' }
		.xmin_ymax { 'xMinYMax' }
		.xmid_ymin { 'xMidYMin' }
		.xmid_ymid { 'xMidYMid' }
		.xmid_ymax { 'xMidYMax' }
		.xmax_ymin { 'xMaxYMin' }
		.xmax_ymid { 'xMaxYMid' }
		.xmax_ymax { 'xMaxYMax' }
		.none { 'none' }
	}
}

// add appends a child element to the root and returns the document for chaining.
pub fn (mut d Document) add(e Element) &Document {
	d.root.children << e
	return d
}

// group adds a group and returns it for further chaining.
pub fn (mut d Document) group() &Element {
	mut g := elem('g')
	d.root.children << g
	return &d.root.children[d.root.children.len - 1]
}

// circle adds a circle and returns the document.
pub fn (mut d Document) circle(cx f64, cy f64, r f64, opts ...StyleProp) &Document {
	mut e := elem('circle')
	e.set_attr('cx', fmt_num(cx))
	e.set_attr('cy', fmt_num(cy))
	e.set_attr('r', fmt_num(r))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// ellipse adds an ellipse and returns the document.
pub fn (mut d Document) ellipse(cx f64, cy f64, rx f64, ry f64, opts ...StyleProp) &Document {
	mut e := elem('ellipse')
	e.set_attr('cx', fmt_num(cx))
	e.set_attr('cy', fmt_num(cy))
	e.set_attr('rx', fmt_num(rx))
	e.set_attr('ry', fmt_num(ry))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// rect adds a rectangle and returns the document.
pub fn (mut d Document) rect(x f64, y f64, w f64, h f64, opts ...StyleProp) &Document {
	mut e := elem('rect')
	e.set_attr('x', fmt_num(x))
	e.set_attr('y', fmt_num(y))
	e.set_attr('width', fmt_num(w))
	e.set_attr('height', fmt_num(h))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// line adds a line and returns the document.
pub fn (mut d Document) line(x1 f64, y1 f64, x2 f64, y2 f64, opts ...StyleProp) &Document {
	mut e := elem('line')
	e.set_attr('x1', fmt_num(x1))
	e.set_attr('y1', fmt_num(y1))
	e.set_attr('x2', fmt_num(x2))
	e.set_attr('y2', fmt_num(y2))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// polyline adds a polyline and returns the document.
pub fn (mut d Document) polyline(points []Vec2, opts ...StyleProp) &Document {
	mut e := elem('polyline')
	e.set_attr('points', points_str(points))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// polygon adds a polygon and returns the document.
pub fn (mut d Document) polygon(points []Vec2, opts ...StyleProp) &Document {
	mut e := elem('polygon')
	e.set_attr('points', points_str(points))
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

fn points_str(points []Vec2) string {
	mut s := ''
	for i, p in points {
		if i > 0 {
			s += ' '
		}
		s += p.str()
	}
	return s
}

// path adds a path from raw path data and returns the document.
pub fn (mut d Document) path(data string, opts ...StyleProp) &Document {
	mut e := elem('path')
	e.set_attr('d', data)
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// text adds a text element and returns the document.
pub fn (mut d Document) text(x f64, y f64, content string, opts ...StyleProp) &Document {
	mut e := elem('text')
	e.set_attr('x', fmt_num(x))
	e.set_attr('y', fmt_num(y))
	e.text = content
	apply_props(mut e, opts)
	d.root.children << e
	return d
}

// defs adds an element into <defs>, creating that section if needed.
pub fn (mut d Document) defs(e Element) &Document {
	mut idx := -1
	for i, child in d.root.children {
		if child.tag == 'defs' {
			idx = i
			break
		}
	}
	if idx < 0 {
		d.root.children << elem('defs')
		idx = d.root.children.len - 1
	}
	d.root.children[idx].children << e
	return d
}

// set_style adds or replaces a CSS rule.
pub fn (mut d Document) set_style(selector string, name string, value string) &Document {
	for i, r in d.css {
		if r.selector == selector {
			d.css[i].style.set(name, value)
			return d
		}
	}
	mut st := Style{}
	st.set(name, value)
	d.css << CssRule{ selector: selector, style: st }
	return d
}

// find looks up an element by id.
pub fn (d Document) find(id string) ?&Element {
	return d.root.find(id)
}

// to_svg serializes the document.
pub fn (d Document) to_svg(opts WriteOptions) string {
	return write_document(d, opts)
}

// save writes the document to a file.
pub fn (d Document) save(path string, opts WriteOptions) ! {
	os.write_file(path, d.to_svg(opts)) or { return err }
}

// StyleProp is a named style value used by the fluent builder.
pub struct StyleProp {
pub:
	name  string
	value string
}

// Convenience constructors for fluent style properties.
pub fn fill(value string) StyleProp {
	return StyleProp{ name: 'fill', value: value }
}

pub fn stroke(value string) StyleProp {
	return StyleProp{ name: 'stroke', value: value }
}

pub fn stroke_width(value f64) StyleProp {
	return StyleProp{ name: 'stroke-width', value: fmt_num(value) }
}

pub fn opacity(value f64) StyleProp {
	return StyleProp{ name: 'opacity', value: fmt_num(value) }
}

pub fn font_size(value f64) StyleProp {
	return StyleProp{ name: 'font-size', value: fmt_num(value) }
}

pub fn class(value string) StyleProp {
	return StyleProp{ name: 'class', value: value }
}

pub fn id(value string) StyleProp {
	return StyleProp{ name: 'id', value: value }
}

pub fn transform(value string) StyleProp {
	return StyleProp{ name: 'transform', value: value }
}

fn apply_props(mut e Element, props []StyleProp) {
	for p in props {
		match p.name {
			'id', 'class', 'transform' {
				e.set_attr(p.name, p.value)
			}
			else {
				e.style.set(p.name, p.value)
			}
		}
	}
}

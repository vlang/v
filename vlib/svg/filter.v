module svg

import image
import math

// FilterKind identifies an SVG filter primitive.
pub enum FilterKind {
	gaussian_blur
	color_matrix
	component_transfer
	composite
	blend
	offset
	flood
	merge
	morphology
	turbulence
	displacement_map
	convolve_matrix
	drop_shadow
	lighting
	unsupported
}

// FilterPrimitive is one primitive inside a filter graph.
pub struct FilterPrimitive {
pub:
	kind   FilterKind = .unsupported
	result string
	attrs  map[string]string
}

// Filter is a parsed <filter> element.
pub struct Filter {
pub mut:
	id         string
	x          f64 = -0.1
	y          f64 = -0.1
	w          f64 = 1.2
	h          f64 = 1.2
	primitives []FilterPrimitive
}

// parse_filter builds a Filter from a <filter> element.
pub fn parse_filter(e &Element) !Filter {
	mut f := Filter{ id: e.id() }
	f.x = attr_f(e, 'x', -0.1)
	f.y = attr_f(e, 'y', -0.1)
	f.w = attr_f(e, 'width', 1.2)
	f.h = attr_f(e, 'height', 1.2)
	for child in e.children {
		mut attrs := map[string]string{}
		for a in child.attrs {
			attrs[a.name] = a.value
		}
		f.primitives << FilterPrimitive{
			kind:   primitive_kind(child.tag)
			result: child.get_attr('result')
			attrs:  attrs
		}
	}
	return f
}

fn primitive_kind(tag string) FilterKind {
	return match tag {
		'feGaussianBlur' { FilterKind.gaussian_blur }
		'feColorMatrix' { FilterKind.color_matrix }
		'feComponentTransfer' { FilterKind.component_transfer }
		'feComposite' { FilterKind.composite }
		'feBlend' { FilterKind.blend }
		'feOffset' { FilterKind.offset }
		'feFlood' { FilterKind.flood }
		'feMerge' { FilterKind.merge }
		'feMorphology' { FilterKind.morphology }
		'feTurbulence' { FilterKind.turbulence }
		'feDisplacementMap' { FilterKind.displacement_map }
		'feConvolveMatrix' { FilterKind.convolve_matrix }
		'feDropShadow' { FilterKind.drop_shadow }
		'feDiffuseLighting', 'feSpecularLighting' { FilterKind.lighting }
		else { FilterKind.unsupported }
	}
}

// supported_filters lists primitives the rasterizer can execute.
pub fn supported_filters() []FilterKind {
	return [
		FilterKind.gaussian_blur
		FilterKind.color_matrix
		FilterKind.offset
		FilterKind.flood
		FilterKind.blend
		FilterKind.composite
		FilterKind.drop_shadow,
	]
}

// filter_is_supported reports whether every primitive can be rasterized.
pub fn filter_is_supported(f Filter) bool {
	supported := supported_filters()
	for p in f.primitives {
		mut ok := false
		for k in supported {
			if p.kind == k {
				ok = true
				break
			}
		}
		if !ok {
			return false
		}
	}
	return true
}

// apply_filter executes the supported primitives of a filter on an image.
pub fn apply_filter(mut img image.RGBA, f Filter, bounds Rect) {
	for p in f.primitives {
		match p.kind {
			.gaussian_blur {
				std := parse_number(p.attrs['stdDeviation']) or { 1 }
				blur_image(mut img, bounds, int(math.max(1, std)))
			}
			.color_matrix {
				mode := p.attrs['type']
				saturate := if mode == 'saturate' {
					parse_number(p.attrs['values']) or { 1 }
				} else {
					1
				}
				if mode == 'saturate' {
					saturate_image(mut img, bounds, saturate)
				}
			}
			.offset {
				dx := int(parse_number(p.attrs['dx']) or { 0 })
				dy := int(parse_number(p.attrs['dy']) or { 0 })
				offset_image(mut img, bounds, dx, dy)
			}
			.flood {
				c := parse_color(p.attrs['flood-color']) or { rgb(0, 0, 0) }
				fill_region(mut img, bounds, c)
			}
			.blend {
				mode := p.attrs['mode']
				if mode == 'multiply' {
					multiply_region(mut img, bounds)
				}
			}
			.drop_shadow {
				dx := int(parse_number(p.attrs['dx']) or { 2 })
				dy := int(parse_number(p.attrs['dy']) or { 2 })
				std := parse_number(p.attrs['stdDeviation']) or { 1 }
				offset_image(mut img, bounds, dx, dy)
				blur_image(mut img, bounds, int(math.max(1, std)))
			}
			else {}
		}
	}
}

fn blur_image(mut img image.RGBA, r Rect, radius int) {
	_ = r
	if radius <= 0 {
		return
	}
	w := img.rect.max.x
	h := img.rect.max.y
	mut horiz := img.pix.clone()
	for y in 0 .. h {
		for x in 0 .. w {
			lo := math.max(0, x - radius)
			hi := math.min(w - 1, x + radius)
			mut sr := 0
			mut sg := 0
			mut sb := 0
			mut sa := 0
			for k in lo .. hi + 1 {
				idx := y * img.stride + k * 4
				sr += int(img.pix[idx])
				sg += int(img.pix[idx + 1])
				sb += int(img.pix[idx + 2])
				sa += int(img.pix[idx + 3])
			}
			n := hi - lo + 1
			idx := y * img.stride + x * 4
			horiz[idx] = u8(sr / n)
			horiz[idx + 1] = u8(sg / n)
			horiz[idx + 2] = u8(sb / n)
			horiz[idx + 3] = u8(sa / n)
		}
	}
	for y in 0 .. h {
		for x in 0 .. w {
			lo := math.max(0, y - radius)
			hi := math.min(h - 1, y + radius)
			mut sr := 0
			mut sg := 0
			mut sb := 0
			mut sa := 0
			for k in lo .. hi + 1 {
				idx := k * img.stride + x * 4
				sr += int(horiz[idx])
				sg += int(horiz[idx + 1])
				sb += int(horiz[idx + 2])
				sa += int(horiz[idx + 3])
			}
			n := hi - lo + 1
			idx := y * img.stride + x * 4
			img.pix[idx] = u8(sr / n)
			img.pix[idx + 1] = u8(sg / n)
			img.pix[idx + 2] = u8(sb / n)
			img.pix[idx + 3] = u8(sa / n)
		}
	}
}

fn saturate_image(mut img image.RGBA, r Rect, amount f64) {
	_ = r
	for y in 0 .. img.rect.max.y {
		for x in 0 .. img.rect.max.x {
			i := y * img.stride + x * 4
			rr := f64(img.pix[i])
			gg := f64(img.pix[i + 1])
			bb := f64(img.pix[i + 2])
			lum := 0.2126 * rr + 0.7152 * gg + 0.0722 * bb
			img.pix[i] = u8(math.round(lum + (rr - lum) * amount))
			img.pix[i + 1] = u8(math.round(lum + (gg - lum) * amount))
			img.pix[i + 2] = u8(math.round(lum + (bb - lum) * amount))
		}
	}
}

fn offset_image(mut img image.RGBA, r Rect, dx int, dy int) {
	_ = r
	w := img.rect.max.x
	h := img.rect.max.y
	mut snapshot := img.pix.clone()
	for y in 0 .. h {
		for x in 0 .. w {
			sx := x - dx
			sy := y - dy
			if sx < 0 || sy < 0 || sx >= w || sy >= h {
				continue
			}
			src := sy * img.stride + sx * 4
			dst := y * img.stride + x * 4
			for c in 0 .. 4 {
				img.pix[dst + c] = snapshot[src + c]
			}
		}
	}
}

fn fill_region(mut img image.RGBA, r Rect, c Color) {
	_ = r
	for y in 0 .. img.rect.max.y {
		for x in 0 .. img.rect.max.x {
			i := y * img.stride + x * 4
			img.pix[i] = c.r
			img.pix[i + 1] = c.g
			img.pix[i + 2] = c.b
			if c.a > 0 {
				img.pix[i + 3] = c.a
			}
		}
	}
}

fn multiply_region(mut img image.RGBA, r Rect) {
	_ = r
	for i in 0 .. img.pix.len / 4 {
		img.pix[i * 4] = u8(int(img.pix[i * 4]) * int(img.pix[i * 4 + 1]) / 255)
	}
}

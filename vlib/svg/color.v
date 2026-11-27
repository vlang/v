module svg

import math
import strconv
import strings

// ColorKind distinguishes paint colors from the special CSS color keywords.
pub enum ColorKind {
	rgb
	current_color
	none
}

// Color is an 8-bit per channel RGBA color plus a kind flag.
pub struct Color {
pub:
	kind ColorKind = .rgb
	r    u8
	g    u8
	b    u8
	a    u8 = 255
}

// rgb creates an opaque color from 0..255 components.
pub fn rgb(r u8, g u8, b u8) Color {
	return Color{ r: r, g: g, b: b, a: 255 }
}

// rgba creates a color from 0..255 components and 0..255 alpha.
pub fn rgba(r u8, g u8, b u8, a u8) Color {
	return Color{ r: r, g: g, b: b, a: a }
}

// Color.from_hex creates a color from 0xRRGGBB or 0xRRGGBBAA.
pub fn Color.from_hex(v u32, has_alpha bool) Color {
	if has_alpha {
		return Color{
			r: u8((v >> 24) & 0xff)
			g: u8((v >> 16) & 0xff)
			b: u8((v >> 8) & 0xff)
			a: u8(v & 0xff)
		}
	}
	return Color{
		r: u8((v >> 16) & 0xff)
		g: u8((v >> 8) & 0xff)
		b: u8(v & 0xff)
		a: 255
	}
}

// hsl creates a color from hue (degrees), saturation and lightness (0..1).
pub fn hsl(h f64, s f64, l f64, a u8) Color {
	hh := math.fmod(math.fmod(h, 360) + 360, 360) / 60
	c := (1 - math.abs(2 * l - 1)) * math.min(s, 1)
	x := c * (1 - math.abs(math.fmod(hh, 2) - 1))
	m := l - c / 2
	mut rr := 0.0
	mut gg := 0.0
	mut bb := 0.0
	match int(hh) {
		0 {
			rr, gg = c, x
		}
		1 {
			rr, gg = x, c
		}
		2 {
			gg, bb = c, x
		}
		3 {
			gg, bb = x, c
		}
		4 {
			rr, bb = x, c
		}
		else {
			rr, bb = c, x
		}
	}
	return Color{
		r: u8(math.round((rr + m) * 255))
		g: u8(math.round((gg + m) * 255))
		b: u8(math.round((bb + m) * 255))
		a: a
	}
}

// parse_color parses an SVG/CSS color: #rgb, #rrggbb, rgb(), rgba(),
// hsl(), hsla(), named colors, transparent, currentColor and none.
pub fn parse_color(input string) !Color {
	s := input.trim_space().to_lower()
	if s == '' {
		return error('empty color')
	}
	if s == 'none' {
		return Color{ kind: .none }
	}
	if s == 'currentcolor' {
		return Color{ kind: .current_color }
	}
	if s == 'transparent' {
		return Color{ a: 0 }
	}
	if s[0] == `#` {
		return parse_hex_color(s[1..])
	}
	if s.starts_with('rgb(') && s.ends_with(')') {
		return parse_rgb_color(s[4..s.len - 1], false)
	}
	if s.starts_with('rgba(') && s.ends_with(')') {
		return parse_rgb_color(s[5..s.len - 1], true)
	}
	if s.starts_with('hsl(') && s.ends_with(')') {
		return parse_hsl_color(s[4..s.len - 1], false)
	}
	if s.starts_with('hsla(') && s.ends_with(')') {
		return parse_hsl_color(s[5..s.len - 1], true)
	}
	if s in named_colors {
		v := named_colors[s]
		return Color{
			r: u8((v >> 16) & 0xff)
			g: u8((v >> 8) & 0xff)
			b: u8(v & 0xff)
			a: 255
		}
	}
	return error('unknown color: ${input}')
}

fn parse_hex_color(s string) !Color {
	mut hex := ''
	if s.len == 3 || s.len == 4 {
		for i in 0 .. s.len {
			hex += s[i..i + 1] + s[i..i + 1]
		}
	} else {
		hex = s
	}
	if hex.len != 6 && hex.len != 8 {
		return error('invalid hex color: #${s}')
	}
	v := strconv.parse_uint(hex, 16, 64) or { return error('invalid hex color: #${s}') }
	if hex.len == 6 {
		return Color.from_hex(u32(v), false)
	}
	return Color.from_hex(u32(v), true)
}

fn split_args(s string) []string {
	mut out := []string{}
	mut sb := strings.new_builder(s.len)
	for i, c in s {
		if c == `,` || c == ` ` || c == `\t` || c == `/` {
			if sb.len > 0 {
				out << sb.str()
				sb = strings.new_builder(s.len)
			}
		} else {
			sb.write_string(s[i..i + 1])
		}
	}
	if sb.len > 0 {
		out << sb.str()
	}
	return out
}

fn channel_value(s string) !f64 {
	if s.ends_with('%') {
		v := parse_number(s[0..s.len - 1]) or { return error('invalid percentage: ${s}') }
		return v * 2.55
	}
	return parse_number(s)
}

fn parse_rgb_color(s string, has_alpha bool) !Color {
	parts := split_args(s)
	mut need := 3
	if has_alpha || (parts.len == 4) {
		need = 4
	}
	if parts.len < need {
		return error('rgb() needs ${need} components')
	}
	r := clamp_limits(channel_value(parts[0]) or { return error('bad red') }, 0, 255)
	g := clamp_limits(channel_value(parts[1]) or { return error('bad green') }, 0, 255)
	b := clamp_limits(channel_value(parts[2]) or { return error('bad blue') }, 0, 255)
	mut a := 255.0
	if parts.len >= 4 {
		av := parts[3]
		if av.ends_with('%') {
			a = clamp_limits(parse_number(av[0..av.len - 1]) or { return error('bad alpha') } * 2.55, 0, 255)
		} else {
			a = clamp_limits(parse_number(av) or { return error('bad alpha') } * 255, 0, 255)
		}
	}
	return Color{
		r: u8(math.round(r))
		g: u8(math.round(g))
		b: u8(math.round(b))
		a: u8(math.round(a))
	}
}

fn parse_hsl_color(s string, has_alpha bool) !Color {
	parts := split_args(s)
	if parts.len < 3 {
		return error('hsl() needs 3 components')
	}
	hv := parse_number(parts[0]) or { return error('bad hue') }
	sp := parts[1]
	lp := parts[2]
	sv := parse_number(sp.trim_right('%')) or { return error('bad saturation') }
	lv := parse_number(lp.trim_right('%')) or { return error('bad lightness') }
	mut a := 255
	if has_alpha || parts.len >= 4 {
		if parts.len >= 4 {
			av := parts[3]
			if av.ends_with('%') {
				a = u8(math.round(clamp_limits(parse_number(av[0..av.len - 1]) or {
					return error('bad alpha')
				} * 2.55, 0, 255)))
			} else {
				a = u8(math.round(clamp_limits(parse_number(av) or {
					return error('bad alpha')
				} * 255, 0, 255)))
			}
		}
	}
	return hsl(hv, clamp_limits(sv / 100, 0, 1), clamp_limits(lv / 100, 0, 1), u8(a))
}

// with_alpha returns a copy of the color with a new alpha channel.
pub fn (c Color) with_alpha(a u8) Color {
	return Color{ kind: c.kind, r: c.r, g: c.g, b: c.b, a: a }
}

// multiplied_alpha returns the color with alpha scaled by factor (0..1).
pub fn (c Color) multiplied_alpha(factor f64) Color {
	return c.with_alpha(u8(math.round(f64(c.a) * clamp_limits(factor, 0, 1))))
}

// str serializes the color as a minimal CSS color string.
pub fn (c Color) str() string {
	match c.kind {
		.none {
			return 'none'
		}
		.current_color {
			return 'currentColor'
		}
		.rgb {}
	}
	if c.a == 255 {
		return '#${hex2(c.r)}${hex2(c.g)}${hex2(c.b)}'
	}
	return 'rgba(${c.r},${c.g},${c.b},${fmt_num(f64(c.a) / 255)})'
}

const hex_digits = '0123456789abcdef'

fn hex2(v u8) string {
	hi := int(v >> 4)
	lo := int(v & 0xf)
	return '${hex_digits[hi..hi + 1]}${hex_digits[lo..lo + 1]}'
}

const named_colors = {
	'aliceblue':            u32(0xf0f8ff)
	'antiquewhite':         u32(0xfaebd7)
	'aqua':                 u32(0x00ffff)
	'aquamarine':           u32(0x7fffd4)
	'azure':                u32(0xf0ffff)
	'beige':                u32(0xf5f5dc)
	'bisque':               u32(0xffe4c4)
	'black':                u32(0x000000)
	'blanchedalmond':       u32(0xffebcd)
	'blue':                 u32(0x0000ff)
	'blueviolet':           u32(0x8a2be2)
	'brown':                u32(0xa52a2a)
	'burlywood':            u32(0xdeb887)
	'cadetblue':            u32(0x5f9ea0)
	'chartreuse':           u32(0x7fff00)
	'chocolate':            u32(0xd2691e)
	'coral':                u32(0xff7f50)
	'cornflowerblue':       u32(0x6495ed)
	'cornsilk':             u32(0xfff8dc)
	'crimson':              u32(0xdc143c)
	'cyan':                 u32(0x00ffff)
	'darkblue':             u32(0x00008b)
	'darkcyan':             u32(0x008b8b)
	'darkgoldenrod':        u32(0xb8860b)
	'darkgray':             u32(0xa9a9a9)
	'darkgreen':            u32(0x006400)
	'darkgrey':             u32(0xa9a9a9)
	'darkkhaki':            u32(0xbdb76b)
	'darkmagenta':          u32(0x8b008b)
	'darkolivegreen':       u32(0x556b2f)
	'darkorange':           u32(0xff8c00)
	'darkorchid':           u32(0x9932cc)
	'darkred':              u32(0x8b0000)
	'darksalmon':           u32(0xe9967a)
	'darkseagreen':         u32(0x8fbc8f)
	'darkslateblue':        u32(0x483d8b)
	'darkslategray':        u32(0x2f4f4f)
	'darkslategrey':        u32(0x2f4f4f)
	'darkturquoise':        u32(0x00ced1)
	'darkviolet':           u32(0x9400d3)
	'deeppink':             u32(0xff1493)
	'deepskyblue':          u32(0x00bfff)
	'dimgray':              u32(0x696969)
	'dimgrey':              u32(0x696969)
	'dodgerblue':           u32(0x1e90ff)
	'firebrick':            u32(0xb22222)
	'floralwhite':          u32(0xfffaf0)
	'forestgreen':          u32(0x228b22)
	'fuchsia':              u32(0xff00ff)
	'gainsboro':            u32(0xdcdcdc)
	'ghostwhite':           u32(0xf8f8ff)
	'gold':                 u32(0xffd700)
	'goldenrod':            u32(0xdaa520)
	'gray':                 u32(0x808080)
	'green':                u32(0x008000)
	'greenyellow':          u32(0xadff2f)
	'grey':                 u32(0x808080)
	'honeydew':             u32(0xf0fff0)
	'hotpink':              u32(0xff69b4)
	'indianred':            u32(0xcd5c5c)
	'indigo':               u32(0x4b0082)
	'ivory':                u32(0xfffff0)
	'khaki':                u32(0xf0e68c)
	'lavender':             u32(0xe6e6fa)
	'lavenderblush':        u32(0xfff0f5)
	'lawngreen':            u32(0x7cfc00)
	'lemonchiffon':         u32(0xfffacd)
	'lightblue':            u32(0xadd8e6)
	'lightcoral':           u32(0xf08080)
	'lightcyan':            u32(0xe0ffff)
	'lightgoldenrodyellow': u32(0xfafad2)
	'lightgray':            u32(0xd3d3d3)
	'lightgreen':           u32(0x90ee90)
	'lightgrey':            u32(0xd3d3d3)
	'lightpink':            u32(0xffb6c1)
	'lightsalmon':          u32(0xffa07a)
	'lightseagreen':        u32(0x20b2aa)
	'lightskyblue':         u32(0x87cefa)
	'lightslategray':       u32(0x778899)
	'lightslategrey':       u32(0x778899)
	'lightsteelblue':       u32(0xb0c4de)
	'lightyellow':          u32(0xffffe0)
	'lime':                 u32(0x00ff00)
	'limegreen':            u32(0x32cd32)
	'linen':                u32(0xfaf0e6)
	'magenta':              u32(0xff00ff)
	'maroon':               u32(0x800000)
	'mediumaquamarine':     u32(0x66cdaa)
	'mediumblue':           u32(0x0000cd)
	'mediumorchid':         u32(0xba55d3)
	'mediumpurple':         u32(0x9370db)
	'mediumseagreen':       u32(0x3cb371)
	'mediumslateblue':      u32(0x7b68ee)
	'mediumspringgreen':    u32(0x00fa9a)
	'mediumturquoise':      u32(0x48d1cc)
	'mediumvioletred':      u32(0xc71585)
	'midnightblue':         u32(0x191970)
	'mintcream':            u32(0xf5fffa)
	'mistyrose':            u32(0xffe4e1)
	'moccasin':             u32(0xffe4b5)
	'navajowhite':          u32(0xffdead)
	'navy':                 u32(0x000080)
	'oldlace':              u32(0xfdf5e6)
	'olive':                u32(0x808000)
	'olivedrab':            u32(0x6b8e23)
	'orange':               u32(0xffa500)
	'orangered':            u32(0xff4500)
	'orchid':               u32(0xda70d6)
	'palegoldenrod':        u32(0xeee8aa)
	'palegreen':            u32(0x98fb98)
	'paleturquoise':        u32(0xafeeee)
	'palevioletred':        u32(0xdb7093)
	'papayawhip':           u32(0xffefd5)
	'peachpuff':            u32(0xffdab9)
	'peru':                 u32(0xcd853f)
	'pink':                 u32(0xffc0cb)
	'plum':                 u32(0xdda0dd)
	'powderblue':           u32(0xb0e0e6)
	'purple':               u32(0x800080)
	'rebeccapurple':        u32(0x663399)
	'red':                  u32(0xff0000)
	'rosybrown':            u32(0xbc8f8f)
	'royalblue':            u32(0x4169e1)
	'saddlebrown':          u32(0x8b4513)
	'salmon':               u32(0xfa8072)
	'sandybrown':           u32(0xf4a460)
	'seagreen':             u32(0x2e8b57)
	'seashell':             u32(0xfff5ee)
	'sienna':               u32(0xa0522d)
	'silver':               u32(0xc0c0c0)
	'skyblue':              u32(0x87ceeb)
	'slateblue':            u32(0x6a5acd)
	'slategray':            u32(0x708090)
	'slategrey':            u32(0x708090)
	'snow':                 u32(0xfffafa)
	'springgreen':          u32(0x00ff7f)
	'steelblue':            u32(0x4682b4)
	'tan':                  u32(0xd2b48c)
	'teal':                 u32(0x008080)
	'thistle':              u32(0xd8bfd8)
	'tomato':               u32(0xff6347)
	'turquoise':            u32(0x40e0d0)
	'violet':               u32(0xee82ee)
	'wheat':                u32(0xf5deb3)
	'white':                u32(0xffffff)
	'whitesmoke':           u32(0xf5f5f5)
	'yellow':               u32(0xffff00)
	'yellowgreen':          u32(0x9acd32)
}

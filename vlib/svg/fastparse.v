module svg

import strconv
import strings

// fast_parse_svg scans SVG markup directly into an Element tree. It handles
// the XML subset used by SVG: comments, processing instructions, DOCTYPE,
// CDATA, quoted attributes and predefined/numeric entities. parse_svg falls
// back to encoding.xml when this scanner rejects an input, so unusual or
// malformed markup keeps the general parser's behavior.
fn fast_parse_svg(input string) ?Element {
	if input.len == 0 {
		return none
	}
	mut p := FastParser{ s: input, i: 0 }
	p.skip_misc()
	if p.i >= p.s.len {
		return none
	}
	return p.parse_element()
}

struct FastParser {
	s string
mut:
	i int
}

fn xml_ws(c u8) bool {
	return c == ` ` || c == `\t` || c == `\n` || c == `\r`
}

fn (mut p FastParser) skip_ws() {
	for p.i < p.s.len && xml_ws(p.s[p.i]) {
		p.i++
	}
}

fn (mut p FastParser) skip_misc() {
	for p.i < p.s.len {
		c := p.s[p.i]
		if xml_ws(c) {
			p.i++
			continue
		}
		if c == `<` && p.i + 1 < p.s.len && p.s[p.i + 1] == `?` {
			end := p.s.index_after('?>', p.i + 2) or { return }
			p.i = end + 2
			continue
		}
		if c == `<` && p.i + 3 < p.s.len && p.s[p.i + 1] == `!` && p.s[p.i + 2] == `-` && p.s[p.i + 3] == `-` {
			end := p.s.index_after('-->', p.i + 4) or { return }
			p.i = end + 3
			continue
		}
		if c == `<` && p.i + 2 < p.s.len && p.s[p.i + 1] == `!` {
			mut j := p.i + 2
			mut depth := 0
			for j < p.s.len {
				if p.s[j] == `[` {
					depth++
				} else if p.s[j] == `]` && depth > 0 {
					depth--
				} else if p.s[j] == `>` && depth == 0 {
					break
				}
				j++
			}
			if j >= p.s.len {
				return
			}
			p.i = j + 1
			continue
		}
		return
	}
}

fn (mut p FastParser) parse_element() ?Element {
	if p.i >= p.s.len || p.s[p.i] != `<` {
		return none
	}
	p.i++
	name_start := p.i
	for p.i < p.s.len && !xml_ws(p.s[p.i]) && p.s[p.i] != `>` && p.s[p.i] != `/` {
		p.i++
	}
	if p.i == name_start {
		return none
	}
	name := p.s[name_start..p.i]
	mut e := elem(name)

	for {
		p.skip_ws()
		if p.i >= p.s.len {
			return none
		}
		if p.s[p.i] == `>` {
			p.i++
			break
		}
		if p.s[p.i] == `/` && p.i + 1 < p.s.len && p.s[p.i + 1] == `>` {
			p.i += 2
			return e
		}
		attr_start := p.i
		for p.i < p.s.len && !xml_ws(p.s[p.i]) && p.s[p.i] != `=` && p.s[p.i] != `>` && p.s[p.i] != `/` {
			p.i++
		}
		if p.i == attr_start {
			return none
		}
		attr_name := p.s[attr_start..p.i]
		p.skip_ws()
		if p.i >= p.s.len || p.s[p.i] != `=` {
			return none
		}
		p.i++
		p.skip_ws()
		if p.i >= p.s.len || (p.s[p.i] != `"` && p.s[p.i] != `'`) {
			return none
		}
		quote := p.s[p.i]
		p.i++
		val_start := p.i
		for p.i < p.s.len && p.s[p.i] != quote {
			p.i++
		}
		if p.i >= p.s.len {
			return none
		}
		value := decode_xml_fast(p.s[val_start..p.i])
		p.i++
		if attr_name == 'style' {
			e.style = parse_style(value) or { Style{} }
		}
		if attr_name == 'xlink:href' && value != '' && !value.starts_with('#') {
			e.set_attr('href', value)
		} else {
			e.set_attr(attr_name, value)
		}
	}

	mut text := strings.new_builder(64)
	for {
		if p.i >= p.s.len {
			return none
		}
		if p.s[p.i] != `<` {
			start := p.i
			for p.i < p.s.len && p.s[p.i] != `<` {
				p.i++
			}
			text.write_string(decode_xml_fast(p.s[start..p.i]))
			continue
		}
		if p.i + 3 < p.s.len && p.s[p.i + 1] == `!` && p.s[p.i + 2] == `-` && p.s[p.i + 3] == `-` {
			end := p.s.index_after('-->', p.i + 4) or { return none }
			p.i = end + 3
			continue
		}
		if p.i + 8 < p.s.len && p.s[p.i + 1..p.i + 9] == '[CDATA[' {
			end := p.s.index_after(']]>', p.i + 9) or { return none }
			text.write_string(p.s[p.i + 9..end])
			p.i = end + 3
			continue
		}
		if p.i + 1 < p.s.len && p.s[p.i + 1] == `?` {
			end := p.s.index_after('?>', p.i + 2) or { return none }
			p.i = end + 2
			continue
		}
		if p.i + 1 < p.s.len && p.s[p.i + 1] == `/` {
			p.i += 2
			for p.i < p.s.len && p.s[p.i] != `>` {
				p.i++
			}
			if p.i >= p.s.len {
				return none
			}
			p.i++
			break
		}
		child := p.parse_element() or { return none }
		e.children << child
	}
	e.text = text.str()
	return e
}

fn decode_xml_fast(s string) string {
	amp := s.index_u8(`&`)
	if amp < 0 {
		return s
	}
	mut sb := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		if s[i] != `&` {
			mut j := i
			for j < s.len && s[j] != `&` {
				j++
			}
			sb.write_string(s[i..j])
			i = j
			continue
		}
		mut semi := i + 1
		for semi < s.len && s[semi] != `;` && s[semi] != `<` && s[semi] != `&` {
			semi++
		}
		if semi >= s.len || s[semi] != `;` {
			sb.write_u8(`&`)
			i++
			continue
		}
		ent := s[i + 1..semi]
		match ent {
			'amp' {
				sb.write_u8(`&`)
			}
			'lt' {
				sb.write_u8(`<`)
			}
			'gt' {
				sb.write_u8(`>`)
			}
			'quot' {
				sb.write_u8(`"`)
			}
			'apos' {
				sb.write_u8(`'`)
			}
			else {
				if ent.len > 1 && ent[0] == `#` {
					mut cp := u32(0)
					if ent.len > 2 && (ent[1] == `x` || ent[1] == `X`) {
						cp = strconv.parse_uint(ent[2..], 16, 32) or { 0 }
					} else {
						cp = strconv.parse_uint(ent[1..], 10, 32) or { 0 }
					}
					if cp == 0 {
						sb.write_string(s[i..semi + 1])
					} else {
						write_utf8(mut sb, cp)
					}
				} else {
					sb.write_string(s[i..semi + 1])
				}
			}
		}
		i = semi + 1
	}
	return sb.str()
}

fn write_utf8(mut sb strings.Builder, cp u32) {
	if cp < 0x80 {
		sb.write_u8(u8(cp))
	} else if cp < 0x800 {
		sb.write_u8(u8(0xc0 | (cp >> 6)))
		sb.write_u8(u8(0x80 | (cp & 0x3f)))
	} else if cp < 0x10000 {
		sb.write_u8(u8(0xe0 | (cp >> 12)))
		sb.write_u8(u8(0x80 | ((cp >> 6) & 0x3f)))
		sb.write_u8(u8(0x80 | (cp & 0x3f)))
	} else {
		sb.write_u8(u8(0xf0 | (cp >> 18)))
		sb.write_u8(u8(0x80 | ((cp >> 12) & 0x3f)))
		sb.write_u8(u8(0x80 | ((cp >> 6) & 0x3f)))
		sb.write_u8(u8(0x80 | (cp & 0x3f)))
	}
}

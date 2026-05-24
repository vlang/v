module yaml

struct FlowParser {
	src string
mut:
	pos int
}

fn parse_flow_value(src string) !Any {
	mut parser := FlowParser{
		src: src
	}
	value := parser.parse_value()!
	parser.skip_space()
	if parser.pos != parser.src.len {
		return error('yaml: unexpected trailing flow content')
	}
	return value
}

fn (mut p FlowParser) parse_value() !Any {
	p.skip_space()
	if p.pos >= p.src.len {
		return error('yaml: unexpected end of flow value')
	}
	return match p.src[p.pos] {
		`[` { p.parse_array() }
		`{` { p.parse_object() }
		`"`, `'` { Any(parse_quoted_flow_string(mut p)!) }
		else { parse_scalar(p.parse_plain_token()) }
	}
}

fn (mut p FlowParser) parse_array() !Any {
	p.pos++
	mut items := []Any{}
	for {
		p.skip_space()
		if p.pos >= p.src.len {
			return error('yaml: unterminated flow array')
		}
		if p.src[p.pos] == `]` {
			p.pos++
			break
		}
		items << p.parse_value()!
		p.skip_space()
		if p.pos >= p.src.len {
			return error('yaml: unterminated flow array')
		}
		if p.src[p.pos] == `,` {
			p.pos++
			continue
		}
		if p.src[p.pos] == `]` {
			p.pos++
			break
		}
		return error('yaml: expected `,` or `]` in flow array')
	}
	return Any(items)
}

fn (mut p FlowParser) parse_object() !Any {
	p.pos++
	mut result := map[string]Any{}
	for {
		p.skip_space()
		if p.pos >= p.src.len {
			return error('yaml: unterminated flow object')
		}
		if p.src[p.pos] == `}` {
			p.pos++
			break
		}
		key := p.parse_key()!
		p.skip_space()
		if p.pos >= p.src.len || p.src[p.pos] != `:` {
			return error('yaml: expected `:` in flow object')
		}
		p.pos++
		result[key] = p.parse_value()!
		p.skip_space()
		if p.pos >= p.src.len {
			return error('yaml: unterminated flow object')
		}
		if p.src[p.pos] == `,` {
			p.pos++
			continue
		}
		if p.src[p.pos] == `}` {
			p.pos++
			break
		}
		return error('yaml: expected `,` or `}` in flow object')
	}
	return Any(result)
}

fn (mut p FlowParser) parse_key() !string {
	p.skip_space()
	if p.pos >= p.src.len {
		return error('yaml: unexpected end of flow key')
	}
	if p.src[p.pos] in [`"`, `'`] {
		return parse_quoted_flow_string(mut p)
	}
	start := p.pos
	for p.pos < p.src.len {
		ch := p.src[p.pos]
		if ch == `:` {
			break
		}
		p.pos++
	}
	return p.src[start..p.pos].trim_space()
}

fn (mut p FlowParser) parse_plain_token() string {
	start := p.pos
	mut bracket_depth := 0
	mut brace_depth := 0
	for p.pos < p.src.len {
		ch := p.src[p.pos]
		if ch == `[` {
			bracket_depth++
		} else if ch == `]` {
			if bracket_depth == 0 {
				break
			}
			bracket_depth--
		} else if ch == `{` {
			brace_depth++
		} else if ch == `}` {
			if brace_depth == 0 {
				break
			}
			brace_depth--
		} else if ch == `,` && bracket_depth == 0 && brace_depth == 0 {
			break
		}
		p.pos++
	}
	return p.src[start..p.pos].trim_space()
}

fn (mut p FlowParser) skip_space() {
	for p.pos < p.src.len && p.src[p.pos].is_space() {
		p.pos++
	}
}

fn parse_quoted_flow_string(mut p FlowParser) !string {
	start := p.pos
	quote := p.src[p.pos]
	p.pos++
	mut escape := false
	for p.pos < p.src.len {
		ch := p.src[p.pos]
		if quote == `"` {
			if escape {
				escape = false
			} else if ch == `\\` {
				escape = true
			} else if ch == `"` {
				p.pos++
				return parse_quoted_string(p.src[start..p.pos])
			}
		} else if ch == `'` {
			if p.pos + 1 < p.src.len && p.src[p.pos + 1] == `'` {
				p.pos += 2
				continue
			}
			p.pos++
			return parse_quoted_string(p.src[start..p.pos])
		}
		p.pos++
	}
	return error('yaml: unterminated quoted flow string')
}

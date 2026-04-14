module yaml

import json
import os
import strconv
import time
import x.json2

// Null is a simple representation of the YAML `null` value.
pub struct Null {}

// null is an instance of `Null`, to ease comparisons with it.
pub const null = Any(Null{})

// Any is the tree representation used by the YAML module.
pub type Any = []Any | Null | bool | f64 | i64 | int | map[string]Any | string | u64

// Doc is a parsed YAML document.
pub struct Doc {
pub:
	root Any
}

// parse_file parses the YAML file at `path`.
pub fn parse_file(path string) !Doc {
	return parse_text(os.read_file(path)!)
}

// parse_text parses the YAML document provided in `text`.
pub fn parse_text(text string) !Doc {
	mut normalized := text.replace('\r\n', '\n').replace('\r', '\n')
	if normalized.len >= 3 && normalized[0] == 0xef && normalized[1] == 0xbb
		&& normalized[2] == 0xbf {
		normalized = normalized[3..]
	}
	trimmed := normalized.trim_space()
	if trimmed == '' {
		return Doc{
			root: Any(map[string]Any{})
		}
	}
	if trimmed.starts_with('{') || trimmed.starts_with('[') {
		raw := json2.decode[json2.Any](trimmed) or { json2.null }
		if raw !is json2.Null {
			return Doc{
				root: from_json2(raw)
			}
		}
	}
	mut parser := Parser{
		lines: normalized.split('\n')
	}
	return Doc{
		root: parser.parse()!
	}
}

// decode decodes YAML text into the target type `T`.
// The generic encode/decode path uses the main `json` module for field parity.
pub fn decode[T](yaml_text string) !T {
	doc := parse_text(yaml_text)!
	return doc.decode[T]()
}

// decode_file decodes the YAML file at `path` into the target type `T`.
pub fn decode_file[T](path string) !T {
	return decode[T](os.read_file(path)!)
}

// encode encodes the value `value` into a YAML string.
// The generic encode/decode path uses the main `json` module for field parity.
pub fn encode[T](value T) string {
	json_text := json.encode(value)
	raw := json2.decode[json2.Any](json_text) or { panic(err) }
	return from_json2(raw).to_yaml()
}

// encode_file encodes `value` as YAML and writes it to `path`.
pub fn encode_file[T](path string, value T) ! {
	os.write_file(path, encode(value))!
}

// decode decodes the YAML document into the target type `T`.
pub fn (d Doc) decode[T]() !T {
	return json.decode(T, d.to_json())!
}

// to_any converts the YAML document to `yaml.Any`.
pub fn (d Doc) to_any() Any {
	return d.root
}

// to_json converts the YAML document to JSON.
pub fn (d Doc) to_json() string {
	return d.root.to_json()
}

// to_yaml converts the YAML document back to YAML text.
pub fn (d Doc) to_yaml() string {
	return d.root.to_yaml()
}

// value queries a value from the YAML document.
// `key` supports dotted keys and array indexing like `servers[0].host`.
pub fn (d Doc) value(key string) Any {
	return d.root.value(key)
}

// value_opt queries a value from the YAML document and returns an error when missing.
pub fn (d Doc) value_opt(key string) !Any {
	return d.root.value_opt(key)
}

// str returns a display-friendly string form of `Any`.
pub fn (a Any) str() string {
	return a.string()
}

// string returns `Any` as a string when possible, or a YAML representation otherwise.
pub fn (a Any) string() string {
	return match a {
		string { a.clone() }
		bool { a.str() }
		f64 { a.str() }
		i64 { a.str() }
		int { a.str() }
		u64 { a.str() }
		Null { 'null' }
		[]Any, map[string]Any { a.to_yaml() }
	}
}

// int returns `Any` as an `int`.
pub fn (a Any) int() int {
	return match a {
		int {
			a
		}
		i64 {
			int(a)
		}
		u64 {
			int(a)
		}
		f64 {
			int(a)
		}
		bool {
			if a {
				1
			} else {
				0
			}
		}
		string {
			a.int()
		}
		else {
			0
		}
	}
}

// i64 returns `Any` as an `i64`.
pub fn (a Any) i64() i64 {
	return match a {
		i64 {
			a
		}
		int {
			i64(a)
		}
		u64 {
			i64(a)
		}
		f64 {
			i64(a)
		}
		bool {
			if a {
				i64(1)
			} else {
				i64(0)
			}
		}
		string {
			a.i64()
		}
		else {
			i64(0)
		}
	}
}

// u64 returns `Any` as a `u64`.
pub fn (a Any) u64() u64 {
	return match a {
		u64 {
			a
		}
		int {
			u64(a)
		}
		i64 {
			u64(a)
		}
		f64 {
			u64(a)
		}
		bool {
			if a {
				u64(1)
			} else {
				u64(0)
			}
		}
		string {
			a.u64()
		}
		else {
			u64(0)
		}
	}
}

// f64 returns `Any` as an `f64`.
pub fn (a Any) f64() f64 {
	return match a {
		f64 {
			a
		}
		int {
			f64(a)
		}
		i64 {
			f64(a)
		}
		u64 {
			f64(a)
		}
		bool {
			if a {
				1.0
			} else {
				0.0
			}
		}
		string {
			a.f64()
		}
		else {
			0.0
		}
	}
}

// bool returns `Any` as a `bool`.
pub fn (a Any) bool() bool {
	return match a {
		bool {
			a
		}
		int {
			a != 0
		}
		i64 {
			a != 0
		}
		u64 {
			a != 0
		}
		f64 {
			a != 0.0
		}
		string {
			lower := a.to_lower()
			lower in ['true', 'yes', 'on', '1']
		}
		else {
			false
		}
	}
}

// array returns `Any` as an array.
pub fn (a Any) array() []Any {
	if a is []Any {
		return a
	}
	if a is map[string]Any {
		mut arr := []Any{}
		for _, value in a {
			arr << value
		}
		return arr
	}
	return [a]
}

// as_map returns `Any` as a map.
pub fn (a Any) as_map() map[string]Any {
	if a is map[string]Any {
		return a
	}
	if a is []Any {
		mut out := map[string]Any{}
		for i, value in a {
			out['${i}'] = value
		}
		return out
	}
	return {
		'0': a
	}
}

// default_to returns `value` when `a` is `Null`.
pub fn (a Any) default_to(value Any) Any {
	return match a {
		Null { value }
		else { a }
	}
}

// value queries a value from the current node using dotted keys and array indices.
pub fn (a Any) value(key string) Any {
	key_split := parse_dotted_key(key) or { return null }
	return a.value_(a, key_split)
}

// value_opt queries a value from the current node and returns an error when missing.
pub fn (a Any) value_opt(key string) !Any {
	key_split := parse_dotted_key(key) or { return error('invalid dotted key') }
	value := a.value_(a, key_split)
	if value is Null {
		return error('no value for key')
	}
	return value
}

// value queries a value from the map.
pub fn (m map[string]Any) value(key string) Any {
	return Any(m).value(key)
}

// value queries a value from the array.
pub fn (a []Any) value(key string) Any {
	return Any(a).value(key)
}

// as_strings returns the contents of the array as `[]string`.
pub fn (a []Any) as_strings() []string {
	mut out := []string{cap: a.len}
	for value in a {
		out << value.string()
	}
	return out
}

// as_strings returns the contents of the map as `map[string]string`.
pub fn (m map[string]Any) as_strings() map[string]string {
	mut out := map[string]string{}
	for key, value in m {
		out[key] = value.string()
	}
	return out
}

// to_json converts `Any` to JSON.
pub fn (a Any) to_json() string {
	return json2.encode(a.to_json2(), json2.EncoderOptions{})
}

// to_yaml converts `Any` to YAML.
pub fn (a Any) to_yaml() string {
	return yaml_from_any(a, 0)
}

// to_yaml converts a YAML array to YAML text.
pub fn (a []Any) to_yaml() string {
	return Any(a).to_yaml()
}

// to_yaml converts a YAML map to YAML text.
pub fn (m map[string]Any) to_yaml() string {
	return Any(m).to_yaml()
}

fn (a Any) value_(current Any, key []string) Any {
	if key.len == 0 {
		return null
	}
	mut value := Any(null)
	k, index := parse_array_key(key[0])
	if k == '' {
		if current is []Any {
			arr := current as []Any
			value = arr[index] or { return null }
		} else {
			return null
		}
	}
	if current is map[string]Any {
		value = current[k] or { return null }
		if index > -1 {
			if value is []Any {
				arr := value as []Any
				value = arr[index] or { return null }
			} else {
				return null
			}
		}
	}
	if key.len <= 1 {
		return value
	}
	return match value {
		[]Any, map[string]Any { a.value_(value, key[1..]) }
		else { null }
	}
}

fn (a Any) to_json2() json2.Any {
	return match a {
		[]Any {
			mut arr := []json2.Any{cap: a.len}
			for value in a {
				arr << value.to_json2()
			}
			json2.Any(arr)
		}
		map[string]Any {
			mut out := map[string]json2.Any{}
			for key, value in a {
				out[key] = value.to_json2()
			}
			json2.Any(out)
		}
		string {
			json2.Any(a)
		}
		bool {
			json2.Any(a)
		}
		f64 {
			json2.Any(a)
		}
		i64 {
			json2.Any(a)
		}
		int {
			json2.Any(a)
		}
		u64 {
			json2.Any(a)
		}
		Null {
			json2.Any(json2.null)
		}
	}
}

fn from_json2(value json2.Any) Any {
	return match value {
		[]json2.Any {
			mut arr := []Any{cap: value.len}
			for item in value {
				arr << from_json2(item)
			}
			Any(arr)
		}
		map[string]json2.Any {
			mut out := map[string]Any{}
			for key, item in value {
				out[key] = from_json2(item)
			}
			Any(out)
		}
		bool {
			Any(value)
		}
		f32, f64 {
			Any(f64(value))
		}
		i8, i16, i32, int {
			Any(int(value))
		}
		i64 {
			Any(value)
		}
		u8, u16, u32, u64 {
			Any(u64(value))
		}
		string {
			Any(value)
		}
		time.Time {
			Any(value.str())
		}
		json2.Null {
			null
		}
	}
}

struct Parser {
	lines []string
mut:
	idx int
}

fn (mut p Parser) parse() !Any {
	p.skip_ignorable()
	if p.idx >= p.lines.len {
		return Any(map[string]Any{})
	}
	indent := p.line_indent(p.idx)!
	if indent < 0 {
		return Any(map[string]Any{})
	}
	return p.parse_node(indent)
}

fn (mut p Parser) parse_node(indent int) !Any {
	p.skip_ignorable()
	if p.idx >= p.lines.len {
		return null
	}
	current_indent := p.line_indent(p.idx)!
	if current_indent < indent {
		return null
	}
	content := p.current_content()!
	if content.starts_with('-') && (content.len == 1 || content[1] == ` `) {
		return p.parse_sequence(current_indent)
	}
	if split_mapping_entry(content).ok {
		return p.parse_mapping(current_indent)
	}
	if content.starts_with('[') || content.starts_with('{') {
		p.idx++
		return parse_flow_value(content)
	}
	p.idx++
	return parse_scalar(content)
}

fn (mut p Parser) parse_mapping(indent int) !Any {
	mut result := map[string]Any{}
	for p.idx < p.lines.len {
		p.skip_ignorable()
		if p.idx >= p.lines.len {
			break
		}
		current_indent := p.line_indent(p.idx)!
		if current_indent < indent {
			break
		}
		if current_indent > indent {
			return error('yaml: unexpected indentation on line ${p.idx + 1}')
		}
		content := p.current_content()!
		entry := split_mapping_entry(content)
		if !entry.ok {
			return error('yaml: expected a mapping entry on line ${p.idx + 1}')
		}
		p.idx++
		result[entry.key] = p.parse_mapping_value(entry.rest, indent)!
	}
	return Any(result)
}

fn (mut p Parser) parse_mapping_value(rest string, indent int) !Any {
	if rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			return p.parse_node(next_indent)
		}
		return null
	}
	if is_block_scalar(rest) {
		return Any(p.parse_block_scalar(indent, rest[0])!)
	}
	if rest.starts_with('[') || rest.starts_with('{') {
		return parse_flow_value(rest)
	}
	return parse_scalar(rest)
}

fn (mut p Parser) parse_sequence(indent int) !Any {
	mut items := []Any{}
	for p.idx < p.lines.len {
		p.skip_ignorable()
		if p.idx >= p.lines.len {
			break
		}
		current_indent := p.line_indent(p.idx)!
		if current_indent < indent {
			break
		}
		if current_indent > indent {
			return error('yaml: unexpected indentation on line ${p.idx + 1}')
		}
		content := p.current_content()!
		if !content.starts_with('-') || (content.len > 1 && content[1] != ` `) {
			break
		}
		rest := if content.len == 1 { '' } else { content[1..].trim_space() }
		p.idx++
		items << p.parse_sequence_item(rest, indent)!
	}
	return Any(items)
}

fn (mut p Parser) parse_sequence_item(rest string, indent int) !Any {
	if rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			return p.parse_node(next_indent)
		}
		return null
	}
	if is_block_scalar(rest) {
		return Any(p.parse_block_scalar(indent, rest[0])!)
	}
	if rest.starts_with('[') || rest.starts_with('{') {
		return parse_flow_value(rest)
	}
	entry := split_mapping_entry(rest)
	if entry.ok {
		mut result := map[string]Any{}
		child_indent := indent + 2
		result[entry.key] = p.parse_mapping_value(entry.rest, child_indent)!
		for p.idx < p.lines.len {
			p.skip_ignorable()
			if p.idx >= p.lines.len {
				break
			}
			current_indent := p.line_indent(p.idx)!
			if current_indent <= indent {
				break
			}
			if current_indent != child_indent {
				return error('yaml: unexpected indentation on line ${p.idx + 1}')
			}
			content := p.current_content()!
			next_entry := split_mapping_entry(content)
			if !next_entry.ok {
				break
			}
			p.idx++
			result[next_entry.key] = p.parse_mapping_value(next_entry.rest, child_indent)!
		}
		return Any(result)
	}
	return parse_scalar(rest)
}

fn (mut p Parser) parse_block_scalar(parent_indent int, style u8) !string {
	start := p.idx
	mut min_indent := -1
	for i := start; i < p.lines.len; i++ {
		line := p.lines[i]
		if line.trim_space() == '' {
			continue
		}
		line_indent := p.line_indent(i)!
		if line_indent <= parent_indent {
			break
		}
		if min_indent == -1 || line_indent < min_indent {
			min_indent = line_indent
		}
	}
	if min_indent == -1 {
		return ''
	}
	mut lines := []string{}
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		if line.trim_space() == '' {
			lines << ''
			p.idx++
			continue
		}
		line_indent := p.line_indent(p.idx)!
		if line_indent <= parent_indent {
			break
		}
		if line.len <= min_indent {
			lines << ''
		} else {
			lines << line[min_indent..]
		}
		p.idx++
	}
	for lines.len > 0 && lines[lines.len - 1] == '' {
		lines.delete(lines.len - 1)
	}
	if style == `|` {
		return lines.join('\n')
	}
	return fold_block_scalar(lines)
}

fn (mut p Parser) skip_ignorable() {
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		trimmed := strip_comments(line).trim_space()
		if trimmed == '' || trimmed == '---' || trimmed == '...' {
			p.idx++
			continue
		}
		break
	}
}

fn (p &Parser) peek_next_indent() int {
	mut i := p.idx
	for i < p.lines.len {
		line := p.lines[i]
		trimmed := strip_comments(line).trim_space()
		if trimmed == '' || trimmed == '---' || trimmed == '...' {
			i++
			continue
		}
		return p.line_indent(i) or { -1 }
	}
	return -1
}

fn (p &Parser) current_content() !string {
	line := p.lines[p.idx]
	indent := p.line_indent(p.idx)!
	if line.len <= indent {
		return ''
	}
	return strip_comments(line[indent..]).trim_space()
}

fn (p &Parser) line_indent(index int) !int {
	line := p.lines[index]
	mut indent := 0
	for indent < line.len && line[indent] == ` ` {
		indent++
	}
	if indent < line.len && line[indent] == `\t` {
		return error('yaml: tabs are not supported for indentation on line ${index + 1}')
	}
	return indent
}

struct MappingEntry {
	key  string
	rest string
	ok   bool
}

fn split_mapping_entry(content string) MappingEntry {
	mut in_single := false
	mut in_double := false
	mut escape := false
	mut bracket_depth := 0
	mut brace_depth := 0
	mut i := 0
	for i < content.len {
		ch := content[i]
		if in_double {
			if escape {
				escape = false
			} else if ch == `\\` {
				escape = true
			} else if ch == `"` {
				in_double = false
			}
			i++
			continue
		}
		if in_single {
			if ch == `'` {
				if i + 1 < content.len && content[i + 1] == `'` {
					i += 2
					continue
				}
				in_single = false
			}
			i++
			continue
		}
		match ch {
			`"` {
				in_double = true
			}
			`'` {
				in_single = true
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			`{` {
				brace_depth++
			}
			`}` {
				if brace_depth > 0 {
					brace_depth--
				}
			}
			`:` {
				if bracket_depth == 0 && brace_depth == 0
					&& (i + 1 == content.len || content[i + 1].is_space()) {
					key_text := content[..i].trim_space()
					if key_text == '' {
						return MappingEntry{}
					}
					return MappingEntry{
						key:  parse_key(key_text) or { return MappingEntry{} }
						rest: if i + 1 < content.len {
							content[i + 1..].trim_space()
						} else {
							''
						}
						ok:   true
					}
				}
			}
			else {}
		}
		i++
	}
	return MappingEntry{}
}

fn parse_key(src string) !string {
	if src.len >= 2 && ((src[0] == `"` && src[src.len - 1] == `"`)
		|| (src[0] == `'` && src[src.len - 1] == `'`)) {
		return parse_quoted_string(src)
	}
	return src.trim_space()
}

fn parse_scalar(text string) !Any {
	value := text.trim_space()
	if value == '' {
		return Any('')
	}
	if value.len >= 2 && ((value[0] == `"` && value[value.len - 1] == `"`)
		|| (value[0] == `'` && value[value.len - 1] == `'`)) {
		return Any(parse_quoted_string(value)!)
	}
	lower := value.to_lower()
	if lower in ['null', '~'] {
		return null
	}
	if lower in ['true', 'yes', 'on'] {
		return Any(true)
	}
	if lower in ['false', 'no', 'off'] {
		return Any(false)
	}
	numeric := value.replace('_', '')
	if is_integer(numeric) {
		if numeric.starts_with('-') {
			return Any(numeric.parse_int(0, 64)!)
		}
		if numeric.starts_with('+') {
			return Any(numeric[1..].parse_uint(0, 64)!)
		}
		return Any(numeric.parse_uint(0, 64)!)
	}
	if is_float(numeric) {
		return Any(strconv.atof64(numeric)!)
	}
	return Any(value)
}

fn parse_quoted_string(src string) !string {
	if src.len < 2 {
		return error('yaml: invalid quoted string')
	}
	quote := src[0]
	inner := src[1..src.len - 1]
	if quote == `'` {
		return inner.replace("''", "'")
	}
	mut out := []u8{}
	mut i := 0
	for i < inner.len {
		ch := inner[i]
		if ch != `\\` {
			out << ch
			i++
			continue
		}
		i++
		if i >= inner.len {
			return error('yaml: invalid escape sequence')
		}
		esc := inner[i]
		match esc {
			`"`, `\\`, `/` {
				out << esc
			}
			`b` {
				out << `\b`
			}
			`f` {
				out << `\f`
			}
			`n` {
				out << `\n`
			}
			`r` {
				out << `\r`
			}
			`t` {
				out << `\t`
			}
			`u` {
				if i + 4 >= inner.len {
					return error('yaml: invalid unicode escape')
				}
				code := inner[i + 1..i + 5]
				r := rune(code.parse_uint(16, 32)!)
				out << r.str().bytes()
				i += 4
			}
			else {
				out << esc
			}
		}
		i++
	}
	return out.bytestr()
}

fn strip_comments(line string) string {
	mut in_single := false
	mut in_double := false
	mut escape := false
	mut bracket_depth := 0
	mut brace_depth := 0
	mut i := 0
	for i < line.len {
		ch := line[i]
		if in_double {
			if escape {
				escape = false
			} else if ch == `\\` {
				escape = true
			} else if ch == `"` {
				in_double = false
			}
			i++
			continue
		}
		if in_single {
			if ch == `'` {
				if i + 1 < line.len && line[i + 1] == `'` {
					i += 2
					continue
				}
				in_single = false
			}
			i++
			continue
		}
		match ch {
			`"` {
				in_double = true
			}
			`'` {
				in_single = true
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			`{` {
				brace_depth++
			}
			`}` {
				if brace_depth > 0 {
					brace_depth--
				}
			}
			`#` {
				if bracket_depth == 0 && brace_depth == 0 {
					return line[..i].trim_right(' \t')
				}
			}
			else {}
		}
		i++
	}
	return line.trim_right(' \t')
}

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

fn parse_dotted_key(key string) ![]string {
	mut out := []string{}
	mut buf := ''
	mut in_string := false
	mut delimiter := u8(` `)
	for ch in key {
		if ch in [`"`, `'`] {
			if !in_string {
				delimiter = ch
				in_string = true
				continue
			}
			if ch == delimiter {
				in_string = false
				if buf != '' {
					out << buf
				}
				buf = ''
				delimiter = ` `
				continue
			}
		}
		buf += ch.ascii_str()
		if !in_string && ch == `.` {
			buf = buf[..buf.len - 1]
			if buf != '' {
				out << buf
			}
			buf = ''
		}
	}
	if buf != '' {
		out << buf
	}
	if in_string {
		return error('yaml: missing closing string delimiter `${delimiter.ascii_str()}`')
	}
	return out
}

fn parse_array_key(key string) (string, int) {
	mut index := -1
	mut k := key
	if k.contains('[') {
		index = k.all_after('[').all_before(']').int()
		if k.starts_with('[') {
			k = ''
		} else {
			k = k.all_before('[')
		}
	}
	return k, index
}

fn yaml_from_any(value Any, indent int) string {
	return match value {
		map[string]Any { yaml_from_map(value, indent) }
		[]Any { yaml_from_array(value, indent) }
		else { yaml_scalar(value) }
	}
}

fn yaml_from_map(value map[string]Any, indent int) string {
	if value.len == 0 {
		return '{}'
	}
	mut lines := []string{cap: value.len}
	padding := ' '.repeat(indent)
	for key, item in value {
		quoted_key := yaml_quote_string(key)
		match item {
			map[string]Any, []Any {
				lines << '${padding}${quoted_key}:'
				lines << yaml_from_any(item, indent + 2)
			}
			else {
				lines << '${padding}${quoted_key}: ${yaml_scalar(item)}'
			}
		}
	}
	return lines.join('\n')
}

fn yaml_from_array(value []Any, indent int) string {
	if value.len == 0 {
		return '[]'
	}
	mut lines := []string{cap: value.len * 2}
	padding := ' '.repeat(indent)
	for item in value {
		match item {
			map[string]Any, []Any {
				lines << '${padding}-'
				lines << yaml_from_any(item, indent + 2)
			}
			else {
				lines << '${padding}- ${yaml_scalar(item)}'
			}
		}
	}
	return lines.join('\n')
}

fn yaml_scalar(value Any) string {
	return match value {
		string {
			yaml_quote_string(value)
		}
		bool {
			if value {
				'true'
			} else {
				'false'
			}
		}
		f64 {
			value.str()
		}
		i64 {
			value.str()
		}
		int {
			value.str()
		}
		u64 {
			value.str()
		}
		Null {
			'null'
		}
		[]Any, map[string]Any {
			yaml_from_any(value, 0)
		}
	}
}

fn yaml_quote_string(value string) string {
	return json.encode(value)
}

fn fold_block_scalar(lines []string) string {
	mut out := ''
	mut pending_newlines := 0
	for line in lines {
		if line == '' {
			pending_newlines++
			continue
		}
		if out.len == 0 {
			out = line
		} else if pending_newlines > 0 {
			out += '\n'.repeat(pending_newlines + 1) + line
		} else {
			out += ' ' + line
		}
		pending_newlines = 0
	}
	if pending_newlines > 0 {
		out += '\n'.repeat(pending_newlines)
	}
	return out
}

fn is_block_scalar(value string) bool {
	return value.len > 0 && value[0] in [`|`, `>`]
}

fn is_integer(value string) bool {
	if value.len == 0 {
		return false
	}
	if value[0] in [`+`, `-`] {
		if value.len == 1 {
			return false
		}
		if value[0] == `-` {
			_ := value.parse_int(0, 64) or { return false }
			return true
		}
		_ := value[1..].parse_uint(0, 64) or { return false }
		return true
	}
	_ := value.parse_uint(0, 64) or { return false }
	return true
}

fn is_float(value string) bool {
	if value.len == 0 {
		return false
	}
	if !value.contains('.') && !value.contains('e') && !value.contains('E') {
		return false
	}
	_ := strconv.atof64(value) or { return false }
	return true
}

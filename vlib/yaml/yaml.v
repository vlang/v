module yaml

import json
import os
import strconv
import strings
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
	mut normalized := text
	if normalized.contains_u8(`\r`) {
		normalized = normalized.replace('\r\n', '\n').replace('\r', '\n')
	}
	if normalized.len >= 3 && normalized[0] == 0xef && normalized[1] == 0xbb
		&& normalized[2] == 0xbf {
		normalized = normalized[3..]
	}
	// `split('\n')` would otherwise turn the canonical trailing line break into
	// a phantom empty last line, which the block-scalar reader then treats as a
	// genuine blank line and over-counts during chomping.
	if normalized.ends_with('\n') {
		normalized = normalized[..normalized.len - 1]
	}
	trimmed := normalized.trim_space()
	if trimmed == '' {
		return Doc{
			root: null
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
	raw := json2.decode[json2.Any](json_text) or { return '' }
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
		string { a }
		bool, f64, i64, int, u64 { a.str() }
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
	return match a {
		[]Any {
			a
		}
		map[string]Any {
			mut arr := []Any{cap: a.len}
			for _, value in a {
				arr << value
			}
			arr
		}
		else {
			[a]
		}
	}
}

// as_map returns `Any` as a map.
pub fn (a Any) as_map() map[string]Any {
	return match a {
		map[string]Any {
			a
		}
		[]Any {
			mut out := map[string]Any{}
			for i, value in a {
				out['${i}'] = value
			}
			out
		}
		else {
			{
				'0': a
			}
		}
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
	return a.value_opt(key) or { null }
}

// value_opt queries a value from the current node and returns an error when missing.
// A YAML key whose value is the explicit `null` literal returns that `Null`
// (it is not treated as missing); only an absent key or a non-traversable
// path raises an error.
pub fn (a Any) value_opt(key string) !Any {
	key_split := parse_dotted_key(key) or { return error('invalid dotted key') }
	return a.value_(a, key_split) or { error('no value for key') }
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
	mut sb := strings.new_builder(256)
	emit_any_as_json(mut sb, a)
	return sb.str()
}

// to_yaml converts `Any` to YAML.
pub fn (a Any) to_yaml() string {
	mut sb := strings.new_builder(256)
	emit_yaml_any(mut sb, a, 0)
	return sb.str()
}

// to_yaml converts a YAML array to YAML text.
pub fn (a []Any) to_yaml() string {
	return Any(a).to_yaml()
}

// to_yaml converts a YAML map to YAML text.
pub fn (m map[string]Any) to_yaml() string {
	return Any(m).to_yaml()
}

fn (a Any) value_(current Any, key []string) ?Any {
	if key.len == 0 {
		return none
	}
	k, index := parse_array_key(key[0])
	value := match current {
		[]Any {
			if k != '' {
				return none
			}
			current[index] or { return none }
		}
		map[string]Any {
			v := current[k] or { return none }
			if index > -1 {
				if v is []Any {
					v[index] or { return none }
				} else {
					return none
				}
			} else {
				v
			}
		}
		else {
			return none
		}
	}

	if key.len <= 1 {
		return value
	}
	return match value {
		[]Any, map[string]Any { a.value_(value, key[1..]) }
		else { none }
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
mut:
	lines           []string
	idx             int
	anchors         map[string]Any
	directives_done bool
}

fn (mut p Parser) parse() !Any {
	p.skip_ignorable()
	if p.idx >= p.lines.len {
		return null
	}
	indent := p.line_indent(p.idx)!
	if indent < 0 {
		return null
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
	d := extract_decorators(content)
	if d.alias != '' && d.rest == '' {
		p.idx++
		return p.resolve_alias(d.alias)
	}
	if is_block_scalar(d.rest) {
		p.idx++
		return p.register_anchor(d.anchor, Any(p.parse_block_scalar(current_indent, d.rest)!))
	}
	if d.rest.starts_with('[') || d.rest.starts_with('{') {
		p.idx++
		full := p.collect_flow_continuation(d.rest)!
		return p.register_anchor(d.anchor, parse_flow_value(full)!)
	}
	p.idx++
	if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
		quoted := p.gather_quoted_continuation(d.rest)!
		return p.register_anchor(d.anchor, parse_scalar(quoted)!)
	}
	folded := p.gather_plain_continuation(d.rest, current_indent)
	return p.register_anchor(d.anchor, parse_scalar(folded)!)
}

// gather_plain_continuation extends a plain scalar with subsequent lines that
// belong to the same node per YAML 1.2 §6.5.1: adjacent non-blank lines fold
// to a single space, blank lines contribute literal `\n`s. The scan stops at
// document markers, structural indicators, or a less-indented line.
fn (mut p Parser) gather_plain_continuation(initial string, base_indent int) string {
	mut sb := strings.new_builder(initial.len * 2)
	sb.write_string(initial.trim_space())
	mut blanks := 0
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		trimmed_raw := line.trim_space()
		if trimmed_raw == '' {
			blanks++
			p.idx++
			continue
		}
		trimmed := strip_comments(line).trim_space()
		if trimmed == '' {
			blanks++
			p.idx++
			continue
		}
		if trimmed == '---' || trimmed == '...' {
			break
		}
		line_indent := p.line_indent(p.idx) or { break }
		if line_indent < base_indent {
			break
		}
		if trimmed.starts_with('- ') || trimmed == '-' {
			break
		}
		if split_mapping_entry(trimmed).ok {
			break
		}
		if blanks > 0 {
			for _ in 0 .. blanks {
				sb.write_u8(`\n`)
			}
			blanks = 0
		} else {
			sb.write_u8(` `)
		}
		sb.write_string(trimmed)
		p.idx++
	}
	return sb.str()
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

fn (mut p Parser) parse_mapping_value(rest_in string, indent int) !Any {
	d := extract_decorators(rest_in)
	if d.alias != '' && d.rest == '' {
		return p.resolve_alias(d.alias)
	}
	value := if d.rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			p.parse_node(next_indent)!
		} else {
			null
		}
	} else if is_block_scalar(d.rest) {
		Any(p.parse_block_scalar(indent, d.rest)!)
	} else if d.rest.starts_with('[') || d.rest.starts_with('{') {
		full := p.collect_flow_continuation(d.rest)!
		parse_flow_value(full)!
	} else if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
		quoted := p.gather_quoted_continuation(d.rest)!
		parse_scalar(quoted)!
	} else {
		parse_scalar(d.rest)!
	}
	return p.register_anchor(d.anchor, value)
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

fn (mut p Parser) parse_sequence_item(rest_in string, indent int) !Any {
	d := extract_decorators(rest_in)
	if d.alias != '' && d.rest == '' {
		return p.resolve_alias(d.alias)
	}
	value := if d.rest == '' {
		next_indent := p.peek_next_indent()
		if next_indent > indent {
			p.parse_node(next_indent)!
		} else {
			null
		}
	} else if is_block_scalar(d.rest) {
		Any(p.parse_block_scalar(indent, d.rest)!)
	} else if d.rest.starts_with('[') || d.rest.starts_with('{') {
		full := p.collect_flow_continuation(d.rest)!
		parse_flow_value(full)!
	} else {
		entry := split_mapping_entry(d.rest)
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
			Any(result)
		} else if d.rest.len > 0 && (d.rest[0] == `"` || d.rest[0] == `'`) {
			quoted := p.gather_quoted_continuation(d.rest)!
			parse_scalar(quoted)!
		} else {
			parse_scalar(d.rest)!
		}
	}
	return p.register_anchor(d.anchor, value)
}

fn (p &Parser) resolve_alias(name string) Any {
	return p.anchors[name] or { null }
}

// register_anchor associates `value` with `anchor` when the latter is set,
// then returns `value` so call sites can `return p.register_anchor(...)`
// in a single statement instead of branching.
fn (mut p Parser) register_anchor(anchor string, value Any) Any {
	if anchor != '' {
		p.anchors[anchor] = value
	}
	return value
}

// parse_block_scalar parses a `|` (literal) or `>` (folded) block, honoring
// the optional chomp indicator from the header (`-` strip, `+` keep, default
// clip). Indent indicators (`|2`) are tolerated but ignored — the block's
// indentation is auto-detected from the first non-empty content line.
fn (mut p Parser) parse_block_scalar(parent_indent int, header string) !string {
	style, chomp := parse_block_header(header)
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
		// Body is entirely empty/blank. Keep chomp (`+`) still preserves the
		// implicit trailing line breaks; strip and clip yield the empty string.
		if chomp != `+` {
			return ''
		}
		mut blanks := 0
		for p.idx < p.lines.len {
			line := p.lines[p.idx]
			if line.trim_space() != '' {
				break
			}
			blanks++
			p.idx++
		}
		if blanks == 0 {
			blanks = 1
		}
		return '\n'.repeat(blanks)
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
	mut stripped_trailing := 0
	for lines.len > 0 && lines[lines.len - 1] == '' {
		stripped_trailing++
		lines.delete(lines.len - 1)
	}
	body := if style == `|` { lines.join('\n') } else { fold_block_scalar(lines) }
	return apply_chomp(body, chomp, stripped_trailing)
}

struct FlowBalance {
mut:
	bracket   int
	brace     int
	in_single bool
	in_double bool
	escape    bool
}

// collect_flow_continuation gathers subsequent lines into `initial` until the
// `[` / `{` brackets and the active quoted strings are all balanced. YAML 1.2
// allows flow collections to span lines; without this the parser would reject
// anything that wraps. The returned text is what `parse_flow_value` receives.
fn (mut p Parser) collect_flow_continuation(initial string) !string {
	mut bal := FlowBalance{}
	bal.scan(initial)
	if !bal.unbalanced() {
		return initial
	}
	mut sb := strings.new_builder(initial.len * 2)
	sb.write_string(initial)
	for p.idx < p.lines.len && bal.unbalanced() {
		line := p.lines[p.idx]
		segment := if bal.in_single || bal.in_double { line } else { strip_comments(line) }
		trimmed := segment.trim_space()
		if trimmed == '' && !bal.in_single && !bal.in_double {
			p.idx++
			continue
		}
		sb.write_u8(` `)
		sb.write_string(trimmed)
		bal.scan(trimmed)
		p.idx++
	}
	if bal.unbalanced() {
		return error('yaml: unterminated flow collection')
	}
	return sb.str()
}

fn (b &FlowBalance) unbalanced() bool {
	return b.bracket > 0 || b.brace > 0 || b.in_single || b.in_double
}

fn (mut b FlowBalance) scan(s string) {
	for i := 0; i < s.len; i++ {
		ch := s[i]
		if b.in_double {
			if b.escape {
				b.escape = false
			} else if ch == `\\` {
				b.escape = true
			} else if ch == `"` {
				b.in_double = false
			}
			continue
		}
		if b.in_single {
			if ch == `'` {
				if i + 1 < s.len && s[i + 1] == `'` {
					i++
					continue
				}
				b.in_single = false
			}
			continue
		}
		match ch {
			`"` { b.in_double = true }
			`'` { b.in_single = true }
			`[` { b.bracket++ }
			`]` { b.bracket-- }
			`{` { b.brace++ }
			`}` { b.brace-- }
			else {}
		}
	}
}

// parse_block_header reads the `|`/`>` style and the optional `+`/`-` chomp
// indicator from a block-scalar header like `|`, `|-`, `|+`, `>2-`.
fn parse_block_header(s string) (u8, u8) {
	if s.len == 0 {
		return `|`, 0
	}
	style := s[0]
	mut chomp := u8(0)
	for i := 1; i < s.len; i++ {
		c := s[i]
		if c == `+` || c == `-` {
			chomp = c
		}
	}
	return style, chomp
}

// apply_chomp rewrites the block body's trailing whitespace per RFC 9.1.1.2:
// strip removes all trailing newlines, clip keeps a single trailing newline
// when the body is non-empty, keep preserves every original trailing newline.
fn apply_chomp(body string, chomp u8, stripped_trailing int) string {
	return match chomp {
		`-` {
			body
		}
		`+` {
			clipped := if body == '' { '' } else { body + '\n' }
			clipped + '\n'.repeat(stripped_trailing)
		}
		else {
			if body == '' {
				''
			} else {
				body + '\n'
			}
		}
	}
}

// is_ignorable_line returns true when `trimmed` is content the parser must
// skip over: blank lines, document markers, and directive lines that occur
// before the first body line. Directives (`%YAML`, `%TAG`, …) become plain
// text once `directives_done` flips, so the skip is conditional on that flag.
fn (p &Parser) is_ignorable_line(trimmed string) bool {
	return trimmed == '' || trimmed == '---' || trimmed == '...'
		|| (trimmed.starts_with('%') && !p.directives_done)
}

fn (mut p Parser) skip_ignorable() {
	for p.idx < p.lines.len {
		line := p.lines[p.idx]
		trimmed := strip_comments(line).trim_space()
		if p.is_ignorable_line(trimmed) {
			p.idx++
			continue
		}
		// `--- <inline>` (or `---\t…`) folds the document marker away and
		// keeps the inline content as the document body at column 0.
		if (line.starts_with('--- ') || line.starts_with('---\t')) && line.len > 4 {
			p.lines[p.idx] = line[4..]
		}
		p.directives_done = true
		break
	}
}

fn (p &Parser) peek_next_indent() int {
	mut i := p.idx
	for i < p.lines.len {
		line := p.lines[i]
		trimmed := strip_comments(line).trim_space()
		if p.is_ignorable_line(trimmed) {
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

// strip_node_decorators removes a leading anchor (`&id`), a tag (`!Type` or
// `!!Type`), or a sequence of both, from a YAML node's text. The semantics of
// anchors and tags are intentionally not implemented: stripping them lets the
// underlying scalar/collection still parse, which matches the common practical
// case where the document carries decorators but does not rely on them. A
// stand-alone alias (`*name`) cannot be resolved without anchor tracking and
// is therefore left untouched, so the caller still sees that something was
// referenced.
fn strip_node_decorators(s string) string {
	return extract_decorators(s).rest
}

// Decorators carries the anchor / alias / remaining content split off a node.
struct Decorators {
	anchor string
	alias  string
	rest   string
}

// extract_decorators peels leading anchor / alias / tag decorators off `s` and
// returns the anchor name, alias name, and remaining content. At most one
// anchor and one alias are recognized; tags are stripped without being
// returned.
fn extract_decorators(s string) Decorators {
	mut anchor := ''
	mut alias := ''
	mut out := s.trim_left(' \t')
	for {
		if out.len < 2 {
			break
		}
		c := out[0]
		if (c == `&` || c == `*`) && out[1] != ` ` && out[1] != `\t` {
			mut i := 1
			for i < out.len && out[i] != ` ` && out[i] != `\t` {
				i++
			}
			name := out[1..i]
			if c == `&` {
				anchor = name
			} else {
				alias = name
			}
			out = out[i..].trim_left(' \t')
			continue
		}
		if c == `!` {
			mut i := 1
			if i < out.len && out[i] == `!` {
				i++
			}
			for i < out.len && out[i] != ` ` && out[i] != `\t` {
				i++
			}
			out = out[i..].trim_left(' \t')
			continue
		}
		break
	}
	return Decorators{
		anchor: anchor
		alias:  alias
		rest:   out
	}
}

// parse_key resolves a mapping-key token: drops anchor / tag decorators,
// unquotes the result if surrounded by matching `"` or `'` quotes, and
// returns the cleaned key string otherwise.
fn parse_key(src string) !string {
	cleaned := strip_node_decorators(src)
	if cleaned.len >= 2 && ((cleaned[0] == `"` && cleaned[cleaned.len - 1] == `"`)
		|| (cleaned[0] == `'` && cleaned[cleaned.len - 1] == `'`)) {
		return parse_quoted_string(cleaned)
	}
	return cleaned.trim_space()
}

// parse_scalar resolves a scalar token to its YAML 1.2 typed value: quoted
// strings unquote, the case-insensitive keywords `null`/`~`, `true`/`yes`/`on`
// and `false`/`no`/`off` produce the matching constants, integer and float
// literals (with `_` digit separators) parse to `i64`/`u64`/`f64`, and any
// other text falls back to a plain string.
fn parse_scalar(text string) !Any {
	value := strip_node_decorators(text).trim_space()
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
	numeric := if value.contains_u8(`_`) { value.replace('_', '') } else { value }
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

// parse_quoted_string unquotes a YAML scalar wrapped in matching `"` or `'`
// quotes. Single-quoted strings only undouble `''` to `'`. Double-quoted
// strings honor the YAML 1.2 §5.7 escape set (`\b \f \n \r \t \" \\ \/
// \uXXXX`); any other backslash sequence is rejected as malformed.
fn parse_quoted_string(src string) !string {
	if src.len < 2 {
		return error('yaml: invalid quoted string')
	}
	quote := src[0]
	inner := fold_quoted_inner(src[1..src.len - 1])
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
				return error('yaml: unknown escape sequence \\${rune(esc).str()}')
			}
		}

		i++
	}
	return out.bytestr()
}

// fold_quoted_inner applies YAML 1.2 §7.3 line folding rules to the body of a
// quoted scalar (the chars between the opening and closing quote): adjacent
// non-blank content lines fold to a single space, runs of N consecutive line
// breaks collapse to N-1 literal newlines, and leading whitespace inside
// continuation lines is stripped. A leading or trailing empty line folds to a
// single space (§7.3.1).
fn fold_quoted_inner(inner string) string {
	if !inner.contains_u8(`\n`) {
		return inner
	}
	lines := inner.split('\n')
	n := lines.len
	mut trimmed := []string{cap: n}
	for i := 0; i < n; i++ {
		line := lines[i]
		if i == 0 && i != n - 1 {
			trimmed << line.trim_right(' \t')
		} else if i == n - 1 && i != 0 {
			trimmed << line.trim_left(' \t')
		} else {
			trimmed << line.trim(' \t')
		}
	}
	has_pre := trimmed[0] == ''
	has_post := trimmed[n - 1] == ''
	mut sb := strings.new_builder(inner.len)
	if has_pre {
		sb.write_u8(` `)
	}
	start := if has_pre { 1 } else { 0 }
	end := if has_post { n - 1 } else { n }
	mut blanks := 0
	mut wrote := false
	for i := start; i < end; i++ {
		f := trimmed[i]
		if f == '' {
			blanks++
			continue
		}
		if wrote {
			if blanks == 0 {
				sb.write_u8(` `)
			} else {
				for _ in 0 .. blanks {
					sb.write_u8(`\n`)
				}
			}
		}
		blanks = 0
		sb.write_string(f)
		wrote = true
	}
	if has_post {
		sb.write_u8(` `)
	}
	return sb.str()
}

// quoted_terminated reports whether `s` (which starts with a `quote` byte)
// ends with the matching closing quote, taking single-quote `''` doubling and
// double-quote `\` escapes into account.
fn quoted_terminated(s string, quote u8) bool {
	if s.len < 2 || s[0] != quote {
		return false
	}
	mut i := 1
	if quote == `'` {
		for i < s.len {
			if s[i] == `'` {
				if i + 1 < s.len && s[i + 1] == `'` {
					i += 2
					continue
				}
				return i == s.len - 1
			}
			i++
		}
		return false
	}
	mut esc := false
	for i < s.len {
		ch := s[i]
		if esc {
			esc = false
			i++
			continue
		}
		if ch == `\\` {
			esc = true
			i++
			continue
		}
		if ch == `"` {
			return i == s.len - 1
		}
		i++
	}
	return false
}

// gather_quoted_continuation accumulates subsequent lines into a quoted scalar
// that doesn't terminate on its first line. The returned string still wraps
// the original line breaks; `fold_quoted_inner` collapses them later.
fn (mut p Parser) gather_quoted_continuation(initial string) !string {
	if initial.len == 0 {
		return initial
	}
	quote := initial[0]
	if quote != `"` && quote != `'` {
		return initial
	}
	if quoted_terminated(initial, quote) {
		return initial
	}
	mut buf := initial.bytes()
	for p.idx < p.lines.len {
		buf << `\n`
		buf << p.lines[p.idx].bytes()
		p.idx++
		snap := buf.bytestr()
		if quoted_terminated(snap, quote) {
			return snap
		}
	}
	return error('yaml: unterminated quoted string')
}

fn strip_comments(line string) string {
	// Fast path: the overwhelming majority of YAML lines have no `#`.
	if !line.contains_u8(`#`) {
		return line.trim_right(' \t')
	}
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

// write_spaces appends `n` spaces to `sb`.
fn write_spaces(mut sb strings.Builder, n int) {
	for _ in 0 .. n {
		sb.write_u8(` `)
	}
}

// emit_yaml_any streams `value` into `sb` as block-style YAML.
fn emit_yaml_any(mut sb strings.Builder, value Any, indent int) {
	match value {
		map[string]Any { emit_yaml_map(mut sb, value, indent) }
		[]Any { emit_yaml_array(mut sb, value, indent) }
		else { emit_yaml_scalar(mut sb, value) }
	}
}

// emit_yaml_map writes `value` as a block-style YAML mapping. An empty map
// is emitted as the inline `{}` form; otherwise each key is JSON-quoted (see
// `yaml_quote_string`) and nested containers indent one level deeper.
fn emit_yaml_map(mut sb strings.Builder, value map[string]Any, indent int) {
	if value.len == 0 {
		sb.write_string('{}')
		return
	}
	mut first := true
	for key, item in value {
		if !first {
			sb.write_u8(`\n`)
		}
		first = false
		write_spaces(mut sb, indent)
		sb.write_string(yaml_quote_string(key))
		match item {
			map[string]Any, []Any {
				sb.write_u8(`:`)
				sb.write_u8(`\n`)
				emit_yaml_any(mut sb, item, indent + 2)
			}
			else {
				sb.write_string(': ')
				emit_yaml_scalar(mut sb, item)
			}
		}
	}
}

// emit_yaml_array writes `value` as a block-style YAML sequence. An empty
// array is emitted as the inline `[]` form; otherwise each item is prefixed
// with `- ` and nested containers indent one level deeper.
fn emit_yaml_array(mut sb strings.Builder, value []Any, indent int) {
	if value.len == 0 {
		sb.write_string('[]')
		return
	}
	mut first := true
	for item in value {
		if !first {
			sb.write_u8(`\n`)
		}
		first = false
		write_spaces(mut sb, indent)
		match item {
			map[string]Any, []Any {
				sb.write_u8(`-`)
				sb.write_u8(`\n`)
				emit_yaml_any(mut sb, item, indent + 2)
			}
			else {
				sb.write_string('- ')
				emit_yaml_scalar(mut sb, item)
			}
		}
	}
}

// emit_yaml_scalar writes `value` as a single YAML scalar token: strings go
// through `yaml_quote_string`, booleans / numbers / null print their literal
// form. The container branch is type-required by V's exhaustive `match` over
// `Any` but is unreachable: `emit_yaml_any` routes maps and arrays to their
// dedicated emitters before falling back here.
fn emit_yaml_scalar(mut sb strings.Builder, value Any) {
	match value {
		string { sb.write_string(yaml_quote_string(value)) }
		bool { sb.write_string(if value { 'true' } else { 'false' }) }
		f64 { sb.write_string(value.str()) }
		i64 { sb.write_string(value.str()) }
		int { sb.write_string(value.str()) }
		u64 { sb.write_string(value.str()) }
		Null { sb.write_string('null') }
		[]Any, map[string]Any { emit_yaml_any(mut sb, value, 0) }
	}
}

// yaml_quote_string returns `value` as a JSON-style double-quoted literal,
// reusing `json.encode` so escaping matches the JSON spec exactly. Always
// quoting (instead of trying to emit a plain scalar) keeps the output
// unambiguous and lossless under round-trip through `parse_text`.
fn yaml_quote_string(value string) string {
	return json.encode(value)
}

// write_json_escaped_string writes `value` as a JSON string literal directly
// into `sb`. Matches `json2.encode`'s rules: standard short escapes for control
// chars, `\u00XX` for the rest below 0x20, and UTF-8 bytes passed through
// verbatim (no per-byte `\uXXXX` re-escape).
fn write_json_escaped_string(mut sb strings.Builder, value string) {
	sb.write_u8(`"`)
	for c in value {
		match c {
			`"` {
				sb.write_string('\\"')
			}
			`\\` {
				sb.write_string('\\\\')
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\r` {
				sb.write_string('\\r')
			}
			`\t` {
				sb.write_string('\\t')
			}
			`\b` {
				sb.write_string('\\b')
			}
			`\f` {
				sb.write_string('\\f')
			}
			else {
				if c < 0x20 {
					sb.write_string('\\u00')
					hex := '0123456789abcdef'
					sb.write_u8(hex[(c >> 4) & 0xf])
					sb.write_u8(hex[c & 0xf])
				} else {
					sb.write_u8(c)
				}
			}
		}
	}
	sb.write_u8(`"`)
}

// emit_any_as_json writes `a` as a compact JSON document.
fn emit_any_as_json(mut sb strings.Builder, a Any) {
	match a {
		map[string]Any {
			sb.write_u8(`{`)
			mut first := true
			for key, value in a {
				if !first {
					sb.write_u8(`,`)
				}
				first = false
				write_json_escaped_string(mut sb, key)
				sb.write_u8(`:`)
				emit_any_as_json(mut sb, value)
			}
			sb.write_u8(`}`)
		}
		[]Any {
			sb.write_u8(`[`)
			mut first := true
			for value in a {
				if !first {
					sb.write_u8(`,`)
				}
				first = false
				emit_any_as_json(mut sb, value)
			}
			sb.write_u8(`]`)
		}
		string {
			write_json_escaped_string(mut sb, a)
		}
		bool {
			sb.write_string(if a { 'true' } else { 'false' })
		}
		f64 {
			sb.write_string(a.str())
		}
		i64 {
			sb.write_string(a.str())
		}
		int {
			sb.write_string(a.str())
		}
		u64 {
			sb.write_string(a.str())
		}
		Null {
			sb.write_string('null')
		}
	}
}

fn fold_block_scalar(lines []string) string {
	mut out := ''
	mut pending_newlines := 0
	mut started := false
	for line in lines {
		if line == '' {
			pending_newlines++
			continue
		}
		if !started {
			out = '\n'.repeat(pending_newlines) + line
			started = true
		} else if pending_newlines > 0 {
			out += '\n'.repeat(pending_newlines) + line
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
	if value.len == 0 || (value[0] != `|` && value[0] != `>`) {
		return false
	}
	// Allow `|`, `>`, `|-`, `|+`, `>2`, `|3-`, etc. Any other char rules it out
	// (e.g. `> something` is just a plain scalar starting with `>`).
	for i := 1; i < value.len; i++ {
		c := value[i]
		if c != `+` && c != `-` && !(c >= `0` && c <= `9`) {
			return false
		}
	}
	return true
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
			value.parse_int(0, 64) or { return false }
			return true
		}
		value[1..].parse_uint(0, 64) or { return false }
		return true
	}
	value.parse_uint(0, 64) or { return false }
	return true
}

fn is_float(value string) bool {
	if value.len == 0 {
		return false
	}
	if !value.contains('.') && !value.contains('e') && !value.contains('E') {
		return false
	}
	strconv.atof64(value) or { return false }
	return true
}

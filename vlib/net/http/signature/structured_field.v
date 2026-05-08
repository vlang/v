// Minimal Structured Field Values (RFC 8941) helpers, scoped to what
// the Signature-Input and Signature header fields actually use:
//
//   Signature-Input: <label>=(<inner-list>);<params>, <label2>=...
//   Signature:       <label>=:<base64-bytes>:, <label2>=...
//
// We deliberately avoid a full RFC 8941 implementation — every
// production-quality SF parser is a few hundred lines of dispatch and
// edge-case handling, and 95% of that surface is unused here. If we
// later need general SF support, factor this into a sibling module.
module signature

import encoding.base64

// ParamValue is the typed value of a signature parameter. RFC 9421 §2.3
// defines the registered parameters as either Integer (`created`,
// `expires`) or String (`keyid`, `nonce`, `tag`, `alg`). Boolean is
// included for future-proofing per RFC 8941 §3.1.2.
pub type ParamValue = i64 | string | bool

// SignatureEntry is one (label, covered-components, parameters) tuple
// parsed out of a Signature-Input header. The raw
// `signature_params_value` is preserved verbatim - the verifier
// re-emits these exact bytes into the signature base, which is what
// the signer signed. Re-serialising from the parsed params would
// introduce ordering ambiguity (RFC 8941 doesn't pin a canonical map
// order) and break verification of signatures produced by other
// stacks.
pub struct SignatureEntry {
pub:
	label                  string
	components             []string
	params                 map[string]ParamValue
	signature_params_value string
}

// serialize_params formats the named parameters in RFC 8941 §3.1.2 form,
// in the order they were inserted into the map. RFC 9421 does not
// constrain the order of parameters, but our output mirrors `pairs` so
// callers control the wire layout.
pub fn serialize_params(pairs []ParamPair) string {
	mut sb := []string{cap: pairs.len * 2}
	for pair in pairs {
		sb << ';'
		sb << pair.name
		sb << '='
		match pair.value {
			i64 { sb << pair.value.str() }
			string { sb << '"' + escape_string(pair.value) + '"' }
			bool { sb << if pair.value { '?1' } else { '?0' } }
		}
	}
	return sb.join('')
}

// ParamPair is one parameter for `serialize_params`. We use a slice of
// these instead of a map because parameter order matters on the wire
// (it doesn't change verification - the verifier reparses - but it
// makes generated headers deterministic and diff-friendly).
pub struct ParamPair {
pub:
	name  string
	value ParamValue
}

// serialize_inner_list returns the canonical Inner List form (parens
// around space-separated quoted-string items) for the covered
// components list. Each component name is emitted as a quoted string.
pub fn serialize_inner_list(items []string) string {
	mut parts := []string{cap: items.len}
	for it in items {
		parts << '"' + escape_string(it) + '"'
	}
	return '(' + parts.join(' ') + ')'
}

// escape_string applies the RFC 8941 §3.3.3 quoted-string escape rules
// (backslash and double-quote are the only escape-required characters).
fn escape_string(s string) string {
	if !s.contains('\\') && !s.contains('"') {
		return s
	}
	mut out := []u8{cap: s.len + 4}
	for c in s {
		if c == `\\` || c == `"` {
			out << `\\`
		}
		out << c
	}
	return out.bytestr()
}

// parse_signature_input parses one Signature-Input header value into a
// list of entries (one per labelled signature). The grammar follows
// RFC 9421 §4.1 / §4.2 and RFC 8941 §3.2.
pub fn parse_signature_input(input string) ![]SignatureEntry {
	mut p := SfParser{
		src: input
		pos: 0
	}
	mut entries := []SignatureEntry{}
	for {
		p.skip_sp()
		if p.done() {
			break
		}
		label := p.parse_key()!
		p.expect(`=`) or {
			return MalformedMessage{
				reason: 'Signature-Input: expected "=" after label "${label}"'
			}
		}
		value_start := p.pos
		components := p.parse_inner_list()!
		params := p.parse_params()!
		value_end := p.pos
		entries << SignatureEntry{
			label:                  label
			components:             components
			params:                 params
			signature_params_value: p.src[value_start..value_end]
		}
		p.skip_sp()
		if p.done() {
			break
		}
		p.expect(`,`) or {
			return MalformedMessage{
				reason: 'Signature-Input: expected "," between entries near offset ${p.pos}'
			}
		}
	}
	return entries
}

// parse_signature parses a Signature header value into a label →
// signature-bytes map. Each value in the source is a `:base64:` byte
// sequence per RFC 8941 §3.3.5.
pub fn parse_signature(input string) !map[string][]u8 {
	mut p := SfParser{
		src: input
		pos: 0
	}
	mut out := map[string][]u8{}
	for {
		p.skip_sp()
		if p.done() {
			break
		}
		label := p.parse_key()!
		p.expect(`=`) or {
			return MalformedMessage{
				reason: 'Signature: expected "=" after label "${label}"'
			}
		}
		bytes := p.parse_byte_sequence()!
		out[label] = bytes
		p.skip_sp()
		if p.done() {
			break
		}
		p.expect(`,`) or {
			return MalformedMessage{
				reason: 'Signature: expected "," between entries near offset ${p.pos}'
			}
		}
	}
	return out
}

// SfParser is a tiny hand-written cursor over a Structured Field byte
// string. Dedicated to the small subset of RFC 8941 we use - keeping
// the state on the struct lets us return precise positions in errors.
struct SfParser {
	src string
mut:
	pos int
}

fn (mut p SfParser) done() bool {
	return p.pos >= p.src.len
}

fn (mut p SfParser) peek() u8 {
	return if p.pos < p.src.len { p.src[p.pos] } else { 0 }
}

fn (mut p SfParser) skip_sp() {
	for p.pos < p.src.len && (p.src[p.pos] == ` ` || p.src[p.pos] == `\t`) {
		p.pos++
	}
}

fn (mut p SfParser) expect(c u8) ! {
	if p.pos >= p.src.len || p.src[p.pos] != c {
		return error('expected "${rune(c)}"')
	}
	p.pos++
}

// parse_key parses a Dictionary key (RFC 8941 §3.2 + §3.1.2). Per
// RFC 9421 §2.3, signature labels are the same lexical form.
fn (mut p SfParser) parse_key() !string {
	if p.done() {
		return MalformedMessage{
			reason: 'unexpected end of input expecting key'
		}
	}
	c := p.peek()
	if !(c == `*` || (c >= `a` && c <= `z`)) {
		return MalformedMessage{
			reason: 'invalid key start byte 0x${c.hex()} at offset ${p.pos}'
		}
	}
	start := p.pos
	for p.pos < p.src.len {
		ch := p.src[p.pos]
		if (ch >= `a` && ch <= `z`) || (ch >= `0` && ch <= `9`) || ch == `_`
			|| ch == `-` || ch == `.` || ch == `*` {
			p.pos++
		} else {
			break
		}
	}
	return p.src[start..p.pos]
}

fn (mut p SfParser) parse_inner_list() ![]string {
	p.expect(`(`) or {
		return MalformedMessage{
			reason: 'expected "(" starting inner list at offset ${p.pos}'
		}
	}
	mut items := []string{}
	for {
		p.skip_sp()
		if p.done() {
			return MalformedMessage{
				reason: 'unterminated inner list'
			}
		}
		if p.peek() == `)` {
			p.pos++
			return items
		}
		items << p.parse_string()!
		// A list item may itself carry parameters (`;k=v`); RFC 9421's
		// covered-components list never sets these, but RFC 8941 allows
		// them so we silently skip them to stay forward-compatible.
		for p.pos < p.src.len && p.src[p.pos] == `;` {
			p.pos++
			p.parse_key()!
			if p.pos < p.src.len && p.src[p.pos] == `=` {
				p.pos++
				p.parse_bare_item()!
			}
		}
	}
	return items
}

// parse_params consumes `;name=value` pairs starting at the current
// position and returns them as a name-keyed map. Callers that need
// the original wire-ordered serialisation must work from the raw
// substring rather than this map (RFC 8941 doesn't pin a canonical
// re-emission order).
fn (mut p SfParser) parse_params() !map[string]ParamValue {
	mut m := map[string]ParamValue{}
	for p.pos < p.src.len && p.src[p.pos] == `;` {
		p.pos++
		p.skip_sp()
		name := p.parse_key()!
		mut value := ParamValue(true)
		if p.pos < p.src.len && p.src[p.pos] == `=` {
			p.pos++
			value = p.parse_bare_item()!
		}
		m[name] = value
	}
	return m
}

fn (mut p SfParser) parse_bare_item() !ParamValue {
	if p.done() {
		return MalformedMessage{
			reason: 'unexpected end of input'
		}
	}
	c := p.peek()
	if c == `"` {
		return ParamValue(p.parse_string()!)
	}
	if c == `?` {
		p.pos++
		if p.pos >= p.src.len {
			return MalformedMessage{
				reason: 'truncated boolean literal'
			}
		}
		b := p.src[p.pos]
		if b != `0` && b != `1` {
			return MalformedMessage{
				reason: 'invalid boolean literal "?${rune(b)}"'
			}
		}
		p.pos++
		return ParamValue(b == `1`)
	}
	if c == `-` || (c >= `0` && c <= `9`) {
		return ParamValue(p.parse_integer()!)
	}
	return MalformedMessage{
		reason: 'unsupported bare item byte 0x${c.hex()} at offset ${p.pos} (this module models only string/integer/boolean parameter values)'
	}
}

fn (mut p SfParser) parse_string() !string {
	p.expect(`"`) or {
		return MalformedMessage{
			reason: 'expected quoted string at offset ${p.pos}'
		}
	}
	mut out := []u8{}
	for p.pos < p.src.len {
		c := p.src[p.pos]
		if c == `"` {
			p.pos++
			return out.bytestr()
		}
		if c == `\\` {
			p.pos++
			if p.pos >= p.src.len {
				return MalformedMessage{
					reason: 'truncated escape sequence in string'
				}
			}
			n := p.src[p.pos]
			if n != `\\` && n != `"` {
				return MalformedMessage{
					reason: 'invalid escape "\\${rune(n)}" in string'
				}
			}
			out << n
			p.pos++
			continue
		}
		out << c
		p.pos++
	}
	return MalformedMessage{
		reason: 'unterminated string'
	}
}

fn (mut p SfParser) parse_integer() !i64 {
	start := p.pos
	if p.pos < p.src.len && p.src[p.pos] == `-` {
		p.pos++
	}
	digits_start := p.pos
	for p.pos < p.src.len && p.src[p.pos] >= `0` && p.src[p.pos] <= `9` {
		p.pos++
	}
	if p.pos == digits_start {
		return MalformedMessage{
			reason: 'integer literal with no digits at offset ${start}'
		}
	}
	return p.src[start..p.pos].i64()
}

fn (mut p SfParser) parse_byte_sequence() ![]u8 {
	p.expect(`:`) or {
		return MalformedMessage{
			reason: 'expected ":" starting byte sequence at offset ${p.pos}'
		}
	}
	start := p.pos
	for p.pos < p.src.len && p.src[p.pos] != `:` {
		p.pos++
	}
	if p.pos >= p.src.len {
		return MalformedMessage{
			reason: 'unterminated byte sequence'
		}
	}
	encoded := p.src[start..p.pos]
	p.pos++ // closing ':'
	return base64.decode(encoded)
}

// encode_byte_sequence produces the wire form `:base64:` used inside
// the Signature header.
pub fn encode_byte_sequence(bytes []u8) string {
	return ':' + base64.encode(bytes) + ':'
}

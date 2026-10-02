module json5

import strings

// EncodeOpts controls how `encode` writes JSON5 text.
@[params]
pub struct EncodeOpts {
pub:
	indent          string // indent unit; the zero value writes compact output
	single_quotes   bool   // write strings with `'` instead of `"`
	unquoted_keys   bool   // write object keys bare when they are valid identifiers
	trailing_commas bool   // write a comma after the last array element and member
}

// default_encode_opts writes compact output that is also valid JSON.
pub const default_encode_opts = EncodeOpts{}

// hex_digits maps a nibble to its lowercase hex digit. It is a byte array
// because indexing a `const` string in a generic-heavy module is not reliable
// in this compiler.
const hex_digits = [u8(`0`), `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `a`, `b`, `c`, `d`, `e`,
	`f`]

// encode writes `value` as compact JSON, which is also valid JSON5. A type may
// customise the output by defining `to_json5() string`.
pub fn encode[T](value T) string {
	return encode_with_opts[T](value, default_encode_opts)
}

// encode_with_opts writes `value` as JSON5 text shaped by `opts`.
pub fn encode_with_opts[T](value T, opts EncodeOpts) string {
	return encode_any(to_any(value), opts)
}

// to_any converts a V value into the dynamic `Any` tree that the encoder writes.
// A type with a `to_json5()` method keeps that text verbatim.
fn to_any[T](value T) Any {
	// `Any` is checked first: it is a sumtype, and the method walk below must not
	// run for it.
	$if T is Any {
		return value
	} $else $if T is $option {
		return option_to_any(value)
	}
	$for method in T.methods {
		$if method.name == 'to_json5' {
			// `Raw` marks text that is already JSON5 and must not be re-quoted.
			return Raw{
				text: value.$method()
			}
		}
	}
	$if T is bool {
		return value
	} $else $if T is string {
		return value
	} $else $if T is $enum {
		return enum_to_string(value)
	} $else $if T is $int {
		return Number{
			text: value.str()
		}
	} $else $if T is $float {
		return float_to_number(value)
	} $else {
		return container_to_any(value)
	}
}

// option_to_any encodes the payload with its concrete type rather than the option type.
fn option_to_any[P](value ?P) Any {
	if present := value {
		return to_any[P](present)
	}
	return null
}

// container_to_any converts maps, arrays and structs into `Any`.
fn container_to_any[T](value T) Any {
	$if T.unaliased_typ is $map {
		mut m := map[string]Any{}
		for key, item in value {
			m[key.str()] = to_any(item)
		}
		return m
	} $else $if T.unaliased_typ is $array_dynamic {
		mut items := []Any{cap: value.len}
		for item in value {
			items << to_any(item)
		}
		return items
	} $else $if T.unaliased_typ is $array_fixed {
		mut items := []Any{cap: value.len}
		for item in value {
			items << to_any(item)
		}
		return items
	} $else $if T.unaliased_typ is $struct {
		mut m := map[string]Any{}
		// `$for` unrolls to straight-line code, so the `@[skip]` check is a
		// nested `if` rather than a loop with `continue`. An embedded struct is
		// flattened, matching the decoder.
		$for field in T.fields {
			key := field_key_name(field.name, field.attrs)
			if key != '' {
				$if field.is_embed {
					$if field.unaliased_typ is $struct {
						for k, v in to_any(value.$(field.name)).as_map() {
							m[k] = v
						}
					}
				} $else {
					m[key] = to_any(value.$(field.name))
				}
			}
		}
		return m
	} $else {
		return null
	}
}

// enum_to_string returns the document spelling of an enum value.
fn enum_to_string[T](value T) string {
	$if T.unaliased_typ is $enum {
		mut name := ''
		$for variant in T.values {
			if variant.value == value {
				// A `@[json5: 'name']` attribute overrides the spelling. The
				// lookup is inlined because `variant.attrs` is not a usable type
				// expression in this compiler.
				name = variant.name
				for attr in variant.attrs {
					if attr.starts_with('json5:') {
						name = unquote(attr.all_after(':').trim_space())
					}
				}
			}
		}
		return name
	} $else {
		return ''
	}
}

// float_to_number renders a float as JSON5 number text, using the JSON5
// spellings for the non-finite values.
fn float_to_number[T](value T) Number {
	f := f64(value)
	if f != f {
		return Number{
			text: 'NaN'
		}
	}
	// Halving an infinity keeps it infinite, so the identity `f * 0.5 == f` only
	// holds for the infinities.
	if f > 0.0 && f * 0.5 == f {
		return Number{
			text: 'Infinity'
		}
	}
	if f < 0.0 && f * 0.5 == f {
		return Number{
			text: '-Infinity'
		}
	}
	return Number{
		text: f.str()
	}
}

// encode_any writes an `Any` tree as JSON5 text.
pub fn encode_any(value Any, opts EncodeOpts) string {
	mut b := strings.new_builder(1024)
	encode_value(value, opts, 0, mut b)
	return b.str()
}

// encode_value appends the JSON5 text of `value` to `mut b`.
fn encode_value(value Any, opts EncodeOpts, depth int, mut b strings.Builder) {
	match value {
		Null {
			b.write_string('null')
		}
		bool {
			// Bind the text before writing: passing an inline `if` expression
			// next to the `mut` receiver makes the compiler hoist the receiver
			// into a snapshot copy, so the write is lost.
			text := if value { 'true' } else { 'false' }
			b.write_string(text)
		}
		Number {
			b.write_string(value.text)
		}
		Raw {
			b.write_string(value.text)
		}
		f64 {
			b.write_string(float_to_number(value).text)
		}
		string {
			write_string(value, opts.single_quotes, mut b)
		}
		[]Any {
			write_array(value, opts, depth, mut b)
		}
		map[string]Any {
			write_object(value, opts, depth, mut b)
		}
	}
}

// write_array appends an array, honouring indentation and trailing commas.
fn write_array(items []Any, opts EncodeOpts, depth int, mut b strings.Builder) {
	if items.len == 0 {
		b.write_string('[]')
		return
	}
	pretty := opts.indent != ''
	b.write_u8(`[`)
	for i, item in items {
		if i > 0 {
			b.write_u8(`,`)
		}
		if pretty {
			b.write_u8(`\n`)
			write_indent(mut b, opts.indent, depth + 1)
		}
		encode_value(item, opts, depth + 1, mut b)
	}
	if pretty && opts.trailing_commas {
		// The comma belongs to the last element, so it is written before the
		// closing bracket is indented onto its own line.
		b.write_u8(`,`)
	}
	if pretty {
		b.write_u8(`\n`)
		write_indent(mut b, opts.indent, depth)
	}
	b.write_u8(`]`)
}

// write_object appends an object, honouring indentation, bare keys and trailing
// commas.
fn write_object(members map[string]Any, opts EncodeOpts, depth int, mut b strings.Builder) {
	if members.len == 0 {
		b.write_string('{}')
		return
	}
	pretty := opts.indent != ''
	b.write_u8(`{`)
	mut written := 0
	for key, value in members {
		if written > 0 {
			b.write_u8(`,`)
		}
		if pretty {
			b.write_u8(`\n`)
			write_indent(mut b, opts.indent, depth + 1)
		}
		if opts.unquoted_keys && is_identifier(key) {
			b.write_string(key)
		} else {
			write_string(key, opts.single_quotes, mut b)
		}
		b.write_u8(`:`)
		if pretty {
			b.write_u8(` `)
		}
		encode_value(value, opts, depth + 1, mut b)
		written++
	}
	if pretty && opts.trailing_commas {
		b.write_u8(`,`)
	}
	if pretty {
		b.write_u8(`\n`)
		write_indent(mut b, opts.indent, depth)
	}
	b.write_u8(`}`)
}

// write_indent appends `depth` copies of the indent unit.
fn write_indent(mut b strings.Builder, unit string, depth int) {
	for _ in 0 .. depth {
		b.write_string(unit)
	}
}

// is_identifier reports whether `key` may be written as a bare JSON5 key.
fn is_identifier(key string) bool {
	if key.len == 0 {
		return false
	}
	mut first := true
	for ch in key.runes() {
		if first {
			if !is_ident_start(int(ch)) {
				return false
			}
			first = false
			continue
		}
		if !is_ident_part(int(ch)) {
			return false
		}
	}
	return !first
}

// write_string appends `value` as a quoted JSON5 string.
fn write_string(value string, single_quotes bool, mut b strings.Builder) {
	quote := if single_quotes { `'` } else { `"` }
	b.write_u8(quote)
	for ch in value.runes() {
		match ch {
			`\\` {
				b.write_u8(`\\`)
				b.write_u8(`\\`)
			}
			`\n` {
				b.write_u8(`\\`)
				b.write_u8(`n`)
			}
			`\r` {
				b.write_u8(`\\`)
				b.write_u8(`r`)
			}
			`\t` {
				b.write_u8(`\\`)
				b.write_u8(`t`)
			}
			rune(0x08) {
				b.write_u8(`\\`)
				b.write_u8(`b`)
			}
			rune(0x0C) {
				b.write_u8(`\\`)
				b.write_u8(`f`)
			}
			quote {
				// Only the chosen delimiter needs escaping; the other quote
				// character is literal.
				b.write_u8(`\\`)
				b.write_u8(u8(ch))
			}
			else {
				if ch < 0x20 {
					b.write_u8(`\\`)
					b.write_u8(`u`)
					b.write_string(hex4(ch))
				} else {
					b.write_rune(ch)
				}
			}
		}
	}
	b.write_u8(quote)
}

// hex4 returns the four-digit lowercase hex form of `ch`.
fn hex4(ch rune) string {
	mut out := []u8{len: 4}
	mut v := u32(ch)
	for i := 3; i >= 0; i-- {
		out[i] = hex_digits[v & 0xF]
		v = v >> 4
	}
	return out.bytestr()
}

// quote_string returns `value` as a quoted JSON5 string.
pub fn quote_string(value string) string {
	mut b := strings.new_builder(64)
	write_string(value, false, mut b)
	return b.str()
}

// reindent reparses `text` and writes it back with two-space indentation, which
// is a convenience for tidying configuration files.
pub fn reindent(text string) !string {
	return encode_with_opts(parse(text)!, EncodeOpts{
		indent: '  '
	})
}

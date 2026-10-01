import yaml

struct EscapedUnicode {}

pub fn (value EscapedUnicode) to_json() string {
	return r'"\uD83D\uDE00"'
}

struct RootEscape {
	s string
}

pub fn (value RootEscape) to_json() string {
	return r'{"s": "\uD83D\uDE00"}'
}

// json2.encode appends a custom `to_json()` result verbatim, so a valid JSON
// escape reaches the YAML parser. A surrogate pair is one character and has to
// survive the round trip.
fn test_surrogate_pair_from_custom_encoder() ! {
	assert yaml.encode[EscapedUnicode](EscapedUnicode{}) == '"😀"'
}

fn test_surrogate_pair_in_nested_values() ! {
	encoded := yaml.encode[RootEscape](RootEscape{})
	assert encoded.contains('😀')
	assert yaml.decode[string](r'"\uD83D\uDE00"')! == '😀'
	// The first and the last supplementary code points.
	assert yaml.decode[string](r'"\uD83C\uDFFF"')! == rune(0x1f3ff).str()
	assert yaml.decode[string](r'"\uDBFF\uDFFF"')! == rune(0x10ffff).str()
}

fn test_surrogate_pair_as_mapping_key() ! {
	doc := yaml.parse_text(r'"\uD83D\uDE00": value')!
	assert doc.value('😀').string() == 'value'
}

fn test_escapes_outside_the_surrogate_ranges_still_work() ! {
	assert yaml.decode[string](r'"A"')! == 'A'
	assert yaml.decode[string](r'"\n\t"')! == '\n\t'
	assert yaml.decode[string](r'"\u00e9"')! == 'é'
	// The edges of the BMP ranges around the surrogate block.
	assert yaml.decode[string](r'"\uD7FF"')! == rune(0xd7ff).str()
	assert yaml.decode[string](r'"\uE000"')! == rune(0xe000).str()
}

fn test_escaped_backslash_u_stays_text() ! {
	// `\\` resolves first, so the `u` that follows is an ordinary character.
	assert yaml.decode[string](r'"a\\u0041"')! == 'a\\u0041'
}

fn test_unpaired_surrogates_are_rejected() ! {
	// A high surrogate with no following escape.
	if _ := yaml.decode[string](r'"\uD83D"') {
		assert false, 'lone high surrogate must not decode'
	}
	// A high surrogate followed by an escape that is not a low surrogate.
	if _ := yaml.decode[string](r'"\uD83D\u0041"') {
		assert false, 'high surrogate followed by a non-surrogate must not decode'
	}
	// A low surrogate on its own.
	if _ := yaml.decode[string](r'"\uDE00"') {
		assert false, 'lone low surrogate must not decode'
	}
	// A truncated pair at the end of the string.
	if _ := yaml.decode[string](r'"\uD83D\uD8"') {
		assert false, 'truncated pair must not decode'
	}
}

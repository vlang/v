module asn1

// Coverage for the `*_with_field_options` entry points and the helpers they
// lean on: `encode_with_field_options`, `decode_with_field_options`,
// `unwrap_with_field_options`, `Element.set_default_value`,
// `FieldOptions.install_default` and `new_key_default`. Every byte string below
// was measured on this tree.

// The element used across the table: UTF8String "hi", DER-encoded as
// `0c 02 68 69`.
const wrapped_object = Utf8String{
	value: 'hi'
}

struct EncodeWithFieldOptionsCase {
	// options string handed to FieldOptions.from_string
	opt  string
	want string
}

const encode_with_field_options_cases = [
	// No wrapping: plain DER of the UTF8String.
	EncodeWithFieldOptionsCase{
		opt:  ''
		want: '0c026869'
	},
	// [5] IMPLICIT UTF8String overwrites the tag with 0x85.
	EncodeWithFieldOptionsCase{
		opt:  'context_specific:5;implicit;inner:12'
		want: '85026869'
	},
	// [5] EXPLICIT UTF8String is constructed and holds the inner TLV.
	EncodeWithFieldOptionsCase{
		opt:  'context_specific:5;explicit;inner:0x0c'
		want: 'a5040c026869'
	},
	EncodeWithFieldOptionsCase{
		opt:  'application:4;implicit;inner:12'
		want: '44026869'
	},
	EncodeWithFieldOptionsCase{
		opt:  'private:4;explicit;inner:12'
		want: 'e4040c026869'
	},
]

fn test_encode_with_field_options_matches_the_plain_and_wrapped_forms() {
	for c in encode_with_field_options_cases {
		fo := FieldOptions.from_string(c.opt) or { FieldOptions{} }
		got := encode_with_field_options(wrapped_object, fo)!
		assert got.hex() == c.want, 'options "${c.opt}": ${got.hex()}, want ${c.want}'
	}
}

// NOTE: an `optional` field with a present bit of false encodes to nothing,
// but with the bit set to true it does not encode at all on this tree:
// `encode_with_rule` sees an `Optional` and calls `Optional.encode()`, which
// calls `encode_with_rule` again, so the process dies with an access violation.
// That path is deliberately not asserted here.
fn test_encode_with_field_options_drops_an_absent_optional() {
	fo := FieldOptions.from_string('optional')!
	got := encode_with_field_options(wrapped_object, fo)!
	assert got.len == 0, 'expected no bytes, got ${got.hex()}'
}

fn test_encode_with_field_options_skips_a_value_equal_to_its_default() {
	mut fo := FieldOptions.from_string('has_default')!
	el := Element(wrapped_object)
	el.set_default_value(mut fo, wrapped_object)!
	got := encode_with_field_options(wrapped_object, fo)!
	assert got.len == 0, 'value equal to the default should not be serialized, got ${got.hex()}'
}

fn test_encode_with_field_options_serializes_a_value_that_differs_from_its_default() {
	mut fo := FieldOptions.from_string('has_default')!
	el := Element(wrapped_object)
	el.set_default_value(mut fo, Utf8String.new('xx')!)!
	got := encode_with_field_options(wrapped_object, fo)!
	assert got.hex() == '0c026869', 'got ${got.hex()}'
}

fn test_encode_with_field_options_rejects_options_without_an_inner_tag() {
	// Built directly: `FieldOptions.from_string` refuses a wrapper without the
	// `inner` part up front, so this is the only way to reach the validator
	// inside `encode_with_field_options`.
	no_inner := FieldOptions{
		cls:    'context_specific'
		tagnum: 5
		mode:   'implicit'
	}
	encode_with_field_options(wrapped_object, no_inner) or {
		assert err.msg() == 'You provides incorrect inner number'
		return
	}
	assert false, 'wrapper options without an inner tag should have failed'
}

fn test_encode_with_field_options_rejects_wrapping_into_the_universal_class() {
	universal := FieldOptions{
		cls:    'universal'
		tagnum: 5
		mode:   'implicit'
		inner:  '12'
	}
	encode_with_field_options(wrapped_object, universal) or {
		assert err.msg() == 'Get unexpected fo.cls value'
		return
	}
	assert false, 'wrapping into the universal class should have failed'
}

fn test_decode_with_field_options_round_trips_the_wrapped_forms() {
	original := Element(wrapped_object)

	implicit_bytes := [u8(0x85), 0x02, 0x68, 0x69]
	got := decode_with_field_options(implicit_bytes, FieldOptions.from_string('context_specific:5;implicit;inner:12')!)!
	assert got.equal(original)

	explicit_bytes := [u8(0xa5), 0x04, 0x0c, 0x02, 0x68, 0x69]
	got2 := decode_with_field_options(explicit_bytes, FieldOptions.from_string('context_specific:5;explicit;inner:0x0c')!)!
	assert got2.equal(original)
}

fn test_decode_with_field_options_rejects_empty_bytes() {
	decode_with_field_options([]u8{}, FieldOptions.from_string('optional')!) or {
		assert err.msg() == 'Empty bytes'
		return
	}
	assert false, 'decode_with_field_options on empty bytes should have failed'
}

fn test_decode_with_field_options_builds_the_optional_placeholder() {
	implicit := FieldOptions.from_string('context_specific:5;implicit;inner:12;optional')!

	// The outer tag matches, so the optional is present and the parser
	// recovers the wrapped UTF8String.
	present := decode_with_field_options([u8(0x85), 0x02, 0x68, 0x69], implicit)!
	assert present.tag().tag_class() == .context_specific
	assert present.tag().tag_number() == 5

	// A non matching tag means the optional is absent: the placeholder is a
	// RawElement holding the expected tag with empty content.
	absent := decode_with_field_options([u8(0x02), 0x01, 0x05], implicit)!
	assert absent.tag().tag_class() == .context_specific
	assert absent.tag().tag_number() == 5
	assert absent.payload()!.len == 0, 'absent payload ${absent.payload()!.hex()}'
}

// NOTE: `decode_with_field_options` cannot be used for an unwrapped element:
// it calls `fo.wrapper_tag()` before it looks at `fo.cls == ''`, and that
// method errors on an empty class. `decode_with_options(bytes, '')` is the
// entry point that covers the plain case.
fn test_decode_with_field_options_rejects_empty_options() {
	got := decode_with_field_options([u8(0x0c), 0x02, 0x68, 0x69], FieldOptions{}) or {
		assert err.msg() == 'You cant build wrapper tag from empty string'
		return
	}
	assert false, 'got ${got.tag().tag_number()} where an error was expected'
}

fn test_unwrap_with_field_options_reverses_the_wrapping() {
	original := Element(wrapped_object)

	implicit := FieldOptions.from_string('context_specific:5;implicit;inner:12')!
	wrapped_implicit := decode(encode_with_field_options(wrapped_object, implicit)!)!
	assert wrapped_implicit.unwrap_with_field_options(implicit)!.equal(original)

	explicit := FieldOptions.from_string('context_specific:5;explicit;inner:0x0c')!
	wrapped_explicit := decode(encode_with_field_options(wrapped_object, explicit)!)!
	assert wrapped_explicit.unwrap_with_field_options(explicit)!.equal(original)
}

fn test_unwrap_with_field_options_leaves_an_unwrapped_element_alone() {
	// An empty `cls` means no unwrapping, so the element comes back as is.
	el := decode([u8(0x0c), 0x02, 0x68, 0x69])!
	assert el.unwrap_with_field_options(FieldOptions{})!.equal(el)
}

fn test_unwrap_with_field_options_rejects_a_universal_element() {
	el := decode([u8(0x0c), 0x02, 0x68, 0x69])!
	fo := FieldOptions.from_string('context_specific:5;implicit;inner:12')!
	// A UTF8String is universal, and unwrapping only applies to wrapped elements.
	el.unwrap_with_field_options(fo) or {
		assert err.msg() == 'you cant unwrap universal element'
		return
	}
	assert false, 'unwrapping a universal element should have failed'
}

// ---------------------------------------------------------------------------
// default value plumbing
// ---------------------------------------------------------------------------

fn test_set_default_value_installs_a_value_with_a_matching_tag() {
	mut fo := FieldOptions.from_string('has_default')!
	el := Element(wrapped_object)
	el.set_default_value(mut fo, wrapped_object)!
	assert fo.default_value != none
	installed := fo.default_value or { Element(Null{}) }
	assert installed.equal(el)
}

fn test_set_default_value_rejects_a_value_with_another_tag() {
	mut fo := FieldOptions.from_string('has_default')!
	el := Element(wrapped_object)
	el.set_default_value(mut fo, Integer.from_int(1)) or {
		assert err.msg() == 'unmatching tag of default value'
		return
	}
	assert false, 'a default value with a different tag should have failed'
}

fn test_set_default_value_requires_the_has_default_flag() {
	mut fo := FieldOptions{}
	el := Element(wrapped_object)
	el.set_default_value(mut fo, wrapped_object) or {
		assert err.msg() == 'you can not install default value when has_default being not set'
		return
	}
	assert false, 'installing a default without has_default should have failed'
}

fn test_install_default_fills_the_optional_default_value_field() {
	mut fo := FieldOptions.from_string('has_default')!
	fo.install_default(wrapped_object, false)!
	installed := fo.default_value or { Element(Null{}) }
	assert installed.equal(Element(wrapped_object))
}

fn test_install_default_rejects_options_without_the_has_default_flag() {
	mut fo := FieldOptions{}
	fo.install_default(wrapped_object, false) or {
		assert err.msg() == 'you can not install default value when has_default being not set'
		return
	}
	assert false, 'install_default without has_default should have failed'
}

fn test_install_default_refuses_to_replace_a_value_without_force() {
	mut fo := FieldOptions.from_string('has_default')!
	fo.install_default(wrapped_object, false)!
	mut rejected := false
	fo.install_default(Utf8String.new('xx')!, false) or {
		rejected = true
		assert err.msg() == 'set force to overide'
	}
	assert rejected, 'a second install_default without force should have failed'
	unchanged := fo.default_value or { Element(Null{}) }
	assert unchanged.equal(Element(wrapped_object))

	fo.install_default(Utf8String.new('xx')!, true)!
	installed := fo.default_value or { Element(Null{}) }
	assert installed.equal(Element(Utf8String.new('xx')!))
}

// ---------------------------------------------------------------------------
// new_key_default
// ---------------------------------------------------------------------------

fn test_new_key_default_returns_an_empty_usable_map() {
	keys := new_key_default()
	assert keys.len == 0, 'a fresh KeyDefault should be empty, got ${keys.len}'

	mut filled := new_key_default()
	filled['a'] = Integer.from_int(1)
	filled['b'] = Integer.from_int(2)
	assert filled.len == 2, 'filled KeyDefault has ${filled.len} entries'
	assert filled['a'].equal(Integer.from_int(1))
	assert filled['b'].equal(Integer.from_int(2))
}

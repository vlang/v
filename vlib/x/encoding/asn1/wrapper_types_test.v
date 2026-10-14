module asn1

// Coverage for the rarely used wrapper types of this module: `Choice`,
// `Optional`, `Enumerated`, `GeneralString`, `AnyDefinedBy`,
// `ApplicationElement` and `PrivateELement`. All expected values were measured
// on this tree from hand-built inputs.

// ---------------------------------------------------------------------------
// CHOICE
// ---------------------------------------------------------------------------

fn choice_cases() []Element {
	return [Element(Integer.from_int(5)), Element(Integer.from_int(6))]
}

fn test_choice_delegates_tag_and_payload_to_the_chosen_element() {
	el := Integer.from_int(5)
	c := Choice.new(el, choice_cases())!
	assert c.tag().equal(el.tag())
	assert c.tag().tag_number() == int(TagType.integer)
	assert c.tag().tag_class() == .universal
	assert c.payload()! == [u8(0x05)], 'payload ${c.payload()!.hex()}'
}

fn test_choice_new_rejects_an_empty_choices_list() {
	Choice.new(Integer.from_int(5), []Element{}) or {
		assert err.msg() == 'bad choices list'
		return
	}
	assert false, 'Choice.new with an empty choices list should have failed'
}

fn test_choice_new_rejects_a_chosen_element_outside_the_choices() {
	Choice.new(Integer.from_int(99), choice_cases()) or {
		assert err.msg() == 'choosen element not within choices list'
		return
	}
	assert false, 'Choice.new with a chosen element outside the list should have failed'
}

fn test_choice_new_rejects_a_choices_list_with_duplicate_entries() {
	duplicates := [Element(Integer.from_int(5)), Element(Integer.from_int(5))]
	Choice.new(Integer.from_int(5), duplicates) or {
		assert err.msg() == 'multiples element within choices matching with choosen element'
		return
	}
	assert false, 'Choice.new with duplicates should have failed'
}

fn test_choice_set_size_bounds_the_choices_list() {
	mut c := Choice.new(Integer.from_int(5), choice_cases())!
	c.set_size(2)!

	c.set_size(256) or {
		assert err.msg() == 'Your size exceed max_choices_size'
		return
	}
	assert false, 'set_size(256) should have been rejected'
}

fn test_choice_set_size_rejects_a_size_smaller_than_the_list() {
	mut c := Choice.new(Integer.from_int(5), choice_cases())!
	c.set_size(1) or {
		assert err.msg() == 'Your choices list exceed size'
		return
	}
	assert false, 'set_size(1) on a two entry list should have been rejected'
}

fn test_choice_choose_swaps_the_chosen_element_within_the_list() {
	mut c := Choice.new(Integer.from_int(5), choice_cases())!
	c.choose(Integer.from_int(6))!
	assert c.payload()! == [u8(0x06)], 'payload after choose: ${c.payload()!.hex()}'

	c.choose(Integer.from_int(7)) or {
		assert err.msg() == 'element not within choices'
		return
	}
	assert false, 'choose() with an element outside the list should have failed'
}

// ---------------------------------------------------------------------------
// OPTIONAL
// ---------------------------------------------------------------------------

fn test_optional_delegates_tag_and_payload_to_the_wrapped_element() {
	el := Integer.from_int(5)
	opt := Optional.new(el, true)!
	assert opt.tag().equal(el.tag())
	assert opt.tag().tag_number() == int(TagType.integer)
	assert opt.payload()! == [u8(0x05)], 'payload ${opt.payload()!.hex()}'
}

fn test_optional_new_rejects_a_nested_optional() {
	inner := Optional.new(Integer.from_int(5), true)!
	Optional.new(inner, true) or {
		assert err.msg() == 'recursive optional is not allowed'
		return
	}
	assert false, 'Optional.new of an Optional should have failed'
}

fn test_optional_set_present_bit_then_recovers_the_underlying_element() {
	mut opt := Optional.new(Integer.from_int(5), false)!
	opt.set_present_bit(true)
	// The payload survives the trip through tag/content and parses back.
	el := opt.into_element()!
	assert el.equal(Integer.from_int(5))
	obj := opt.into_object[Integer]()!
	assert obj.as_i64()! == 5
}

fn test_optional_into_object_rejects_a_type_the_element_does_not_hold() {
	opt := Optional.new(Integer.from_int(5), true)!
	opt.into_object[Null]() or {
		assert err.msg() == 'Element el does not holding T}'
		return
	}
	assert false, 'into_object[Null] on an Integer should have failed'
}

// ---------------------------------------------------------------------------
// ENUMERATED
// ---------------------------------------------------------------------------

struct EnumeratedPayloadCase {
	value int
	want  string
}

// NOTE: these payloads are what this tree produces, not the DER encoding of the
// value. `Enumerated.payload_with_rule` writes
// `u8(e.value >> u32(n - 1 - j) * 8)`; here the `* 8` binds to the shift result
// rather than to the shift amount, so every content octet comes out scaled by
// eight. The values below record that behaviour rather than correcting it.
const enumerated_payload_cases = [
	EnumeratedPayloadCase{
		value: 2
		want:  '10'
	},
	EnumeratedPayloadCase{
		value: -1
		want:  'f8'
	},
	EnumeratedPayloadCase{
		value: 200
		want:  '2040'
	},
	EnumeratedPayloadCase{
		value: 256
		want:  '0000'
	},
	EnumeratedPayloadCase{
		value: 300
		want:  'b060'
	},
]

fn test_enumerated_carries_the_universal_tag_ten_in_primitive_form() {
	e := Enumerated.new(2)
	assert e.tag().tag_number() == int(TagType.enumerated)
	assert e.tag().tag_class() == .universal
	assert !e.tag().is_constructed()
	assert e.value == 2
}

fn test_enumerated_payload_matches_the_current_tree() {
	for c in enumerated_payload_cases {
		e := Enumerated.new(c.value)
		got := e.payload()!
		assert got.hex() == c.want, 'Enumerated(${c.value}).payload(): ${got.hex()}, want ${c.want}'
	}
}

fn test_enumerated_str_formats_the_value() {
	assert '${Enumerated.new(2)}' == 'Enumerated (2)'
	assert '${Enumerated.new(-5)}' == 'Enumerated (-5)'
}

// ---------------------------------------------------------------------------
// GENERALSTRING
// ---------------------------------------------------------------------------

fn test_generalstring_round_trips_ascii_content() {
	g := GeneralString.new('Hello')!
	assert g.tag().tag_number() == int(TagType.generalstring)
	assert g.tag().tag_class() == .universal
	assert g.payload()! == 'Hello'.bytes(), 'payload ${g.payload()!.hex()}'
}

fn test_generalstring_new_rejects_non_ascii_content() {
	GeneralString.new('héllo') or {
		assert err.msg() == 'GeneralString: contains non-ascii chars'
		return
	}
	assert false, 'GeneralString.new with non-ascii content should have failed'
}

fn test_generalstring_str_marks_an_empty_value() {
	assert '${GeneralString.new('Hello')!}' == 'GeneralString (Hello)'
	assert '${GeneralString.new('')!}' == 'GeneralString (<empty>)'
}

// ---------------------------------------------------------------------------
// ANY DEFINED BY
// ---------------------------------------------------------------------------

fn test_any_defined_by_delegates_tag_and_payload_to_its_params() {
	empty := AnyDefinedBy.new(Null{})
	assert empty.tag().tag_number() == int(TagType.null)
	assert empty.payload()!.len == 0, 'payload ${empty.payload()!.hex()}'

	filled := AnyDefinedBy.new(Integer.from_int(5))
	assert filled.tag().tag_number() == int(TagType.integer)
	assert filled.payload()! == [u8(0x05)], 'payload ${filled.payload()!.hex()}'
}

// ---------------------------------------------------------------------------
// APPLICATION and PRIVATE class
// ---------------------------------------------------------------------------

fn test_application_element_new_keeps_tag_and_content() {
	tag := Tag.new(.application, true, 4)!
	app := ApplicationElement.new(tag, [u8(1), 2])!
	assert app.tag().equal(tag)
	assert app.tag().tag_class() == .application
	assert app.tag().tag_number() == 4
	assert app.tag().is_constructed()
	assert app.payload()! == [u8(1), 2], 'payload ${app.payload()!.hex()}'
}

fn test_application_element_new_rejects_a_tag_of_another_class() {
	ApplicationElement.new(Tag.new(.private, true, 4)!, [u8(1)]) or {
		assert err.msg() == 'Your tag is not .application'
		return
	}
	assert false, 'ApplicationElement.new with a private tag should have failed'
}

fn test_application_element_from_element_wraps_in_both_modes() {
	el := Integer.from_int(5)

	implicit := ApplicationElement.from_element(el, 4, .implicit)!
	assert implicit.tag().tag_class() == .application
	assert implicit.tag().tag_number() == 4
	// IMPLICIT inherits the constructed bit of the wrapped INTEGER, so it stays primitive.
	assert !implicit.tag().is_constructed()
	assert implicit.payload()! == [u8(0x05)], 'implicit payload ${implicit.payload()!.hex()}'

	explicit := ApplicationElement.from_element(el, 4, .explicit)!
	assert explicit.tag().tag_class() == .application
	assert explicit.tag().tag_number() == 4
	// EXPLICIT is always constructed and holds the whole TLV of the inner element.
	assert explicit.tag().is_constructed()
	assert explicit.payload()! == [u8(0x02), 0x01, 0x05], 'explicit payload ${explicit.payload()!.hex()}'
}

fn test_private_element_new_keeps_tag_and_content() {
	tag := Tag.new(.private, true, 4)!
	prv := PrivateELement.new(tag, [u8(3), 4])!
	assert prv.tag().equal(tag)
	assert prv.tag().tag_class() == .private
	assert prv.tag().tag_number() == 4
	assert prv.payload()! == [u8(3), 4], 'payload ${prv.payload()!.hex()}'
}

fn test_private_element_new_rejects_a_tag_of_another_class() {
	PrivateELement.new(Tag.new(.application, true, 4)!, [u8(1)]) or {
		assert err.msg() == 'Your tag is not private'
		return
	}
	assert false, 'PrivateELement.new with an application tag should have failed'
}

fn test_private_element_from_element_implicit_keeps_the_inner_content() {
	prv := PrivateELement.from_element(Integer.from_int(5), 4, .implicit)!
	assert prv.tag().tag_class() == .private
	assert prv.tag().tag_number() == 4
	assert !prv.tag().is_constructed()
	assert prv.payload()! == [u8(0x05)], 'payload ${prv.payload()!.hex()}'
}

// ---------------------------------------------------------------------------
// Tag.universal_tag_type
// ---------------------------------------------------------------------------

fn test_universal_tag_type_maps_the_known_numbers() {
	// Mixed-type arrays are not expressible, so each pair is spelled out.
	assert Tag.new(.universal, false, 1)!.universal_tag_type()! == .boolean
	assert Tag.new(.universal, false, 2)!.universal_tag_type()! == .integer
	assert Tag.new(.universal, false, 5)!.universal_tag_type()! == .null
	assert Tag.new(.universal, false, 10)!.universal_tag_type()! == .enumerated
	assert Tag.new(.universal, false, 12)!.universal_tag_type()! == .utf8string
	assert Tag.new(.universal, true, 16)!.universal_tag_type()! == .sequence
	assert Tag.new(.universal, true, 17)!.universal_tag_type()! == .set
	assert Tag.new(.universal, false, 22)!.universal_tag_type()! == .ia5string
	assert Tag.new(.universal, false, 26)!.universal_tag_type()! == .visiblestring
	assert Tag.new(.universal, false, 27)!.universal_tag_type()! == .generalstring
}

fn test_universal_tag_type_rejects_other_classes_and_reserved_numbers() {
	private_tag := Tag.new(.private, false, 5)!
	if _ := private_tag.universal_tag_type() {
		assert false, 'universal_tag_type on a private tag should have failed'
	} else {
		assert err.msg() == 'Not universal class type'
	}

	// 15 sits inside the reserved gap of the universal table.
	reserved := Tag.new(.universal, false, 15)!
	reserved.universal_tag_type() or {
		assert err.msg() == 'reserved or unknonw number'
		return
	}
	assert false, 'universal_tag_type on tag 15 should have failed'
}

// ---------------------------------------------------------------------------
// Size limits
// ---------------------------------------------------------------------------

fn test_sequence_set_size_accepts_and_rejects() {
	mut seq := Sequence.new()!
	seq.set_size(4)!
	assert seq.size == 4, 'sequence size ${seq.size}'

	seq.set_size(0) or {
		assert err.msg() == 'provides with correct limit'
		return
	}
	assert false, 'Sequence.set_size(0) should have failed'
}

fn test_set_set_size_accepts_and_rejects() {
	mut set := Set.new()!
	set.set_size(4)!
	assert set.size == 4, 'set size ${set.size}'

	set.set_size(-1) or {
		assert err.msg() == 'provides the correct size'
		return
	}
	assert false, 'Set.set_size(-1) should have failed'
}

// ---------------------------------------------------------------------------
// SEQUENCE OF conversions
// ---------------------------------------------------------------------------

fn test_is_sequence_of_checks_every_field() {
	seq := Sequence.from_list([Element(Integer.from_int(5)), Element(Integer.from_int(6))])!
	assert seq.is_sequence_of[Integer]()
	assert !seq.is_sequence_of[Null]()

	// An empty field list vacuously satisfies the check.
	empty := Sequence.new()!
	assert empty.is_sequence_of[Integer]()
}

// NOTE: `into_sequence_of` opens with `if seq.is_sequence_of[T]() { return
// error(...) }`, so it refuses exactly the sequences it is meant to convert.
// The assertions below record that current behaviour.
fn test_into_sequence_of_currently_rejects_a_matching_sequence() {
	seq := Sequence.from_list([Element(Integer.from_int(5)), Element(Integer.from_int(6))])!
	if _ := seq.into_sequence_of[Integer]() {
		assert false, 'into_sequence_of[T] on a SequenceOf[T] should have failed on this tree'
	} else {
		assert err.msg() == 'This sequence is not SequenceOf[T]'
	}

	empty := Sequence.new()!
	empty.into_sequence_of[Integer]() or {
		assert err.msg() == 'This sequence is not SequenceOf[T]'
		return
	}
	assert false, 'into_sequence_of[T] on an empty sequence should have failed on this tree'
}

// ---------------------------------------------------------------------------
// RawElement inner_element
// ---------------------------------------------------------------------------

fn test_raw_element_inner_element_round_trips_an_implicit_wrapper() {
	el := Integer.from_int(5)
	raw := RawElement.from_element(el, .context_specific, 5, .implicit)!
	inner := raw.inner_element()!
	assert inner.equal(el)
	assert raw.payload()! == [u8(0x05)], 'content ${raw.payload()!.hex()}'
}

fn test_raw_element_inner_element_round_trips_an_explicit_wrapper() {
	el := Integer.from_int(5)
	raw := RawElement.from_element(el, .context_specific, 5, .explicit)!
	inner := raw.inner_element()!
	assert inner.equal(el)
	assert raw.payload()! == [u8(0x02), 0x01, 0x05], 'content ${raw.payload()!.hex()}'
}

fn test_raw_element_inner_element_requires_a_tag_and_a_non_universal_class() {
	plain := RawElement.new(Tag.new(.private, false, 3)!, [u8(1)])!
	assert plain.inner_tag() == none
	if _ := plain.inner_element() {
		assert false, 'inner_element without an inner_tag should have failed'
	} else {
		assert err.msg() == 'You dont set an inner_tag'
	}

	universal := RawElement.new(Tag.new(.universal, false, 2)!, [u8(1)])!
	universal.inner_element() or {
		assert err.msg() == 'inner element from universal class is not availables'
		return
	}
	assert false, 'inner_element on a universal element should have failed'
}

fn test_raw_element_new_rejects_a_constructed_universal_tag_that_is_not_a_sequence() {
	if _ := RawElement.new(Tag.new(.universal, true, 4)!, [u8(1)]) {
		assert false, 'RawElement.new with a constructed universal OCTET STRING tag should have failed'
	} else {
		assert err.msg() == 'unexpected_tag_value required sequence or set number'
	}

	// The same number in primitive form is accepted as a plain raw element.
	RawElement.new(Tag.new(.universal, false, 4)!, [u8(1)]) or {
		assert false, 'RawElement.new with a primitive universal tag failed: ${err.msg()}'
		return
	}
}

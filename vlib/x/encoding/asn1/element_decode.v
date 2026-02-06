// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

// Handling of deserialization of bytes array into some Element.
//

// decode decodes single element from bytes, its not allowing trailing data.
//
// Examples:
//
// Original object was Utf8String with tag == 12 (0c)
// ```v
// import x.encoding.asn1
//
// original_obj := asn1.Utf8String.new('hi')!
// bytes_data := [u8(0x0C), 0x02, 0x68, 0x69]
// decoded_obj := asn1.decode(bytes_data)!
// assert decoded_obj.equal(original_obj)
// ```
pub fn decode(bytes []u8) !Element {
	// call Element.decode_with_rule directly
	el, pos := Element.decode_with_rule(bytes, 0, .der)!
	if pos > bytes.len {
		return error('decode on data with trailing data')
	}
	return el
}

// decode_with_options decodes single element from bytes with options support, its not allowing trailing data.
// Its accepts options string to drive decoding process.
//
// Examples:
//
// `UTF8String` with implicit tagging definded as `[5] IMPLICIT UTF8String` was encoded into `85 02 68 69`
//
// ```v
// original_obj := asn1.Utf8String.new('hi')!
// implicit_bytes := [u8(0x85), 0x02, 0x68, 0x69]
// obj_2 := asn1.decode_with_options(implicit_bytes, 'context_specific:5;implicit;inner:12')!
//
// assert obj_2.equal(original_obj)
// dump(obj_2) // Output: obj_2: asn1.Element(Utf8String: (hi))
// ```
//
// `UTF8String` with explicit tagging defined as `[5] EXPLICIT UTF8String` was encoded into `A5 04 0C 02 68 69`
//
// ```v
// explicit_bytes := [u8(0xA5), 0x04, 0x0C, 0x02, 0x68, 0x69]
// obj_3 := asn1.decode_with_options(explicit_bytes, 'context_specific:5;explicit;inner:0x0c')!
//
// assert obj_3.equal(original_obj)
// dump(obj_3) // output: obj_3: asn1.Element(Utf8String: (hi))
// ```
pub fn decode_with_options(bytes []u8, opt string) !Element {
	// if null-option length, call Element.decode_with_rule directly
	if opt.len == 0 {
		el, pos := Element.decode_with_rule(bytes, 0, .der)!
		if pos > bytes.len {
			return error('decode on data with trailing data')
		}
		return el
	}
	fo := FieldOptions.from_string(opt)!
	return decode_with_field_options(bytes, fo)!
}

// decode_with_field_options is similar to `decode_with_options`, but its more controllable through FieldOptions.
pub fn decode_with_field_options(bytes []u8, fo FieldOptions) !Element {
	// TODO
	if bytes.len == 0 {
		return error('Empty bytes')
	}
	fo.check_wrapper()!

	// check for optional, and return it, maybe nil optional
	// expected optional tag is outer wrapper tag
	wrp_tag := fo.wrapper_tag()!
	if fo.optional {
		opt := decode_optional(bytes, wrp_tag)!
		return opt
	}
	// read an element from bytes
	mut p := Parser.new(bytes)
	tlv := p.read_tlv()!
	p.finish()!

	// semantically no wraps
	if fo.cls == '' {
		return tlv
	}

	// wrapped
	if tlv.tag().class != wrp_tag.class {
		return error('Get different class')
	}
	if tlv.tag().number != wrp_tag.number {
		return error('Get different tag number')
	}
	// TODO: default
	return tlv.unwrap_with_field_options(fo)!
}

fn decode_optional(bytes []u8, expected_tag Tag) !Element {
	mut p := Parser.new(bytes)
	ct := p.peek_tag()!
	// when the tag is equal expected_tag, this mean, present this optional element
	if ct.equal(expected_tag) {
		// present
		el := p.read_tlv()!
		mut opt := Optional.new(el, true)!
		return opt
	}
	// optional element with no-presence semantic
	el := RawElement.new(expected_tag, []u8{})!
	opt := Optional.new(el, false)!
	return opt
}

// unwrap_with_options performs unwrapping operations to the element with options provided.
// Its technically reverse operation of the `.wrap()` applied to the element
// with the same options. If you provide with diferent options,
// the result is in undesired behaviour, even its success
pub fn (el Element) unwrap_with_options(opt string) !Element {
	if opt.len == 0 {
		return el
	}
	fo := FieldOptions.from_string(opt)!
	return el.unwrap_with_field_options(fo)!
}

// unwrap_with_field_options performs unwrapping operations with FieldOptions.
pub fn (el Element) unwrap_with_field_options(fo FieldOptions) !Element {
	if fo.cls == '' {
		// no unwrap
		return el
	}
	el.validate_options(fo)!
	// first, checks class of the element being to unwrap, should not universal class.
	if el.tag().class == .universal {
		return error('you cant unwrap universal element')
	}
	// its also happens to fo.cls, should not be an universal class
	if fo.cls == 'universal' {
		return error('you cant unwrap universal element')
	}

	// element being unwrap should have matching with tag within options.
	mode := TaggedMode.from_string(fo.mode)!
	inner_tag := fo.inner_tag()!
	if mode == .explicit {
		if !el.tag().constructed {
			return error('explicit mode should have constructed tag')
		}
		// checks inner tag from payload
		tag, _ := Tag.decode_with_rule(el.payload()!, 0, .der)!
		if !tag.equal(inner_tag) {
			return error('Get unexpected inner tag from payload')
		}
	}
	inner_form := inner_tag.constructed
	constructed := if mode == .explicit { true } else { inner_form }

	// check for tag equality
	// The tag of element being unwrapped with tag from FieldOptions should matching.
	// Its should comes from same options on wrapping.
	cls := TagClass.from_string(fo.cls)!
	if el.tag().class != cls {
		return error('unmatching outer tag class')
	}
	built_tag := Tag.new(cls, constructed, fo.tagnum)!
	// check outer tag equality
	if !el.tag().equal(built_tag) {
		return error('Element tag unequal with tag from options')
	}

	// return unwrapped element
	return unwrap(el, mode, inner_tag)!
}

// unwrap the provided element, turn into inner element.
fn unwrap(el Element, mode TaggedMode, inner_tag Tag) !Element {
	match mode {
		.explicit {
			// el.payload is serialized of inner element
			bytes := el.payload()!
			inner_elem := decode(bytes)!
			// recheck the tag
			if !inner_elem.tag().equal(inner_tag) {
				return error('unmatching inner_tag')
			}
			return inner_elem
		}
		.implicit {
			// el.payload() is content of inner element
			bytes := el.payload()!
			inner_elem := parse_element(inner_tag, bytes)!
			return inner_elem
		}
	}
}
